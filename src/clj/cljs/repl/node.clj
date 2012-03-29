(ns cljs.repl.node
  (:refer-clojure :exclude [loaded-libs])
  (:require [clojure.string :as string]
            [clojure.java.io :as io]
            [cljs.compiler :as comp]
            [cljs.repl :as repl])
  (:import cljs.repl.IJavaScriptEnv
           java.io.PipedReader
           java.io.PipedWriter))

(def loaded-libs (atom #{}))

(defn- load-as-tempfile
  "Copy a file from the classpath into a temporary file.
  Return the path to the temporary file."
  [filename]
  (let [tempfile (java.io.File/createTempFile "cljsrepl" ".js")
        resource (io/resource filename)]
    (.deleteOnExit tempfile)
    (assert resource (str "Can't find " filename " in classpath"))
    (with-open [in (io/input-stream resource)
                out (io/output-stream tempfile)]
      (io/copy in out))
    (.getAbsolutePath tempfile)))

(defn- output-filter
  "Take a reader and wrap a filter around it which swallows and
  acts on output events from the subprocess. Keep the filter
  thread running until alive-func returns false."
  [reader alive-func]
  (let [pipe (PipedWriter.)]
    (future
      (while (alive-func)
        (let [line (.readLine reader)
              data (read-string line)]
          (if-let [output (:output data)]
            (print output)
            (doto pipe
              (.write (str line "\n"))
              (.flush))))))
    (io/reader (PipedReader. pipe))))

(defn- process-alive?
  "Test if a process is still running."
  [^Process process]
  (try (.exitValue process) false
       (catch IllegalThreadStateException e true)))

(defn- launch-node-process
  "Launch the Node subprocess."
  []
  ; Launch repl.js through an eval to trick Node into thinking it was
  ; started from the current directory, allowing require() to work as
  ; expected.
  (let [launch-script (str "eval(require(\"fs\").readFileSync(\"" (load-as-tempfile "repl.js") "\",\"utf8\"))")
        process (let [pb (ProcessBuilder. ["node" "-e" launch-script])]
                  (.start pb))]
    {:input (output-filter (io/reader (.getInputStream process)) #(process-alive? process))
     :output (io/writer (.getOutputStream process))}))

(defn js-eval [env filename line code]
  (let [{:keys [input output]} env]
    (.write output (str {:file filename :line line :code code} "\n"))
    (.flush output)
    (let [result-string (.readLine input)
          result (read-string result-string)]
      result)))

(defn node-setup [repl-env]
  (repl/load-file repl-env "cljs/core.cljs")
  (let [env {:context :statement :locals {} :ns (@comp/namespaces comp/*cljs-ns*)}]
    (repl/evaluate-form repl-env env "<cljs repl>" '(ns cljs.user (:require [cljs.nodejs :as node])))))

(defn node-eval [repl-env filename line js]
  (let [result (js-eval repl-env filename line js)]
    (if-let [error (:error result)]
      {:status :exception :value (:stack error)}
      {:status :success :value (:result result)})))

(defn- object-query-str
  "Given a list of goog namespaces, create a JavaScript string which,
  when evaluated, will return true if all of the namespaces exist and
  false if any do not exist."
  [ns]
  (str "(" (apply str (interpose " && " (map #(str "goog.getObjectByName('" (name %) "')") ns))) ")?true:false;"))

(defn load-javascript [repl-env ns url]
  (let [missing (remove #(contains? @loaded-libs %) ns)]
    (when (seq missing)
      (let [res (js-eval repl-env "<cljs repl>" 1 (object-query-str ns))]
        (when-not (:result res)
          (js-eval repl-env (.getPath url) 1 (slurp url))))
      (swap! loaded-libs (partial apply conj) missing))))

(defn node-tear-down [repl-env]
  (let [process (:process repl-env)]
    (doto process
      (.destroy)
      (.waitFor))))

(defn load-resource
  "Load a JS file from the classpath into the REPL environment."
  [env filename]
  (let [resource (io/resource filename)]
    (assert resource (str "Can't find " filename " in classpath"))
    (js-eval env filename 1 (slurp resource))))

(extend-protocol repl/IJavaScriptEnv
  clojure.lang.IPersistentMap
  (-setup [this]
    (node-setup this))
  (-evaluate [this filename line js]
    (node-eval this filename line js))
  (-load [this ns url]
    (load-javascript this ns url))
  (-tear-down [this]
    (node-tear-down this)))

(defn repl-env
  "Create a Node.js REPL environment."
  []
  (doto (launch-node-process)
    (load-resource "goog/base.js")
    (load-resource "goog/deps.js")))
