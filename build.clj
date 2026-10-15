(ns build
  (:require [clojure.tools.build.api :as b]
            [borkdude.gh-release-artifact :as gh]
            [deps-deploy.deps-deploy :as dd])
  (:import [clojure.lang ExceptionInfo]))

(def org "replikativ")
(def lib 'org.replikativ/partial-cps)
(def major 0)
(def minor 1)
(defn commit-count [] (b/git-count-revs nil))
(defn version [] (format "%d.%d.%s" major minor (commit-count)))
(def class-dir "target/classes")
(defn jar-file [] (format "target/%s-%s.jar" (name lib) (version)))
(def basis (delay (b/create-basis {:project "deps.edn"})))

(defn clean [_]
  (b/delete {:path "target"}))

(defn jar [_]
  (clean nil)
  (let [v (version)
        jf (jar-file)]
    (b/write-pom {:class-dir class-dir
                  :lib lib
                  :version v
                  :basis @basis
                  :src-dirs ["src" "src-core-async"]
                  :scm {:url "https://github.com/replikativ/partial-cps"
                        :connection "scm:git:git://github.com/replikativ/partial-cps.git"
                        :developerConnection "scm:git:ssh://git@github.com/replikativ/partial-cps.git"
                        :tag (str "v" v)}
                  :pom-data [[:description "Lightweight Clojure/ClojureScript library for partial continuation-passing style transformations."]
                             [:url "https://github.com/replikativ/partial-cps"]
                             [:licenses
                              [:license
                               [:name "Eclipse Public License"]
                               [:url "http://www.eclipse.org/legal/epl-v10.html"]]]]})
    (b/copy-dir {:src-dirs ["src" "src-core-async" "resources"]
                 :target-dir class-dir})
    (b/jar {:class-dir class-dir
            :jar-file jf})
    (println "JAR created:" jf)))

(defn deploy [_]
  (jar nil)
  (dd/deploy {:installer :remote
              :artifact (jar-file)
              :pom-file (b/pom-path {:lib lib :class-dir class-dir})})
  (println "Deployed to Clojars!"))

(defn install [_]
  (jar nil)
  (b/install {:basis @basis
              :lib lib
              :version (version)
              :jar-file (jar-file)
              :class-dir class-dir})
  (println "Installed to local Maven repo"))

;; ---------------------------------------------------------------------------
;; GitHub release (borkdude/gh-release-artifact), as spindel does: the JAR as
;; an asset of the release at tag vX.Y.Z, retried with a Fibonacci backoff.
;; ---------------------------------------------------------------------------

(defn fib [a b]
  (lazy-seq (cons a (fib b (+ a b)))))

(defn retry-with-fib-backoff [retries exec-fn test-fn]
  (loop [idle-times (take retries (fib 1 2))]
    (let [result (exec-fn)]
      (if (test-fn result)
        (do (println "Returned: " result)
            (if-let [sleep-ms (first idle-times)]
              (do (println "Retrying with remaining back-off times (in s): " idle-times)
                  (Thread/sleep (* 1000 sleep-ms))
                  (recur (rest idle-times)))
              result))
        result))))

(defn try-release []
  (try (gh/overwrite-asset {:org org
                            :repo (name lib)
                            :tag (str "v" (version))
                            :commit (b/git-process {:git-args "rev-parse HEAD"})
                            :file (jar-file)
                            :content-type "application/java-archive"
                            :draft false})
       (catch ExceptionInfo e
         (assoc (ex-data e) :failure? true))))

(defn release
  "Create a GitHub release at tag vX.Y.Z and upload the built JAR as an
   asset. Requires GITHUB_TOKEN env var."
  [_]
  (jar nil)
  (println "Trying to release artifact at tag v" (version))
  (let [ret (retry-with-fib-backoff 10 try-release :failure?)]
    (if (:failure? ret)
      (do (println "GitHub release failed!")
          (System/exit 1))
      (println (:url ret)))))
