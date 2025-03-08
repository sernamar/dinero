(ns build
  (:require [clojure.tools.build.api :as b]
            [deps-deploy.deps-deploy :as dd]))

(def lib 'com.sernamar/dinero)
(def version "0.2.1")

(def target-dir "target/")
(def class-dir (str target-dir "classes/"))
(def basis (b/create-basis {}))
(def jar-file (str target-dir (format "%s-%s.jar" (name lib) version)))

(defn- jar-opts
  [opts]
  (assoc opts
         :lib lib
         :version version
         :jar-file  jar-file
         :basis     basis
         :class-dir class-dir
         :target    target-dir
         :src-dirs  ["src"]))

(defn jar
  [opts]
  (let [opts (jar-opts opts)]
    (b/copy-dir {:src-dirs ["src" "resources"]
                 :target-dir class-dir})
    (b/jar opts)))

(defn uber
  [opts]
  (let [opts (jar-opts opts)]
    (b/copy-dir {:src-dirs ["src" "resources"]
                 :target-dir class-dir})
    (b/compile-clj opts)
    (b/uber opts)))

(defn deploy
  [opts]
  (let [opts (jar-opts opts)]
    (b/delete {:path class-dir})
    (b/write-pom opts)
    (jar opts)
    (dd/deploy {:installer :remote
                :artifact (b/resolve-path (:jar-file opts))
                :pom-file (b/pom-path opts)})))

(defn install
  "Install built jar to local maven repo."
  [opts]
  (jar opts)
  (b/install (jar-opts opts))
  (println "Installed version" lib version))
