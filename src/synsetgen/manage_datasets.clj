(ns synsetgen.manage-datasets
  (:require [clojure.java.io :as io])
  (:import (java.io File)))

(defn list-file-absolute-paths
  [directory]
  (map #(.getAbsolutePath %)
       (.listFiles (File. directory))))

(defn list-file-names
  [directory]
  (map #(.getName %)
       (.listFiles (File. directory))))

(defn get-filelist
  "Get list of annotation files of form <some name>.<extension> "
  [directory extension]
  (sort
   (filter #(> (.indexOf % (str "." extension)) 0)
           (filter #(< (.indexOf % "~") 0)
                   (list-file-absolute-paths directory)))))

(defn list-user-datasets
  [username]
  (list-file-names (format "resources/public/output/%s" username)))

(defn get-filelist
  "Get list of annotation files of form <some name>.<extension> "
  [directory extension]
  (sort
   (filter #(> (.indexOf % (str "." extension)) 0)
           (filter #(< (.indexOf % "~") 0)
                   (list-file-absolute-paths directory)))))

(defn map-user-datasets
  "Generate a tree of user datasets and containing files."
  [workdir username]
  (reduce (fn [newmap datasetname]
            (assoc newmap datasetname (list-file-names (format "%s/%s/%s/"
                                                               workdir username datasetname))))
          {} (list-file-names (format "%s/%s" workdir username))))

(defn map-user-dataset-filename
  "Map filename to dataset containing file."
  [workdir username datasetname filename]
  (format "%s/%s/%s/%s" workdir username datasetname filename))




