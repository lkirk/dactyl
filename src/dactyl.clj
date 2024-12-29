(ns dactyl
  (:require
   [clojure.java.io :as io]
   [clojure.string :as string]
   [clojure.tools.cli :refer [parse-opts]]
   [dactyl.config :refer [config-from-file config-from-string]]
   [dactyl.convert :refer [convert-scads formats]]
   [dactyl.models :refer [write-models]])
  (:gen-class))

(def cli-options
  [["-f" "--file FILE" "Configuration file" :parse-fn #(config-from-file %)]
   ["-s" "--conf CONF" "Configuration string" :parse-fn #(config-from-string %)]
   ["-o" "--out OUT" "Output dir" :parse-fn #(io/file %) :default (io/file ".")]
   ["-p" "--print-config" "Print the model configuration and exit"]
   ["-k" "--keycaps" "Add keycaps to model (useful for mockups)"]
   ["-c" "--convert FORMAT"
    "Convert to specified format (supported by openscad)"
    :parse-fn #(keyword %)
    :validate [#(if (nil? %) true (contains? formats %))
               (format "Conversion format unsupported, must be one of:\n%s"
                       (string/join " " (map name formats)))]]
   ["-h" "--help"]])

(defn usage [options-summary]
  (->>
   ["Generate SCAD files for the dactyl manuform keyboard"
    ""
    "Usage: dactyl [options]"
    ""
    "Options:"
    options-summary
    ""]
   (string/join \newline)))

(defn error-msg [errors]
  (format "Error while parsing command:\n\n%s\n"
          (string/join \newline errors)))

(defn exit [status msg]
  (println msg)
  (System/exit status))

(defn -main [& args]
  (let [{:keys [options _ errors summary]} (parse-opts args cli-options)
        kwargs {:keycaps (:keycaps options) :dry-run (:print-config options)}
        files (map #(io/file (:out options) %) '("left.scad" "right.scad"))]
    (cond
      (:help options) (exit 0 (usage summary))
      errors
      (exit 1 (error-msg errors))
      (:conf options)
      (write-models (:conf options) (:out options) kwargs)
      (:file options)
      (write-models (:file options) (:out options) kwargs)
      :else
      (exit 1 (error-msg
               '("either --file or --conf are required (see --help)"))))
    (when (:convert options)
      (convert-scads files (:convert options))))
  (shutdown-agents)) ;; clean up after using futures
