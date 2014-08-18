(ns wolfram-plaintext-api.core
  (:gen-class))

(require '[clojure.xml :as xml]
         '[clojure.zip :as zip]
         '[clojure.string :as str])

(import [java.net URLEncoder])

(defn gen-url
  [api-key question]
  (let [query (URLEncoder/encode question)]
    (str "http://api.wolframalpha.com/v2/query?appid="
         api-key
         "&input="
         query
         "&format=plaintext")))

(defn url->parsed-xml
  [url]
  (get ; parse returns two parts, one empty
   (zip/xml-zip
    (xml/parse (java.io.ByteArrayInputStream. (.getBytes (slurp url))))) 0))

(defn make-query
  [api-key question]
  (url->parsed-xml (gen-url api-key question)))

(defn success?
  "Returns boolean whether query worked or not"
  [data]
  (= "true" (get-in data [:attrs :success])))

(defn get-pods
  "Gets all pods"
  [data]
  (filter #(= (:tag %) :pod) (:content data)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defn get-from-attr                                       ;;
;;   "Grabs pod with given attr (:title :id, etc)"           ;;
;;   [data attr]                                             ;;
;;   (for [pod (get-pods data)                               ;;
;;         :when (contains? pod (get-in pod [:attrs attr]))] ;;
;;     pod))                                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-plaintext-from-pod
  [pod]
  (get-in (into {} pod) [:content 0 :content 0 :content]))

(defn get-pod-id
  [pod]
  (get-in pod [:attrs :id]))

(defn filter-by-id
  "Given vector of ids returns all pods with ids"
  [pods [& ids]]
  (filter #(contains? (set ids) (get-pod-id %)) pods))

(defn filter-out-id
  "Given vector of ids returns all pods without ids"
  [pods [& ids]]
  (filter #(not (contains? (set ids) (get-pod-id %))) pods))

(defn get-first-non-input-pod
  [pods]
  (first
    (filter-out-id pods ["Input"])))

(defn lucky-guess
  [data]
  (let [good-ids [""]]))

(defn first-result
  [data]
  (-> data
      (get-pods)
      (get-first-non-input-pod)
      (get-plaintext-from-pod)))

(defn feeling-lucky-query
  [api question]
  (first-result (make-query api question)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
