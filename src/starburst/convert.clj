(ns starburst.convert)

(defn reformat-name
  [s & {:keys [sep] :or {sep \-}}]
  (let [replacement (str "$1" sep "$2")]
    (-> s
        (clojure.string/replace #"(.)([A-Z][a-z]+)" replacement)
        (clojure.string/replace #"([a-z0-9])([A-Z])" replacement)
        (clojure.string/lower-case))))

(defn str->short-ns-kword
  [s]
  (symbol (str "::" s)))

(defn str->ns-kword
  [s & {:keys [nms] :or {nms "primitives"}}]
  `(clojure.spec/get-spec ~(symbol (str ":orion." nms "/" s))))
