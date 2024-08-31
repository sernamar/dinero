(ns dev
  (:require [clojure.xml :as xml]
            [clojure.java.io :as io]
            [clojure.pprint :as pprint]
            [clojure.string :as str]))

(defn- currency->map
  "Transforms a currency entry into a map."
  [currency]
  (let [transformed (->> currency
                         (map (fn [{:keys [tag content]}]
                                [tag (first content)]))
                         (into {}))
        currency (:Ccy transformed)]
    (when currency
      (let [currency-code (:Ccy transformed)
            currency (keyword (str/lower-case currency-code))
            currency-minor-units (some-> (:CcyMnrUnts transformed) parse-long)]
        {currency {:type :iso-4217
                   :currency-code currency-code                   
                   :minor-units currency-minor-units}}))))

(defn- parse-currency-xml
  "Parses a ISO 4217 XML data source and returns a map with the currencies.

  The source can be a File, InputStream or String naming a URI."
  [source]
  (let [doc (-> source xml/parse)]
    (->> doc
         xml-seq
         (filter (comp #{:CcyNtry} :tag))
         (map :content)
         (map currency->map)
         (remove nil?) ; remove nil values (entries where 'Ccy' is missing)
         (apply merge-with merge) ; merge maps with the same key (duplicate currencies)
         (into (sorted-map)))))

(defn currency-xml-file->edn
  "Parses a ISO 4217 XML data source from a file and writes the currencies to an EDN file."
  [xml-filepath edn-filepath]
  (let [file (-> xml-filepath io/resource io/file)
        parsed (parse-currency-xml file)]
    (with-open [writer (io/writer edn-filepath)]
      (binding [*out* writer]
        (pprint/pprint parsed)))))

(defn currency-xml->edn
  "Parses a ISO 4217 XML data source from its URI and writes the currencies to an EDN file."
  [edn-filepath]
  (let [uri "https://www.six-group.com/dam/download/financial-information/data-center/iso-currrency/lists/list-one.xml"
        parsed (parse-currency-xml uri)]
    (with-open [writer (io/writer edn-filepath)]
      (binding [*out* writer]
        (pprint/pprint parsed)))))

(comment
  (currency-xml-file->edn "iso-4217.xml" "resources/currencies.edn")
  (currency-xml->edn "resources/currencies.edn")
  )

