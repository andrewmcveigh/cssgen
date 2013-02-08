(ns cssgen.sass.compiler
  (:use
    [clojure.pprint])
  (:require
    [clojure.string :as string]
    [clojure.zip :as zip]
    [clojure.java.io :as io]))

(def lines (line-seq (io/reader (io/resource "screen.sass"))))

(defn get-level [line]
  (count (first (re-seq #"^\s+" line))))

(def levels
  (map #(array-map :text (string/replace % #"^\s+" "")
                   :level (get-level %)) lines))

(defn ->tree [levels] 
  (reduce (fn [coll {l2 :level :as b}]
            (let [{l1 :level text :text :as a} (last coll)]
              (if (and (and l1 l2) (< l1 l2))
                (conj (vec (butlast coll))
                      (update-in a [:content] #(->tree (conj (vec %) b))))
                (conj coll b))))
          [] levels))

(defn comment? [{text :text}]
  (when text (or (zero? (count text))
                 (re-seq #"(?:^/\*)|(?:^//)" text))))

(defn filter-tree [f coll]
  (when coll
    (if (sequential? coll)
      (remove nil? (map #(filter-tree f %) coll))
      (when-not (f coll)
        (update-in coll [:content] #(filter-tree f %))))))

(def sass-zip (filter-tree comment? {:level -1 :content (->tree levels)}))

(defn tree->rules
  ([root]
   (letfn [(condense-content [content]
             (reduce #(if (and (sequential? %2) (= 'css/rule (first %2)))
                        (concat %1 (list %2))
                        (concat %1 %2))
                     (list) (map tree->rules content)))
           (sub-property [[_ & text] {r :text}]
             (let [[a b] (string/split r #"\s+")]
               (list (keyword (apply str (concat text [\-] (rest a)))) b)))
           (read-property [{:keys [text content]}]
             (if (re-seq #"^:" text)
               (mapcat (partial sub-property text) content)
               (let [[a b] (string/split text #":\s+")]
                 (list (keyword a) b))))
           (symbolize [prms s]
             (if (and (sequential? s) (= 'css/rule (first s)))
               (cons 'css/rule (map (partial symbolize prms) (rest s)))
               (or (some #(if (= % s) (symbol s)) prms) s)))
           (read-mixin-decl [{:keys [text content]}]
             (let [[_ a & prms] (string/split text #"(?:^=)|(?:[(,\s)])+")]
               (list 'defn (symbol a) (mapv symbol prms)
                     (concat (list 'css/mixin) (map (partial symbolize prms)
                                                (condense-content content))))))
           (read-mixin-call [{text :text}]
             (let [[_ a & more] (string/split text #"(?:^\+)|(?:[(,\s)])+")]
               (list (cons (symbol a) more))))
           (gen-rule [{:keys [text content]}]
             (concat (list 'css/rule text) (condense-content content)))]
     (if (and (:level root) (>= (:level root) 0))
       (when (:text root)
         (cond (re-seq #":" (:text root)) (read-property root) 
               (re-seq #"^=" (:text root)) (read-mixin-decl root)
               (re-seq #"^\+" (:text root)) (read-mixin-call root)
               (re-seq #"^@" (:text root)) nil
               :default (gen-rule root)))
       (map tree->rules (:content root)))))
  ([root & more]
   (mapcat tree->rules (cons root more))))

(defmacro gen-sass->css-rules []
  (require '[cssgen.core :as css])
  (doseq [rule (remove nil? (tree->rules sass-zip))]
     (eval rule)))
