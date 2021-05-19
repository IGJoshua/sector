(ns lojban.grammar
  (:require
   [clojure.string :as str]
   [farolero.core :as far
    :refer [handler-bind handler-case restart-case
            values multiple-value-bind multiple-value-list
            tagbody go block return-from]]
   [lojban.common :as c]
   [sector.core :as p]))

(declare bridi mods selbri term)

(defn of-selmaho
  [selmaho]
  (p/expect (str "selma'o " (str/upper-case (name selmaho)))
            (comp #{selmaho} :selmaho)
            p/any))

(def sei-clause
  (p/comp-keys :sei (mods (of-selmaho :sei))
               :bridi (mods #'bridi)
               :sehu (p/maybe (mods (of-selmaho :sehu)))))

(def coi-clause
  (p/comp-keys :coi (mods (of-selmaho :coi))
               :greeted (p/alt "a valid selbri, cmevla, or sumti"
                               selbri
                               (of-selmaho :cmevla)
                               term)))

(def free-modifier
  (p/alt "an attitudinal, SEI, COI, or similar clause"
         (of-selmaho :ui)
         sei-clause
         coi-clause))

(defn mods
  [parser]
  (p/map #(if (seq (second %))
            (assoc (first %) :modifiers (second %))
            (first %))
         (p/comp parser
                 (p/many free-modifier))))

(def i-clause
  (mods (of-selmaho :i)))

(def sentence-start
  (p/alt "a NIhO or I clause, without a connective"
   (p/map first (p/comp i-clause
                        (p/comp (p/lookahead-not
                                 (p/comp (p/many (mods (of-selmaho :bai)))
                                         (of-selmaho :bo)))
                                (p/lookahead-not
                                 (of-selmaho :ja)))))
   (mods (of-selmaho :niho))))

(def sentence-link
  (mods
   (p/comp-keys :i i-clause
                :joiner (mods
                         (p/alt "a connective"
                                (p/comp-keys :bai (p/many (mods (of-selmaho :bai)))
                                             :bo (of-selmaho :bo))
                                (of-selmaho :ja))))))

(def prenex (mods (of-selmaho :zohu)))

(def connective (of-selmaho :ja))
(def term (of-selmaho :koha))
(def selbri (of-selmaho :selbri))
(def bridi-tail
  (p/comp-keys :cu (p/maybe (mods (of-selmaho :cu)))
               :selbri selbri
               :sumti (p/many term)))
(def bridi
  (p/comp-keys :head (p/many term)
               :tails (p/map #(cons (first %) (second %))
                       (p/comp bridi-tail
                               (p/many (p/comp-keys :link connective
                                                    :tail bridi-tail))))))

(def jufra
  (p/comp-keys :prenex (p/maybe prenex)
               :bridi (p/map #(cons (first %) (second %))
                             (p/comp bridi
                                     (p/many
                                      (p/comp-keys :link sentence-link
                                                   :bridi bridi))))
               :iau-clause (p/maybe (mods (of-selmaho :iau)))))

(def mulno-jufra
  (p/map #(assoc (:jufra %) :sentence-start (:sentence-start %))
         (p/comp-keys :sentence-start sentence-start
                      :jufra jufra)))

(def text
  (p/map (fn [[mods init-jufra more]]
           {:modifiers mods
            :jufra (cond
                     (and init-jufra (seq more)) (cons init-jufra more)
                     (seq more) (seq more)
                     init-jufra (list init-jufra)
                     :else nil)})
         (p/comp free-modifier
                 (p/maybe jufra)
                 (p/many mulno-jufra))))
