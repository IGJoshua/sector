(ns lojban.morphology
  (:require
   [clojure.string :as str]
   [farolero.core :as far
    :refer [handler-bind handler-case restart-case
            values multiple-value-bind multiple-value-list
            tagbody go block return-from]]
   [lojban.common :as c]
   [sector.core :as p]))

(defn word
  [s]
  (p/expect (str "the word " (pr-str s))
            #{s}
            (p/map (partial apply str) (p/repeat (count s) p/any))))

(def space
  (p/matches "witespace" #{\space \newline \tab \.}))

(defn skip-space
  [parser]
  (p/map first (p/comp parser (p/skip-many space))))

(def jbovla
  (p/map (fn [ch-seq]
           (let [word (apply str ch-seq)]
             {:word word
              :selmaho (keyword (str/replace word #"'" "h"))}))
         (skip-space (p/many1 (p/map second (p/comp (p/lookahead-not space) p/any))))))

(def delimited-quote
  (fn [coll]
    (let [[[_ delimiter] remainder]
          (multiple-value-list
           ((skip-space
             (p/comp (p/skip-many space)
                     jbovla
                     (p/alt "the end of the delimiter" (p/matches #{\.}) space)))
            coll))
          delim (word delimiter)
          [quoted-text remainder]
          (multiple-value-list
           (apply
            str
            ((p/many1 (p/comp (p/lookahead-not (p/comp (p/skip-many1 space) delim)) p/any)
                      ((map second) conj) [])
             remainder)))
          [_ remainder] (multiple-value-list
                         ((skip-space (p/comp (p/skip-many1 space) delim))
                          remainder))]
      (values
       {:delimiter delimiter
        :quote quoted-text}))))

(def zoi-clause
  (p/map #(assoc (:rest %)
                 :selmaho :zoi
                 :word "zoi")
         (skip-space
          (p/comp-keys :word (word "zoi")
                       :rest delimited-quote))))

(def laho-clause
  (p/map #(assoc (:rest %)
                 :selmaho :laho
                 :word "la'o")
         (skip-space
          (p/comp-keys :word (word "la'o")
                       :rest delimited-quote))))

(def zohoi-clause
  (p/comp-keys :word (word "zo'oi")
               :selmaho (p/return :zohoi)
               :quote (skip-space
                       (p/map (comp (partial apply str) second)
                              (p/comp (p/skip-many space)
                                      (p/many (p/map second (p/comp (p/lookahead-not space) p/any))))))))
