(ns sector.core
  (:require
   [farolero.core :as far
    :refer [handler-bind handler-case restart-case
            values multiple-value-bind multiple-value-list
            tagbody go block return-from]])
  (:refer-clojure
   :rename {comp fn-comp
            map seq-map
            repeat fn-repeat}))

(defmethod far/report-condition ::error
  [c & {:keys [expected actual]}]
  (str "Parse error, expected " expected " but was " (pr-str actual)))

;; Parsers are functions from sequences of tokens to multiple values, the first
;; being the parsed result, the second the sequence of remaining tokens.

(def eof
  (fn [coll]
   (if (seq coll)
     (far/error ::error
                :expected "eof"
                :actual (first coll)
                :remaining coll)
     (values
      nil
      nil))))

(def none
  (fn [coll]
    (values nil coll)))

(defn return
  [val]
  (fn [coll]
    (values val coll)))

(def any
  (fn [coll]
    (if (seq coll)
      (values (first coll) (rest coll))
      (far/error ::error
                 :expected "any"
                 :actual nil
                 :remaining nil))))

(defn matches
  ([pred] (matches "to match" pred))
  ([description pred]
   (fn [coll]
     (multiple-value-bind [[v r] (any coll)]
       (if-some [ret (pred v)]
         (values ret r)
         (far/error ::error
                    :expected description
                    :actual (first coll)
                    :remaining coll))))))

(defn many
  ([parser] (many parser conj []))
  ([parser rf init]
   (fn [coll]
     (let [ret (volatile! init)
           remaining (volatile! coll)]
       (block return
         (tagbody
          loop
          (handler-case
              (do (when-not (seq @remaining)
                    (return-from return (values @ret nil)))
                  (multiple-value-bind [[v r] (parser @remaining)]
                    (vswap! ret rf v)
                    (vreset! remaining r)
                    (go loop)))
            (::error [c & {:keys [remaining]}]
              (return-from
               return
               (values
                @ret
                remaining))))))))))

(defn many1
  ([parser] (many1 parser conj []))
  ([parser rf init]
   (fn [coll]
     (multiple-value-bind [[ret remaining] (parser coll)]
       ((many parser rf (rf init ret)) remaining)))))

(defn skip-many
  [parser]
  (fn [coll]
    ((many parser (fn [_ _] nil) nil) coll)
    nil))

(defn skip-many1
  [parser]
  (fn [coll]
    ((many1 parser (fn [_ _] nil) nil) coll)
    nil))

(defn comp*
  [rf init & parsers]
  (fn [coll]
    (let [ret (volatile! init)
          remaining (volatile! coll)]
      (doseq [parser parsers]
        (multiple-value-bind [[v r] (parser @remaining)]
          (vswap! ret rf v)
          (vreset! remaining r)))
      (values
       @ret
       @remaining))))

(defn comp
  [& parsers]
  (apply comp* conj [] parsers))

(defn comp-keys
  [& parsers]
  (let [parser-pairs (partition 2 parsers)]
    (fn [coll]
      (loop [acc {}
             coll coll
             parsers parser-pairs]
        (if (seq parsers)
          (let [[k p] (first parsers)
                [v r] (multiple-value-list (p coll))]
            (recur (assoc acc k v)
                   r
                   (rest parsers)))
          (values acc coll))))))

(defn alt
  {:arglists '([& parsers] [description & parsers])}
  ([& parsers]
   (let [[description & parsers]
         (if (string? (first parsers))
           parsers
           (cons "one of the options" parsers))]
     (fn [coll]
       (block return
         (doseq [parser parsers]
           (handler-case (parser coll)
             (::error [& _])
             (:no-error [v r] (return-from return (values v r)))))
         (far/error ::error
                    :expected description
                    :actual (first coll)
                    :remaining coll))))))

(defn lookahead
  [parser]
  (fn [coll]
    (parser coll)
    (values nil coll)))

(defn lookahead-not
  ([parser] (lookahead-not "not to succeed at lookahead" parser))
  ([description parser]
   (fn [coll]
     (handler-case (parser coll)
       (::error [& _] (values nil coll))
       (:no-error [v r]
         (far/error ::error
                    :expected description
                    :actual (take (- (count coll) (count r)) coll)
                    :remaining coll))))))

(defn maybe
  [parser]
  (fn [coll]
    (handler-case (parser coll)
      (::error [& _] (values nil coll)))))

(defn if-pred
  [pred]
  (matches #(if (pred %) % nil)))

(defn map
  [f parser]
  (fn [coll]
    (f (parser coll))))

(defn expect
  ([pred parser] (expect "the predicate to pass" pred parser))
  ([description pred parser]
   (fn [coll]
     (multiple-value-bind [[v r] (parser coll)]
       (when-not (pred v)
         (far/error ::error
                    :expected description
                    :actual v
                    :remaining coll))
       (values v r)))))

(defn repeat
  [n parser]
  (apply comp (fn-repeat n parser)))

(defn parse
  [parser coll]
  (handler-case (parser coll)
    (::error [& args]
      (println (apply far/report-condition args)))
    (:no-error [ret remaining]
      ret)))

(defn parse-all
  [parser coll]
  (first (parse (comp parser eof) coll)))
