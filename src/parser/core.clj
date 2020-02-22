(ns parser.core
  (:require [clojure.string :as str]))

(defn partial-apply
  [f p]
  (try
    (f p)
    (catch clojure.lang.ArityException _ (partial f p))))

(defn parse
  [pf str]
  (pf str))

(defn pure [v]
  (fn [inp] [v inp]))

(def item (fn [inp]
            (if (= inp "")
              nil
              [(first inp) (subs inp 1)])))

(defn fmap
  [f v]
  (fn [inp]
    (let [out (parse v inp)
          [x s] out]
      (if (some? out)
        [(partial-apply f x) s]))))

(defn <*>
  ([a b]
   (fn [inp]
     (let [out (parse a inp)
           [f s] out]
       (if (some? out)
         (parse (fmap f b) s)))))
  ([a b & c]
   (apply <*> (<*> a b) c)))

(defn >>=
  ([a b]
   (fn [inp]
     (let [out (parse a inp)
           [x s] out]
       (if (some? out)
         (parse (b x) s)))))
  ([a b & c]
   (apply >>= (>>= a b) c)))

(defn por
  ([a b]
   (fn [inp]
     (let [o (parse a inp)]
       (if (some? o)
         o
         (parse b inp)))))
  ([a b & c]
   (apply por (por a b) c)))

(defn pany
  [p]
  (fn [inp]
    (loop [inputstr inp
           com_out []]
      (let [out (parse p inputstr)
            [x s] out]
        (if (some? out)
          (recur s (conj com_out x))
          [com_out inputstr])))))

(defn psome
  [p]
  (fn [inp]
    (let [[x s] (parse (pany p) inp)]
      (if (empty? x)
        nil
        [x s]))))

(defmacro doMonad
  ([l f]
   (if (empty? l)
     f
     (let [[a b] l
           restOfList (vec (drop 2 l))]
       `(>>= ~b
             (fn [~a] (doMonad ~restOfList ~f)))))))

(defn acc [f]
  (fn [inp]
    (let [o (parse item inp)
          [x, s] o]
      (if (and (some? o)
               (f x))
        o))))

(def pNumber (acc #(java.lang.Character/isDigit  %)))
(def pUnsignedInt (fmap #(read-string (apply str %))
                        (psome pNumber)))

(def pInt (por pUnsignedInt
               (doMonad
                [_ (acc #(= \- %))
                 num pUnsignedInt]
                (pure (* -1 num)))))

(def pwhitespace (acc #(java.lang.Character/isWhitespace %)))
(def cleanWhitespace (pany pwhitespace))

(def pletter (acc #(java.lang.Character/isLetter %)))
(def pword (doMonad [_ cleanWhitespace
                     word (fmap #(apply str %) (psome pletter))]
                    (pure word)))

(def special-char (acc #(contains? #{\- \< \> \{ \}} %)))
(def letter-or-special (por pletter special-char))
(def psword (doMonad [_ cleanWhitespace
                      word (fmap #(apply str %) (psome letter-or-special))]
                     (pure word)))

(defn pkeyword [kw]
  (fn [inp]
    (let [o (parse psword inp)
          [x s] o]
      (if (and (some? o) (= x kw))
        o nil))))


