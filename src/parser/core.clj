(ns parser.core
  (:require [clojure.string :as str]
            [clojure.walk :as w]))

(defrecord Parser [pf])

(defn partial-apply
  [f p]
  (try
    (f p)
    (catch clojure.lang.ArityException _ (partial f p))))

(defn parse
  [p str]
  ((:pf p) str))

(defn pure [v]
  (Parser. (fn [inp] [v inp])))

(def item (Parser. (fn [inp]
                     (if (= inp "")
                       nil
                       [(first inp) (subs inp 1)]))))

(defn fmap
  [f v]
  (Parser. (fn [inp]
             (let [out (parse v inp)
                   [x s] out]
               (if (some? out)
                 [(partial-apply f x) s])))))

(defn <*>
  ([a b]
   (Parser. (fn [inp]
              (let [out (parse a inp)
                    [f s] out]
                (if (some? out)
                  (parse (fmap f b) s))))))
  ([a b & c]
   (apply <*> (<*> a b) c)))

(defn >>=
  ([a b]
   (Parser. (fn [inp]
              (let [out (parse a inp)
                    [x s] out]
                (if (some? out)
                  (parse (b x) s))))))
  ([a b & c]
   (apply >>= (>>= a b) c)))

(defn por
  ([a b]
   (Parser. (fn [inp]
              (let [o (parse a inp)]
                (if (some? o)
                  o
                  (parse b inp))))))
  ([a b & c]
   (apply por (por a b) c)))

(defn pany
  [p]
  (Parser. (fn [inp]
             (loop [inputstr inp
                    com_out []]
               (let [out (parse p inputstr)
                     [x s] out]
                 (if (some? out)
                   (recur s (conj com_out x))
                   [com_out inputstr]))))))

(defn psome
  [p]
  (Parser. (fn [inp]
             (let [[x s] (parse (pany p) inp)]
               (if (empty? x)
                 nil
                 [x s])))))


(defmacro doMonad
  ([l f]
   (if (empty? l)
     f
     (let [[a b] l
           restOfList (vec (drop 2 l))]
       `(>>= ~b
             (fn [~a] (doMonad ~restOfList ~f)))))))

(macroexpand '(doMonad [a b c d] (e)))

(w/macroexpand-all '(doMonad [a pNumber
                              b pInt]
                             (pure [a b])))

(defn acc [f]
  (Parser. (fn [inp]
             (let [o (parse item inp)
                   [x, s] o]
               (if (and (some? o)
                        (f x))
                 o)))))

(def pNumber (acc #(java.lang.Character/isDigit  %)))
(def pUnsignedInt (fmap #(read-string (apply str %))
                        (psome pNumber)))

(def pInt (por pUnsignedInt
               (doMonad
                [_ (acc #(= \- %))
                 num pUnsignedInt]
                (pure (* -1 num)))))

(def pletter (acc #(java.lang.Character/isLetter %)))
(def pword (fmap #(apply str %) (psome pletter)))

(def special-char (acc #(contains? #{\- \< \> \{ \}} %)) )
(def letter-or-special (por pletter special-char))
(def psword (fmap #(apply str %) (psome letter-or-special)))

(defn pkeyword [kw]
  (Parser. (fn [inp]
             (let [o (parse psword inp)
                   [x s] o]
               (if (and (some? o) (= x kw))
                 o nil)))))


(def pwhitespace (acc #(java.lang.Character/isWhitespace %)))
(def cleanWhitespace (pany pwhitespace))

;; Test my parsers



(parse item "Foo")

(parse (>>= item
            (fn [_] (>>= item
                         (fn [x1]
                           (>>= item
                                (fn [x2]
                                  (pure [x1 x2])))))))
       "Fooo")


(def transition (doMonad [_ cleanWhitespace
                          _ (pkeyword "transition")
                          _ cleanWhitespace
                          from pword
                          _ cleanWhitespace
                          _ (pkeyword "->")
                          _ cleanWhitespace
                          to pword]
                         (pure {:transiton true
                                :from from
                                :to to})))

(defn action [type]
  (doMonad [_ cleanWhitespace
            _ (pkeyword type)
            _ cleanWhitespace
            n pword]
           (pure [(keyword type) n])))

(def entryAction (action "entry"))
(def doAction (action "do"))
(def exitAction (action "exit"))

(parse entryAction "entry Fooo")

(parse transition "transition stateA -> stateB")

(def f (doMonad [a pInt
                 _ cleanWhitespace
                 b pInt]
                (pure [a b])))

(parse pNumber "1adfa2")

(parse (psome pNumber) "123")


(parse f "4  -233432342 fooo")

(parse (pkeyword "Foo") "Foo Bar")

(parse pword "Foo Bar")

(parse cleanWhitespace "Fooo")

(parse pInt "-123 Foo")

(parse pUnsignedInt "123")

(parse (pany pNumber) "A12Fo")

(parse item "Fooo")
(parse item "")

(parse (fmap (fn [x] 1) item) "foo")

(parse (fmap (fn [x] 1) item) "")

(parse (<*> (pure #(.toUpperCase (str %)))
            item) "foo")

(parse (pany item) "foo")
(parse (pany item) "")

(parse (psome item) "Bla")
(parse (psome item) "")

(parse (<*> (pure (fn [x1 x2] [x1 x2]))
            item
            item)
       "Fooo")

