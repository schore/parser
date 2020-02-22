(ns parser.core-test
  (:require [clojure.test :refer :all]
            [parser.core :refer :all]))

(deftest test-item
  (is (= [\F , "oo"] (parse item "Foo")))
  (is (nil? (parse item ""))))

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

(deftest doMonad-test
  (def f (doMonad [a pInt
                   _ cleanWhitespace
                   b pInt]
                  (pure [a b])))
  (is (= [[1 2] , ""] (parse f "1 2")))
  (is (nil? (parse f "1 aba")))
  (is (nil? (parse f "aba 2 3")))
  (is (= [[-12 32], "aba"] (parse f "-12 32aba"))))

(deftest pnumber-test
  (is (= [\1, "adfa2"] (parse pNumber "1adfa2")))
  (is (nil? (parse pNumber "ad")))
  (is (nil? (parse pNumber ""))))


(deftest keyword-test
  (def p (pkeyword "Foo"))
  (is (= ["Foo" " Bar"] (parse p "Foo Bar")))
  (is (nil? (parse p "Bar")))
  (is (nil? (parse p "FooBar")))
  (is (= ["Foo" ""] (parse p "Foo"))))


(deftest word-test
  (is (= ["Foo" " Bar"] (parse pword "Foo Bar")))
  (is (= ["Foo" " Bar"] (parse pword "  Foo Bar")))
  (is (nil? (parse pword "")))
  (is (nil? (parse pword "-12")))
  (is (= ["fo" "1"] (parse pword "fo1"))))

(deftest cleanWhitespace-test
  (is (= "Foo" (second (parse cleanWhitespace "   Foo"))))
  (is (= "Foo" (second (parse cleanWhitespace "Foo")))))


(deftest pint-test
  (is (= [123 "Foo"] (parse pInt "123Foo")))
  (is (= [-123 "Foo"] (parse pInt "-123Foo"))))


(deftest fmap-test
  (def p (fmap (fn [x] 1) item))
  (is (= [1 "oo"] (parse p "Foo")))
  (is (= [1 "oo"] (parse p "_oo")))
  (is (nil? (parse p ""))))

(deftest applicative-test
  (def p (<*> (pure #(.toUpperCase (str %))) pletter))
  (is (= ["F" "oo"] (parse p "foo")))
  (is (nil? (parse p "1foo")))
  (def p2 (<*> (pure (fn [x1 x2] [x1 x2]))
               item item))
  (is (= [[\1 \2] "bar"] (parse p2 "12bar")))
  (is (nil? (parse p2 "1")))
  (is (nil? (parse p2 ""))))


(deftest pany-test
  (is (= [[\f \o \o] ""] (parse (pany item) "foo")))
  (is (= [[] ""] (parse (pany item) ""))))

(deftest psome-test
  (is (= [[\B \l \a] ""] (parse (psome item) "Bla")))
  (is (nil? (parse (psome item) ""))))

