;; Copyright © 2020, JUXT LTD.

(ns juxt.pick.alpha.ring-test
  (:require
   [juxt.pick.alpha.ring :as pick]
   [clojure.test :refer [deftest is]]))

(deftest api-test
  (let [variants
        [{"content-type" "text/html;charset=utf-8"
          "content-language" "en"}

         {"content-type" "text/html;charset=utf-8"
          "content-language" "de"}

         {"content-type" "text/plain;charset=utf-8"}]]
    (is
     (=
      "en"
      (get
       (:juxt.pick.alpha/representation
        (pick/pick
         {:request-method :get
          :uri "/"
          :headers {"accept" "text/html"
                    "accept-language" "en, de, es"}}
         variants))
       "content-language")))

    (is
     (=
      "de"
      (get
       (:juxt.pick.alpha/representation
        (pick/pick
         {:request-method :get
          :uri "/"
          :headers {"accept" "text/html"
                    "accept-language" "de"}}
         variants))
       "content-language")))

    (is
     (nil?
      (:juxt.pick.alpha/representation
       (pick/pick
        {:request-method :get
         :uri "/"
         :headers {"accept" "text/html"
                   "accept-language" "es"}}
        variants))))

    (is
     (=
      "en"
      (get
       (:juxt.pick.alpha/representation
        (pick/pick
         {:request-method :get
          :uri "/"
          :headers {"accept" "text/html"
                    "accept-language" "es, en"}}
         variants))
       "content-language")))

    (is
     (=
      "text/plain;charset=utf-8"
      (get
       (:juxt.pick.alpha/representation
        (pick/pick
         {:request-method :get
          :uri "/"
          :headers {"accept" "text/plain"}}
         variants))
       "content-type")))

    (is
     (=
      "text/html;charset=utf-8"
      (get
       (:juxt.pick.alpha/representation
        (pick/pick
         {:request-method :get
          :uri "/"
          :headers {"accept" "text/html"}}
         variants))
       "content-type")))

    (is
     (=
      "text/plain;charset=utf-8"
      (get
       (:juxt.pick.alpha/representation
        (pick/pick
         {:request-method :get
          :uri "/"
          :headers {"accept" "text/plain"}}
         variants))
       "content-type")))

    (is
     (=
      "text/plain;charset=utf-8"
      (get
       (:juxt.pick.alpha/representation
        (pick/pick
         {:request-method :get
          :uri "/"
          :headers {"accept" "text/html;q=0.8,text/plain"}}
         variants))
       "content-type")))

    (is
     (=
      ["accept" "accept-language"]
      (:juxt.pick.alpha/vary
       (pick/pick
        {:request-method :get
         :uri "/"
         :headers {"accept" "text/html"}}
        variants
        {:juxt.pick.alpha/vary? true}))))))

(deftest malformed-content-type-test
  (is
   (thrown-with-msg?
    clojure.lang.ExceptionInfo
    #"Malformed content-type"
    (pick/pick
     {:request-method :get
      :uri "/"
      :headers {"accept" "text/html"}}
     [{"content-type" "texthtml"}]))))
