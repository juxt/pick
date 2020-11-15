;; Copyright © 2020, JUXT LTD.

(ns juxt.pick.alpha.api-test
  (:require
   [juxt.pick.alpha.api :as pick]
   [clojure.test :refer [deftest is]]))

(deftest api-test
  (let [variants
        [{:juxt.pick/content-type "text/html;charset=utf-8"
          :juxt.pick/content-language "en"}

         {:juxt.pick/content-type "text/html;charset=utf-8"
          :juxt.pick/content-language "de"}

         {:juxt.pick/content-type "text/plain;charset=utf-8"}]]
    (is
     (=
      "en"
      (:juxt.pick/content-language
       (:juxt.pick/representation
        (pick/pick
         {:request-method :get
          :uri "/"
          :headers {"accept" "text/html"
                    "accept-language" "en, de, es"}}
         variants)))))

    (is
     (=
      "de"
      (:juxt.pick/content-language
       (:juxt.pick/representation
        (pick/pick
         {:request-method :get
          :uri "/"
          :headers {"accept" "text/html"
                    "accept-language" "de"}}
         variants)))))

    (is
     (nil?
      (:juxt.pick/representation
       (pick/pick
        {:request-method :get
         :uri "/"
         :headers {"accept" "text/html"
                   "accept-language" "es"}}
        variants))))

    (is
     (=
      "en"
      (:juxt.pick/content-language
       (:juxt.pick/representation
        (pick/pick
         {:request-method :get
          :uri "/"
          :headers {"accept" "text/html"
                    "accept-language" "es, en"}}
         variants)))))

    (is
     (=
      "text/plain;charset=utf-8"
      (:juxt.pick/content-type
       (:juxt.pick/representation
        (pick/pick
         {:request-method :get
          :uri "/"
          :headers {"accept" "text/plain"}}
         variants)))))

    (is
     (=
      "text/html;charset=utf-8"
      (:juxt.pick/content-type
       (:juxt.pick/representation
        (pick/pick
         {:request-method :get
          :uri "/"
          :headers {"accept" "text/html"}}
         variants)))))

    (is
     (=
      "text/plain;charset=utf-8"
      (:juxt.pick/content-type
       (:juxt.pick/representation
        (pick/pick
         {:request-method :get
          :uri "/"
          :headers {"accept" "text/plain"}}
         variants)))))

    (is
     (=
      "text/plain;charset=utf-8"
      (:juxt.pick/content-type
       (:juxt.pick/representation
        (pick/pick
         {:request-method :get
          :uri "/"
          :headers {"accept" "text/html;q=0.8,text/plain"}}
         variants)))))

    (is
     (=
      ["accept" "accept-language"]
      (:juxt.pick/vary
       (pick/pick
        {:request-method :get
         :uri "/"
         :headers {"accept" "text/html"}}
        variants
        {:juxt.pick/vary? true}))))))
