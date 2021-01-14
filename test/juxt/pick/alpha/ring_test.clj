;; Copyright © 2020, JUXT LTD.

(ns juxt.pick.alpha.ring-test
  (:require
   [juxt.pick.alpha.ring :as pick]
   [clojure.test :refer [deftest is]]))

(deftest api-test
  (let [variants
        [{:juxt.pick.alpha/representation-metadata
          {"content-type" "text/html;charset=utf-8"
           "content-language" "en"}}

         {:juxt.pick.alpha/representation-metadata
          {"content-type" "text/html;charset=utf-8"
           "content-language" "de"}}

         {:juxt.pick.alpha/representation-metadata
          {"content-type" "text/plain;charset=utf-8"}}]]
    (is
     (=
      "en"
      (get-in
       (:juxt.pick.alpha/representation
        (pick/pick
         {:request-method :get
          :uri "/"
          :headers {"accept" "text/html"
                    "accept-language" "en, de, es"}}
         variants))
       [:juxt.pick.alpha/representation-metadata "content-language"])))

    (is
     (=
      "de"
      (get-in
       (:juxt.pick.alpha/representation
        (pick/pick
         {:request-method :get
          :uri "/"
          :headers {"accept" "text/html"
                    "accept-language" "de"}}
         variants))
       [:juxt.pick.alpha/representation-metadata "content-language"])))

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
      (get-in
       (:juxt.pick.alpha/representation
        (pick/pick
         {:request-method :get
          :uri "/"
          :headers {"accept" "text/html"
                    "accept-language" "es, en"}}
         variants))
       [:juxt.pick.alpha/representation-metadata "content-language"])))

    (is
     (=
      "text/plain;charset=utf-8"
      (get-in
       (:juxt.pick.alpha/representation
        (pick/pick
         {:request-method :get
          :uri "/"
          :headers {"accept" "text/plain"}}
         variants))
       [:juxt.pick.alpha/representation-metadata "content-type"])))

    (is
     (=
      "text/html;charset=utf-8"
      (get-in
       (:juxt.pick.alpha/representation
        (pick/pick
         {:request-method :get
          :uri "/"
          :headers {"accept" "text/html"}}
         variants))
       [:juxt.pick.alpha/representation-metadata "content-type"])))

    (is
     (=
      "text/plain;charset=utf-8"
      (get-in
       (:juxt.pick.alpha/representation
        (pick/pick
         {:request-method :get
          :uri "/"
          :headers {"accept" "text/plain"}}
         variants))
       [:juxt.pick.alpha/representation-metadata "content-type"])))

    (is
     (=
      "text/plain;charset=utf-8"
      (get-in
       (:juxt.pick.alpha/representation
        (pick/pick
         {:request-method :get
          :uri "/"
          :headers {"accept" "text/html;q=0.8,text/plain"}}
         variants))
       [:juxt.pick.alpha/representation-metadata "content-type"])))

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
     [{:juxt.pick.alpha/representation-metadata
       {"content-type" "texthtml"}}]))))

(deftest no-metadata-key-test
  (is
   (thrown-with-msg?
    clojure.lang.ExceptionInfo
    #"Representation must have metadata .*"
    (pick/pick
     {:request-method :get
      :uri "/"
      :headers {"accept" "text/html"}}
     [{}]))))

(deftest no-content-type-test-provided
  (is
   (thrown-with-msg?
    clojure.lang.ExceptionInfo
    #"Representation must have a value for content-type"
    (pick/pick
     {:request-method :get
      :uri "/"
      :headers {"accept" "text/html"}}
     [{:juxt.pick.alpha/representation-metadata
       {}}]))))
