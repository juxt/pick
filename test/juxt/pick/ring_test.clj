;; Copyright © 2020-2024, JUXT LTD.

(ns juxt.pick.ring-test
  (:require
   [juxt.pick.ring :as pick.ring]
   [clojure.test :refer [deftest is]]))

(def variants [{:juxt.http/content-type "text/html;charset=utf-8"
                :juxt.http/content-language "en"}

               {:juxt.http/content-type "text/html;charset=utf-8"
                :juxt.http/content-language "de"}

               {:juxt.http/content-type "text/plain;charset=utf-8"}])

(deftest api-test
  (let [variants
        [{:juxt.http/content-type "text/html;charset=utf-8"
          :juxt.http/content-language "en"}

         {:juxt.http/content-type "text/html;charset=utf-8"
          :juxt.http/content-language "de"}

         {:juxt.http/content-type "text/plain;charset=utf-8"}]]
    (is
     (=
      "en"
      (get-in
       (pick.ring/pick
        {:ring.request/method :get
         :ring.request/path "/"
         :ring.request/headers {"accept" "text/html"
                                "accept-language" "en, de, es"}}
        variants)
       [:juxt.pick/representation :juxt.http/content-language])))

    (is
     (=
      "de"
      (get-in
       (pick.ring/pick
        {:ring.request/method :get
         :ring.request/path "/"
         :ring.request/headers {"accept" "text/html"
                                "accept-language" "de"}}
        variants)
       [:juxt.pick/representation :juxt.http/content-language])))

    (is
     (nil?
      (:juxt.pick/representation
       (pick.ring/pick
        {:ring.request/method :get
         :ring.request/path "/"
         :ring.request/headers {"accept" "text/html"
                                "accept-language" "es"}}
        variants))))

    (is
     (=
      "en"
      (get-in
       (pick.ring/pick
        {:ring.request/method :get
         :ring.request/path "/"
         :ring.request/headers {"accept" "text/html"
                                "accept-language" "es, en"}}
        variants)
       [:juxt.pick/representation :juxt.http/content-language])))

    (is
     (=
      "text/plain;charset=utf-8"
      (get-in
       (pick.ring/pick
        {:ring.request/method :get
         :ring.request/path "/"
         :ring.request/headers {"accept" "text/plain"}}
        variants)
       [:juxt.pick/representation :juxt.http/content-type])))

    (is
     (=
      "text/html;charset=utf-8"
      (get-in
       (pick.ring/pick
        {:ring.request/method :get
         :ring.request/path "/"
         :ring.request/headers {"accept" "text/html"}}
        variants)
       [:juxt.pick/representation :juxt.http/content-type])))

    (is
     (=
      "text/plain;charset=utf-8"
      (get-in
       (pick.ring/pick
        {:ring.request/method :get
         :ring.request/path "/"
         :ring.request/headers {"accept" "text/plain"}}
        variants)
       [:juxt.pick/representation :juxt.http/content-type])))

    (is
     (=
      "text/plain;charset=utf-8"
      (get-in
       (pick.ring/pick
        {:ring.request/method :get
         :ring.request/path "/"
         :ring.request/headers {"accept" "text/html;q=0.8,text/plain"}}
        variants)
       [:juxt.pick/representation :juxt.http/content-type])))

    (is
     (=
      ["accept" "accept-language"]
      (:juxt.pick/vary
       (pick.ring/pick
        {:ring.request/method :get
         :ring.request/path "/"
         :ring.request/headers {"accept" "text/html"}}
        variants
        {:juxt.pick/vary? true}))))))

(deftest malformed-content-type-test
  (is
   (thrown-with-msg?
    clojure.lang.ExceptionInfo
    #"Malformed content-type"
    (pick.ring/pick
     {:ring.request/method :get
      :ring.request/path "/"
      :ring.request/headers {"accept" "text/html"}}
     [{:juxt.http/content-type "texthtml"}]))))

(deftest no-metadata-key-test
  (is
   (thrown-with-msg?
    clojure.lang.ExceptionInfo
    #"Representation must have a value for content-type.*"
    (pick.ring/pick
     {:ring.request/method :get
      :ring.request/path "/"
      :ring.request/headers {"accept" "text/html"}}
     [{}]))))


(deftest string-key-test
  (let [variants
        [^{"content-type" "text/html;charset=utf-8"
           "content-language" "en"}
         (fn [] {:type :html :lang :en})

         ^{"content-type" "text/html;charset=utf-8"
           "content-language" "de"}
         (fn [] {:type :html :lang :de})

         ^{"content-type" "text/plain;charset=utf-8"}
         (fn [] {:type :plain :lang :de})
         ]]
    (is (=
         :en
         (let [{:juxt.pick/keys [representation]}
               (pick.ring/pick
                {:ring.request/method :get
                 :ring.request/path "/"
                 :ring.request/headers {"accept" "text/html"
                                        "accept-language" "en, de, es"}}
                variants)]

           (:lang (representation)))))))
