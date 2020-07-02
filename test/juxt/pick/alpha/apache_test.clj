;; Copyright © 2020, JUXT LTD.

(ns juxt.pick.alpha.apache-test
  (:require
   [clojure.test :refer [deftest is are testing]]
   [juxt.pick.alpha.apache :refer [select-variant]]
   [juxt.reap.alpha.decoders :as reap]))

(deftest accept-test

  (are [accept-header expected-content]
      (= expected-content
         (->
          (select-variant
           {:juxt.http/request-headers
            {"accept" (reap/accept accept-header)}
            :juxt.http/variants
            [{:id :html
              :juxt.http/content "<h1>Hello World!</h1>"
              :juxt.http/content-type
              (reap/content-type "text/html;charset=utf-8")}

             {:id :html-level-2
              :juxt.http/content "<h1>Hello World!</h1>"
              :juxt.http/content-type
              (reap/content-type "text/html;level=2;charset=utf-8")}

             {:id :plain-text
              :juxt.http/content "Hello World!"
              :juxt.http/content-type
              (reap/content-type "text/plain;charset=utf-8")}]})
          (get-in [:juxt.http/variant :id])))

      "text/html" :html
      "TEXT/HTML" :html

      "text/html;q=0.8,text/plain;q=0.7" :html
      "text/plain" :plain-text
      "text/html;q=0.8,text/plain" :plain-text

      "TEXT/HTML;level=2;text/html;q=0.8" :html-level-2))

;; TODO: Test quality-of-source

(deftest accept-encoding-test
  (are [accept-encoding-header variants expected-id]
      (=
       expected-id
       (-> (select-variant
            {:juxt.http/request-headers
             {"accept-encoding"
              (reap/accept-encoding
               accept-encoding-header)}
             :juxt.http/variants variants})
           (get-in [:juxt.http/variant :id])))

      "gzip"
      [{:id :gzip
        :juxt.http/content-encoding
        (reap/content-encoding "gzip")}]
      :gzip

      "deflate"
      [{:id :deflate
        :juxt.http/content-encoding
        (reap/content-encoding "deflate")}]
      :deflate

      "gzip;q=0.8,deflate"
      [{:id :deflate
        :juxt.http/content-encoding
        (reap/content-encoding "deflate")}
       {:id :gzip
        :juxt.http/content-encoding
        (reap/content-encoding "gzip")}]
      :deflate

      ;; Pick first acceptable variant as per variant order, rather than
      ;; accept-encoding header order.
      "gzip,deflate"
      [{:id :deflate
        :juxt.http/content-encoding
        (reap/content-encoding "deflate")}
       {:id :gzip
        :juxt.http/content-encoding
        (reap/content-encoding "gzip")}]
      :deflate

      "gzip,deflate"
      [{:id :gzip-then-deflate
        :juxt.http/content-encoding
        (reap/content-encoding "gzip,deflate")}]
      :gzip-then-deflate

      "gzip"
      [{:id :gzip-then-deflate
        :juxt.http/content-encoding
        (reap/content-encoding "gzip,deflate")}
       {:id :identity}]
      :identity

      ;; "If an Accept-Encoding header field is present in a request and none of
      ;; the available representations for the response have a content-coding
      ;; that is listed as acceptable, the origin server SHOULD send a response
      ;; without any content-coding." -- RFC 7231 Section 5.3.4
      "br,compress"
      [{:id :identity}
       {:id :gzip
        :juxt.http/content-encoding (reap/content-encoding "gzip")}]
      :identity))

(deftest accept-language-test
  (let [variants
        [{:id :en
          :juxt.http/content "Hello!"
          :juxt.http/content-language
          (reap/content-language "en")
          :juxt.http/language-quality-factor 0.5}

         {:id :en-us
          :juxt.http/content-language
          (reap/content-language "en-US")
          ;; https://en.wikipedia.org/wiki/Howdy
          ;; Not everyone in the US uses 'Howdy!' but this is just a test...
          :juxt.http/content "Howdy!"
          :juxt.http/language-quality-factor 0.5}

         {:id :ar-eg
          :juxt.http/content-language
          (reap/content-language "ar-eg,en")
          :juxt.http/content "ألسّلام عليكم"
          :juxt.http/language-quality-factor 0.5}

         ;; TODO: Test for when no content-language is specified - what should
         ;; we default to?
         ]]

    (are [accept-language-header expected-greeting]
        (= expected-greeting
           (->
            (select-variant
             {:juxt.http/request-headers
              {"accept-language" (reap/accept-language accept-language-header)}
              :juxt.http/variants variants})
            (get-in [:juxt.http/variant :juxt.http/content])))
        "en" "Hello!"
        "en-us" "Howdy!"
        "ar-eg" "ألسّلام عليكم"
        "en-us,en;q=0.8,ar-eg;q=0.2" "Howdy!"
        "*" "Hello!"
        "en-us,*;q=0.1" "Howdy!"
        ;; No rules of precedence apply to languages. If a '*' has greater
        ;; qvalue than another more specific language, it is still
        ;; selected. Hence, en and ar-eg are preferred over en-us, and en is
        ;; selected because it comes before ar-eg.
        "en-us;q=0.8,*" "Hello!"

        nil "Hello!")

    ;; If no Accept-Language header, just pick the first variant.
    (is (= "Hello!"
           (-> (select-variant
                {:juxt.http/request-headers {}
                 :juxt.http/variants variants})
               (get-in [:juxt.http/variant :juxt.http/content]))))

    ;; The language quality factor of a variant, if present, is used in
    ;; preference to an Accept-Language header.
    (is
     (=
      "Bonjour!"
      (-> (select-variant
           {:juxt.http/request-headers
            {"accept-language" (reap/accept-language "en")}
            :juxt.http/variants
            (conj
             variants
             {:id :fr-fr
              :juxt.http/content "Bonjour!"
              :juxt.http/content-language (reap/content-language "fr-FR")
              :juxt.http/language-quality-factor 1.0})})
          (get-in [:juxt.http/variant :juxt.http/content]))))))

(deftest integrated-test
  (is
   (select-variant
    {:juxt.http/request
     {"accept" (reap/accept "text/html")}
     :juxt.http/variants
     [{:id :html
       :juxt.http/content-type (reap/content-type "text/html")}
      {:id :plain
       :juxt.http/content-type (reap/content-type "text/plain")}]
     :juxt.http/explain? false})))

(deftest explain-test
  (let [request
        {"accept" (reap/accept "text/plain,text/html;q=0.1")
         "accept-encoding" (reap/accept-encoding "gzip;q=0.8,deflate")
         "accept-language" (reap/accept-language "en;q=0.2,en-US")}
        variants
        [{:id :html
          :juxt.http/content-type
          (reap/content-type "text/html;charset=utf-8")}

         {:id :html-old
          :juxt.http/content-type
          (reap/content-type "text/html;charset=usascii")}

         {:id :plain-gzip
          :juxt.http/content-type
          (reap/content-type "text/plain")
          :juxt.http/content-encoding
          (reap/content-encoding "gzip")}

         {:id :plain-deflate-it
          :juxt.http/content-type
          (reap/content-type "text/plain")
          :juxt.http/content-encoding
          (reap/content-encoding "deflate")
          :juxt.http/content-language
          (reap/content-language "it")}

         {:id :plain-deflate-en
          :juxt.http/content-type
          (reap/content-type "text/plain")
          :juxt.http/content-encoding
          (reap/content-encoding "deflate")
          :juxt.http/content-language
          (reap/content-language "en")}]

        select-explain
        (select-variant
         {:juxt.http/request request
          :juxt.http/variants variants
          :juxt.http/explain? true})

        explain
        (:juxt.http/explain select-explain)]

    (testing "disable explain"
      (is
       (nil?
        (find
         (select-variant
          {:juxt.http/request request
           :juxt.http/variants variants
           :juxt.http/explain? false})
         :juxt.http/explain))))

    (testing "no explain by default"
      (is
       (nil?
        (find
         (select-variant
          {:juxt.http/request request
           :juxt.http/variants variants})
         :juxt.http/explain))))

    (testing (is (map? explain)))))
