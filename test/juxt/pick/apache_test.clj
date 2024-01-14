;; Copyright © 2020, JUXT LTD.

(ns juxt.pick.apache-test
  (:require
   [clojure.test :refer [deftest is are]]
   [juxt.pick.impl.apache :refer [apache-select-representation]]
   [juxt.reap.decoders :as reap]
   [juxt.reap.rfc7231 :as rfc7231]))

(deftest accept-test

  (are [accept-header expected-content]
      (= expected-content
         (->
          (apache-select-representation
           {:juxt.pick/request-headers
            {"accept" (reap/accept accept-header)}
            :juxt.pick/representations
            [{:id :html
              :content "<h1>Hello World!</h1>"
              ::rfc7231/content-type
              (reap/content-type "text/html;charset=utf-8")}

             {:id :html-level-2
              :content "<h1>Hello World!</h1>"
              ::rfc7231/content-type
              (reap/content-type "text/html;level=2;charset=utf-8")}

             {:id :plain-text
              :content "Hello World!"
              ::rfc7231/content-type
              (reap/content-type "text/plain;charset=utf-8")}

             {:id :edn
              ::rfc7231/content-type
              (reap/content-type "application/edn")
              :juxt.pick/quality-of-source 1.0}

             ;; Test that :json is selected over :json-large, since it
             ;; has a lesser content-length
             {:id :json-large
              ::rfc7231/content-type
              (reap/content-type "application/json")
              :juxt.pick/quality-of-source 0.8
              :juxt.http/content-length 2000}

             {:id :json
              ::rfc7231/content-type
              (reap/content-type "application/json")
              :juxt.pick/quality-of-source 0.8
              :juxt.http/content-length 1000}

             ]})
          (get-in [:juxt.pick/representation :id])))

      "text/html" :html
      "TEXT/HTML" :html

      "text/html;q=0.8,text/plain;q=0.7" :html
      "text/plain" :plain-text
      "text/html;q=0.8,text/plain" :plain-text

      "TEXT/HTML;level=2,text/html;q=0.8" :html-level-2

      ;; Quality for source tests
      "application/edn" :edn
      "application/json" :json
      "application/json,application/edn" :edn
      ;; We still get EDN here because of the :juxt.pick/quality-of-source
      ;; mulitplier
      "application/json,application/edn;q=0.9" :edn
      "application/json,application/edn;q=0.1" :json))

(deftest accept-encoding-test
  (are [accept-encoding-header variants expected]
      (=
       expected
       (let [actual (apache-select-representation
                     {:juxt.pick/request-headers
                      {"accept-encoding"
                       (reap/accept-encoding
                        accept-encoding-header)}
                      :juxt.pick/representations variants})]
         {:id (:id (:juxt.pick/representation actual))
          :qvalue (:juxt.pick/encoding-qvalue (:juxt.pick/representation actual))}))

      "gzip"
      [{:id :gzip
        ::rfc7231/content-encoding
        (reap/content-encoding "gzip")}]
      {:id :gzip :qvalue 1.0}


      "deflate"
      [{:id :deflate-1
        ::rfc7231/content-encoding
        (reap/content-encoding "deflate")}]
      {:id :deflate-1 :qvalue 1.0}

      "gzip;q=0.8,deflate"
      [{:id :deflate-2
        ::rfc7231/content-encoding
        (reap/content-encoding "deflate")}
       {:id :gzip
        ::rfc7231/content-encoding
        (reap/content-encoding "gzip")}]
      {:id :deflate-2 :qvalue 1.0}

      ;; Pick first acceptable variant as per variant order, rather than
      ;; accept-encoding header order.
      "gzip,deflate"
      [{:id :deflate-3
        ::rfc7231/content-encoding
        (reap/content-encoding "deflate")}
       {:id :gzip
        ::rfc7231/content-encoding
        (reap/content-encoding "gzip")}]
      {:id :deflate-3 :qvalue 1.0}

      "gzip,deflate"
      [{:id :gzip-then-deflate
        ::rfc7231/content-encoding
        (reap/content-encoding "gzip,deflate")}]
      {:id :gzip-then-deflate :qvalue 1.0}

      "gzip"
      [{:id :gzip-then-deflate
        ::rfc7231/content-encoding
        (reap/content-encoding "gzip,deflate")}
       {:id :identity}]
      {:id :identity :qvalue 1.0}

      ;; "If an Accept-Encoding header field is present in a request and none of
      ;; the available representations for the response have a content-coding
      ;; that is listed as acceptable, the origin server SHOULD send a response
      ;; without any content-coding." -- RFC 7231 Section 5.3.4
      "br,compress"
      [{:id :identity}
       {:id :gzip
        ::rfc7231/content-encoding (reap/content-encoding "gzip")}]
      {:id :identity :qvalue 1.0}

      ;; "If the Accept-Encoding field-value is empty, then only the "identity"
      ;; encoding is acceptable." -- RFC 2616
      ""
      [{:id :identity}
       {:id :gzip
        ::rfc7231/content-encoding (reap/content-encoding "gzip")}]
      {:id :identity :qvalue 1.0}

      )

  ;; "If no Accept-Encoding field is in the request, any content-coding is
  ;; considered acceptable by the user agent."
  (let [actual (apache-select-representation
                {:juxt.pick/request-headers
                 {}
                 :juxt.pick/representations
                 [{:id :gzip-3
                   ::rfc7231/content-encoding (reap/content-encoding "gzip")}
                  {:id :compress
                   ::rfc7231/content-encoding (reap/content-encoding "compress")}]})]

    (is (= :gzip-3 (:id (:juxt.pick/representation actual))))
    (is (= 1.0 (:juxt.pick/encoding-qvalue (:juxt.pick/representation actual)))))

  ;; If no identity encoding is acceptable we return unacceptable representations
  ;; in the :representations key. It is now up to the caller to return use the
  ;; encoding in a 200 or return a 406 status.
  (let [actual (apache-select-representation
                {:juxt.pick/request-headers
                 {"accept-encoding"
                  (reap/accept-encoding
                   "")}
                 :juxt.pick/representations
                 [{:id :gzip-1
                   ::rfc7231/content-encoding (reap/content-encoding "gzip")}]})]
    (is (= 1 (count (:juxt.pick/representations actual))))
    (is (= false (:juxt.pick/acceptable? (first (:juxt.pick/representations actual))))))

  ;; As above, but return multiple (albeit unacceptable) representations and
  ;; allow the caller to pick one, or return a status 300 or a status 406
  ;; response.
  (let [actual (apache-select-representation
                {:juxt.pick/request-headers
                 {"accept-encoding"
                  (reap/accept-encoding
                   "")}
                 :juxt.pick/representations
                 [{:id :gzip-2
                   ::rfc7231/content-encoding (reap/content-encoding "gzip")}
                  {:id :compress
                   ::rfc7231/content-encoding (reap/content-encoding "compress")}]})]
    (is (= 2 (count (:juxt.pick/representations actual))))
    (is (= false (:juxt.pick/acceptable? (first (:juxt.pick/representations actual)))))
    (is (= false (:juxt.pick/acceptable? (second (:juxt.pick/representations actual)))))))

(deftest accept-language-test
  (let [variants
        [{:id :en
          :content "Hello!"
          ::rfc7231/content-language
          (reap/content-language "en")}

         {:id :en-us
          ::rfc7231/content-language
          (reap/content-language "en-US")
          ;; https://en.wikipedia.org/wiki/Howdy
          ;; Not everyone in the US uses 'Howdy!' but this is just a test...
          :content "Howdy!"}

         {:id :ar-eg
          ::rfc7231/content-language
          (reap/content-language "ar-eg")
          :content "ألسّلام عليكم"}]]

    (are [accept-language-header expected-greeting]
        (= expected-greeting
           (->
            (apache-select-representation
             {:juxt.pick/request-headers
              {"accept-language" (reap/accept-language accept-language-header)}
              :juxt.pick/representations variants})
            (get-in [:juxt.pick/representation :content])))
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
        ;;
        ;; But, see RFC2616 Section 14.4.: 'The special range "*", if present in
        ;; the Accept-Language field, matches every tag not matched by any other
        ;; range present in the Accept-Language field.'

        ;; So we should see Hello! here, not Howdy!
        "en-us;q=0.8,*" "Hello!"

        nil "Hello!")

    ;; If no Accept-Language header, just pick the first variant.
    (is (= "Hello!"
           (-> (apache-select-representation
                {:juxt.pick/request-headers {}
                 :juxt.pick/representations variants})
               (get-in [:juxt.pick/representation :content]))))))

;; Check only one language is chosen, and the order in the Accept-Language
;; header is used if necessary. We don't want multiple languages going into the
;; encoding selection step, according to step 2.3 of the Apache httpd Negotation
;; Algorithm (see
;; https://httpd.apache.org/docs/current/en/content-negotiation.html#algorithm)
(deftest single-language-selected
  (is
   (:juxt.pick/representation
    (apache-select-representation
     {:juxt.pick/request-headers
      {"accept-language" (reap/accept-language "en,fr,de")}
      :juxt.pick/representations
      [{:id :en
        ::rfc7231/content-language (reap/content-language "en")}
       {:id :fr
        ::rfc7231/content-language (reap/content-language "fr")}
       {:id :de
        ::rfc7231/content-language (reap/content-language "de")}]}))))

;; Integration testing

(deftest integration-test
  (is
   (apache-select-representation
    {:juxt.pick/request
     {"accept" (reap/accept "text/html")}
     :juxt.pick/representations
     [{:id :html
       ::rfc7231/content-type (reap/content-type "text/html")}
      {:id :plain
       ::rfc7231/content-type (reap/content-type "text/plain")}]
     :juxt.pick/explain? false})))

;; Awaiting a redesigned test-suite for juxt.pick/explain? and juxt.pick/vary?
#_(deftest explain-test
  (let [request
        {"accept" (reap/accept "text/plain,text/html;q=0.1")
         "accept-encoding" (reap/accept-encoding "gzip;q=0.8,deflate")
         "accept-language" (reap/accept-language "en;q=0.2,en-US")}
        variants
        [{:id :html
          ::rfc7231/content-type
          (reap/content-type "text/html;charset=utf-8")}

         {:id :html-old
          ::rfc7231/content-type
          (reap/content-type "text/html;charset=usascii")}

         {:id :plain-gzip
          ::rfc7231/content-type
          (reap/content-type "text/plain")
          ::rfc7231/content-encoding
          (reap/content-encoding "gzip")}

         {:id :plain-deflate-it
          ::rfc7231/content-type
          (reap/content-type "text/plain")
          ::rfc7231/content-encoding
          (reap/content-encoding "deflate")
          ::rfc7231/content-language
          (reap/content-language "it")}

         {:id :plain-deflate-en
          ::rfc7231/content-type
          (reap/content-type "text/plain")
          ::rfc7231/content-encoding
          (reap/content-encoding "deflate")
          ::rfc7231/content-language
          (reap/content-language "en")}]

        select-explain
        (apache-select-representation
         {:juxt.pick/request request
          :juxt.pick/representations variants
          :juxt.pick/explain? true})

        explain
        (:juxt.pick/explain select-explain)]

    (testing "disable explain"
      (is
       (nil?
        (find
         (apache-select-representation
          {:juxt.pick/request request
           :juxt.pick/representations variants
           :juxt.pick/explain? false})
         :juxt.pick/explain))))

    (testing "no explain by default"
      (is
       (nil?
        (find
         (apache-select-representation
          {:juxt.pick/request request
           :juxt.pick/representations variants})
         :juxt.pick/explain))))

    (testing (is (map? explain)))))
