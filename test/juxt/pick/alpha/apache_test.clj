;; Copyright © 2020, JUXT LTD.

(ns juxt.pick.alpha.apache-test
  (:require
   [clojure.test :refer [deftest is are testing]]
   [juxt.pick.alpha.core :refer [pick]]
   [juxt.pick.alpha.apache :refer [using-apache-algo]]
   [juxt.reap.alpha.decoders :as reap]))

(deftest accept-test

  (are [accept-header expected-content]
      (= expected-content
         (->
          (pick
           using-apache-algo
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
              (reap/content-type "text/plain;charset=utf-8")}

             {:id :edn
              :juxt.http/content-type
              (reap/content-type "application/edn")
              :juxt.http/quality-of-source 1.0}

             {:id :json
              :juxt.http/content-type
              (reap/content-type "application/json")
              :juxt.http/quality-of-source 0.8}

             ]})
          (get-in [:juxt.http/variants 0 :id])))

      "text/html" :html
      "TEXT/HTML" :html

      "text/html;q=0.8,text/plain;q=0.7" :html
      "text/plain" :plain-text
      "text/html;q=0.8,text/plain" :plain-text

      "TEXT/HTML;level=2;text/html;q=0.8" :html-level-2

      ;; Quality for source tests
      "application/edn" :edn
      "application/json" :json
      "application/json,application/edn" :edn
      ;; We still get EDN here because of the :juxt.http/quality-of-source
      ;; mulitplier
      "application/json,application/edn;q=0.9" :edn
      "application/json,application/edn;q=0.1" :json
      ))

(deftest accept-encoding-test
  (are [accept-encoding-header variants expected]
      (=
       expected
       (let [actual (pick
                     using-apache-algo
                     {:juxt.http/request-headers
                      {"accept-encoding"
                       (reap/accept-encoding
                        accept-encoding-header)}
                      :juxt.http/variants variants})]
         (vec
          (for [v (:juxt.http/variants actual)]
            {:id (:id v)
             :qvalue (:juxt.http.content-negotiation/encoding-qvalue v)}))))

      "gzip"
      [{:id :gzip
        :juxt.http/content-encoding
        (reap/content-encoding "gzip")}]
      [{:id :gzip :qvalue 1.0}]

      "deflate"
      [{:id :deflate-1
        :juxt.http/content-encoding
        (reap/content-encoding "deflate")}]
      [{:id :deflate-1 :qvalue 1.0}]

      "gzip;q=0.8,deflate"
      [{:id :deflate-2
        :juxt.http/content-encoding
        (reap/content-encoding "deflate")}
       {:id :gzip
        :juxt.http/content-encoding
        (reap/content-encoding "gzip")}]
      [{:id :deflate-2 :qvalue 1.0}]

      ;; Pick first acceptable variant as per variant order, rather than
      ;; accept-encoding header order.
      "gzip,deflate"
      [{:id :deflate-3
        :juxt.http/content-encoding
        (reap/content-encoding "deflate")}
       {:id :gzip
        :juxt.http/content-encoding
        (reap/content-encoding "gzip")}]
      [{:id :deflate-3 :qvalue 1.0} {:id :gzip :qvalue 1.0}]

      "gzip,deflate"
      [{:id :gzip-then-deflate
        :juxt.http/content-encoding
        (reap/content-encoding "gzip,deflate")}]
      [{:id :gzip-then-deflate :qvalue 1.0}]

      "gzip"
      [{:id :gzip-then-deflate
        :juxt.http/content-encoding
        (reap/content-encoding "gzip,deflate")}
       {:id :identity}]
      [{:id :identity :qvalue 1.0}]

      ;; "If an Accept-Encoding header field is present in a request and none of
      ;; the available representations for the response have a content-coding
      ;; that is listed as acceptable, the origin server SHOULD send a response
      ;; without any content-coding." -- RFC 7231 Section 5.3.4
      "br,compress"
      [{:id :identity}
       {:id :gzip
        :juxt.http/content-encoding (reap/content-encoding "gzip")}]
      [{:id :identity :qvalue 1.0}]

      ;; "If the Accept-Encoding field-value is empty, then only the "identity"
      ;; encoding is acceptable." -- RFC 2616
      ""
      [{:id :identity}
       {:id :gzip
        :juxt.http/content-encoding (reap/content-encoding "gzip")}]
      [{:id :identity :qvalue 1.0}]

      ;; As above, but if no identity encoding is acceptable we return a variant
      ;; even if it is not acceptable (has a qvalue of 0). It is now up to the
      ;; caller to return use the encoding in a 200 or return a 406 status.
      ""
      [{:id :gzip-1
        :juxt.http/content-encoding (reap/content-encoding "gzip")}]
      [{:id :gzip-1 :qvalue 0.0}]

      ;; As above, but return multiple (albeit unacceptable) representations and
      ;; allow the caller to pick one, or return a status 300 or a status 406
      ;; response.
      ""
      [{:id :gzip-2
        :juxt.http/content-encoding (reap/content-encoding "gzip")}
       {:id :compress
        :juxt.http/content-encoding (reap/content-encoding "compress")}]
      [{:id :gzip-2 :qvalue 0.0}
       {:id :compress :qvalue 0.0}]

      ;; "If no Accept-Encoding field is in the request, any content-coding is
      ;; considered acceptable by the user agent."
      nil
      [{:id :gzip-3
        :juxt.http/content-encoding (reap/content-encoding "gzip")}
       {:id :compress
        :juxt.http/content-encoding (reap/content-encoding "compress")}]
      [{:id :gzip-3 :qvalue 1.0}
       {:id :compress :qvalue 1.0}]))

(deftest accept-language-test
  (let [variants
        [{:id :en
          :juxt.http/content "Hello!"
          :juxt.http/content-language
          (reap/content-language "en")}

         {:id :en-us
          :juxt.http/content-language
          (reap/content-language "en-US")
          ;; https://en.wikipedia.org/wiki/Howdy
          ;; Not everyone in the US uses 'Howdy!' but this is just a test...
          :juxt.http/content "Howdy!"}

         {:id :ar-eg
          :juxt.http/content-language
          (reap/content-language "ar-eg")
          :juxt.http/content "ألسّلام عليكم"}]]

    (are [accept-language-header expected-greeting]
        (= expected-greeting
           (->
            (pick
             using-apache-algo
             {:juxt.http/request-headers
              {"accept-language" (reap/accept-language accept-language-header)}
              :juxt.http/variants variants})
            (get-in [:juxt.http/variants 0 :juxt.http/content])))
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
           (-> (pick
                using-apache-algo
                {:juxt.http/request-headers {}
                 :juxt.http/variants variants})
               (get-in [:juxt.http/variants 0 :juxt.http/content]))))))

;; Check only one language is chosen, and the order in the Accept-Language
;; header is used if necessary. We don't want multiple languages going into the
;; encoding selection step, according to step 2.3 of the Apache httpd Negotation
;; Algorithm (see
;; https://httpd.apache.org/docs/current/en/content-negotiation.html#algorithm)
(deftest single-language-selected
  (is
   (= 1
      (count
       (:juxt.http/variants
        (pick
         using-apache-algo
         {:juxt.http/request-headers
          {"accept-language" (reap/accept-language "en,fr,de")}
          :juxt.http/variants
          [{:id :en
            :juxt.http/content-language (reap/content-language "en")}
           {:id :fr
            :juxt.http/content-language (reap/content-language "fr")}
           {:id :de
            :juxt.http/content-language (reap/content-language "de")}]}))))))

;; Integration testing

(deftest integrated-test
  (is
   (pick
    using-apache-algo
    {:juxt.http/request
     {"accept" (reap/accept "text/html")}
     :juxt.http/variants
     [{:id :html
       :juxt.http/content-type (reap/content-type "text/html")}
      {:id :plain
       :juxt.http/content-type (reap/content-type "text/plain")}]
     :juxt.http.content-negotiation/explain? false})))

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
        (pick
         using-apache-algo
         {:juxt.http/request request
          :juxt.http/variants variants
          :juxt.http.content-negotiation/explain? true})

        explain
        (:juxt.http.content-negotiation/explain select-explain)]

    (testing "disable explain"
      (is
       (nil?
        (find
         (pick
          using-apache-algo
          {:juxt.http/request request
           :juxt.http/variants variants
           :juxt.http.content-negotiation/explain? false})
         :juxt.http.content-negotiation/explain))))

    (testing "no explain by default"
      (is
       (nil?
        (find
         (pick
          using-apache-algo
          {:juxt.http/request request
           :juxt.http/variants variants})
         :juxt.http.content-negotiation/explain))))

    (testing (is (map? explain)))))
