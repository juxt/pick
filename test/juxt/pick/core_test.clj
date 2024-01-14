;; Copyright © 2020-2024, JUXT LTD.

(ns juxt.pick.core-test
  (:require
   [clojure.test :refer [deftest is are]]
   [juxt.pick.core
    :refer [match-parameters? acceptable-content-type-quality
            acceptable-charset-quality
            assign-language-quality basic-language-match?
            assign-language-ordering
            acceptable-encoding-qvalue assign-encoding-quality]]
   [juxt.reap.alpha.decoders :as rdec]))

(defn round [n]
  (when n
    (/ (Math/rint (* n 100000)) 100000)))

;; TODO: test for content-type-match?

;; TODO: Refactor and polish these tests so they are consistent with each
;; other. Try to write this tests in a way that references each part of Section
;; 5.3

;; TODO: Test for nils, blank strings, negative qvalues, malformed strings -
;; when and how should a 400 be signaled?

(deftest match-parameters-test
  (is (match-parameters? nil nil))
  (is (match-parameters? {} {}))
  (is (match-parameters? {} {"level" "1" "foo" "bar"}))
  (is (match-parameters? {"LEVEL" "1"} {"level" "1" "foo" "bar"}))
  (is (not (match-parameters? {"LEVEL" "1"} {"level" "2" "foo" "bar"})))
  (is (match-parameters? {"LEVEL" "1" "foo" "bar"} {"level" "1" "foo" "bar"}))
  (is (not (match-parameters? {"LEVEL" "1" "foo" "bar"} {"level" "1" "foo" "baz"}))))

(deftest acceptable-content-type-quality-test
  (are [content-type expected]
      (= expected
         (select-keys
          (acceptable-content-type-quality
           (rdec/accept "text/html;q=0.1,text/html;level=2;q=0.4,text/html;LEVEL=3;q=0.5,text/*;q=0.02,*/*;q=0.01")
           (rdec/content-type content-type))
          [:qvalue :precedence]))
    "application/json" {:precedence 1 :qvalue 0.01}
    "text/html" {:precedence 3 :qvalue 0.1}
    "text/HTML" {:precedence 3 :qvalue 0.1}
    "text/plain" {:precedence 2 :qvalue 0.02}
    "TEXT/PLAIN" {:precedence 2 :qvalue 0.02}
    "Text/plain" {:precedence 2 :qvalue 0.02}
    "TEXT/HTML" {:precedence 3 :qvalue 0.1} ; case-insensitive
    "text/html;charset=utf-8" {:precedence 3 :qvalue 0.1}
    "text/html;level=2;charset=utf-8" {:precedence 4 :qvalue 0.4}
    "text/html;LEVEL=2;charset=utf-8" {:precedence 4 :qvalue 0.4}
    "text/html;level=3;charset=utf-8" {:precedence 4 :qvalue 0.5}
    "text/html;LEVEL=3;charset=utf-8" {:precedence 4 :qvalue 0.5}))

;; This test represents the table in RFC 7231 Section 5.3.2, where quality
;; values are determined from matching a representation's content-type according
;; to rules of precedence. These rules are specified in the RFC and are
;; independent of the actual content negotiation algorithm that is used.
(deftest acceptable-content-type-qvalue-test

  (let [accepts
        (rdec/accept "text/*;q=0.3, text/html;q=0.7, text/html;level=1, text/html;level=2;q=0.4, */*;q=0.5")]

    (are [content-type expected]
        (= expected
           (:qvalue
            (acceptable-content-type-quality
             accepts
             (rdec/content-type content-type))))

      "text/html;level=1" 1.0
      "text/html" 0.7
      "text/plain" 0.3
      "image/jpeg" 0.5
      "text/html;level=2" 0.4
      "text/html;level=3" 0.7)))
;; See RFC 7231 Section 5.3.3: Accept-Charset

(deftest acceptable-charset-quality-test
  (are [accept-charset charset expected]
      (= expected
         (->
          (acceptable-charset-quality
           (rdec/accept-charset accept-charset)
           charset)
          (select-keys [:qvalue :precedence])))

    "iso-8859-5, unicode-1-1;q=0.8"
    "iso-8859-5"
    {:qvalue 1.0 :precedence 2}

    "iso-8859-5, unicode-1-1;q=0.8"
    "unicode-1-1"
    {:qvalue 0.8 :precedence 2}

    "iso-8859-5, unicode-1-1;q=0.8"
    "utf-8"
    {:qvalue 0.0 :precedence 0}

    "iso-8859-5, unicode-1-1;q=0.8,*"
    "utf-8"
    {:qvalue 1.0 :precedence 1}

    ;; "A charset is identified by a case-insensitive token." -- Section 3.1.1.2
    ;; of RFC 7231
    "utf-8"
    "UTF-8"
    {:qvalue 1.0 :precedence 2}))

;; See RFC 7231 Section 5.3.4: Accept-Encoding

(deftest acceptable-encoding-qvalue-test
  (are [accept-encoding content-encoding expected-qvalue]
      (= (Math/rint (* 1000 expected-qvalue))
         (Math/rint (* 1000 (acceptable-encoding-qvalue
                             (rdec/accept-encoding accept-encoding)
                             (rdec/content-encoding content-encoding)))))
    "gzip" "gzip" 1.0
    "gzip;q=0.8" "gzip" 0.8
    "gzip" "deflate" 0.0
    "gzip,deflate" "gzip,deflate" 1.0
    "gzip;q=0.8,deflate;q=0.5,*" "identity" 1.0
    "gzip;q=0.8,deflate;q=0.5,*;q=0.1" "identity" 0.1

    ;; Multiple codings applied to content, if all are acceptable, we
    ;; determine the total qvalue with multiplication.
    "gzip" "gzip,deflate" 0.0
    "deflate" "gzip,deflate" 0.0
    "gzip;q=0.9,deflate;q=0.5,compress;q=0.2" "gzip,deflate" 0.45
    "gzip;q=0.4,deflate;q=0.5,compress;q=0.2" "gzip,deflate,compress" 0.04))

(deftest assign-encoding-quality-test
  (let [representations
        [{:id :gzip
          :juxt.reap.alpha.rfc7231/content-encoding
          (rdec/content-encoding "gzip")}

         {:id :deflate
          :juxt.reap.alpha.rfc7231/content-encoding
          (rdec/content-encoding "deflate")}

         {:id :gzip-then-deflate
          :juxt.reap.alpha.rfc7231/content-encoding
          (rdec/content-encoding "gzip,deflate")}

         {:id :identity
          :juxt.reap.alpha.rfc7231/content-encoding
          (rdec/content-encoding "identity")}

         ;; :juxt.reap.alpha.rfc7231/content-encoding defaults to 'identity'
         {:id :unspecified}]]

    (are [accept-encoding-header expected]
        (=
         expected
         (map
          (juxt :id (comp round :juxt.pick/encoding-qvalue))
          (map
           (assign-encoding-quality
            (rdec/accept-encoding
             accept-encoding-header))
           representations)))

      ;; Rule 1: "If no Accept-Encoding field is in the request, any content-coding is
      ;; considered acceptable by the user agent."
      nil [[:gzip 1.0]
           [:deflate 1.0]
           [:gzip-then-deflate 1.0]
           [:identity 1.0]
           [:unspecified 1.0]]

      "gzip" [[:gzip 1.0]
              [:deflate 0.0]
              [:gzip-then-deflate 0.0]
              [:identity 1.0]

              ;; Rule 2: "If the representation has no content-coding, then it
              ;; is acceptable by default unless specifically excluded by the
              ;; Accept-Encoding field stating either 'identity;q=0' or '*;q=0'
              ;; without a more specific entry for 'identity'."
              [:unspecified 1.0]]

      ;; "… unless specifically excluded by the Accept-Encoding field stating
      ;; either 'identity;q=0'"
      "gzip,identity;q=0"
      [[:gzip 1.0]
       [:deflate 0.0]
       [:gzip-then-deflate 0.0]
       [:identity 0.0]
       [:unspecified 0.0]]

      ;; "… or '*;q=0'
      "gzip,*;q=0"
      [[:gzip 1.0]
       [:deflate 0.0]
       [:gzip-then-deflate 0.0]
       [:identity 0.0]
       [:unspecified 0.0]]

      ;; without a more specific entry for 'identity'.
      "gzip,identity;q=0.8,*;q=0.2"
      [[:gzip 1.0]
       [:deflate 0.2]
       [:gzip-then-deflate 0.2]
       [:identity 0.8]
       [:unspecified 0.8]]

      "deflate" [[:gzip 0.0]
                 [:deflate 1.0]
                 [:gzip-then-deflate 0.0]
                 [:identity 1.0]
                 [:unspecified 1.0]]

      "gzip,deflate;q=0.0" [[:gzip 1.0]
                            [:deflate 0.0]
                            [:gzip-then-deflate 0.0]
                            [:identity 1.0]
                            [:unspecified 1.0]]

      ;; "The asterisk '*' symbol in an Accept-Encoding field matches any available
      ;; content-coding not explicitly listed in the header field."
      "*" [[:gzip 1.0]
           [:deflate 1.0]
           [:gzip-then-deflate 1.0]
           [:identity 1.0]
           [:unspecified 1.0]]

      "gzip;q=0.5,*" [[:gzip 0.5]
                      [:deflate 1.0]
                      [:gzip-then-deflate 0.5]
                      [:identity 1.0]
                      [:unspecified 1.0]]

      "*,gzip;q=0.5" [[:gzip 0.5]
                      [:deflate 1.0]
                      [:gzip-then-deflate 0.5]
                      [:identity 1.0]
                      [:unspecified 1.0]]

      "gzip;q=0.5,*" [[:gzip 0.5]
                      [:deflate 1.0]
                      [:gzip-then-deflate 0.5]
                      [:identity 1.0]
                      [:unspecified 1.0]]

      "gzip;q=0.5,*;q=0.2" [[:gzip 0.5]
                            [:deflate 0.2]
                            [:gzip-then-deflate 0.1]
                            [:identity 0.2]
                            [:unspecified 0.2]]

      "deflate,gzip" [[:gzip 1.0]
                      [:deflate 1.0]
                      [:gzip-then-deflate 1.0]
                      [:identity 1.0]
                      [:unspecified 1.0]]

      "deflate;q=0.5,gzip;q=0.2"
      [[:gzip 0.2]
       [:deflate 0.5]
       [:gzip-then-deflate 0.1]
       [:identity 1.0]
       [:unspecified 1.0]]

      "gzip,identity;q=0.8" [[:gzip 1.0]
                             [:deflate 0.0]
                             [:gzip-then-deflate 0.0]
                             [:identity 0.8]
                             [:unspecified 0.8]]

      "gzip,identity;q=0" [[:gzip 1.0]
                           [:deflate 0.0]
                           [:gzip-then-deflate 0.0]
                           [:identity 0.0]
                           [:unspecified 0.0]]

      "gzip,*;q=0" [[:gzip 1.0]
                    [:deflate 0.0]
                    [:gzip-then-deflate 0.0]
                    [:identity 0.0]
                    [:unspecified 0.0]]

      ;; "All content-coding values are case-insensitive" -- Section 3.1.2.1,
      ;; RFC 7231
      "DEFLATE,GZIP" [[:gzip 1.0]
                      [:deflate 1.0]
                      [:gzip-then-deflate 1.0]
                      [:identity 1.0]
                      [:unspecified 1.0]])))

;; See RFC 7231 Section 5.3.5: Accept-Language

;; This test represents the example in RFC 4647 Section 3.3.1.
(deftest basic-language-match-test
  (is
   (basic-language-match?
    (:juxt.reap.alpha.rfc4647/language-range (first (rdec/accept-language "en")))
    (:juxt.reap.alpha.rfc5646/langtag (first (rdec/content-language "en")))))

  (is
   (basic-language-match?
    (:juxt.reap.alpha.rfc4647/language-range (first (rdec/accept-language "de-de")))
    (:juxt.reap.alpha.rfc5646/langtag (first (rdec/content-language "de-DE-1996")))))

  (is
   (not
    (basic-language-match?
     (:juxt.reap.alpha.rfc4647/language-range (first (rdec/accept-language "de-de")))
     (:juxt.reap.alpha.rfc5646/langtag (first (rdec/content-language "de-Latn-DE"))))))

  ;; "At all times, language tags and their subtags, including private use and
  ;; extensions, are to be treated as case insensitive: there exist conventions
  ;; for the capitalization of some of the subtags, but these MUST NOT be taken
  ;; to carry meaning." -- Section 2.1.1, RFC 5646
  (is
   (not
    (basic-language-match?
     (:juxt.reap.alpha.rfc4647/language-range (first (rdec/accept-language "de-de")))
     (:juxt.reap.alpha.rfc5646/langtag (first (rdec/content-language "DE-LATN-DE"))))))

  (is
   (not
    (basic-language-match?
     (:juxt.reap.alpha.rfc4647/language-range (first (rdec/accept-language "en-gb")))
     (:juxt.reap.alpha.rfc5646/langtag (first (rdec/content-language "en"))))))

  (is
   (basic-language-match?
    (:juxt.reap.alpha.rfc4647/language-range (first (rdec/accept-language "*")))
    (:juxt.reap.alpha.rfc5646/langtag (first (rdec/content-language "de"))))))

;; TODO: Implement this test
;; RFC 7231 Section 5.3.5:
;; For example,
;;
;;      Accept-Language: da, en-gb;q=0.8, en;q=0.7
;;
;;    would mean: "I prefer Danish, but will accept British English and
;;    other types of English".

(deftest assign-language-quality-test
  (let [variants
        [;; Hello!
         {:id :en
          :juxt.reap.alpha.rfc7231/content-language
          (rdec/content-language "en")}

         ;; Howdy!
         ;; (Not everyone in the US uses 'Howdy!' but this is just a test…)
         ;; https://en.wikipedia.org/wiki/Howdy
         {:id :en-us
          :juxt.reap.alpha.rfc7231/content-language
          (rdec/content-language "en-US")}

         ;; ألسّلام عليكم
         {:id :ar-eg
          :juxt.reap.alpha.rfc7231/content-language
          (rdec/content-language "ar-eg")}

         ;; Content that requires the reader to understand both English and Arabic
         ;; Hello: ألسّلام عليكم
         {:id :ar-eg-and-en
          :juxt.reap.alpha.rfc7231/content-language
          (rdec/content-language "ar-eg,en")}

         ;; Unlike with encoding, if no content-language is specified we don't
         ;; provide a quality. This leaves the decision up to the algorithm.
         {:id :unspecified}]]

    (are [accept-language-header expected]
        (=
         expected
         (map
          (juxt :id (comp round :juxt.pick/language-qvalue))
          (map
           (assign-language-quality
            (rdec/accept-language accept-language-header))
           variants)))

      "en"
      [[:en 1.0]
       [:en-us 1.0]
       [:ar-eg 0.0]
       [:ar-eg-and-en 0.0]
       [:unspecified nil]]

      "en-us"
      [[:en 0.0]
       [:en-us 1.0]
       [:ar-eg 0.0]
       [:ar-eg-and-en 0.0]
       [:unspecified nil]]

      "ar-eg"
      [[:en 0.0]
       [:en-us 0.0]
       [:ar-eg 1.0]
       [:ar-eg-and-en 0.0]
       [:unspecified nil]]

      "ar"
      [[:en 0.0]
       [:en-us 0.0]
       [:ar-eg 1.0]
       [:ar-eg-and-en 0.0]
       [:unspecified nil]]

      "en-us,en;q=0.8,ar;q=0.2"
      [[:en 0.8]
       [:en-us 1.0]
       [:ar-eg 0.2]
       ;; Both en and ar-eg languages understood, the quality value is the
       ;; result of muliplying qvalues for en (0.8) an ar (0.2)
       [:ar-eg-and-en 0.16]
       [:unspecified nil]]

      "*"
      [[:en 1.0]
       [:en-us 1.0]
       [:ar-eg 1.0]
       [:ar-eg-and-en 1.0]
       [:unspecified nil]]

      "en-us,*;q=0.1"
      [[:en 0.1]
       [:en-us 1.0]
       [:ar-eg 0.1]
       [:ar-eg-and-en 0.01]
       [:unspecified nil]])

    (are [accept-language-header expected]
        (=
         expected
         (map
          (juxt :id :juxt.pick/language-ordering-weight)
          (map
           (assign-language-ordering
            (rdec/accept-language accept-language-header))
           variants)))

      "en"
      [[:en 1]
       [:en-us 1]
       [:ar-eg 0]
       [:ar-eg-and-en 1]
       [:unspecified nil]]

      "en-us"
      [[:en 0]
       [:en-us 1]
       [:ar-eg 0]
       [:ar-eg-and-en 0]
       [:unspecified nil]]

      "ar-eg"
      [[:en 0]
       [:en-us 0]
       [:ar-eg 1]
       [:ar-eg-and-en 1]
       [:unspecified nil]]

      "ar"
      [[:en 0]
       [:en-us 0]
       [:ar-eg 1]
       [:ar-eg-and-en 1]
       [:unspecified nil]]

      "en-us,en;q=0.8,ar;q=0.2"
      [[:en 2]
       [:en-us 6]
       [:ar-eg 1]
       [:ar-eg-and-en 3]
       [:unspecified nil]]

      "*"
      [[:en 1]
       [:en-us 1]
       [:ar-eg 1]
       [:ar-eg-and-en 1]
       [:unspecified nil]]

      "en-us,*;q=0.1"
      [[:en 1]
       [:en-us 3]
       [:ar-eg 1]
       [:ar-eg-and-en 1]
       [:unspecified nil]])))
