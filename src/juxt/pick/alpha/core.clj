;; Copyright © 2020, JUXT LTD.

(ns juxt.pick.alpha.core
  (:require
   [clojure.string :as str]))

;; Content types

(defn match-parameters?
  "Return true if all parameters in the accept parameters, are matched by values
  in the content map. Keys are case insensitive, but always lower-case in the
  content-map."
  [accept-map content-map]
  (loop [[[k v] & accept-entries] accept-map]
    (if k
      (when (= v (get content-map (str/lower-case k)))
        (recur accept-entries))
      true)))

(defn- content-type-match?
  "Return truthy if the given accept-field (reap format) accepts the given
  content-type (reap format). The return value is a the precedence value (if
  matched), nil otherwise."
  [parsed-accept-field parsed-content-type]
  (cond
    (and
     (.equalsIgnoreCase (:juxt.http/type parsed-accept-field) (:juxt.http/type parsed-content-type))
     (.equalsIgnoreCase (:juxt.http/subtype parsed-accept-field) (:juxt.http/subtype parsed-content-type))
     ;; Try to match on all the parameters asked for in the accept,
     ;; but discard all others in the content type.
     (pos? (count (:juxt.http/parameters parsed-accept-field)))
     (match-parameters?
      (:juxt.http/parameters parsed-accept-field)
      (:juxt.http/parameter-map parsed-content-type)))

    ;; The precedence could be 3, plus the number of parameters in the
    ;; accept. For now, we don't include the count of the parameters
    ;; in the determination of precedence.
    4

    (and
     (.equalsIgnoreCase (:juxt.http/type parsed-accept-field) (:juxt.http/type parsed-content-type))
     (.equalsIgnoreCase (:juxt.http/subtype parsed-accept-field) (:juxt.http/subtype parsed-content-type))
     (zero? (count (:juxt.http/parameters parsed-accept-field))))
    3

    (and
     (.equalsIgnoreCase (:juxt.http/type parsed-accept-field) (:juxt.http/type parsed-content-type))
     (= "*" (:juxt.http/subtype parsed-accept-field)))
    2

    (and
     (= "*" (:juxt.http/type parsed-accept-field))
     (= "*" (:juxt.http/subtype parsed-accept-field)))
    1))

(defn- select-better-content-type-match
  "Designed to be used as a reducing function, if the parsed-accept-field is a
  higher precedence (or same precedence with higher qvalue), return an updated
  best-match map."

  [best-match parsed-accept-field]

  (let [precedence (content-type-match? parsed-accept-field (:content-type best-match))
        qvalue (get parsed-accept-field :juxt.http/qvalue 1.0)]

    (cond-> best-match
      (and
       precedence
       (or
        (> precedence (get best-match :precedence 0))
        (and (= precedence (get best-match :precedence 0))
             (> qvalue (get best-match :qvalue 0.0)))))
      (conj
       [:qvalue qvalue]
       [:precedence precedence]
       [:apex.debug/parsed-accept-field parsed-accept-field]))))

;; TODO: Support nil arg
(defn acceptable-content-type-rating
  "Determine the given content-type's rating (precedence, qvalue) with respect to
  what is acceptable. The parsed-accept-header parameter is a data structure
  returned from parsing the Accept header with reap. The variant is a map
  corresponding to the resource of the variant.

  This function determines the qvalue according to rules of precedence in RFC
  7231 Section 5.3.2 which are independent of the actual content negotation
  used.

  The qvalue is set to 0 if 'not acceptable' (See RFC 7231 Section 5.3.1). This
  still gives the content negotiation algorithm the chance of returning a
  variant, if there are no more preferable variants and if returning one is
  preferable to returning a 406 status code."

  [parsed-accept-header parsed-content-type]

  (reduce
   select-better-content-type-match
   {:qvalue 0.0
    :content-type parsed-content-type}
   parsed-accept-header))

(defn assign-content-type-quality
  "Return a transducer that will assign a content-type quality to each variant in
  a sequence according to the given (parsed) accept header. '

  'A request without any Accept header field implies that the user agent will
  accept any media type in response' -- RFC 7231 Section 5.3.2"
  [parsed-accept-header]
  (map
   (fn [variant]
     (assert variant)
     (let [content-type (:juxt.http/content-type variant)]
       (cond-> variant
         content-type
         (assoc
          :juxt.http.content-negotiation/content-type-qvalue
          (if parsed-accept-header
            (:qvalue
             (acceptable-content-type-rating
              parsed-accept-header
              content-type))
            1.0)))))))

;; Charsets

(defn acceptable-charset-rating
  [parsed-accept-charset-header parsed-content-type]
  (when-let [charset (get-in parsed-content-type [:juxt.http/parameter-map "charset"])]
    (reduce
     (fn [best-match field]
       (cond
         (= charset (:juxt.http/charset field))
         (cond-> best-match
           (< (get best-match :precedence) 2)
           (conj [:qvalue (get field :juxt.http/qvalue 1.0)]
                 [:precedence 2]
                 [:apex.debug/parsed-accept-charset-field field]))
         (= "*" (:juxt.http/charset field))
         (cond-> best-match
           (= (get best-match :precedence) 0)
           (conj [:qvalue (get field :juxt.http/qvalue 1.0)]
                 [:precedence 1]
                 [:apex.debug/parsed-accept-charset-field field]))
         :else best-match))
     {:qvalue 0.0
      :precedence 0}
     parsed-accept-charset-header)))

(defn assign-charset-quality
  "Return a transducer that will assign a charset quality to each variant in a
  sequence according to the given parsed accept-charset header. This argument
  can be nil, meaning that no accept-charset was received.

  'A request without any Accept-Charset header field implies that the user agent
  will accept any charset in response.' -- RFC 7231 Section 5.3.3

  "
  [parsed-accept-charset-header]
  (map
   (fn [variant]
     (let [qvalue
           (when-let [content-type (:juxt.http/content-type variant)]
             ;; TODO: This condition can be checked ahead-of-time
             (if parsed-accept-charset-header
               (:qvalue
                (acceptable-charset-rating
                 parsed-accept-charset-header
                 content-type))
               1.0))]
       (cond-> variant
         qvalue (conj [:juxt.http.content-negotiation/charset-qvalue qvalue]))))))

;; Encodings

(defn select-best-encoding-match [parsed-accept-encoding-header entry]
  (reduce
   (fn [best-match {accept-coding :juxt.http/codings :as field}]

     (cond
       (= accept-coding (get entry :juxt.http/content-coding "identity"))
       (cond-> best-match
         (< (get best-match :precedence) 2)
         (conj [:qvalue (get field :juxt.http/qvalue 1.0)]
               [:precedence 2]
               [:apex.debug/parsed-accept-encoding-field field]))

       (= accept-coding "*")
       (cond-> best-match
         (= (get best-match :precedence) 0)
         (conj [:qvalue (get field :juxt.http/qvalue 1.0)]
               [:precedence 1]
               [:apex.debug/parsed-accept-encoding-field field]))

       :else best-match))

   {:precedence 0
    :qvalue (if
                ;; "If the representation has no content-coding, then it is
                ;; acceptable by default unless specifically excluded by the
                ;; Accept-Encoding field stating either 'identity;q=0' or
                ;; '*;q=0' without a more specific entry for 'identity'."
                ;;
                ;; -- RFC 7231 Section 5.3.4
                (= (get entry :juxt.http/content-coding "identity") "identity")
              1.0
              0.0)}

   parsed-accept-encoding-header))

(defn acceptable-encoding-qvalue
  "Determine the qvalue for the given parsed content-encoding according to the
  given parsed Accept-Encoding header.

  The content-encoding can be nil.

  > If the representation has no content-coding, then it is acceptable by
  default unless specifically excluded by the Accept-Encoding field stating
  either 'identity;q=0' or '*;q=0' without a more specific entry for 'identity'.
  -- RFC 7231 Section 5.3.4

  "
  [parsed-accept-encoding-header parsed-content-encoding]

  (double
   (reduce
    ;; For content-encodings with multiple codings, it feels sensible to
    ;; multiply the qvalues together. Any unsupported coding will yield a total
    ;; qvalue 0.0, while if all qvalues are 1.0, the total will be 1.0.
    *
    (for [entry parsed-content-encoding]
      (:qvalue
       (select-best-encoding-match parsed-accept-encoding-header entry))))))

(defn assign-encoding-quality
  "Returns a transducer that will apply a rating on each of a collection of
  variants, according to the given parsed Accept-Encoding header. This argument
  can be nil, which is interpretted to mean that no Accept-Encoding header is
  present.

 'A request without an Accept-Encoding header field implies that the user agent
  has no preferences regarding content-codings.  Although this allows the server
  to use any content-coding in a response, it does not imply that the user agent
  will be able to correctly process all encodings.'  -- RFC 7231 Section 5.3.4
  "
  [parsed-accept-encoding-header]
  (map
   (fn [variant]
     (let [qvalue
           ;; TODO: This condition can be checked ahead-of-time
           (if parsed-accept-encoding-header
             (acceptable-encoding-qvalue
              parsed-accept-encoding-header
              (get
               variant
               :juxt.http/content-encoding
               ;; default it no content-encoding found on variant
               {:juxt.http/content-coding "identity"}))

             ;; "If no Accept-Encoding field is in the request, any
             ;; content-coding is considered acceptable by the user agent."
             ;; -- RFC 7231 Section 5.3.4
             1.0)]
       (cond-> variant
         qvalue (conj [:juxt.http.content-negotiation/encoding-qvalue qvalue]))))))

;; Languages

(defn basic-language-match?
  "Basic filtering as per RFC 4647 Section 3.3.1."
  [^String language-range ^String language-tag]

  (assert language-range)
  (assert (string? language-range))
  (assert language-tag)
  (assert (string? language-tag))

  (or
   (and
    ;; "A language range matches a particular language tag if, in a
    ;; case-insensitive comparison, it exactly equals the tag, …"
    (.regionMatches language-range true 0 language-tag 0 (. language-range length))

    ;; "or if it exactly equals a prefix of the tag such that the first character
    ;; following the prefix is '-'. "
    (if (< (count language-range) (count language-tag))
      (= \- (.charAt language-tag (count language-range)))
      true))

   ;; "The special range '*' in a language priority list matches any tag."
   (.equals language-range "*")))

(defn- select-better-language-match
  [best-match parsed-accept-language-field]
  (let [qvalue (get parsed-accept-language-field :juxt.http/qvalue 1.0)]
    (cond-> best-match
      (and
       (> qvalue (get best-match :qvalue 0.0))
       (basic-language-match?
        (:juxt.http/language-range parsed-accept-language-field)
        (get-in best-match [:language-tag :juxt.http/langtag])))
      (conj
       [:qvalue qvalue]
       [:apex.debug/parsed-accept-language-field parsed-accept-language-field]))))

(defn acceptable-language-rating
  "Determine the given language's rating (precedence, qvalue) with respect to what
  is acceptable. The parsed-accept-language-header parameter is a data structure
  returned from parsing the Accept-Language header with reap.

  This function determines the qvalue according to rules of precedence in RFC
  7231 Section 5.3.2 which are independent of the actual content negotation
  algorithm used.

  The qvalue is set to 0 if 'not acceptable' (See RFC 7231 Section 5.3.1). This
  still gives the content negotiation algorithm the chance of returning a
  variant, if there are no more preferable variants and if returning one is
  preferable to returning a 406 status code.
  "

  ;; TODO: Improve this function by allowing multiple language tags and using
  ;; multiplication.

  [parsed-accept-language-header parsed-language-tag]

  (reduce
   select-better-language-match
   {:qvalue 0.0
    :language-tag parsed-language-tag}
   parsed-accept-language-header))

(defn assign-language-quality
  "Return a transducer that will assign a language quality to each variant in a
  sequence according to the given parsed accept-language header. This argument
  can be nil, meaning that no accept-language header was received.

  'A request without any Accept-Language header field implies that the user
  agent will accept any language in response.' -- RFC 7231 Section 5.3.5"
  [parsed-accept-language-header]
  (map
   (fn [variant]
     (let [qvalue
           (when-let [content-language (:juxt.http/content-language variant)]
             (if parsed-accept-language-header
               (double
                (apply
                 *
                 (for [lang content-language]
                   (:qvalue
                    (acceptable-language-rating
                     parsed-accept-language-header
                     lang)))))
               1.0))]
       (cond-> variant
         qvalue (conj [:juxt.http.content-negotiation/language-qvalue qvalue]))))))

(defprotocol VariantSelector
  :extend-via-metadata true
  (select-variant [_ opts]))
