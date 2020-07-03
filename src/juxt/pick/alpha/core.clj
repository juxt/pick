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

(defn acceptable-content-type-quality
  "Determine the given content-type's quality (precedence, qvalue) with respect to
  what is acceptable. The parsed-accept-header parameter is sequence returned
  from parsing the Accept header (with reap).

  This function determines the qvalue according to rules of precedence in RFC
  7231 Section 5.3.2 which are independent of the actual content negotation
  used.

  The qvalue is set to 0 if 'not acceptable' (See RFC 7231 Section 5.3.1). This
  still gives the content negotiation algorithm the chance of returning a
  variant, if there are no more preferable variants and if returning one is
  preferable to returning a 406 status code.

  The parsed-accept-header argument can be nil, which represents the absence of
  the header: 'A request without any Accept header field implies that the user
  agent will accept any media type in response' -- RFC 7231 Section 5.3.2"

  [parsed-accept-header parsed-content-type]

  (assert parsed-content-type)

  (if (seq parsed-accept-header)
    (reduce
     select-better-content-type-match
     {:qvalue 0.0
      :content-type parsed-content-type}
     parsed-accept-header)
    {:qvalue 1.0
     :content-type parsed-content-type}))

(defn assign-content-type-quality
  "Return a function that will assign the content-type quality to a given
  variant according to the given (parsed) accept header."
  [parsed-accept-header]
  (fn [variant]
    (assert variant)
    (if-let [content-type (:juxt.http/content-type variant)]
      (assoc variant
             :juxt.http.content-negotiation/content-type-qvalue
             (:qvalue (acceptable-content-type-quality parsed-accept-header content-type)))
      ;; No content-type on variant, return variant untouched.
      variant)))

;; Charsets

(defn acceptable-charset-quality
  "(charset cannot be nil)"
  [parsed-accept-charset-header charset]
  (if (seq parsed-accept-charset-header)
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
     parsed-accept-charset-header)
    {:qvalue 1.0}))

(defn assign-charset-quality
  "Return a function that will assign a charset quality to a variant according to
  the given parsed accept-charset header. This argument can be nil, meaning that
  no accept-charset was received.

  'A request without any Accept-Charset header field implies that the user agent
  will accept any charset in response.' -- RFC 7231 Section 5.3.3"
  [parsed-accept-charset-header]
  (fn [variant]
    (assert variant)
    (if-let [charset (get-in variant [:juxt.http/content-type :juxt.http/parameter-map "charset"])]
      (assoc
       variant
       :juxt.http.content-negotiation/charset-qvalue
       (:qvalue
        (acceptable-charset-quality
         parsed-accept-charset-header
         charset)))
      ;; No charset on variant, return variant untouched.
      variant)))

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

  The content-encoding can be nil: 'If the representation has no content-coding,
  then it is acceptable by default unless specifically excluded by the
  Accept-Encoding field stating either 'identity;q=0' or '*;q=0' without a more
  specific entry for 'identity'.' -- RFC 7231 Section 5.3.4"
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
  "Returns a function that will assoc a quality on a variant, according to the
  given parsed Accept-Encoding header. This argument can be nil, which is
  interpreted to mean that no Accept-Encoding header is present.

 'A request without an Accept-Encoding header field implies that the user agent
  has no preferences regarding content-codings.  Although this allows the server
  to use any content-coding in a response, it does not imply that the user agent
  will be able to correctly process all encodings.'  -- RFC 7231 Section 5.3.4
  "
  [parsed-accept-encoding-header]
  (fn [variant]
    (assert variant)
    (let [qvalue
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
        qvalue (conj [:juxt.http.content-negotiation/encoding-qvalue qvalue])))))

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

(defn acceptable-language-quality
  "Determine the given language's quality (precedence, qvalue) with respect to what
  is acceptable. The parsed-accept-language-header parameter is a data structure
  returned from parsing the Accept-Language header with reap. This argument can be nil, meaning
  that no accept-language header was received:

  'A request without any Accept-Language header field implies that the user
  agent will accept any language in response.' -- RFC 7231 Section 5.3.5

  This function determines the qvalue according to rules of precedence in RFC
  7231 Section 5.3.2 which are independent of the actual content negotation
  algorithm used.

  The qvalue is set to 0 if 'not acceptable' (See RFC 7231 Section 5.3.1). This
  still gives the content negotiation algorithm the chance of returning a
  variant, if there are no more preferable variants and if returning one is
  preferable to returning a 406 status code."

  [parsed-accept-language-header parsed-language-tag]

  (if (seq parsed-accept-language-header)
    (reduce
     select-better-language-match
     {:qvalue 0.0
      :language-tag parsed-language-tag}
     parsed-accept-language-header)
    ;; No accept-language header, this language is therefore acceptable.
    {:qvalue 1.0
     :language-tag parsed-language-tag}))

(defn assign-language-quality
  "Return a function that will assign a language quality to a variant according to
  the given parsed accept-language header."
  [parsed-accept-language-header]
  (fn [variant]
    (assert variant)
    (if-let [content-language (:juxt.http/content-language variant)]
      (assoc
       variant
       :juxt.http.content-negotiation/language-qvalue
       (if parsed-accept-language-header
         (double
          (apply
           *
           (for [lang content-language]
             (:qvalue
              (acceptable-language-quality
               parsed-accept-language-header
               lang)))))
         ;; No accept-language header, so language is acceptable.
         1.0))
      ;; No content-language, so no quality applied.
      variant)))

(defn rate-variants [request-headers variants]
  (map

   ;; Ordering of dimensions is as per description here:
   ;; http://httpd.apache.org/docs/current/en/content-negotiation.html#algorithm

   (comp

    (assign-content-type-quality
     (get request-headers "accept"))

    (assign-language-quality
     (get request-headers "accept-language"))

    (assign-encoding-quality
     (get request-headers "accept-encoding"))

    (assign-charset-quality
     (get request-headers "accept-charset")))

   variants))

(defn segment-by
  "Return a map containing a :variants entry containing a collection of variants
  which match the highest score, and :rejects containing a collection of
  variants with a lower score. The score is determined by the scorer function
  which is called with the variant as a single argument.

  This function is commonly used when negotiating representations based on
  process-of-elimination techniques."
  [variants scorer comparator]
  (reduce
   (fn [acc variant]
     (let [score (or (scorer variant) 0)
           max-score-so-far (get acc :max-score-so-far 0)
           variant (assoc variant :score score)]
       (cond
         (comparator score max-score-so-far)
         (-> acc
             (assoc
              :variants [variant]
              :max-score-so-far score)
             (update :rejects into (:variants acc)))

         (= score max-score-so-far)
         (update acc :variants conj variant)

         :else
         (update acc :rejects conj variant))))
   {:variants [] :rejects []}
   variants))

(defprotocol VariantSelector
  :extend-via-metadata true
  (select-variant [_ opts]))
