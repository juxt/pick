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
  accumulator."

  [acc parsed-accept-field]

  (let [precedence (content-type-match? parsed-accept-field (:content-type acc))
        qvalue (get parsed-accept-field :juxt.http/qvalue 1.0)]

    (cond-> acc
      (and
       precedence
       (or
        (> precedence (get acc :precedence 0))
        (and (= precedence (get acc :precedence 0))
             (> qvalue (get acc :qvalue 0.0)))))
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
     (fn [acc field]
       (cond
         (= charset (:juxt.http/charset field))
         (cond-> acc
           (< (get acc :precedence) 2)
           (conj [:qvalue (get field :juxt.http/qvalue 1.0)]
                 [:precedence 2]
                 [:apex.debug/parsed-accept-charset-field field]))
         (= "*" (:juxt.http/charset field))
         (cond-> acc
           (= (get acc :precedence) 0)
           (conj [:qvalue (get field :juxt.http/qvalue 1.0)]
                 [:precedence 1]
                 [:apex.debug/parsed-accept-charset-field field]))
         :else acc))
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
   (fn [acc {accept-coding :juxt.http/codings :as field}]

     (cond
       (= accept-coding (get entry :juxt.http/content-coding "identity"))
       (cond-> acc
         (< (get acc :precedence) 2)
         (conj [:qvalue (get field :juxt.http/qvalue 1.0)]
               [:precedence 2]
               [:apex.debug/parsed-accept-encoding-field field]))

       (= accept-coding "*")
       (cond-> acc
         (= (get acc :precedence) 0)
         (conj [:qvalue (get field :juxt.http/qvalue 1.0)]
               [:precedence 1]
               [:apex.debug/parsed-accept-encoding-field field]))

       :else acc))

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
  [acc parsed-accept-language-field]
  (let [qvalue (get parsed-accept-language-field :juxt.http/qvalue 1.0)
        ;; '*' matches "every tag not matched by any other range" (RFC2616) so we
        ;; use a :precedence value for this purpose. A value of 1 means that a *
        ;; has been encountered. A value of 2 means that a specific language match
        ;; as occurred. An implicit value of 0 otherwise.
        precedence (get acc :precedence 0)]
    (if (.equals (:juxt.http/language-range parsed-accept-language-field) "*")
      (cond-> acc
        (= precedence 0)
        (conj
         [:qvalue qvalue]
         [:precedence 1]
         [:apex.debug/parsed-accept-language-field parsed-accept-language-field]))
      (cond-> acc
        (and
         (> qvalue (get acc :qvalue 0.0))
         (basic-language-match?
          (:juxt.http/language-range parsed-accept-language-field)
          (get-in acc [:language-tag :juxt.http/langtag])))
        (conj
         [:qvalue qvalue]
         [:precedence 2]
         [:apex.debug/parsed-accept-language-field parsed-accept-language-field])))))

(defn acceptable-language-quality
  "Determine the given language's quality (qvalue) with respect to what is
  acceptable. The parsed-accept-language-header parameter is a data structure
  returned from parsing the Accept-Language header with reap. This argument can
  be nil, meaning that no accept-language header was received:

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
      ;; TODO: I don't like using the accumulator to smuggle in a constant, use
      ;; a closure instead.
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
      (let [qualities
            (when parsed-accept-language-header
              (for [lang content-language]
                (acceptable-language-quality
                 parsed-accept-language-header
                 lang)))

            combined-qvalue
            (if qualities
              (double (apply * (map :qvalue qualities)))
              1.0)]

        (assoc
         variant
         :juxt.http.content-negotiation/language-qvalue
         combined-qvalue
         ))
      ;; No content-language, so no quality applied.
      variant)))

(defn assign-language-ordering
  "Return a function that will assign a language ordering weight to a variant
  according to the given parsed accept-language header. A content-language is
  composed of usually one, but possibly multiple, language tags. Each distinct
  content-language in the set of possible variants is assigned a language ordering
  weight. In the event that there are multiple content languages that share the same highest quality value, then a determination is made based on order given in the Accept-Language

  This is to help satisfy the requirement of step 3 of the Apache
  content-negotiation algorithm, but this function is core since it might be
  used by alternative algorithms."
  [parsed-accept-language-header]
  (fn [variant]
    (assert variant)
    (if-let [content-language
             (when parsed-accept-language-header
               (:juxt.http/content-language variant))]
      (let [weight (fn [accept weighting-factor]
                     (if (some #(basic-language-match?
                                 (:juxt.http/language-range accept)
                                 %) (map :juxt.http/langtag content-language))
                       weighting-factor
                       0))
            ;; Weight factors is a power series to create a weight in base N
            ;; where N is the number of accept fields.
            weight-factors (iterate
                            #(/ % 2)
                            (long (Math/pow 2 (dec (count parsed-accept-language-header)))))
            combined-ordering-weight (reduce + (map weight parsed-accept-language-header weight-factors))]
        (assoc variant :juxt.http.content-negotiation/language-ordering-weight combined-ordering-weight))
      ;; No content-language (or no accept-language header) so no
      ;; language-ordering applied.
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

    (assign-language-ordering
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
           max-score-so-far (get acc :max-score-so-far -1)
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
  (pick [_ opts]))
