;; Copyright © 2020, JUXT LTD.

(ns juxt.pick.alpha.apache
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
  what is acceptable. The parsed-accept-fields parameter is a data structure
  returned from parsing the Accept header with reap. The variant is a map
  corresponding to the resource of the variant.

  This function determines the qvalue according to rules of precedence in RFC
  7231 Section 5.3.2 which are independent of the actual content negotation
  used.

  The qvalue is set to 0 if 'not acceptable' (See RFC 7231 Section 5.3.1). This
  still gives the content negotiation algorithm the chance of returning a
  variant, if there are no more preferable variants and if returning one is
  preferable to returning a 406 status code."

  [parsed-accept-fields parsed-content-type]

  (reduce
   select-better-content-type-match
   {:qvalue 0.0
    :content-type parsed-content-type}
   parsed-accept-fields))

;; Charsets

(defn acceptable-charset-rating
  [parsed-accept-charset-fields parsed-content-type]
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
     parsed-accept-charset-fields)))

;; Encodings

(defn select-best-encoding-match [accept-encoding-fields entry]
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

   accept-encoding-fields))

(defn acceptable-encoding-qvalue
  "Determine the qvalue for the given parsed content-encoding according to the
  given parsed Accept-Encoding header fields.

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
  is acceptable. The parsed-accept-language-fields parameter is a data structure
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

  [parsed-accept-language-fields parsed-language-tag]

  (reduce
   select-better-language-match
   {:qvalue 0.0
    :language-tag parsed-language-tag}
   parsed-accept-language-fields))


;; Apache httpd Negotiation Algorithm -- http://httpd.apache.org/docs/current/en/content-negotiation.html#algorithm

;; Dimensions:
;;
;; media-type
;; language
;; encoding
;; charset

;; httpd can use the following algorithm to select the 'best' variant (if any) to return to the browser. This algorithm is not further configurable. It operates as follows:

;;     First, for each dimension of the negotiation, check the appropriate Accept* header field and assign a quality to each variant. If the Accept* header for any dimension implies that this variant is not acceptable, eliminate it. If no variants remain, go to step 4.
;;     Select the 'best' variant by a process of elimination. Each of the following tests is applied in order. Any variants not selected at each test are eliminated. After each test, if only one variant remains, select it as the best match and proceed to step 3. If more than one variant remains, move on to the next test.
;;         Multiply the quality factor from the Accept header with the quality-of-source factor for this variants media type, and select the variants with the highest value.
;;         Select the variants with the highest language quality factor.
;;         Select the variants with the best language match, using either the order of languages in the Accept-Language header (if present), or else the order of languages in the LanguagePriority directive (if present).
;;         Select the variants with the highest 'level' media parameter (used to give the version of text/html media types).
;;         Select variants with the best charset media parameters, as given on the Accept-Charset header line. Charset ISO-8859-1 is acceptable unless explicitly excluded. Variants with a text/* media type but not explicitly associated with a particular charset are assumed to be in ISO-8859-1.
;;         Select those variants which have associated charset media parameters that are not ISO-8859-1. If there are no such variants, select all variants instead.
;;         Select the variants with the best encoding. If there are variants with an encoding that is acceptable to the user-agent, select only these variants. Otherwise if there is a mix of encoded and non-encoded variants, select only the unencoded variants. If either all variants are encoded or all variants are not encoded, select all variants.
;;         Select the variants with the smallest content length.
;;         Select the first variant of those remaining. This will be either the first listed in the type-map file, or when variants are read from the directory, the one whose file name comes first when sorted using ASCII code order.
;;     The algorithm has now selected one 'best' variant, so return it as the response. The HTTP response header Vary is set to indicate the dimensions of negotiation (browsers and caches can use this information when caching the resource). End.
;;     To get here means no variant was selected (because none are acceptable to the browser). Return a 406 status (meaning "No acceptable representation") with a response body consisting of an HTML document listing the available variants. Also set the HTTP Vary header to indicate the dimensions of variance.

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
             ;; TODO: Ahead of time
             (if parsed-accept-language-header
               (:qvalue
                (acceptable-language-rating
                 parsed-accept-language-header
                 ;; Content languages can be lists of language tags for the
                 ;; 'intended audience'. But for the purposes of language
                 ;; negotiation, we pick the FIRST content-language in the
                 ;; list. The matching of multiple languages with a language tag
                 ;; is not defined by any RFC (as far as I can tell).
                 ;;
                 ;; TODO: We should now use the accept-encoding method of
                 ;; arriving at the combined quality factor via multiplication.
                 (first content-language)))
               ;; TODO: But note, this should be the qsf to allow server
               ;; preference.
               1.0))]
       (cond-> variant
         qvalue (conj [:juxt.http.content-negotiation/language-qvalue qvalue]))))))

(defn assign-encoding-quality
  "Returns a transducer that will apply a rating on each of a collection of
  variants, according to the given parsed Accept-Encoding fields. This argument
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

(defn assign-charset-quality
  "Return a transducer that will assign a charset quality to each variant in a
  sequence according to the given parsed accept-charset fields. This argument
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

(defn rate-variants [request-headers variants]
  (sequence

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

   ;; TODO: Repeat for other dimensions. Short circuit, so if 0 variants left,
   ;; don't keep parsing headers! But with one left, keep parsing because maybe
   ;; that will be eliminated too! Keep in mind that 406 is discouraged for
   ;; unacceptable languages: "or honor the header field by sending a 406 (Not
   ;; Acceptable) response.  However, the latter is not encouraged, as doing so
   ;; can prevent users from accessing content that they might be able to use
   ;; (with translation software, for example). -- RFC 7231 Section 5.3.5"

   variants))

(defn- segment-by
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

(defn select-variant
  "Implementation of the Apache httpd content-negotiation algorithm detailed at
  https://httpd.apache.org/docs/current/en/content-negotiation.html#algorithm

  Options include:

  :juxt.http/request-headers – a map, keyed by the lower-case header name with the reap parsed headers as values

  :juxt.http/variants – a collection of variants

  :juxt.http/explain? – if truthy, provide an explain in the return value

  :juxt.http/inject-steps – in future, this will be used to inject additional steps

  "
  [{:juxt.http/keys [request-headers variants explain?]}]
  (let [rated-variants (rate-variants request-headers variants)
        explain
        (reduce
         (fn [acc step]
           ;; Short-circuit the algorithm when 0 or 1 representation remains.
           (if (< (count (:variants acc)) 2)
             (reduced acc)
             (-> acc :variants step (assoc :prev acc))))

         {:variants (vec rated-variants)
          :rejects []
          :phase "init"}

         ;; Algorithm steps
         [(fn [variants]
            (let [result
                  (segment-by
                   variants
                   #(* (get % :juxt.http.content-negotiation/content-type-qvalue 1.0)
                       (get % :juxt.http/quality-of-source 1.0))
                   >)]
              (assoc result :phase "select highest content type quality factor")))

          (fn [variants]
            (let [result (segment-by variants #(get % :juxt.http/language-quality-factor 1.0) >)]
              (assoc result :phase "select highest language quality factor")))

          (fn [variants]
            (let [result
                  (segment-by variants :juxt.http.content-negotiation/language-qvalue >)]
              (assoc result :phase "select best language match")))

          ;; TODO: Select the variants with the highest 'level' media
          ;; parameter (used to give the version of text/html media types).

          (fn [variants]
            (let [result (segment-by variants :juxt.http.content-negotiation/charset-qvalue >)]
              (assoc result :phase "select best charset media parameters")))

          ;; TODO: Select those variants which have associated charset media
          ;; parameters that are not ISO-8859-1.

          (fn [variants]
            (let [result
                  (segment-by variants :juxt.http.content-negotiation/encoding-qvalue >)]
              (assoc result :phase "select best encoding")))

          (fn [variants]
            (-> variants
                (segment-by :juxt.http/content-length <)
                (assoc :phase "select smallest content length")))

          ;; Select the first variant of those remaining
          (fn [variants]
            {:phase "select first variant"
             :variants [(first variants)]
             :rejects (next variants)})])]

    (cond->
        {:juxt.http/variant (first (:variants explain))
         :juxt.http/vary
         (cond-> []
           (> (count (distinct (keep :juxt.http/content-type variants))) 1)
           (conj {:juxt.http/field-name "accept"})
           (> (count (distinct (keep :juxt.http/content-encoding variants))) 1)
           (conj {:juxt.http/field-name "accept-encoding"})
           (> (count (distinct (keep :juxt.http/content-language variants))) 1)
           (conj {:juxt.http/field-name "accept-language"})
           (> (count (distinct (keep (comp #(get % "charset") :juxt.http/parameter-map :juxt.http/content-type) variants))) 1)
           (conj {:juxt.http/field-name "accept-charset"}))}
      explain? (assoc :juxt.http/explain explain))))

;; TODO: Produce an 'explain' for each content negotiation that can be
;; logged/response on a 406 (and to support debugging). Perhaps as a 406 body
;; but also by using an Expect (which is a 'must understand' semantic) or Prefer
;; header (which isn't)? See RFC 7240.
