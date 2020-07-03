;; Copyright © 2020, JUXT LTD.

(ns juxt.pick.alpha.apache
  (:require
   [juxt.pick.alpha.core
    :refer [rate-variants segment-by pick]]))

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

(defn apache-select-variant
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

          ])]

    (cond->
        {:juxt.http/variants (:variants explain)
         :juxt.http/varying
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

;; Extend the juxt.pick.alpha.core.VariantSelector via metadata
(def using-apache-algo
  (with-meta
    {:description "Implementation of Apache's content negotiation algorithm"
     :url "http://httpd.apache.org/docs/current/en/content-negotiation.html#algorithm"}
    {`pick (fn [_ opts] (apache-select-variant opts))}))
