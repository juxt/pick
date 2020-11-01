;; Copyright © 2020, JUXT LTD.

(ns juxt.pick.alpha.apache
  (:require
   [juxt.pick.alpha.core
    :refer [rate-representations segment-by pick]]
   [juxt.reap.alpha.rfc7231 :as rfc7231]))

;; An implementation in Clojure of Apache's httpd Negotiation Algorithm:
;; http://httpd.apache.org/docs/current/en/content-negotiation.html#algorithm

(defn add-meta [result var]
  (assoc result :juxt.pick.apache/fn-meta (meta var)))

(defn
  ^{:juxt.pick.apache/step 1}
  select-media-type
  "Multiply the quality factor from the Accept header with the quality-of-source
  factor for this variants media type, and select the variants with the highest
  value."
  [{:juxt.pick/keys [variants]}]
  (let [result
        (segment-by
         variants
         #(* (get % :juxt.pick/content-type-qvalue 1.0)
             (get % :juxt.pick/quality-of-source 1.0))
         >)]
    (add-meta result #'select-media-type)))

(defn
  ^{:juxt.pick.apache/step 2}
  select-languages
  "Select the variants with the highest language quality factor."
  [{:juxt.pick/keys [variants]}]
  (let [result
        (segment-by variants :juxt.pick/language-qvalue >)]
    (add-meta result #'select-languages)))

(defn
  ^{:juxt.pick.apache/step 3}
  select-language
  "Select the variants with the best language match, using either the order of
  languages in the Accept-Language header (if present)."
  [{:juxt.pick/keys [variants]}]
  (let [result
        (segment-by variants :juxt.pick/language-ordering-weight >)]
    (add-meta result #'select-language)))

(defn ^{:juxt.pick.apache/step 5}
  select-charsets
  "Select variants with the best charset media parameters, as given on the
  Accept-Charset header line. Charset ISO-8859-1 is acceptable unless explicitly
  excluded. Variants with a text/* media type but not explicitly associated with
  a particular charset are assumed to be in ISO-8859-1."
  [{:juxt.pick/keys [variants]}]
  (let [result (segment-by variants :juxt.pick/charset-qvalue >)]
    (add-meta result #'select-charsets)))

(defn
  ^{:juxt.pick.apache/step 7}
  select-encoding
  "Select the variants with the best encoding. If there are variants with an
  encoding that is acceptable to the user-agent, select only these
  variants. Otherwise if there is a mix of encoded and non-encoded variants,
  select only the unencoded variants. If either all variants are encoded or all
  variants are not encoded, select all variants."
  [{:juxt.pick/keys [variants]}]
  (let [result
        (segment-by variants :juxt.pick/encoding-qvalue >)]
    (-> result
     (add-meta #'select-encoding)
     (assoc :originals variants))))

(defn
  ^{:juxt.pick.apache/step 8}
  select-smallest-content-length
  "Select the variants with the smallest content length."
  [{:juxt.pick/keys [variants]}]
  (-> {:variants variants :rejects []}
      (assoc :phase "select smallest content length")))

(defn apache-select-variant
  "Implementation of the Apache httpd content-negotiation algorithm detailed at
  https://httpd.apache.org/docs/current/en/content-negotiation.html#algorithm

  Options include:

  :juxt.http/request-headers – a map, keyed by the lower-case header name with the reap parsed headers as values
  :juxt.http/variants – a collection of variants
  :juxt.http.content-negotiation/explain? – if truthy, provide an explain in the return value
  :juxt.http.content-negotiation/inject-steps – in future, this will be used to inject additional steps

  "
  [{:juxt.pick/keys [request-headers variants explain?] :as opts}]
  (let [rated-variants (rate-representations request-headers variants)
        explain
        (reduce
         (fn [acc step]
           ;; Short-circuit the algorithm when 0 or 1 representation remains.
           #_(-> (step (assoc opts :juxt.pick/variants (:variants acc)))
               (assoc :prev acc))

           (if (< (count (:variants acc)) 2)
               (reduced acc)
               (-> (step (assoc opts :juxt.pick/variants (:variants acc)))
                   (assoc :prev acc))))

         {:variants (vec rated-variants)
          :rejects []}

         ;; Algorithm steps
         [select-media-type

          select-languages

          select-language

          ;; TODO: Select the variants with the highest 'level' media
          ;; parameter (used to give the version of text/html media types).

          select-charsets

          ;; TODO: Select those variants which have associated charset media
          ;; parameters that are not ISO-8859-1.

          select-encoding

          select-smallest-content-length])]

    (cond->
        {:juxt.pick/variants (:variants explain)
         :juxt.pick/varying
         (cond-> []
           (> (count (distinct (keep ::rfc7231/content-type variants))) 1)
           (conj {:juxt.pick/field-name "accept"})
           (> (count (distinct (keep ::rfc7231/content-encoding variants))) 1)
           (conj {:juxt.pick/field-name "accept-encoding"})
           (> (count (distinct (keep ::rfc7231/content-language variants))) 1)
           (conj {:juxt.pick/field-name "accept-language"})
           (> (count (distinct (keep (comp #(get % "charset") ::rfc7231/parameter-map ::rfc7231/content-type) variants))) 1)
           (conj {:juxt.pick/field-name "accept-charset"}))}
        explain? (assoc :juxt.pick/explain explain))))

;; Extend the juxt.pick.alpha.core.VariantSelector via metadata
(def using-apache-algo
  (with-meta
    {:description "Implementation of Apache's content negotiation algorithm"
     :url "http://httpd.apache.org/docs/current/en/content-negotiation.html#algorithm"}
    {`pick (fn [_ opts] (apache-select-variant opts))}))
