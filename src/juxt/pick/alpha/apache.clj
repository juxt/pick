;; Copyright © 2020, JUXT LTD.

(ns juxt.pick.alpha.apache
  (:require
   [juxt.pick.alpha.core
    :refer [rate-representations segment-by]]))

(alias 'http (create-ns 'juxt.http.alpha))
(alias 'pick (create-ns 'juxt.pick.alpha))

;; An implementation in Clojure of Apache's httpd Negotiation Algorithm:
;; http://httpd.apache.org/docs/current/en/content-negotiation.html#algorithm

(defn add-meta [result var]
  (assoc result ::fn-meta (meta var)))

(defn
  ^{::step 1}
  select-media-type
  "Section 2.1: 'Multiply the quality factor from the Accept header with the
  quality-of-source factor for this variants media type, and select the variants
  with the highest value.'"
  [{::pick/keys [representations]}]
  (let [result
        (segment-by
         representations
         #(* (get % ::pick/content-type-qvalue 1.0)
             (get % ::pick/quality-of-source 1.0))
         >)]
    (add-meta result #'select-media-type)))

(defn
  ^{::step 2}
  select-languages
  "Section 2.2: 'Select the variants with the highest language quality factor.'"
  [{::pick/keys [representations]}]
  (let [result
        (segment-by representations :juxt.pick.alpha/language-qvalue >)]
    (add-meta result #'select-languages)))

(defn
  ^{::step 3}
  select-language
  "Section 2.3: 'Select the variants with the best language match, using either
  the order of languages in the Accept-Language header (if present).'"
  [{::pick/keys [representations]}]
  (let [result
        (segment-by representations ::pick/language-ordering-weight >)]
    (add-meta result #'select-language)))

(defn ^{::step 5}
  select-charsets
  "Section 2.5: 'Select variants with the best charset media parameters, as given
  on the Accept-Charset header line. Charset ISO-8859-1 is acceptable unless
  explicitly excluded. Variants with a text/* media type but not explicitly
  associated with a particular charset are assumed to be in ISO-8859-1.'"
  [{::pick/keys [representations]}]
  (let [result (segment-by representations ::pick/charset-qvalue >)]
    (add-meta result #'select-charsets)))

(defn
  ^{::step 7}
  select-encoding
  "Section 2.7: 'Select the variants with the best encoding. If there are variants
  with an encoding that is acceptable to the user-agent, select only these
  variants. Otherwise if there is a mix of encoded and non-encoded variants,
  select only the unencoded variants. If either all variants are encoded or all
  variants are not encoded, select all variants.'"
  [{::pick/keys [representations]}]
  (let [result
        (segment-by representations ::pick/encoding-qvalue >)]
    (-> result
        (add-meta #'select-encoding))))

(defn
  ^{::step 8}
  select-smallest-content-length
  "Section 2.8: 'Select the variants with the smallest content length.'"
  [{::pick/keys [representations]}]
  (let [[representation & rejects]
      (sort-by
       ::http/content-length
       (map (fn [x] (dissoc x :juxt.http.alpha/body)) representations))]
  (->
   {:representations [representation] :rejects rejects}
   (add-meta #'select-smallest-content-length))))

(defn
  ^{::step 9}
  select-first-remaining
  "Section 2.9: 'Select the first variant of those remaining.'"
  [{::pick/keys [representations]}]
  (let [[f & t] representations]
    (-> {:representations [f] :rejects (vec t)}
        (add-meta #'select-first-remaining))))

(defn apache-select-representation
  "Implementation of the Apache httpd content-negotiation algorithm detailed at
  https://httpd.apache.org/docs/current/en/content-negotiation.html#algorithm

  Options include:

  :juxt.pick.alpha/request-headers – a map, keyed by the lower-case header name with the reap parsed headers as values
  :juxt.pick.alpha/representations – a collection of representations
  :juxt.pick.alpha/explain? – if truthy, provide an explain in the return value
  :juxt.pick.alpha/vary? – if truthy, compute the preferences that will vary the choice
  :juxt.pick.alpha/inject-steps – in future, this will be used to inject additional steps

  "
  [{::pick/keys [request-headers representations vary? explain?] :as opts}]
  (let [rated-representations (rate-representations request-headers representations)

        ;; "If the Accept* header for any dimension implies that this variant is
        ;; not acceptable, eliminate it."
        acceptable
        (filter ::pick/acceptable? rated-representations)

        reductions
        (reductions
         (fn [acc step]
           ;; Short-circuit the algorithm when 0 or 1 representation remains.
           (if (< (count (:representations acc)) 2)
             (reduced (dissoc acc :rejects))
             (-> (step (assoc opts ::pick/representations (:representations acc))))))

         {:representations acceptable
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

          select-smallest-content-length

          select-first-remaining
          ])]

    (cond->
        {::pick/representation (first (:representations (last reductions)))
         ::pick/representations rated-representations}
        vary?
        (conj [::pick/vary
               (cond-> []
                 (> (count (distinct (keep :juxt.reap.alpha.rfc7231/content-type representations))) 1)
                 (conj "accept")
                 (> (count (distinct (keep :juxt.reap.alpha.rfc7231/content-encoding representations))) 1)
                 (conj "accept-encoding")
                 (> (count (distinct (keep :juxt.reap.alpha.rfc7231/content-language representations))) 1)
                 (conj "accept-language")
                 (> (count (distinct (keep (comp #(get % "charset") :juxt.reap.alpha.rfc7231/parameter-map :juxt.reap.alpha.rfc7231/content-type) representations))) 1)
                 (conj "accept-charset"))])

        explain? (conj [::pick/rejects
                        (reduce
                         (fn [rejects reduction]
                           (->> (:rejects reduction)
                                (map (fn [res]
                                       (assoc res ::pick/rejected-by (::fn-meta reduction))))
                                (into rejects)))
                         [] reductions)]
                       [::pick/reductions reductions]))))
