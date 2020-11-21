;; Copyright © 2020, JUXT LTD.

(ns juxt.pick.alpha.apache
  (:require
   [juxt.pick.alpha.core
    :refer [rate-representations segment-by]]))

;; An implementation in Clojure of Apache's httpd Negotiation Algorithm:
;; http://httpd.apache.org/docs/current/en/content-negotiation.html#algorithm

(defn add-meta [result var]
  (assoc result :juxt.pick.alpha.apache/fn-meta (meta var)))

(defn
  ^{:juxt.pick.alpha.apache/step 1}
  select-media-type
  "Section 2.1: 'Multiply the quality factor from the Accept header with the
  quality-of-source factor for this variants media type, and select the variants
  with the highest value.'"
  [{:juxt.pick.alpha/keys [representations]}]
  (let [result
        (segment-by
         representations
         #(* (get % :juxt.pick.alpha/content-type-qvalue 1.0)
             (get % :juxt.pick.alpha/quality-of-source 1.0))
         >)]
    (add-meta result #'select-media-type)))

(defn
  ^{:juxt.pick.alpha.apache/step 2}
  select-languages
  "Section 2.2: 'Select the variants with the highest language quality factor.'"
  [{:juxt.pick.alpha/keys [representations]}]
  (let [result
        (segment-by representations :juxt.pick.alpha/language-qvalue >)]
    (add-meta result #'select-languages)))

(defn
  ^{:juxt.pick.alpha.apache/step 3}
  select-language
  "Section 2.3: 'Select the variants with the best language match, using either
  the order of languages in the Accept-Language header (if present).'"
  [{:juxt.pick.alpha/keys [representations]}]
  (let [result
        (segment-by representations :juxt.pick.alpha/language-ordering-weight >)]
    (add-meta result #'select-language)))

(defn ^{:juxt.pick.alpha.apache/step 5}
  select-charsets
  "Section 2.5: 'Select variants with the best charset media parameters, as given
  on the Accept-Charset header line. Charset ISO-8859-1 is acceptable unless
  explicitly excluded. Variants with a text/* media type but not explicitly
  associated with a particular charset are assumed to be in ISO-8859-1.'"
  [{:juxt.pick.alpha/keys [representations]}]
  (let [result (segment-by representations :juxt.pick.alpha/charset-qvalue >)]
    (add-meta result #'select-charsets)))

(defn
  ^{:juxt.pick.alpha.apache/step 7}
  select-encoding
  "Section 2.7: 'Select the variants with the best encoding. If there are variants
  with an encoding that is acceptable to the user-agent, select only these
  variants. Otherwise if there is a mix of encoded and non-encoded variants,
  select only the unencoded variants. If either all variants are encoded or all
  variants are not encoded, select all variants.'"
  [{:juxt.pick.alpha/keys [representations]}]
  (let [result
        (segment-by representations :juxt.pick.alpha/encoding-qvalue >)]
    (-> result
        (add-meta #'select-encoding))))

(defn
  ^{:juxt.pick.alpha.apache/step 8}
  select-smallest-content-length
  "Section 2.8: 'Select the variants with the smallest content length.'"
  [{:juxt.pick.alpha/keys [representations]}]
  (-> {:representations representations :rejects []}
      (add-meta #'select-smallest-content-length)))

(defn
  ^{:juxt.pick.alpha.apache/step 9}
  select-first-remaining
  "Section 2.9: 'Select the first variant of those remaining.'"
  [{:juxt.pick.alpha/keys [representations]}]
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
  [{:juxt.pick.alpha/keys [request-headers representations vary? explain?] :as opts}]
  (let [rated-representations (rate-representations request-headers representations)

        ;; "If the Accept* header for any dimension implies that this variant is
        ;; not acceptable, eliminate it."
        {acceptable true _ false}
        (group-by :juxt.pick.alpha/acceptable? rated-representations)

        reductions
        (reductions
         (fn [acc step]
           ;; Short-circuit the algorithm when 0 or 1 representation remains.
           (if (< (count (:representations acc)) 2)
             (reduced (dissoc acc :rejects))
             (-> (step (assoc opts :juxt.pick.alpha/representations (:representations acc))))))

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
        {:juxt.pick.alpha/representation (first (:representations (last reductions)))
         :juxt.pick.alpha/representations rated-representations}
        vary?
        (conj [:juxt.pick.alpha/vary
               (cond-> []
                 (> (count (distinct (keep :juxt.reap.alpha.rfc7231/content-type representations))) 1)
                 (conj "accept")
                 (> (count (distinct (keep :juxt.reap.alpha.rfc7231/content-encoding representations))) 1)
                 (conj "accept-encoding")
                 (> (count (distinct (keep :juxt.reap.alpha.rfc7231/content-language representations))) 1)
                 (conj "accept-language")
                 (> (count (distinct (keep (comp #(get % "charset") :juxt.reap.alpha.rfc7231/parameter-map :juxt.reap.alpha.rfc7231/content-type) representations))) 1)
                 (conj "accept-charset"))])

        explain? (conj [:juxt.pick.alpha/rejects
                        (reduce
                         (fn [rejects reduction]
                           (->> (:rejects reduction)
                                (map (fn [res]
                                       (assoc res :juxt.pick.alpha/rejected-by (:juxt.pick.alpha.apache/fn-meta reduction))))
                                (into rejects)))
                         [] reductions)]
                       [:juxt.pick.alpha/reductions reductions]))))
