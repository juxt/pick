;; Copyright © 2020-2024, JUXT LTD.

(ns juxt.pick.ring
  (:require
   [juxt.pick.impl.apache :refer [apache-select-representation]]
   [juxt.reap.regex :as re]
   [juxt.reap.ring :as reap.ring]
   [juxt.reap.decoders.rfc7231 :as rfc7231]))

(def content-type-decoder (rfc7231/content-type {}))
(def content-language-decoder (rfc7231/content-language {}))
(def content-encoding-decoder (rfc7231/content-encoding {}))

(defn wrap-representation
  "A representation can be a Clojure map or an object (satisfying
  clojure.lang.IMeta) with metadata. Therefore, we wrap this
  representation within a Clojure map."
  [rep]
  (let [meta (meta rep)
        content-type (or (get meta "content-type")
                         (when (associative? rep) (:juxt.http/content-type rep)))
        content-language (or (get meta "content-language")
                             (when (associative? rep)
                               (:juxt.http/content-language rep)))
        content-encoding (or (get meta "content-encoding")
                             (when (associative? rep) (:juxt.http/content-encoding rep)))
        ;; If content-length is given as a string key with a string
        ;; value, we parse the string into a long since it may be
        ;; required as a sorting key in the selection algorithm.
        content-length (get meta "content-length")]

    (when-not content-type
      (throw
       (ex-info
        "Representation must have a value for content-type"
        {:representation rep})))

    ;; TODO: Ultimately, need to use delays to avoid parsing these headers
    ;; multiple times in the same request.

    (cond-> {:juxt.pick/wrapped-representation rep}
      (and content-type (not (:juxt.reap.rfc7231/content-type rep)))
      (assoc :juxt.reap.rfc7231/content-type
             (or (content-type-decoder (re/input content-type))
                 (throw (ex-info "Malformed content-type" {:input content-type}))))

      (and content-language (not (:juxt.reap.rfc7231/content-language rep)))
      (assoc :juxt.reap.rfc7231/content-language
             (or
              (content-language-decoder (re/input content-language))
              (throw (ex-info "Malformed content-language" {:input content-language}))))

      (and content-encoding (not (:juxt.reap.rfc7231/content-encoding rep)))
      (assoc :juxt.reap.rfc7231/content-encoding
             (or
              (content-encoding-decoder (re/input content-encoding))
              (throw (ex-info "Malformed content-encoding" {:input content-encoding}))))

      (string? content-length)
      (assoc :juxt.http/content-length (Long/parseLong content-length)))))

(defn pick
  ([request representations]
   (pick request representations nil))
  ([request representations opts]
   (->
    (apache-select-representation
     (into
      {:juxt.pick/request-headers (reap.ring/headers->decoded-preferences
                                   (or
                                    (:ring.request/headers request)
                                    ;; For compatibility
                                    (:headers request)))
       :juxt.pick/representations (map wrap-representation representations)}
      opts))
    (update :juxt.pick/representation :juxt.pick/wrapped-representation)
    (update :juxt.pick/representations #(map :juxt.pick/wrapped-representation %)))))
