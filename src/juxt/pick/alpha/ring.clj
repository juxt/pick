;; Copyright © 2020, JUXT LTD.

(ns juxt.pick.alpha.ring
  (:require
   [juxt.pick.alpha :as pick]
   [juxt.reap.alpha.regex :as re]
   [juxt.reap.alpha.ring :as reap.ring]
   [juxt.pick.alpha.apache :refer [apache-select-representation]]
   [juxt.reap.alpha.decoders.rfc7231 :as rfc7231]))

(def content-type-decoder (rfc7231/content-type {}))
(def content-language-decoder (rfc7231/content-language {}))
(def content-encoding-decoder (rfc7231/content-encoding {}))

(defn decode-maybe [rep]
  (let [content-type (get-in rep [::pick/representation-metadata "content-type"])
        content-language (get-in rep [::pick/representation-metadata "content-language"])
        content-encoding (get-in rep [::pick/representation-metadata "content-encoding"])]
    (cond-> rep
      (and content-type (not (:juxt.reap.alpha.rfc7231/content-type rep)))
      (assoc :juxt.reap.alpha.rfc7231/content-type
             (or (content-type-decoder (re/input content-type))
                 (throw (ex-info "Malformed content-type" {:input content-type}))))

      (and content-language (not (:juxt.reap.alpha.rfc7231/content-language rep)))
      (assoc :juxt.reap.alpha.rfc7231/content-language
             (or
              (content-language-decoder (re/input content-language))
              (throw (ex-info "Malformed content-language" {:input content-language}))))

      (and content-encoding (not (:juxt.reap.alpha.rfc7231/content-encoding rep)))
      (assoc :juxt.reap.alpha.rfc7231/content-encoding
             (or
              (content-encoding-decoder (re/input content-encoding))
              (throw (ex-info "Malformed content-encoding" {:input content-encoding})))))))

(defn pick
  ([request representations]
   (pick request representations nil))
  ([request representations opts]
   (apache-select-representation
    (into
     {::pick/request-headers (reap.ring/headers->decoded-preferences (:headers request))
      ::pick/representations (map decode-maybe representations)}
     opts))))
