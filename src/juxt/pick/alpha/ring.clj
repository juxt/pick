;; Copyright © 2020, JUXT LTD.

(ns juxt.pick.alpha.ring
  (:require
   [juxt.reap.alpha.regex :as re]
   [juxt.reap.alpha.ring :as reap.ring]
   [juxt.pick.alpha.apache :refer [apache-select-representation]]
   [juxt.reap.alpha.decoders.rfc7231 :as rfc7231]))

(def content-type (rfc7231/content-type {}))
(def content-language (rfc7231/content-language {}))
(def content-encoding (rfc7231/content-encoding {}))

(defn decode-maybe [r]
  (cond-> r
    (and (get r "content-type") (not (:juxt.reap.alpha.rfc7231/content-type r)))
    (assoc :juxt.reap.alpha.rfc7231/content-type
           (or
            (content-type (re/input (get r "content-type")))
            (throw (ex-info "Malformed content-type" {:input (get r "content-type")}))))
    (and (get r "content-language") (not (:juxt.reap.alpha.rfc7231/content-language r)))
    (assoc :juxt.reap.alpha.rfc7231/content-language
           (or
            (content-language (re/input (get r "content-language")))
            (throw (ex-info "Malformed content-language" {:input (get r "content-language")}))))
    (and (get r "content-encoding") (not (:juxt.reap.alpha.rfc7231/content-encoding r)))
    (assoc :juxt.reap.alpha.rfc7231/content-encoding
           (or
            (content-encoding (re/input (get r "content-encoding")))
            (throw (ex-info "Malformed content-encoding" {:input (get r "content-encoding")}))))))

(defn pick
  ([request representations]
   (pick request representations nil))
  ([request representations opts]
   (apache-select-representation
    (into
     {:juxt.pick.alpha/request-headers (reap.ring/headers->decoded-preferences (:headers request))
      :juxt.pick.alpha/representations (map decode-maybe representations)}
     opts))))
