;; Copyright © 2020, JUXT LTD.

(ns juxt.pick.alpha.api
  (:require
   [juxt.pick.alpha.apache :refer [apache-select-representation]]
   [juxt.reap.alpha.decoders :as reap]
   [juxt.reap.alpha.rfc7231 :as rfc7231]))

(defn decode-maybe [r]
  (cond-> r
    (and (:juxt.pick/content-type r) (not (::rfc7231/content-type r)))
    (conj [::rfc7231/content-type (reap/content-type (:juxt.pick/content-type r))])
    (and (:juxt.pick/content-language r) (not (::rfc7231/content-language r)))
    (conj [::rfc7231/content-language (reap/content-language (:juxt.pick/content-language r))])
    (and (:juxt.pick/content-encoding r) (not (::rfc7231/content-encoding r)))
    (conj [::rfc7231/content-encoding (reap/content-encoding (:juxt.pick/content-encoding r))])))

(defn pick
  ([request representations]
   (pick request representations nil))
  ([request representations opts]
   (apache-select-representation
    (into
     {:juxt.pick/request-headers (reap/request->decoded-preferences request)
      :juxt.pick/representations (map decode-maybe representations)}
     opts))))
