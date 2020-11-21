;; Copyright © 2020, JUXT LTD.

(ns juxt.pick.alpha.api
  (:require
   [juxt.reap.alpha.regex :as re]
   [juxt.pick.alpha.apache :refer [apache-select-representation]]
   [juxt.reap.alpha.ring :as ring]
   [juxt.reap.alpha.decoders.rfc7231 :as rfc7231]))

(def content-type (rfc7231/content-type {}))
(def content-language (rfc7231/content-language {}))
(def content-encoding (rfc7231/content-encoding {}))

(defn decode-maybe [r]
  (cond-> r
    (and (:juxt.pick.alpha/content-type r) (not (:juxt.reap.alpha.rfc7231/content-type r)))
    (conj [:juxt.reap.alpha.rfc7231/content-type (content-type (re/input (:juxt.pick.alpha/content-type r)))])
    (and (:juxt.pick.alpha/content-language r) (not (:juxt.reap.alpha.rfc7231/content-language r)))
    (conj [:juxt.reap.alpha.rfc7231/content-language (content-language (re/input (:juxt.pick.alpha/content-language r)))])
    (and (:juxt.pick.alpha/content-encoding r) (not (:juxt.reap.alpha.rfc7231/content-encoding r)))
    (conj [:juxt.reap.alpha.rfc7231/content-encoding (content-encoding (re/input (:juxt.pick.alpha/content-encoding r)))])))

(defn pick
  ([request representations]
   (pick request representations nil))
  ([request representations opts]
   (apache-select-representation
    (into
     {:juxt.pick.alpha/request-headers (ring/request->decoded-preferences request)
      :juxt.pick.alpha/representations (map decode-maybe representations)}
     opts))))
