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
    (and (:juxt.pick/content-type r) (not (:juxt.reap.alpha.rfc7231/content-type r)))
    (conj [:juxt.reap.alpha.rfc7231/content-type (content-type (re/input (:juxt.pick/content-type r)))])
    (and (:juxt.pick/content-language r) (not (:juxt.reap.alpha.rfc7231/content-language r)))
    (conj [:juxt.reap.alpha.rfc7231/content-language (content-language (re/input (:juxt.pick/content-language r)))])
    (and (:juxt.pick/content-encoding r) (not (:juxt.reap.alpha.rfc7231/content-encoding r)))
    (conj [:juxt.reap.alpha.rfc7231/content-encoding (content-encoding (re/input (:juxt.pick/content-encoding r)))])))

(defn pick
  ([request representations]
   (pick request representations nil))
  ([request representations opts]
   (apache-select-representation
    (into
     {:juxt.pick/request-headers (ring/request->decoded-preferences request)
      :juxt.pick/representations (map decode-maybe representations)}
     opts))))

#_(ring/request->decoded-preferences
                              {:request-method :get
                               :uri "/"
                               :headers {"accept" "text/html"
                                         "accept-language" "de"}})

#_(map decode-maybe [{:juxt.pick/content-type "text/html;charset=utf-8"
                      :juxt.pick/content-language "en"}

                     {:juxt.pick/content-type "text/html;charset=utf-8"
                      :juxt.pick/content-language "de"}

                   {:juxt.pick/content-type "text/plain;charset=utf-8"}])


#_(apache-select-representation
 {:juxt.pick/request-headers (ring/request->decoded-preferences
                              {:request-method :get
                               :uri "/"
                               :headers {"accept" "text/html"
                                         "accept-language" "de"}})
  :juxt.pick/representations
  (map decode-maybe [{:juxt.pick/content-type "text/html;charset=utf-8"
                      :juxt.pick/content-language "en"}

                     {:juxt.pick/content-type "text/html;charset=utf-8"
                      :juxt.pick/content-language "de"}

                     {:juxt.pick/content-type "text/plain;charset=utf-8"}])})
