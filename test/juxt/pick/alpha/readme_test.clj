;; Copyright © 2020, JUXT LTD.

(ns juxt.pick.alpha.readme-test
  (:require
   [juxt.pick.alpha.ring :as pick]
   [clojure.test :refer [deftest is are testing]]))

(pick/pick
  ;; A ring request (where headers indicate preferences)
  {:request-method :get
   :uri "/"
   :headers {"accept" "text/html"
             "accept-language" "de"}}
  ;; Possible representations
  [{:juxt.pick.alpha/content-type "text/html;charset=utf-8"
    :juxt.pick.alpha/content-language "en"}

   {:juxt.pick.alpha/content-type "text/html;charset=utf-8"
    :juxt.pick.alpha/content-language "de"}

   {:juxt.pick.alpha/content-type "text/plain;charset=utf-8"}])
