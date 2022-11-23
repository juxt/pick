;; Copyright © 2020, JUXT LTD.

(ns juxt.pick.readme-test
  (:require
   [clojure.test :refer [deftest is]]
   [juxt.pick.ring :refer [pick]]
   [juxt.http :as-alias http]))

(deftest readme-test
  (is
   (=
    2
    (get-in
     (pick
      ;; A ring request (where headers indicate preferences)
      {:request-method :get
       :uri "/"
       :headers {"accept" "text/html"
                 "accept-language" "de"}}

      ;; Possible representations
      [{:id 1
        ::http/content-type "text/html;charset=utf-8"
        ::http/content-language "en"}

       {:id 2
        ::http/content-type "text/html;charset=utf-8"
        ::http/content-language "de"}

       {:id 3
        ::http/content-type "text/plain;charset=utf-8"}])
     [:juxt.pick/representation :id]))))
