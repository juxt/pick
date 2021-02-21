;; Copyright © 2020, JUXT LTD.

(ns juxt.pick.alpha.readme-test
  (:require
   [juxt.pick.alpha.ring :refer [pick]]
   [clojure.test :refer [deftest is]]))

(alias 'http (create-ns 'juxt.http.alpha))

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
     [:juxt.pick.alpha/representation :id]))))
