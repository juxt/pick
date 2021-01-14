;; Copyright © 2020, JUXT LTD.

(ns juxt.pick.alpha.readme-test
  (:require
   [juxt.pick.alpha.ring :refer [pick]]
   [juxt.pick.alpha :as pick]
   [clojure.test :refer [deftest is are testing]]))

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
        ::pick/representation-metadata
        {"content-type" "text/html;charset=utf-8"
         "content-language" "en"}}

       {:id 2
        ::pick/representation-metadata
        {"content-type" "text/html;charset=utf-8"
         "content-language" "de"}}

       {:id 3
        ::pick/representation-metadata
        {"content-type" "text/plain;charset=utf-8"}}])
     [::pick/representation :id]))))
