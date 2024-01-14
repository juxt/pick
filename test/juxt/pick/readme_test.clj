;; Copyright © 2020-2024, JUXT LTD.

(ns juxt.pick.readme-test
  (:require
   [clojure.test :refer [deftest is]]
   [juxt.pick.ring :refer [pick]]))

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
        :juxt.http/content-type "text/html;charset=utf-8"
        :juxt.http/content-language "en"}

       {:id 2
        :juxt.http/content-type "text/html;charset=utf-8"
        :juxt.http/content-language "de"}

       {:id 3
        :juxt.http/content-type "text/plain;charset=utf-8"}])
     [:juxt.pick/representation :id]))))
