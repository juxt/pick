;; Copyright © 2020, JUXT LTD.

(ns juxt.pick.alpha.ring-test
  (:require
   [juxt.pick.alpha.ring :as pick.ring]
   [clojure.test :refer [deftest is]]))

(alias 'pick (create-ns 'juxt.pick.alpha))
(alias 'http (create-ns 'juxt.http.alpha))

(def variants [{::http/content-type "text/html;charset=utf-8"
                ::http/content-language "en"}

              {::http/content-type "text/html;charset=utf-8"
               ::http/content-language "de"}

              {::http/content-type "text/plain;charset=utf-8"}])

(deftest api-test
  (let [variants
        [{::http/content-type "text/html;charset=utf-8"
          ::http/content-language "en"}

         {::http/content-type "text/html;charset=utf-8"
          ::http/content-language "de"}

         {::http/content-type "text/plain;charset=utf-8"}]]
    (is
     (=
      "en"
      (get-in
       (pick.ring/pick
        {:request-method :get
         :uri "/"
         :headers {"accept" "text/html"
                   "accept-language" "en, de, es"}}
        variants)
       [::pick/representation ::http/content-language])))

    (is
     (=
      "de"
      (get-in
       (pick.ring/pick
        {:request-method :get
         :uri "/"
         :headers {"accept" "text/html"
                   "accept-language" "de"}}
        variants)
       [::pick/representation ::http/content-language])))

    (is
     (nil?
      (::pick/representation
       (pick.ring/pick
        {:request-method :get
         :uri "/"
         :headers {"accept" "text/html"
                   "accept-language" "es"}}
        variants))))

    (is
     (=
      "en"
      (get-in
       (pick.ring/pick
        {:request-method :get
         :uri "/"
         :headers {"accept" "text/html"
                   "accept-language" "es, en"}}
        variants)
       [::pick/representation ::http/content-language])))

    (is
     (=
      "text/plain;charset=utf-8"
      (get-in
       (pick.ring/pick
        {:request-method :get
         :uri "/"
         :headers {"accept" "text/plain"}}
        variants)
       [::pick/representation ::http/content-type])))

    (is
     (=
      "text/html;charset=utf-8"
      (get-in
       (pick.ring/pick
        {:request-method :get
         :uri "/"
         :headers {"accept" "text/html"}}
        variants)
       [::pick/representation ::http/content-type])))

    (is
     (=
      "text/plain;charset=utf-8"
      (get-in
       (pick.ring/pick
        {:request-method :get
         :uri "/"
         :headers {"accept" "text/plain"}}
        variants)
       [::pick/representation ::http/content-type])))

    (is
     (=
      "text/plain;charset=utf-8"
      (get-in
       (pick.ring/pick
        {:request-method :get
         :uri "/"
         :headers {"accept" "text/html;q=0.8,text/plain"}}
        variants)
       [::pick/representation ::http/content-type])))

    (is
     (=
      ["accept" "accept-language"]
      (::pick/vary
       (pick.ring/pick
        {:request-method :get
         :uri "/"
         :headers {"accept" "text/html"}}
        variants
        {::pick/vary? true}))))))

(deftest malformed-content-type-test
  (is
   (thrown-with-msg?
    clojure.lang.ExceptionInfo
    #"Malformed content-type"
    (pick.ring/pick
     {:request-method :get
      :uri "/"
      :headers {"accept" "text/html"}}
     [{::http/content-type "texthtml"}]))))

(deftest no-metadata-key-test
  (is
   (thrown-with-msg?
    clojure.lang.ExceptionInfo
    #"Representation must have a value for content-type.*"
    (pick.ring/pick
     {:request-method :get
      :uri "/"
      :headers {"accept" "text/html"}}
     [{}]))))
