(ns plauna.markup-test
  (:require [clojure.string :as str]
            [clojure.test :refer :all]
            [plauna.client :as client]
            [plauna.markup :as markup]))

(deftest disconnected-banner-is-hidden-when-no-connections-are-disconnected
  (with-redefs [client/disconnected-connections (fn [] [])]
    (let [html (markup/administration {:repl {:status false}})]
      (is (false? (str/includes? html "IMAP connection not active:"))))))

(deftest disconnected-banner-shows-disconnected-accounts
  (with-redefs [client/disconnected-connections (fn [] [{:user "me" :host "imap.example.com"}])]
    (let [html (markup/administration {:repl {:status false}})]
      (is (str/includes? html "IMAP connection not active:"))
      (is (str/includes? html "me@imap.example.com")))))
