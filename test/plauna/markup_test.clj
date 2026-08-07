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

(deftest email-list-renders-explicit-column-filter-workflow-and-active-chips
  (with-redefs [client/disconnected-connections (fn [] [])]
    (let [html (markup/list-emails
                []
                {:filter "all" :size 20 :page 1 :total-pages 0
                 :any-filter-active? true
                 :search-active? true :search-text "annual invoice"
                 :subject-filter-active? true :subject-filter-label "Subject: 1 selected"
                 :subject-filter-badge "1"}
                {:categories [] :category-filter-options []
                 :subjects [{:subject "Invoice" :checked? true}
                            {:subject "Newsletter" :checked? false}]
                 :senders [] :recipients []})]
      (is (str/includes? html "Active filters:"))
      (is (str/includes? html "Body: annual invoice"))
      (is (str/includes? html "Subject: 1 selected"))
      (is (str/includes? html "Select all visible"))
      (is (str/includes? html "<details class=\"dropdown\">"))
      (is (str/includes? html "<summary class=\"inline-flex"))
      (is (str/includes? html "w-96 max-w-[90vw] min-w-0 overflow-hidden"))
      (is (str/includes? html "id=\"subject-checklist\" class=\"max-h-56 overflow-y-auto overflow-x-hidden flex flex-col gap-1 w-full max-w-full min-w-0\""))
      (is (str/includes? html "grid grid-cols-3 gap-1 mt-2 w-full max-w-full min-w-0"))
      (is (str/includes? html "id=\"missing-email-category-dialog\" role=\"dialog\""))
      (is (str/includes? html "E-Mail konnte nicht gefunden werden. Trotzdem Kategorie ändern?"))
      (is (str/includes? html "answerMissingEmailCategory(false)\">Nein"))
      (is (str/includes? html "answerMissingEmailCategory(true)\">Ja"))
      (is (str/includes? html "fields.force = 'true'"))
      (is (str/includes? html ">Apply</button>"))
      (is (str/includes? html ">Reset</button>"))
      (is (str/includes? html ">Cancel</button>"))
      (is (str/includes? html "data-applied-checked=\"true\""))
      (is (str/includes? html "data-applied-checked=\"false\""))
      (is (not (str/includes? html "onchange=\"document.getElementById('page-form').submit()\"")))))
  "Column-filter edits stay local until Apply and active filters remain visible above the table")
