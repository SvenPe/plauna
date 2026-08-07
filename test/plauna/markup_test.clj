(ns plauna.markup-test
  (:require [clojure.string :as str]
            [clojure.test :refer :all]
            [plauna.client :as client]
            [plauna.markup :as markup]
            [plauna.preferences :as preferences]))

(deftest timestamps-are-rendered-in-the-configured-time-zone
  (with-redefs [preferences/zone-id (fn [] (java.time.ZoneId/of "Europe/Berlin"))]
    (is (= (java.time.LocalDateTime/of 1970 1 1 1 0)
           (markup/timestamp->date 0)))))

(deftest confidences-are-rendered-as-percentages
  (is (= "100.00 %" (markup/confidence->percent 1.0)))
  (is (= "87.65 %" (markup/confidence->percent 0.87654)))
  (is (= "0.00 %" (markup/confidence->percent 0)))
  (is (= "—" (markup/confidence->percent nil))))

(deftest preferences-page-renders-the-daily-training-time-and-time-zone
  (with-redefs [client/disconnected-connections (fn [] [])]
    (let [html (markup/preferences-page {:automatic-training-time "03:30"
                                         :time-zone "Europe/Berlin"
                                         :categorization-model "naive-bayes"
                                         :categorization-model-options [{:id "naive-bayes" :name "Naive Bayes"}
                                                                        {:id "maxent" :name "Maximum Entropy (MaxEnt)"}]
                                         :log-level-options []})]
      (is (str/includes? html "name=\"automatic-training-time\" type=\"time\" value=\"03:30\""))
      (is (str/includes? html "name=\"time-zone\" value=\"Europe/Berlin\""))
      (is (str/includes? html "action=\"/admin/preferences/model\""))
      (is (str/includes? html "value=\"naive-bayes\" selected"))
      (is (str/includes? html "value=\"maxent\""))
      (is (str/includes? html "Aktuelle Kategoriezuordnungen für das Training verwenden"))
      (is (str/includes? html "Training starten"))
      (is (str/includes? html "confirmationInput.value !== 'Training starten'")))))

(deftest disconnected-banner-is-hidden-when-no-connections-are-disconnected
  (with-redefs [client/disconnected-connections (fn [] [])]
    (let [html (markup/administration {:repl {:status false}})]
      (is (false? (str/includes? html "IMAP connection not active:"))))))

(deftest disconnected-banner-shows-disconnected-accounts
  (with-redefs [client/disconnected-connections (fn [] [{:user "me" :host "imap.example.com"}])]
    (let [html (markup/administration {:repl {:status false}})]
      (is (str/includes? html "IMAP connection not active:"))
      (is (str/includes? html "me@imap.example.com")))))

(deftest connection-pages-never-render-stored-secrets-or-statistics-scripts
  (with-redefs [client/disconnected-connections (fn [] [])]
    (let [stored-imap-secret "imap-secret-must-not-reach-browser"
          stored-client-secret "oauth-secret-must-not-reach-browser"
          html (markup/connection
                {:id "connection-1"
                 :host "imap.example.com"
                 :user "me@example.com"
                 :secret stored-imap-secret
                 :auth-providers [{:id 1
                                   :name "Example"
                                   :client-secret stored-client-secret}]}
                []
                [])]
      (is (not (str/includes? html stored-imap-secret)))
      (is (not (str/includes? html stored-client-secret)))
      (is (str/includes? html "Leave blank to keep the current secret"))
      (is (not (str/includes? html "cdn.jsdelivr.net")))
      (is (not (str/includes? html "/js/vendor/vega.min.js")))))
  "Admin pages contain blank password fields and do not load chart code")

(deftest statistics-page-loads-only-local-chart-scripts
  (with-redefs [client/disconnected-connections (fn [] [])]
    (let [html (markup/statistics-overall [] [] [] [])]
      (is (str/includes? html "/js/vendor/vega.min.js"))
      (is (str/includes? html "/js/vendor/vega-lite.min.js"))
      (is (str/includes? html "/js/vendor/vega-embed.min.js"))
      (is (not (str/includes? html "cdn.jsdelivr.net"))))))

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
