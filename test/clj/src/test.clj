(ns test
  (:require
   [clj-arsenal.check :as check]
   [clj-arsenal.action]))

(defn run
  [& _]
  (check/report-all-checks-and-exit!))
