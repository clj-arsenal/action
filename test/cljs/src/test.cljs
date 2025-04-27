(ns test
  (:require
   [clj-arsenal.check :as check]
   [clj-arsenal.action]))

(defn run
  []
  (check/report-all-checks-and-exit!))
