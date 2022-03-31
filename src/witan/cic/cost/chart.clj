(ns witan.cic.cost.chart
  (:require [kixi.large :as xl]
            [kixi.plot :as plot]
            [kixi.plot.colors :as colors]
            [kixi.plot.series :as series]
            [tablecloth.api :as tc]
            [tick.core :as tick]))

(defn cyp-count-per-placement-per-day [cost-and-cyp-summary]
  (into []
        (comp
         (map (fn [[k v]]
                [(get k "Placement")
                 {:data (tc/select-columns v
                                           ["Placement" "Date"
                                            "Min CYP" "Low 95% CYP" "Q1 CYP" "Median CYP" "Q3 CYP" "High 95% CYP" "Max CYP"])}]))
         (map (fn [[k {:keys [data] :as v}]]
                [k (assoc v :series
                          (series/ds->line-and-double-ribbon
                           data
                           {:color colors/mc-dark-blue
                            :shape \X
                            :x "Date"
                            :line-y "Median CYP"
                            :ribbon-1-high-y "Q3 CYP"
                            :ribbon-1-low-y "Q1 CYP"
                            :ribbon-2-high-y "High 95% CYP"
                            :ribbon-2-low-y "Low 95% CYP"}))]))
         (map (fn [[k {:keys [series] :as v}]]
                [k (assoc v :chart
                          (as-> {::series/series series
                                 ::plot/title {::plot/label (format "%s CYP per day" k)}
                                 ::plot/x-axis {::plot/tick-formatter (fn [d] (tick/format (tick/formatter "yyyy-MMM") d))
                                                ::plot/label          "Month"}
                                 ::plot/y-axis {::plot/tick-formatter int
                                                ::plot/label          "CYP"}
                                 ::plot/size {:width 1539 :height 1037 :background colors/white}
                                 } $
                            (plot/zero-y-index $)))])))
        cost-and-cyp-summary))

(defn cost-per-placement-type-per-day [cost-and-cyp-summary]
  (into []
        (comp
         (map (fn [[k v]]
                [(get k "Placement")
                 {:data (tc/select-columns v
                                           ["Placement" "Date"
                                            "Min Cost" "Low 95% Cost" "Q1 Cost" "Median Cost" "Q3 Cost" "High 95% Cost" "Max Cost"])}]))
         (map (fn [[k {:keys [data] :as v}]]
                [k (assoc v :series
                          (series/ds->line-and-double-ribbon
                           data
                           {:color colors/mc-dark-blue
                            :shape \X
                            :x "Date"
                            :line-y "Median Cost"
                            :ribbon-1-high-y "Q3 Cost"
                            :ribbon-1-low-y "Q1 Cost"
                            :ribbon-2-high-y "High 95% Cost"
                            :ribbon-2-low-y "Low 95% Cost"}))]))
         (map (fn [[k {:keys [series] :as v}]]
                [k (assoc v :chart
                          (as-> {::series/series series
                                 ::plot/title {::plot/label (format "%s Cost per day" k)}
                                 ::plot/x-axis {::plot/tick-formatter (fn [d] (tick/format (tick/formatter "yyyy-MMM") d))
                                                ::plot/label          "Month"}
                                 ::plot/y-axis {::plot/tick-formatter #(format "%,.0f" %) #_plot/millions-formatter
                                                ::plot/label          "Daily Cost (£)"}
                                 ::plot/size {:width 1539 :height 1037 :background colors/white}
                                 } $
                            (plot/zero-y-index $)))])))
        cost-and-cyp-summary))

(defn ->workbook [summary file-name]
  (-> (into []
            (comp
             (map (fn [[k {:keys [data chart]}]]
                    {::xl/sheet-name k
                     ::xl/data data
                     ::xl/images [{::xl/image (-> chart ::plot/canvas :buffer plot/->byte-array)}]})))
            summary)
      (xl/create-workbook)
      (xl/save-workbook! file-name)))

(comment

  ;; Use it like this


  )
