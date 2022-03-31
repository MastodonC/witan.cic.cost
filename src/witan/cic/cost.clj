(ns witan.cic.cost
  (:require [clojure.string :as s]
            [com.climate.claypoole :as cp]
            [com.climate.claypoole.lazy :as lazy]
            [kixi.large :as xl]
            [kixi.plot.series :as series]
            [kixi.plot.colors :as colors]
            [kixi.plot :as plot]
            [net.cgrand.xforms :as x]
            [tablecloth.api :as tc]
            [tech.v3.datatype.functional :as dfn]
            [tech.v3.dataset :as ds]
            [tech.v3.dataset.reductions :as ds-reduce]
            [tick.core :as tick]
            [tick.alpha.interval :as tai]))

(set! *warn-on-reflection* true)

(defn projection-file->dataset [projection-file-name]
  (-> projection-file-name
      (tc/dataset)
      (tc/rename-columns (fn [s] (-> s (s/lower-case) (s/replace #" " "-") keyword)))))


(defn cyp-per-day [{:keys [projection-data
                           start-date end-date
                           cpu-pool]}]
  (let [cyp-per-day-f (fn [ds]
                        (-> ds
                            (ds/row-mapcat
                             (fn [row]
                               (x/into []
                                       (map (fn [d] {:date d}))
                                       #_(comp
                                          (map #(-> % tick/year-month))
                                          (x/by-key identity x/count)
                                          (map (fn [rec] {:year-month (first rec) :days (second rec)})))
                                       (tick/range (tick/max start-date (:start row))
                                                   (tick/min end-date (:end row))))))
                            (tc/group-by [:placement :date])
                            (tc/aggregate {:cyp tc/row-count} {:parallel? true})))]
    (as-> projection-data $
      (tc/select-rows $ (fn [r]
                          (tick/< start-date (:end r))))
      (tc/select-columns $ [:simulation :id :placement :start :end])
      (tc/group-by $ [:simulation] {:result-type :as-seq})
      (lazy/upmap cpu-pool cyp-per-day-f $))))

(defn simple-cost-calculation [placement-unit-costs ds]
  (-> ds
      (tc/inner-join placement-unit-costs [:placement])
      (tc/map-columns :total-daily-cost [:cyp :unit-cost-per-day]
                      (fn [cyp cost] (* cyp cost)))))

(defn cost-per-day [{:keys [projection-data
                            cost-simulation-transform-f
                            start-date end-date
                            cpu-pool]
                     :as config}]
  (as-> config $
    (cyp-per-day $)
    (lazy/upmap cpu-pool cost-simulation-transform-f $)
    (ds-reduce/group-by-column-agg
     [:placement :date]
     {:min-cyp        (ds-reduce/prob-quantile :cyp 0.0)
      :low-95-cyp     (ds-reduce/prob-quantile :cyp 0.05)
      :q1-cyp         (ds-reduce/prob-quantile :cyp 0.25)
      :median-cyp     (ds-reduce/prob-quantile :cyp 0.50)
      :q3-cyp         (ds-reduce/prob-quantile :cyp 0.75)
      :high-95-cyp    (ds-reduce/prob-quantile :cyp 0.95)
      :max-cyp        (ds-reduce/prob-quantile :cyp 1.0)

      :min-cost        (ds-reduce/prob-quantile :total-daily-cost 0.0)
      :low-95-cost     (ds-reduce/prob-quantile :total-daily-cost 0.05)
      :q1-cost         (ds-reduce/prob-quantile :total-daily-cost 0.25)
      :median-cost     (ds-reduce/prob-quantile :total-daily-cost 0.50)
      :q3-cost         (ds-reduce/prob-quantile :total-daily-cost 0.75)
      :high-95-cost    (ds-reduce/prob-quantile :total-daily-cost 0.95)
      :max-cost        (ds-reduce/prob-quantile :total-daily-cost 1.0)}
     $)
    (tc/reorder-columns $ [:placement :date
                           :min-cyp :low-95-cyp :q1-cyp :median-cyp :q3-cyp :high-95-cyp :max-cyp
                           :min-cost :low-95-cost :q1-cost :median-cost :q3-cost :high-95-cost :max-cost])
    (tc/order-by $ [:placement :date])))

(defn friendly-column-names [ds]
  (tc/rename-columns ds {:placement "Placement"
                         :date "Date"
                         :min-cyp "Min CYP"
                         :low-95-cyp "Low 95% CYP"
                         :q1-cyp "Q1 CYP"
                         :median-cyp "Median CYP"
                         :q3-cyp "Q3 CYP"
                         :high-95-cyp "High 95% CYP"
                         :max-cyp "Max CYP"

                         :min-cost "Min Cost"
                         :low-95-cost "Low 95% Cost"
                         :q1-cost "Q1 Cost"
                         :median-cost "Median Cost"
                         :q3-cost "Q3 Cost"
                         :high-95-cost "High 95% Cost"
                         :max-cost "Max Cost"}))

(comment



  )
