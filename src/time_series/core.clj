(ns time-series.core
  (:use [clojure.algo.monads])
  (:require [incanter.core :as incanter]
            [incanter.charts :as charts]))

(defn time-series
  "Takes a function f, an initial state, and a number n.
   Generates a time series (with the number of iterations given
   by num-iterations) by repeatedly calling f with
   the n most-recently returned values in the time series.

   Returns a vector containing the time-series and the final state.
   It is probably better to use one of the functions `time-series-result'
   or `time-series-trace'."
  [f init-state n num-iterations]
  (let [call-f (fn [state]
                 (let [retval (try (f state)
                                   (catch Exception e (throw (Exception. (str "caught exception: " (.getMessage e) "\n"
                                                                              "state is: " state)))))
                       num (count state)
                       new-state (vec (if (= num n) (drop 1 (conj state retval)) (conj state retval)))]
                   [retval new-state]))]
    ((with-monad state-m
       (m-seq (cons (fn [s] [(first s) s]) (repeat num-iterations call-f))))
     (vector init-state))))

(defn time-series-result
  "Same parameters as `time-series', but only returns the final result."
  [f init-state n num-iterations]
  (->> (time-series f init-state n num-iterations) first last))

(defn time-series-trace
  "Same parameters as `time-series', but returns the trace of the time series"
  [f init-state n num-iterations]
  (first (time-series f init-state n num-iterations)))

(defn time-series-compare
  "Same parameters as `time-series', but the function f is replaced by a list
   of functions.  Shows a chart of the different functions."
  [fs init-state n num-iterations]
  (let [results (map #(time-series-trace % init-state n num-iterations) fs)
        initial-chart (charts/xy-plot (range num-iterations) (first results))
        add-to-chart (fn [[[chart results]]]
                       [(charts/add-lines chart (range num-iterations) (first results)) (rest results)])
        chart (first (time-series-result add-to-chart [initial-chart (rest results)] 1 (count (rest results))))]
    (incanter/view chart)))

(comment
  (defn time-series-add [x]
    (apply + x))

  (defn time-series-subtract [x]
    (apply - x))

  (defn deliberate-exception [x]
    (if (= (count x) 3)
      (/ 1 0)
      (apply + x)))

  (defn time-series-compound-interest [interest-rate]
    (fn [x]
      (* (+ 1 interest-rate) (last x))))

  (time-series-trace time-series-add 1 2 20) ; notice anything?

  (time-series deliberate-exception 1 3 20)

  (time-series (time-series-compound-interest 0.03) 100 2 20)

  (time-series-compare [(time-series-compound-interest 0.03) (time-series-compound-interest 0.05)] 100 1 50)
  )

