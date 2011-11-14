(ns time-series.test.core
  (:use [time-series.core])
  (:use [clojure.test]))

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

(deftest addition-test
  (is (= (time-series-trace time-series-add 1 2 6)
         '(1 1 2 3 5 8 13))))

(deftest interest-test
  (is (> (time-series-result (time-series-compound-interest 0.05) 100 1 30)
         (time-series-result (time-series-compound-interest 0.03) 100 1 30))))

(deftest result-test
  (is (= 13 (time-series-result time-series-add 1 2 6))))

(deftest exception-test
  (is (thrown-with-msg? Exception #"Divide by zero"
        (time-series-result deliberate-exception 100 3 20))))

(comment

  (time-series deliberate-exception 1 3 20)

  (time-series (time-series-compound-interest 0.03) 100 2 20)


  (time-series-compare [(time-series-compound-interest 0.03) (time-series-compound-interest 0.05)] 100 1 50)
  )
