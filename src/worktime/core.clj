(ns worktime.core)
(use '[clojure.java.io :only [reader]])
(use '[clojure.tools.cli :only [cli]])
(use '[clojure.string :only [split join]])
(use '[clj-time.core :only [within? date-time year month day hour minute
                            minutes plus interval in-minutes]])
(use '[clj-time.format :only [formatter parse]])

(defn get-lines [filename]
  (with-open [rdr (reader filename)]
    (doall (line-seq rdr))))

(def time-formatter (formatter "dd.MM.YYYY' 'HH:mm"))
(def cli-formatter (formatter "dd.MM.YYYY"))

(defn parse-line [line]
  (let [pieces (split line #"\s")
        start-string (join " " [(pieces 0) (pieces 1)])
        stop-string (join " " [(pieces 0) (pieces 3)])
        start-time (parse time-formatter start-string)
        stop-candidate (parse time-formatter stop-string)
        stop-time (if (and (= (hour stop-candidate) 23) (= (minute stop-candidate) 59))
                    (plus stop-candidate (minutes 1)) stop-candidate)
        duration (interval start-time stop-time)]
    [(date-time (year start-time) (month start-time) (day start-time)) (in-minutes duration)]))

(defn parse-cli-date [date]
  (parse cli-formatter date))

(defn -main
  "I don't do a whole lot."
  [& args]
  (let [[opts args usage] (cli args
                               ["-f" "--file" "Filename" :default "worktime.txt"]
                               ["-s" "--start" "Start" :default (date-time 1 1 1) :parse-fn parse-cli-date]
                               ["-e" "--end" "End" :default (date-time 9999 12 31) :parse-fn parse-cli-date]
                               ["-h" "--help" "Print usage" :flag true])]
    (when (opts :help)
      (println usage)
      (System/exit 0))
    (let [entries (map parse-line (get-lines (opts :file)))
          relevant (filter #(within? (opts :start) (opts :end) (% 0)) entries)
          worktime (reduce (fn [acc e] (+ acc (e 1))) 0 relevant)
          hours (quot worktime 60)
          minutes (mod worktime 60)]
      (if (= minutes 0)
        (printf "Worked %d hours\n" hours)
        (printf "Worked %d hours, %d minutes\n" hours minutes)))))
