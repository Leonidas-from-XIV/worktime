;;; Work time calculator
(ns worktime.core)
(use '[clojure.java.io :only [reader]])
(use '[clojure.tools.cli :only [cli]])
(use '[clojure.string :only [split join]])
(use '[clj-time.core :only [within? date-time year month day hour minute
                            minutes plus interval in-minutes]])
(use '[clj-time.format :only [formatter parse]])

(defn get-lines
  "Returns the lines of a file as a seq"
  [filename]
  (with-open [rdr (reader filename)]
    (doall (line-seq rdr))))

;; formatter for file entries
(def time-formatter (formatter "dd.MM.YYYY' 'HH:mm"))
;; formatter for dates entered via the CLI
(def cli-formatter (formatter "dd.MM.YYYY"))

(defn parse-line
  "Parses a line and returns each line as vector of [date minutes-worked]"
  [line]
  (let [pieces (split line #"\s")
        start-string (join " " [(pieces 0) (pieces 1)])
        stop-string (join " " [(pieces 0) (pieces 3)])
        start-time (parse time-formatter start-string)
        stop-candidate (parse time-formatter stop-string)
        stop-time (if (and (= (hour stop-candidate) 23) (= (minute stop-candidate) 59))
                    (plus stop-candidate (minutes 1)) stop-candidate)
        duration (interval start-time stop-time)]
    [(date-time (year start-time) (month start-time) (day start-time)) (in-minutes duration)]))

(defn parse-cli-date
  "Parses a date from the command line using a pre-defined formatter"
  [date]
  (parse cli-formatter date))

(defn -main
  "Main entry point into program"
  [& args]
  ;; do some command line parsing
  (let [[opts args usage] (cli args
                               ["-f" "--file" "Filename" :default "worktime.txt"]
                               ;; well, a small date is 1.1.1
                               ["-s" "--start" "Start" :default (date-time 1 1 1) :parse-fn parse-cli-date]
                               ;; a large date, probably far enough in the future
                               ;; beware the year 9999 problem :>
                               ["-e" "--end" "End" :default (date-time 9999 12 31) :parse-fn parse-cli-date]
                               ["-h" "--help" "Print usage" :flag true])]
    (when (opts :help)
      ;; somebody requested help, print and exit
      (println usage)
      (System/exit 0))
    ;; alright, let's get to work:
    ;; first we need to read the lines and parse them
    (let [entries (map parse-line (get-lines (opts :file)))
          ;; filter out all lines that are not in the desired date range
          relevant (filter #(within? (opts :start) (opts :end) (% 0)) entries)
          ;; accumulate the minutes from all others
          worktime (reduce (fn [acc e] (+ acc (e 1))) 0 relevant)
          ;; calculate hours & minutes from there
          hours (quot worktime 60)
          minutes (mod worktime 60)]
      (if (= minutes 0)
        (printf "Worked %d hours\n" hours)
        (printf "Worked %d hours, %d minutes\n" hours minutes)))))
