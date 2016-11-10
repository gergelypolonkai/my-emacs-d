;;; minari.el --- Calculate Minari dates

;;; Commentary:
;; 

;;; Code:

(defconst minari-special-day-names
  '("Hëður" "Rideyy" "Morkh" "Morkh+" "Khmerd" "Chamog")
  "Names of the special (monthless) days of the year.")

(defconst minari-weekday-names
  '("Ro’unn" "Mïrdu" "Hëmi" "Drak" "Þodon" "Charm")
  "Names of weekdays.")

(defconst minari-month-names
  '("Mëbel" "Dirann" "Ma’uþ"
    "Gerub" "Þrei" "Dimoc"
    "Xentor" "Mëðïr" "Draþ"
    "Quaden" "Ridïmel" "Rodom")
  "Names of months.")

(defun minari-today-string ()
  "Calculate today's date according to the Minari Calendar."
  (let ((today (decode-time (current-time))))
    ;; FIXME: I’m not sure about this one…  If I increment by 11, I get a
    ;; wrong date according to my GNOME Shell extension.  If I increment by
    ;; 10, the date is OK, but by calculating it in my head, it’s bad.
    (incf (nth 3 today) 10)
    (let* ((minari-leap (date-leap-year-p (nth 5 today)))
           (doy (time-to-day-in-year (apply 'encode-time today)))
           (minari-year (- (nth 5 today) 1873))
           (minari-month 0)
           (minari-doy 0)
           (minari-day 0)
           (minari-weekday 0)
           (minari-special-day -1))
      (cond
       ((eq doy 0) (setq minari-special-day 0))
       ((eq doy 91) (setq minari-special-day 1))
       ((eq doy 182) (setq minari-special-day 2))
       ((and (eq doy 183) minari-leap) (setq minari-special-day 3))
       ((and (eq doy 273) (not minari-leap)) (setq minari-special-day 4))
       ((and (eq doy 274) minari-leap) (setq minari-special-day 4))
       ((and (eq doy 364) (not minari-leap)) (setq minari-special-day 5))
       ((and (eq doy 365) minari-leap) (setq minari-special-day 5))
       (t (let ((decr 0)
                (minari-doy doy))
            (when (> minari-doy 0) (incf decr))
            (when (> minari-doy 91) (incf decr))
            (when (> minari-doy 182) (incf decr))
            (when (and (> minari-doy 183) minari-leap) (incf decr))
            (when (and (> minari-doy 273) (not minari-leap)) (incf decr))
            (when (and (> minari-doy 274) minari-leap) (incf decr))
            (setq minari-doy (- minari-doy (- decr 1))
                  minari-month (round (fceiling (/ minari-doy 30.0)))
                  minari-day (% minari-doy 30))
            (when (eq minari-day 0) (setq minari-day 30))
            (setq minari-weekday (% minari-day 6)))))
      (if (eq -1 minari-special-day)
          (format "%d %s %s (%s)"
                  minari-year
                  (nth (- minari-month 1) minari-month-names)
                  minari-day
                  (nth minari-weekday minari-weekday-names))
        (nth (- minari-special-day 1) minari-special-day-names)))))

(provide 'minari)

;;; minari.el ends here
