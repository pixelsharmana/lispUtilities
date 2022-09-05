(defconstant +day-names+ (list 'monday 'tuesday 'wednesday 'thursday 'friday 'saturday 'sunday))
(defconstant +month-names+ (list 'january 'february 'march 'april 'may 'june 'july 'august 'september 'october 'november 'december))

(defun date-symbol (&optional (time (get-universal-time)))
  (let (date (multiple-value-list (decode-universal-time time)))
    (
