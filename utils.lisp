;;;; Utilities and helper functions

(in-package #:fhir-medical-app)

(defun generate-id (type)
    "Generates a unique ID for a resource"
    (case type
        (:patient
            (incf *patient-counter*)
            (format nil "patient-~A" *patient-counter*))
        (:observation
            (incf *observation-counter*)
            (format nil "observation-~A" *observation-counter*))
        (otherwise
            (error "Unknown resource type: ~A" type))))

(defun format-date (universal-time)
    "Formats a time into an ISO 8601 string"
    (multiple-value-bind (sec min hour date month year)
        (decode-universal-time universal-time)
        (format nil "~4, '0D-~2, '0D-~2, '0DT~2, '0D:~2. '0D:~2, '0DZ"
            year month date hour min sec)))

(defun parse-data (date-string)
    "Parses ISO 8601 date to universal-time (simplified version)"
    (if (and date-string (> (length date-string) 9))
        (let ((year (parse-integer (subseq date-string 0 4) :junk-allowed t))
            (month (parse-integer (subseq date-string 5 7) :junk-allowed t))
            (day (parse-integer (subseq date-string 8 10) :junk-allowed t)))
            (when (and year month day)
                (encode-universal-time 0 0 0 day month year)))
        (get-universal-time)))

(defun current-iso-date ()
    "Returns the current date in ISO format"
    (format-date (get-universal-time)))

(defun validate-gender (gender)
    "Validates the patient's gender"
    (member gender '(:male :female :other :unknown)))

(defun validate-status (status)
    "Validates observation status"
    (member status '(:registered :preliminary :final :amended :cancelled)))

(defun safe-string (value)
    "Safely converts a value to a string"
    (if value
        (princ-to-string value)
        ""))