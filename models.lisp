;;;; Data models for FHIR resources

(in-package #:fhir-medical-app)

;;; Structure for the patient (FHIR Patient resource)
(defstruct patient
    "Patient structure according to FHIR Patient resource"
    (id nil :type (or null string))
    (name "" :type string)
    (birth-date nil :type (or null string))
    (gender nil :type (or null (member :male :female :other :unknown)))
    (active t :type boolean)
    (created-at (get-universal-time) :type integer)
    (updated-at (get-universal-time) :type integer))

;;; Structure for observation (FHIR Observation resource)
(defstruct observation
    "Observation structure according to FHIR Observation resource"
    (id nil :type (or null string))
    (patient-id nil :type (or null string))
    (code "" :type string)
    (display "" :type string)
    (value nil :type (or null string number))
    (unit "" :type string)
    (date nil :type (or null string))
    (status :final :type (member :registered :prelimenary :final :amended :canceled))
    (created-at (get-universal-time) :type integer)
    (updated-at (get-universal-time) :type integer))

;;; Global data store (TODO: Rewrite for PostgreSQL)
(defparameter *patients* (make-hash-table :test 'equal)
    "Patients store")

(defparameter *observations* (make-hash-table :test 'equal)
    "Observations store")

(defparameter *patient-counter* 0
    "Counter for generating patient IDs")

(defparameter *observation-counter* 0
    "Counter for generating observation IDs")