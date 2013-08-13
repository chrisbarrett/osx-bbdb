;;; osx-bbdb.el --- Import OS X Address Book to bbdb.

;; Copyright (C) 2013 Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>
;; Version: 0.1
;; Package-Requires: ((s "1.6.1") (dash "1.5.0") (cl-lib "0.2") (bbdb "20130728.2143"))
;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Import OS X Address Book contacts into BBDB. Requires the `contacts' program.

;;; Code:

(require 'cl-lib)
(require 's)
(require 'dash)
(autoload 'bbdb-record-name "bbdb")
(autoload 'bbdb-record-mail "bbdb")
(autoload 'bbdb-create-internal "bbdb-com")
(autoload 'bbdb-records "bbdb")
(autoload 'bbdb-save "bbdb")

;;; Custom variables

(defgroup osx-bbdb nil
  "Import OS X Address book contacts into BBDB."
  :group 'system
  :prefix "osxb")

(defcustom osxb-import-with-timer t
  "When non-nil, periodically sync your contacts with a timer."
  :group 'osx-bbdb)

(defcustom osxb-timer-period (* 20 60)
  "The delay in seconds between BBDB updates."
  :group 'osx-bbdb)

;;; Internal

(defvar osxb/timer
  (when osxb-import-with-timer
    (run-with-timer
     osxb-timer-period
     osxb-timer-period
     ;; Perform update next time Emacs is idling.
     (lambda ()
       (run-with-idle-timer 2 nil 'import-osx-contacts-to-bbdb 'quiet))))
  "Timer that periodically imports OS X contacts into BBDB.")

(defun osxb/contacts-to-string ()
  "Return a formatted string representing the user's contacts."
  (shell-command-to-string
   (format "contacts -H -l -f '%s'"
           (s-join "\t"
                   '(
                     ;; Names
                     "%n"                   ; last name
                     "%c"                   ; company
                     "%nn"                  ; nickname
                     ;; email
                     "%he"                  ; home
                     "%we"                  ; work
                     "%oe"                  ; other
                     ;; phone
                     "%hp"                  ; home
                     "%mp"                  ; mobile
                     "%Mp"                  ; main
                     "%wp"                  ; work
                     )))))

(defun* osxb/parse-card
    ((&optional
      name company aka
      home-email work-email other-email
      home-phone mobile-phone main-phone work-phone))
  "Parse a list representation of a card into the format expected by `bbdb-create-internal'."

  ;; BBDB will bork if it sees empty strings, so define some helper methods that
  ;; translate empty strings to null values.
  (cl-flet ((maybe (s) (unless (s-blank? s) s))
            (maybe-vec (label s) (unless (s-blank? s) (vector label s)))
            (non-null (&rest xs) (-remove 'null xs)))
    (list
     name
     nil ; no affix
     (non-null (maybe aka))
     (non-null (maybe company))
     (non-null (maybe home-email) (maybe work-email) (maybe other-email))
     (non-null (maybe-vec "home" home-phone)
               (maybe-vec "mobile" mobile-phone)
               (maybe-vec "main" main-phone)
               (maybe-vec "work" work-phone)))))

(defun osxb/parse-contacts (contacts-shell-output)
  "Parse the output from contacts into a form usable by `bbdb-create-internal'.
CONTACTS-SHELL-OUTPUT is the result from `osxb/contacts-to-string'."
  (->> contacts-shell-output
    ;; Each line represents a card.
    (s-split "\n")
    ;; Split each line into fields
    (--map (-map 's-trim (s-split "\t" it)))
    ;; BBDB requires that names can be split into first+last. Filter degenerate
    ;; cards that don't conform to this.
    (--filter (let ((name (car it)))
                (equal 2 (length (s-split-words name)))))
    ;; Parse individual cards.
    (-map 'osxb/parse-card)))

(defun osxb/bbdb-contains-record? (record)
  "Check whether BBDB contains an entry with the name name or email address as RECORD."
  (destructuring-bind (&optional name _affix _company _aka mails &rest rest) record
    (--any? (or (equal (bbdb-record-name it) name)
                (-intersection (bbdb-record-mail it) mails))
            (bbdb-records))))

;;;###autoload
(defun import-osx-contacts-to-bbdb (&optional quiet)
  "Import contacts from the OS X address book to BBDB.
When QUIET is non-nil, do not print summary of added items."
  (interactive "P")
  (unless (equal system-type 'darwin)
    (warn "Using OS X contacts importer, but not running Mac OS X!"))
  (require 'bbdb)
  (let ((counter 0))
    ;; Import contacts.
    (--each (osxb/parse-contacts (osxb/contacts-to-string))
      (unless (osxb/bbdb-contains-record? it)
        (apply 'bbdb-create-internal it)
        (incf counter)))
    ;; Clean up.
    (bbdb-save)
    (unless quiet
      (message "%s %s added to BBDB" counter
               (if (= 1 counter) "contact" "contacts")))))

(provide 'osx-bbdb)

;; Local Variables:
;; lexical-binding: t
;; End:

;;; osx-bbdb.el ends here
