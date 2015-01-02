;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp -*-
;;;
;;; Definitions of some C type for cl-readline, bindings to GNU Readline
;;; library.
;;;
;;; Copyright (c) 2015 Mark Karpov
;;;
;;; This program is free software: you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by the
;;; Free Software Foundation, either version 3 of the License, or (at your
;;; option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
;;; Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License along
;;; with this program. If not, see <http://www.gnu.org/licenses/>.

(in-package :cl-readline)

(defctype int-char
    (:wrapper :int
              :from-c code-char
              :to-c   char-code)
  "This wrapper performs conversion between C int and Lisp character.")

(defctype version
    (:wrapper :int
              :from-c decode-version)
  "This wrapper performs conversion between raw C int representing version
of Readline library and Lisp values.")

(defctype state
    (:wrapper :int
              :from-c decode-state)
  "This wrapper performs conversion between raw C int representing state of
Readline and list of keywords.")

(defcenum editing-mode
  "Enumeration of all possible editing modes in Readline."
  :vi
  :emacs)

(defcenum undo-code
  "This enumeration contains codes for various types of undo operations."
  :undo-delete
  :undo-insert
  :undo-begin
  :undo-end)

(defcenum unix-signal
  "Enumeration of some Unix signals for use with some Readline functions,
see section 'Signal Handling'."
  (:sighup  1)
  (:sigint  2)
  (:sigquit 3)
  (:sigalrm 14)
  (:sigterm 15)
  (:sigtstp 20)
  (:sigttin 26)
  (:sigttou 27))

(defcenum completion-type
  "Types of completion performed by Readline. See description of
*COMPLETION-TYPE* for more information."
  (:standard-completion 9)
  (:display-and-perform 33)
  (:insert-all          42)
  (:list-all            63)
  (:not-list-cmn-prefix 64))
