;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp -*-
;;;
;;; Utilities for cl-readline, bindings to GNU Readline library.
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

(defvar +states+
  '(:initializing  ; 0x0000001 initializing
    :initialized   ; 0x0000002 initialization done
    :termprepped   ; 0x0000004 terminal is prepped
    :readcmd       ; 0x0000008 reading a command key
    :metanext      ; 0x0000010 reading input after ESC
    :dispatching   ; 0x0000020 dispatching to a command
    :moreinput     ; 0x0000040 reading more input in a command function
    :isearch       ; 0x0000080 doing incremental search
    :nsearch       ; 0x0000100 doing non-incremental search
    :search        ; 0x0000200 doing a history search
    :numericarg    ; 0x0000400 reading numeric argument
    :macroinput    ; 0x0000800 getting input from a macro
    :macrodef      ; 0x0001000 defining keyboard macro
    :overwrite     ; 0x0002000 overwrite mode
    :completing    ; 0x0004000 doing completion
    :sighandler    ; 0x0008000 in readline sighandler
    :undoing       ; 0x0010000 doing an undo
    :inputpending  ; 0x0020000 rl_execute_next called
    :ttycsaved     ; 0x0040000 tty special chars saved
    :callback      ; 0x0080000 using the callback interface
    :vimotion      ; 0x0100000 reading vi motion arg
    :multikey      ; 0x0200000 reading multiple-key command
    :vicmdonce     ; 0x0400000 entered vi command mode at least once
    :readisplaying ; 0x0800000 updating terminal display
    :done)         ; 0x1000000 done; accepted line
  "Possible state values for +READLINE-STATE+.")

(defvar *c-buffer-size* 256)

(defun decode-version (version)
  "Transform VERSION into two values representing major and minor numbers of
Readline library version."
  (values (ldb (byte 8 8) version)
          (ldb (byte 8 0) version)))

(defun decode-state (state)
  "Transform Readline STATE into list of keywords."
  (mapcan (lambda (index keyword)
            (when (logbitp index state)
              (list keyword)))
          (iota (length +states+))
          +states+))

(defmacro produce-callback (function return-type &optional func-arg-list)
  "Return pointer to callback that calls FUNCTION."
  (let ((gensymed-list (mapcar (lambda (x) (list (gensym) x))
                               func-arg-list)))
    (with-gensyms (temp)
      `(if ,function
           (progn
             (defcallback ,temp ,return-type ,gensymed-list
               (funcall ,function ,@(mapcar #'car gensymed-list)))
             (get-callback ',temp))
           (null-pointer)))))

(defun to-list-of-strings (pointer)
  "Convert null-terminated array of pointers to chars into list of Lisp
strings."
  (unless (null-pointer-p pointer)
    (let (result)
      (do ((i 0 (1+ i)))
          ((null-pointer-p (mem-aref pointer :pointer i))
           (reverse result))
        (push (foreign-string-to-lisp (mem-aref pointer :pointer i))
              result)))))

(defun to-array-of-strings (list)
  "Convert list of Lisp strings into null-terminated array of C
strings. Memory for every string and the array itself should be freed with
'free'. If LIST is NIL, null pointer will be returned."
  (if list
      (let* ((len (length list))
             (ptr (foreign-funcall "malloc"
                                   :unsigned-int
                                   (* (1+ len)
                                      (foreign-type-size :pointer))
                                   :pointer)))
        (setf (mem-aref ptr :pointer len)
              (null-pointer))
        (do ((i   0 (1+ i))
             (lst list (cdr lst)))
            ((null lst) ptr)
          (let* ((string  (car lst))
                 (buffer  (foreign-funcall "malloc"
                                           :unsigned-int
                                           (* *c-buffer-size*
                                              (foreign-type-size :char))
                                           :pointer)))
            (setf (mem-aref ptr :pointer i)
                  (lisp-string-to-foreign string buffer *c-buffer-size*)))))
      (null-pointer)))
