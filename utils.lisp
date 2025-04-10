;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp -*-
;;;
;;; Utilities for cl-readline, bindings to GNU Readline library.
;;;
;;; Copyright © 2015–2018 Mark Karpov
;;;
;;; cl-readline is free software: you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by the
;;; Free Software Foundation, either version 3 of the License, or (at your
;;; option) any later version.
;;;
;;; cl-readline is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
;;; Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License along
;;; with this program. If not, see <http://www.gnu.org/licenses/>.

(in-package :cl-readline)

(defvar +states+
  '(:initializing ; 0x0000001 initializing
    :initialized  ; 0x0000002 initialization done
    :termprepped  ; 0x0000004 terminal is prepped
    :readcmd      ; 0x0000008 reading a command key
    :metanext     ; 0x0000010 reading input after ESC
    :dispatching  ; 0x0000020 dispatching to a command
    :moreinput    ; 0x0000040 reading more input in a command function
    :isearch      ; 0x0000080 doing incremental search
    :nsearch      ; 0x0000100 doing non-incremental search
    :search       ; 0x0000200 doing a history search
    :numericarg   ; 0x0000400 reading numeric argument
    :macroinput   ; 0x0000800 getting input from a macro
    :macrodef     ; 0x0001000 defining keyboard macro
    :overwrite    ; 0x0002000 overwrite mode
    :completing   ; 0x0004000 doing completion
    :sighandler   ; 0x0008000 in readline sighandler
    :undoing      ; 0x0010000 doing an undo
    :inputpending ; 0x0020000 rl_execute_next called
    :ttycsaved    ; 0x0040000 tty special chars saved
    :callback     ; 0x0080000 using the callback interface
    :vimotion     ; 0x0100000 reading vi motion arg
    :multikey     ; 0x0200000 reading multiple-key command
    :vicmdonce    ; 0x0400000 entered vi command mode at least once
    :redisplaying ; 0x0800000 updating terminal display
    :done)        ; 0x1000000 done; accepted line
  "Possible state values for `+readline-state+'.")

(defvar +c-buffer-size+ 256
  "How many bytes to allocate per Lisp string when converting list of
Lisp strings into array of C strings.")

(defun decode-version (version)
  "Transform VERSION into two values representing major and minor numbers of
Readline library version."
  (values (ldb (byte 8 8) version)
          (ldb (byte 8 0) version)))

(defun decode-state (state)
  "Transform Readline state STATE into list of keywords. See `+states+' for
list of components that can appear in result list."
  (mapcan (lambda (index keyword)
            (when (logbitp index state)
              (list keyword)))
          (alexandria:iota (length +states+))
          +states+))

(defmacro produce-callback (function return-type &optional func-arg-list)
  "Return pointer to callback that calls FUNCTION. RETURN-TYPE specifies
return type of the function and FUNC-ARG-LIST is list of argument types (it
can be ommited if FUNCTION doesn't take any arguments)."
  (let ((gensymed-list (mapcar (lambda (x) (list (gensym) x))
                               func-arg-list)))
    (alexandria:with-gensyms (temp)
      `(if ,function
           (progn
             (cffi:defcallback ,temp ,return-type ,gensymed-list
               (funcall ,function ,@(mapcar #'car gensymed-list)))
             (cffi:get-callback ',temp))
           (cffi:null-pointer)))))

(defun produce-callback* (function return-type &optional func-arg-list)
  "Variant of PRODUCE-CALLBACK that should hopefully be more portable.
This avoids using a GENSYM as the name of a callback, and is also funcallable."
  (let ((gensymed-list (mapcar (lambda (x) (list (gensym) x))
                               func-arg-list)))
    (alexandria:with-gensyms (temp)
      (if function
          (progn
            (eval `(cffi:defcallback ,temp ,return-type ,gensymed-list
                     (funcall ,function ,@(mapcar #'car gensymed-list))))
            (cffi:get-callback temp))
          (cffi:null-pointer)))))

(defun to-list-of-strings (pointer)
  "Convert a null-terminated array of pointers to chars that POINTER points
to into list of Lisp strings."
  (unless (cffi:null-pointer-p pointer)
    (let (result)
      (do ((i 0 (1+ i)))
          ((cffi:null-pointer-p (cffi:mem-aref pointer :pointer i))
           (reverse result))
        (push (cffi:foreign-string-to-lisp (cffi:mem-aref pointer :pointer i))
              result)))))

(defun to-array-of-strings (list)
  "Convert a list of Lisp strings LIST into null-terminated array of C
strings. Memory for every string and the array itself should be freed with
`free' (C function). If LIST is NIL, null pointer will be returned."
  (if list
      (let* ((len (length list))
             (ptr (cffi:foreign-funcall "malloc"
                                   :unsigned-int
                                   (* (1+ len)
                                      (cffi:foreign-type-size :pointer))
                                   :pointer)))
        (setf (cffi:mem-aref ptr :pointer len)
              (cffi:null-pointer))
        (do ((i   0 (1+ i))
             (lst list (cdr lst)))
            ((null lst) ptr)
          (let* ((string (car lst))
                 (buffer (cffi:foreign-funcall "malloc"
                                          :unsigned-int
                                          (* +c-buffer-size+
                                             (cffi:foreign-type-size :char))
                                          :pointer)))
            (setf (cffi:mem-aref ptr :pointer i)
                  (cffi:lisp-string-to-foreign string buffer +c-buffer-size+)))))
      (cffi:null-pointer)))
