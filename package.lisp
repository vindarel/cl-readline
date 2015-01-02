;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp -*-
;;;
;;; Package definition for cl-readline, bindings to GNU Readline library.
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

(cl:defpackage :cl-readline
  (:nicknames  :rl)
  (:use        #:common-lisp
               #:alexandria
               #:cffi)
  (:export
   ;; Readline Variables
   #:*line-buffer*
   #:*point*
   #:*end*
   #:*mark*
   #:*done*
   #:*dispatching*
   #:+prompt+
   #:*display-prompt*
   #:+library-version+
   #:+readline-version+
   #:+gnu-readline-p+
   #:*terminal-name*
   #:*readline-name*
   #:*prefer-env-winsize*
   #:+executing-keymap+
   #:+binding-keymap+
   #:+executing-macro+
   #:+executing-key+
   #:+executing-keyseq+
   #:+key-sequence-length+
   #:+readline-state+
   #:+explicit-arg+
   #:+numeric-arg+
   #:+editing-mode+
   #:+emacs-std-keymap+
   #:+emacs-meta-keymap+
   #:+emacs-ctlx-keymap+
   #:+vi-insertion-keymap+
   #:+vi-movement-keymap+
   #:*catch-signals*
   #:*catch-sigwinch*
   #:*change-environment*
   #:*basic-word-break-characters*
   #:*basic-quote-characters*
   #:*completer-word-break-characters*
   #:*completion-query-items*
   #:*completion-append-character*
   #:*ignore-completion-duplicates*
   #:*sort-completion-matches*
   #:+completion-type+
   #:*inhibit-completion*
   ;; Basic Functionality
   #:readline
   #:add-defun
   ;; Hooks and Custom Functions
   #:register-hook
   #:register-function
   ;; Work with Keymaps
   #:make-keymap
   #:copy-keymap
   #:free-keymap
   #:get-keymap
   #:set-keymap
   #:get-keymap-by-name
   #:get-keymap-name
   #:with-new-keymap
   ;; Binding keys
   #:bind-key
   #:unbind-key
   #:unbind-command
   #:bind-keyseq
   #:parse-and-bind
   #:read-init-file
   ;; Associating Function Names and Bindings
   #:function-dumper
   #:list-funmap-names
   #:funmap-names
   #:add-funmap-entry
   ;; Allowing Undoing
   #:undo-group
   #:add-undo
   #:free-undo-list
   #:do-undo
   #:modifying
   ;; Redisplay
   #:redisplay
   #:forced-update-display
   #:on-new-line
   #:reset-line-state
   #:crlf
   #:show-char
   #:with-message
   #:set-prompt
   ;; Modifying Text
   #:insert-text
   #:delete-text
   #:kill-text
   ;; Character Input
   #:read-key
   #:stuff-char
   #:execute-next
   #:clear-pending-input
   #:set-keyboard-input-timeout
   ;; Terminal Management
   #:prep-terminal
   #:deprep-terminal
   #:tty-set-default-bindings
   #:tty-unset-default-bindings
   #:reset-terminal
   ;; Utility Functions
   #:replace-line
   #:extend-line-buffer
   #:initialize
   #:ding
   ;; Miscellaneous Functions
   #:macro-dumper
   #:variable-bind
   #:variable-value
   #:variable-dumper
   #:set-paren-blink-timeout
   #:clear-history
   ;; Signal Handling
   #:cleanup-after-signal
   #:free-line-state
   #:reset-after-signal
   #:echo-signal-char
   #:resize-terminal
   #:set-screen-size
   #:get-screen-size
   #:reset-screen-size
   #:set-signals
   #:clear-signals))
