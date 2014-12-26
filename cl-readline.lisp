;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp -*-
;;;
;;; cl-readline, bindings to GNU Readline library.
;;;
;;; Copyright (c) 2014 Mark Karpov
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

(cl:defpackage #:cl-readline
  (:use        #:common-lisp
               #:alexandria
               #:cffi)
  (:export     #:readline
               #:rl-clear-history))

(in-package #:cl-readline)

(define-foreign-library readline
  (:unix (:or "libreadline.so.6.3"
              "libreadline.so.6"
              "libreadline.so"))
  (t     (:default "libreadline")))

(use-foreign-library readline)

(defun readline (&key prompt add-history)
  "Get a line from user with editing. If PROMPT supplied (and it's a string
designator), it will be printed before reading of input. If PROMPT is NIL,
no prompt will be printed. If ADD-HISTORY supplied and its value is not NIL,
user's input will be added to history. However, blank lines don't get into
history anyway. Return value on success is actual string and NIL on
failure."
  (let* ((prompt (if (typep prompt 'string-designator)
                     (string prompt)
                     ""))
         (ptr (foreign-funcall "readline"
                               :string prompt
                               :pointer)))
    (when (and (pointerp ptr)
               (not (null-pointer-p ptr)))
      (unwind-protect
           (let ((str (foreign-string-to-lisp ptr)))
             (when (and add-history
                        (not (emptyp str)))
               (foreign-funcall "add_history"
                                :string str
                                :void))
             str)
        (foreign-funcall "free"
                         :pointer ptr
                         :void)))))

;; Readline Convenience Functions

;; naming a funciton - rl_add_defun

;; work with keymaps:
;; rl_make_bare_keymap
;; rl_copy_keymap
;; rl_make_keymap
;; rl_discard_keymap
;; rl_free_keymap
;; rl_get_keymap
;; rl_set_keymap
;; rl_get_keymap_by_name
;; rl_get_keymap_name
;; + predefined keymaps: emacs_standard_keymap, emacs_meta_keymap,
;; emacs_ctlx_keymap, vi_insertion_keymap, vi_movement_keymap

;; binding keys

;; function for handly bindings (use rl_startup_hook perhaps)

;; rl_bind_key
;; rl_bind_key_in_map
;; rl_bind_key_if_unbound
;; rl_bind_key_if_unbound_in_map
;; rl_unbind_key
;; rl_unbind_key_in_map
;; rl_unbind_function_in_map
;; rl_unbind_command_in_map
;; rl_bind_keyseq
;; rl_bind_keyseq_in_map
;; rl_set_key
;; rl_bind_keyseq_if_unbound
;; rl_bind_keyseq_if_unbound_in_map
;; rl_generic_bind
;; rl_parse_and_bind
;; rl_read_init_file

;; associating function names and bindings

;; rl_named_function
;; rl_function_of_keyseq
;; rl_invoking_keyseqs
;; rl_invoking_keyseqs_in_map
;; rl_function_dumper
;; rl_list_funmap_names
;; rl_funmap_names
;; rl_add_funmap_entry

;; allowing undoing

;; rl_begin_undo_group (macro may be useful here)
;; rl_end_undo_group   (macro may be useful here)
;; rl_add_undo
;; rl_free_undo_list
;; rl_do_undo
;; rl_modifying

;; redisplay

;; rl_redisplay
;; rl_forced_update_display
;; rl_on_new_line
;; rl_on_new_line_with_prompt
;; rl_reset_line_state
;; rl_crlf
;; rl_show_char
;; rl_message
;; rl_clear_message
;; rl_save_prompt
;; rl_restore_prompt
;; rl_expand_prompt
;; rl_set_prompt

;; modifying text

;; rl_insert_text
;; rl_delete_text
;; rl_copy_text
;; rl_kill_text
;; rl_push_macro_input

;; character input

;; rl_read_key
;; rl_getc
;; rl_stuff_char
;; rl_execute_next
;; rl_clear_pending_input
;; rl_set_keyboard_input_timeout

;; terminal management

;; rl_prep_terminal
;; rl_deprep_terminal
;; rl_tty_set_default_bindings
;; rl_tty_unset_default_bindings
;; rl_reset_terminal

;; utility functions

;; rl_save_state
;; rl_restore_state
;; rl_free
;; rl_replace_line
;; rl_extend_line_buffer
;; rl_initialize
;; rl_ding
;; rl_alphabetic
;; rl_display_match_list
;; _rl_uppercase_p
;; _rl_lowercase_p
;; _rl_digit_p
;; _rl_to_upper
;; _rl_to_lower
;; _rl_digit_value

;; miscellaneous functions

;; rl_macro_bind
;; rl_macro_dumper
;; rl_variable_bind
;; rl_variable_value
;; rl_variable_dumper
;; rl_set_paren_blink_timeout
;; rl_get_termcap
;; rl_clear_history

(defcfun "rl_clear_history" :void
  "Clear the history list by deleting all of the entries.")

;; custom completers

;; rl_complete
;; rl_completion_entry_function <- pointer to function that produces completions

;; ... see more in the manual.

;; good interface would be something like this:

;; (rl-use-completion <function or nil> completion-type)

;; function should take a string to complete and return list of completions
