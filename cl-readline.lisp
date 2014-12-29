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

(cl:defpackage :cl-readline
  (:use        #:common-lisp
               #:alexandria
               #:cffi)
  (:export
   ;; Readline Variables
   #:*rl-line-buffer*
   #:*rl-point*
   #:*rl-end*
   #:*rl-mark*
   #:*rl-done*
   #:+rl-prompt+
   #:*rl-display-prompt*
   #:+rl-library-version+
   #:+rl-readline-version+
   #:+rl-gnu-readline-p+
   #:+rl-terminal-name+
   #:*rl-readline-name*
   #:*rl-prefer-env-winsize*
   #:+rl-executing-keymap+
   #:+rl-binding-keymap+
   #:+rl-executing-macro+
   #:+rl-executing-key+
   #:+rl-executing-keyseq+
   #:+rl-key-sequence-length+
   #:+rl-readline-state+
   #:+rl-explicit-arg+
   #:+rl-numeric-arg+
   #:+rl-editing-mode+
   #:readline
   #:rl-clear-history))

(in-package #:cl-readline)

(define-foreign-library readline
  (:unix (:or "libreadline.so.6.3"
              "libreadline.so.6"
              "libreadline.so"))
  (t     (:default "libreadline")))

(use-foreign-library readline)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                        ;;
;;                      Foreign Structures and Types                      ;;
;;                                                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defctype int-char (:wrapper :int
                             :from-c code-char
                             :to-c   char-code)
  "This wrapper performs conversion between C int and Lisp character.")

(defun decode-rl-version (version)
  "Transform VERSION into two values representing major and minor parts of
Readline library version."
  (values (ldb (byte 8 8) version)
          (ldb (byte 8 0) version)))

(defun encode-rl-version (major minor)
  "Transform MAJOR and MINOR parts of Readline version into integer."
  (+ (ash major 8) minor))

(defctype rl-ver (:wrapper :int
                           :from-c decode-rl-version
                           :to-c   encode-rl-version)
  "This wrapper performs conversion between raw C int representing version
of Readline library and Lisp values.")

;; KEYMAP-ENTRY is structure used in Readline to remember which function it
;; should invoke on pressing of specified char. Readline uses typedef'ed
;; type 'Keymap' as return value of many functions. 'Keymap' is pointer to
;; array of keymap entries.

(defcstruct keymap-entry
  (type :char)
  (func :pointer)) ; rl_command_func_t *

;; READLINE-STATE is the structure that contains parameters of realine, some
;; functions return this structure or take it as an argument.

(defcstruct readline-state
  ;; line state
  (point     :int)
  (end       :int)
  (mark      :int)
  (buffer    (:pointer :char))
  (buflen    :int)
  (undo-list :pointer) ; UNDO_LIST *
  (prompt    (:pointer :char))
  ;; global state
  (rlstate   :int)
  (done      :int)
  (kmap      (:pointer (:struct keymap-entry)))
  ;; input state
  (lastfunc  :pointer) ; rl_command_func_t *
  (insmode   :int)
  (edmode    :int)
  (kseqlen   :int)
  (inf       :pointer) ; FILE *
  (outf      :pointer) ; FILE *
  (pendingin :int)
  (macro     (:pointer :char))
  ;; signal state
  (catchsigs :int)
  (chachsiginch :int)
  ;; search state
  ;; completion state
  ;; options state
  (reserved :char :count 64)) ; reserved for future expansion

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                        ;;
;;                                Helpers                                 ;;
;;                                                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ???

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                        ;;
;;                           Readline Variables                           ;;
;;                                                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Descriptions here from official documentation for GNU Readline, the
;; documentation can be found at
;; http://cnswww.cns.cwru.edu/php/chet/readline/readline.html

(defcvar "rl_line_buffer" :string
  "This is the line gathered so far. You are welcome to modify the contents
of the line, but see 2.4.5 Allowing Undoing. The function
RL-EXTEND-LINE-BUFFER is available to increase the memory allocated to
*RL-LINE-BUFFER*.")

(defcvar "rl_point" :int
  "The offset of the current cursor position in *RL-LINE-BUFFER* (the
point).")

(defcvar "rl_end" :int
  "The number of characters present in RL-LINE-BUFFER. When *RL-POINT* is at
the end of the line, *RL_POINT* and *RL_END* are equal.")

(defcvar "rl_mark" :int
  "The mark (saved position) in the current line. If set, the mark and point
define a region.")

(defcvar "rl_done" :boolean
  "Setting this to a non-NIL value causes Readline to return the current
line immediately.")

(defcvar "rl_num_chars_to_read" :int
  "Setting this to a positive value before calling READLINE causes Readline
to return after accepting that many characters, rather than reading up to a
character bound to accept-line.")

(defcvar "rl_pending_input" :int ;; not used
  "Setting this to a value makes it the next keystroke read. This is a way
to stuff a single character into the input stream.")

(defcvar "rl_dispatching" :int ;; use as arg of function called by a cmd
  "Set to a non-zero value if a function is being called from a key binding;
zero otherwise. Application functions can test this to discover whether they
were called directly or by Readline's dispatching mechanism. ")

(defcvar "rl_erase_empty_line" :int
  "Setting this to a non-zero value causes Readline to completely erase the
current line, including any prompt, any time a newline is typed as the only
character on an otherwise-empty line. The cursor is moved to the beginning
of the newly-blank line.")

(defcvar ("rl_prompt" +rl-prompt+ :read-only t) :string
  "The prompt Readline uses. This is set from the argument to READLINE,
and should not be assigned to directly. The RL-SET-PROMPT function may be
used to modify the prompt string after calling READLINE.")

(defcvar "rl_display_prompt" :string
  "The string displayed as the prompt. This is usually identical to
+RL_PROMPT+, but may be changed temporarily by functions that use the prompt
string as a message area, such as incremental search.")

(defcvar "rl_already_prompted" :boolean
  "If an application wishes to display the prompt itself, rather than have
Readline do it the first time READLINE is called, it should set this
variable to a non-NIL value after displaying the prompt. The prompt must
also be passed as the argument to READLINE so the redisplay functions can
update the display properly. The calling application is responsible for
managing the value; Readline never sets it.")

(defcvar ("rl_library_version" +rl-library-version+ :read-only t) :string
  "The version number of this revision of the library.")

(defcvar ("rl_readline_version" +rl-readline-version+ :read-only t) rl-ver
  "Major and minor version numbers of Readline library.")

(defcvar ("rl_gnu_readline_p" +rl-gnu-readline-p+ :read-only t) :boolean
  "Always set to T, denoting that this is GNU readline rather than some
emulation.")

(defcvar ("rl_terminal_name" +rl-terminal-name+ :read-only t) :string
  "The terminal type, used for initialization. If not set by the
application, Readline sets this to the value of the TERM environment
variable the first time it is called.")

(defcvar "rl_readline_name" :string
  "This variable is set to a unique name by each application using
Readline. The value allows conditional parsing of the inputrc file.")

(defcvar "rl_instream" :pointer ;;; ???
  "The stdio stream from which Readline reads input. If NULL, Readline
defaults to stdin.")

(defcvar "rl_outstream" :pointer ;;; ???
  "The stdio stream to which Readline performs output. If NULL, Readline
defaults to stdout.")

(defcvar "rl_prefer_env_winsize" :boolean
  "If non-zero, Readline gives values found in the LINES and COLUMNS
environment variables greater precedence than values fetched from the kernel
when computing the screen dimensions.")

(defcvar "rl_last_func" :pointer ;;; ???
  "The address of the last command function Readline executed. May be used
to test whether or not a function is being executed twice in succession, for
example.")

(defcvar "rl_startup_hook" :pointer ;; separate function to instantiate it
  "If non-zero, this is the address of a function to call just before
readline prints the first prompt.")

(defcvar "rl_pre_input_hook" :pointer ;; sep function to inst it
  "If non-zero, this is the address of a function to call after the first
prompt has been printed and just before readline starts reading input
characters.")

(defcvar "rl_event_hook" :pointer ;; sep function to inst it
  "If non-zero, this is the address of a function to call periodically when
Readline is waiting for terminal input. By default, this will be called at
most ten times a second if there is no keyboard input.")

(defcvar "rl_getc_function" :pointer ;; sep function to inst it
  "If non-zero, Readline will call indirectly through this pointer to get a
character from the input stream. By default, it is set to rl_getc, the
default Readline character input function (see section 2.4.8 Character
Input). In general, an application that sets rl_getc_function should
consider setting rl_input_available_hook as well.")

(defcvar "rl_signal_event_hook" :pointer ;; sep function to inst it
  "If non-zero, this is the address of a function to call if a read system
call is interrupted when Readline is reading terminal input.")

(defcvar "rl_input_available_hook" :pointer ;; sep function to inst it
  "If non-zero, Readline will use this function's return value when it needs
to determine whether or not there is available input on the current input
source.")

(defcvar "rl_redisplay_function" :pointer ;; sep function to inst it
  "If non-zero, Readline will call indirectly through this pointer to update
the display with the current contents of the editing buffer. By default, it
is set to rl_redisplay, the default Readline redisplay function (see section
2.4.6 Redisplay).")

(defcvar "rl_prep_term_function" :pointer ;; sep function to inst it
  "If non-zero, Readline will call indirectly through this pointer to
initialize the terminal. The function takes a single argument, an int flag
that says whether or not to use eight-bit characters. By default, this is
set to rl_prep_terminal (see section 2.4.9 Terminal Management).")

(defcvar "rl_deprep_term_function" :pointer ;; sep function to inst it
  "If non-zero, Readline will call indirectly through this pointer to reset
the terminal. This function should undo the effects of
rl_prep_term_function. By default, this is set to rl_deprep_terminal (see
section 2.4.9 Terminal Management).")

(defcvar ("rl_executing_keymap" +rl-executing-keymap+ :read-only t)
    (:pointer (:struct keymap-entry)) ;; wrapper for keymaps ????
  "This variable is set to the keymap in which the currently executing
readline function was found.")

(defcvar ("rl_binding_keymap" +rl-binding-keymap+ :read-only t)
    (:pointer (:struct keymap-entry)) ;; see above
  "This variable is set to the keymap in which the last key binding
occurred.")

(defcvar ("rl_executing_macro" +rl-executing-macro+ :read-only t) :string
  "This variable is set to the text of any currently-executing macro.")

(defcvar ("rl_executing_key" +rl-executing-key+ :read-only t) int-char
  "The key that caused the dispatch to the currently-executing Readline
function.")

(defcvar ("rl_executing_keyseq" +rl-executing-keyseq+ :read-only t) :string
  "The full key sequence that caused the dispatch to the currently-executing
Readline function.")

(defcvar ("rl_key_sequence_length" +rl-key-sequence-length+ :read-only t) :int
  "The number of characters in +RL-EXECUTING-KEYSEQ+.")

(defcvar ("rl_readline_state" +rl-readline-state+ :read-only t) :int
  ;; need a wrapper of something to work with, + correct description when done
  "A variable with bit values that encapsulate the current Readline state. A
bit is set with the RL_SETSTATE macro, and unset with the RL_UNSETSTATE
macro. Use the RL_ISSTATE macro to test whether a particular state bit is
set.")

;; ...states...

(defcvar ("rl_explicit_arg" +rl-explicit-arg+ :read-only t) :boolean
  "Set to T if an explicit numeric argument was specified by the user. Only
valid in a bindable command function.")

(defcvar ("rl_numeric_arg" +rl-numeric-arg+ :read-only t) :boolean
  "Set to the value of any numeric argument explicitly specified by the user
before executing the current Readline function. Only valid in a bindable
command function.")

(defcvar ("rl_editing_mode" +rl-editing-mode+ :read-only t) :int
  ;; need a wrapper here, 1 -> :emacs, 0 -> :vi
  "Set to a value denoting Readline's current editing mode. A value of 1
means Readline is currently in emacs mode; 0 means that vi mode is active.")

;; hey there, we need a macro (read-only "blah") =>
;; ("blah" +blah+ :read-only t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                        ;;
;;                          Basic Functionality                           ;;
;;                                                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: incorporate rl_already_prompted into readline

;; TODO: readline calls rl_initialize when it first called, however some
;; functions require rl be initialized. We want them never fail, so create
;; lisp var *rl-initialized*, set it to T when rl is initialized so those
;; functions (exact list must be determined) can invoke it if needed

;; TODO: incorporate rl_num_chars_to_read

;; TODO: try rl_erase_empty_line

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

;; Readline Convenience Functions (this section is still vague)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                        ;;
;;                          Terminal Management                           ;;
;;                                                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcfun "rl_prep_terminal" :void
  "Modify the terminal settings for Readline's use, so READLINE can read a
single character at a time from the keyboard. The EIGHT-BIT-INPUT argument
should be non-NIL if Readline should read eight-bit input."
  (eight-bit-input :boolean))

(defcfun "rl_deprep_terminal" :void
  "Undo the effects of RL-PREP-TERMINAL, leaving the terminal in the state
in which it was before the most recent call to RL-PREP-TERMINAL.")


;; rl_tty_set_default_bindings


;; rl_tty_unset_default_bindings


(defcfun "rl_reset_terminal" :boolean
  "Reinitialize Readline's idea of the terminal settings using terminal_name
as the terminal type (e.g., vt100). If terminal_name is NULL, the value of
the TERM environment variable is used."
  (terminal (:pointer :char)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                        ;;
;;                           Utility Functions                            ;;
;;                                                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; rl_save_state



;; rl_restore_state


(defcfun "rl_replace_line" :void
  "Replace the contents of rl_line_buffer with text. The point and mark are
preserved, if possible. If clear_undo is non-zero, the undo list associated
with the current line is cleared."
  (text       :string)
  (clear-undo :boolean))

(defcfun "rl_extend_line_buffer" :void
  "Ensure that line buffer has enough space to hold len characters,
possibly reallocating it if necessary."
  (len :int))

(defcfun "rl_initialize" :boolean
  "Initialize or re-initialize Readline's internal state. It's not strictly
necessary to call this; READLINE calls it before reading any input.")

(defcfun "rl_ding" :boolean
  "Ring the terminal bell, obeying the setting of bell-style.")

(defcfun "rl_alphabetic" :boolean
  "Return T if CHAR is an alphabetic character."
  (char int-char))

;; rl_display_match_list


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                        ;;
;;                         Miscelaneous Functions                         ;;
;;                                                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: make sure that readline is initialized before using rl-macro-dumper
;; or rl-variable-dumper

(defun rl-macro-dumper (&optional (readable t)) ; TODO: output redirection
  "Print the key sequences bound to macros and their values, using the
current keymap to *STANDARD-OUTPUT*. If READABLE is non-NIL (T is default),
the list is formatted in such a way that it can be made part of an inputrc
file and re-read."
  (foreign-funcall "rl_macro_dumper"
                   :boolean readable
                   :void))

(defcfun "rl_variable_bind" :boolean
  "Make the Readline variable variable have value. This behaves as if the
readline command 'set variable value' had been executed in an inputrc file."
  (variable :string)
  (value    :string))

(defcfun "rl_variable_value" :string
  "Return a string representing the value of the Readline variable
variable. For boolean variables, this string is either 'on' or 'off'."
  (variable :string))

(defun rl-variable-dumper (&optional (readable t)) ; TODO: output redir
  "Print the readline variable names and their current values to
*STANDARD-OUTPUT*. If readable is non-zero, the list is formatted in such a
way that it can be made part of an inputrc file and re-read."
  (foreign-funcall "rl_variable_dumper"
                   :boolean readable
                   :void))

(defcfun "rl_set_paren_blink_timeout" :int
  "Set the time interval (in microseconds) that Readline waits when showing
a balancing character when blink-matching-paren has been enabled. The
function returns previous value of the parameter."
  (micros :int))

(defcfun "rl_get_termcap" :string
  "Retrieve the string value of the termcap capability CAP. Readline fetches
the termcap entry for the current terminal name and uses those capabilities
to move around the screen line and perform other terminal-specific
operations, like erasing a line. Readline does not use all of a terminal's
capabilities, and this function will return values for only those
capabilities Readline uses."
  (cap :string))

(defcfun "rl_clear_history" :void
  "Clear the history list by deleting all of the entries.")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                        ;;
;;                            Signal Handling                             ;;
;;                                                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ???

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                        ;;
;;                           Custom Completion                            ;;
;;                                                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; custom completers

;; rl_complete
;; rl_completion_entry_function <- pointer to function that produces completions

;; ... see more in the manual.

;; good interface would be something like this:

;; (rl-use-completion <function or nil> completion-type)

;; function should take a string to complete and return list of completions

