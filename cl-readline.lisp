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
   ;; Basic Functionality
   #:readline
   ;; Hooks and Custom Functions
   #:register-startup-hook
   #:register-pre-input-hook
   #:register-event-hook
   #:register-getc-function
   #:register-signal-event-hook
   #:register-input-available-hook
   #:register-redisplay-function
   #:register-prep-term-function
   #:register-deprep-term-function
   ;; Work with Keymaps
   #:make-bare-keymap
   #:copy-keymap
   #:make-keymap
   #:free-keymap
   #:get-keymap
   #:set-keymap
   #:get-keymap-by-name
   #:get-keymap-name
   #:with-new-keymap
   ;; Binding keys

   ;; Associating Function Names and Bindings

   ;; Allowing Undoing

   ;; Redisplay

   ;; Modifying Text

   ;; Character Input
   
   ;; Terminal Management
   #:prep-terminal
   #:deprep-terminal
   #:tty-set-default-bindings
   #:tty-unset-default-bindings
   ;; Utility Functions
   #:replace-line
   #:extend-line-buffer
   #:initialize
   #:ding
   ;; Miscelaneous Functions
   #:macro-dumper
   #:variable-bind
   #:variable-value
   #:variable-dumper
   #:set-paren-blink-timeout
   #:clear-history
   ;; Signal Handling

   ;; Custom Completion
   ))

(in-package #:cl-readline)

(define-foreign-library readline
  (:unix (:or "libreadline.so.6.3"
              "libreadline.so.6"
              "libreadline.so"))
  (t     (:default "libreadline")))

(use-foreign-library readline)

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
  "Possible state values for +RL-READLINE-STATE+.")

(defvar +editing-modes+
  '(:vi            ; vi mode is active
    :emacs)        ; Emacs mode is active
  "Value denoting Readline's current editing mode.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                        ;;
;;                                Helpers                                 ;;
;;                                                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun decode-version (version)
  "Transform VERSION into two values representing major and minor numbers of
Readline library version."
  (values (ldb (byte 8 8) version)
          (ldb (byte 8 0) version)))

(defun decode-state (state)
  "Transform Readline STATE into corresponding keyword."
  (mapcan (lambda (index keyword)
            (when (logbitp index state)
              (list keyword)))
          (iota (length +states+))
          +states+))

(defun decode-editing-mode (mode)
  "Transform C int into a keyword representing current editing mode."
  (or (nth mode +editing-modes+)
      :unknown))

(defmacro set-callback (place function &optional func-arg-list)
  "Sets PLACE to pointer to callback that calls FUNCTION."
  (with-gensyms (temp)
    `(progn
       (defcallback ,temp :boolean ,func-arg-list
         (funcall ,function ,@(mapcar #'car func-arg-list)))
       (setf ,place (get-callback ',temp)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                        ;;
;;                      Foreign Structures and Types                      ;;
;;                                                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
Readline and readable Lisp keyword.")

(defctype editing-mode
    (:wrapper :int
              :from-c decode-editing-mode)
  "This wrapper performs conversion between C int and a keyword representing
current editing mode.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                        ;;
;;                           Readline Variables                           ;;
;;                                                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Descriptions here from official documentation for GNU Readline, the
;; documentation can be found at
;; http://cnswww.cns.cwru.edu/php/chet/readline/readline.html

(defcvar ("rl_line_buffer" *line-buffer*) :string
  "This is the line gathered so far. You are welcome to modify the contents
of the line, but remember about undoing. The function EXTEND-LINE-BUFFER is
available to increase the memory allocated to *LINE-BUFFER*.")

(defcvar ("rl_point" *point*) :int
  "The offset of the current cursor position in *LINE-BUFFER* (the point).")

(defcvar ("rl_end" *end*) :int
  "The number of characters present in *LINE-BUFFER*. When *POINT* is at the
end of the line, *POINT* and *END* are equal.")

(defcvar ("rl_mark" *mark*) :int
  "The mark (saved position) in the current line. If set, the mark and point
define a region.")

(defcvar ("rl_done" *done*) :boolean
  "Setting this to a non-NIL value causes Readline to return the current
line immediately.")

(defcvar ("rl_num_chars_to_read" *num-chars-to-read*) :int
  "Setting this to a positive value before calling READLINE causes Readline
to return after accepting that many characters, rather than reading up to a
character bound to accept-line.")

(defcvar ("rl_pending_input" *pending-input*) :int ;; not used
  "Setting this to a value makes it the next keystroke read. This is a way
to stuff a single character into the input stream.")

(defcvar ("rl_dispatching" *dispatching*) :boolean
  ;; TODO: use as arg of function called by a cmd
  "Set to a non-NIL value if a function is being called from a key binding;
NIL otherwise. Application functions can test this to discover whether they
were called directly or by Readline's dispatching mechanism. ")

(defcvar ("rl_erase_empty_line" *erase-empty-line*) :boolean
  "Setting this to a non-NIL value causes Readline to completely erase the
current line, including any prompt, any time a newline is typed as the only
character on an otherwise-empty line. The cursor is moved to the beginning
of the newly-blank line.")

(defcvar ("rl_prompt" +prompt+ :read-only t) :string
  "The prompt Readline uses. This is set from the argument to READLINE,
and should not be assigned to directly. The SET-PROMPT function may be used
to modify the prompt string after calling READLINE.")

(defcvar ("rl_display_prompt" *display-prompt*) :string
  "The string displayed as the prompt. This is usually identical to
+PROMPT+, but may be changed temporarily by functions that use the prompt
string as a message area, such as incremental search.")

(defcvar ("rl_already_prompted" *already-prompted*) :boolean
  "If an application wishes to display the prompt itself, rather than have
Readline do it the first time READLINE is called, it should set this
variable to a non-NIL value after displaying the prompt. The prompt must
also be passed as the argument to READLINE so the redisplay functions can
update the display properly. The calling application is responsible for
managing the value; Readline never sets it.")

(defcvar ("rl_library_version" +library-version+ :read-only t) :string
  "The version number of this revision of the library.")

(defcvar ("rl_readline_version" +readline-version+ :read-only t) version
  "Major and minor version numbers of Readline library.")

(defcvar ("rl_gnu_readline_p" +gnu-readline-p+ :read-only t) :boolean
  "Always evaluated to T, denoting that this is GNU readline rather than
some emulation.")

(defcvar ("rl_terminal_name" *terminal-name*) :string
  "The terminal type, used for initialization. If not set by the
application, Readline sets this to the value of the TERM environment
variable the first time it is called.")

(defcvar ("rl_readline_name" *readline-name*) :string
  "This symbol-macro should be set to a unique name by each application
using Readline. The value allows conditional parsing of the inputrc file.")

(defcvar ("rl_instream" *instream*) :pointer ;; ???
  "The stdio stream from which Readline reads input. If NULL, Readline
defaults to stdin.")

(defcvar ("rl_outstream" *outstream*) :pointer ;; ???
  "The stdio stream to which Readline performs output. If NULL, Readline
defaults to stdout.")

(defcvar ("rl_prefer_env_winsize" *prefer-env-winsize*) :boolean
  "If non-NIL, Readline gives values found in the LINES and COLUMNS
environment variables greater precedence than values fetched from the kernel
when computing the screen dimensions.")

(defcvar ("rl_last_func" *last-func*) :pointer ;; not used
  "The address of the last command function Readline executed. May be used
to test whether or not a function is being executed twice in succession, for
example.")

(defcvar ("rl_startup_hook" *startup-hook*) :pointer
  "If non-zero, this is the address of a function to call just before
readline prints the first prompt.")

(defcvar ("rl_pre_input_hook" *pre-input-hook*) :pointer
  "If non-zero, this is the address of a function to call after the first
prompt has been printed and just before readline starts reading input
characters.")

(defcvar ("rl_event_hook" *event-hook*) :pointer
  "If non-zero, this is the address of a function to call periodically when
Readline is waiting for terminal input. By default, this will be called at
most ten times a second if there is no keyboard input.")

(defcvar ("rl_getc_function" *getc-function*) :pointer
  "If non-zero, Readline will call indirectly through this pointer to get a
character from the input stream. By default, it is set to rl_getc, the
default Readline character input function (see section 2.4.8 Character
Input). In general, an application that sets rl_getc_function should
consider setting rl_input_available_hook as well.")

(defcvar ("rl_signal_event_hook" *signal-event-hook*) :pointer
  "If non-zero, this is the address of a function to call if a read system
call is interrupted when Readline is reading terminal input.")

(defcvar ("rl_input_available_hook" *input-available-hook*) :pointer
  "If non-zero, Readline will use this function's return value when it needs
to determine whether or not there is available input on the current input
source.")

(defcvar ("rl_redisplay_function" *redisplay-function*) :pointer
  "If non-zero, Readline will call indirectly through this pointer to update
the display with the current contents of the editing buffer. By default, it
is set to rl_redisplay, the default Readline redisplay function (see section
2.4.6 Redisplay).")

(defcvar ("rl_prep_term_function" *prep-term-function*) :pointer
  "If non-zero, Readline will call indirectly through this pointer to
initialize the terminal. The function takes a single argument, an int flag
that says whether or not to use eight-bit characters. By default, this is
set to rl_prep_terminal (see section 2.4.9 Terminal Management).")

(defcvar ("rl_deprep_term_function" *deprep-term-function*) :pointer
  "If non-zero, Readline will call indirectly through this pointer to reset
the terminal. This function should undo the effects of
rl_prep_term_function. By default, this is set to rl_deprep_terminal (see
section 2.4.9 Terminal Management).")

(defcvar ("rl_executing_keymap" +executing-keymap+ :read-only t) :pointer
  "This variable is evaluated to the keymap in which the currently executing
Readline function was found.")

(defcvar ("rl_binding_keymap" +binding-keymap+ :read-only t) :pointer
  "This variable is evaluated to the keymap in which the last key binding
occurred.")

(defcvar ("rl_executing_macro" +executing-macro+ :read-only t) :string
  "This variable is evaluated to the text of any currently-executing
macro.")

(defcvar ("rl_executing_key" +executing-key+ :read-only t) int-char
  "The key that caused the dispatch to the currently-executing Readline
function.")

(defcvar ("rl_executing_keyseq" +executing-keyseq+ :read-only t) :string
  "The full key sequence that caused the dispatch to the currently-executing
Readline function.")

(defcvar ("rl_key_sequence_length" +key-sequence-length :read-only t) :int
  "The number of characters in +EXECUTING-KEYSEQ+.")

(defcvar ("rl_readline_state" +readline-state+ :read-only t) state
  "This symbol macro is evaluated to a list containing keywords that denote
state of Readline.")

(defcvar ("rl_explicit_arg" +explicit-arg+ :read-only t) :boolean
  "Evaluated to T if an explicit numeric argument was specified by the
user. Only valid in a bindable command function.")

(defcvar ("rl_numeric_arg" +numeric-arg+ :read-only t) :int
  "Evaluated to the value of any numeric argument explicitly specified by
the user before executing the current Readline function. Only valid in a
bindable command function.")

(defcvar ("rl_editing_mode" +editing-mode+ :read-only t) editing-mode
  "Evaluated to keyword denoting actual editing mode: :EMACS, :VI,
or :UNKNOWN.")

(defcvar ("emacs_standard_keymap" +emacs-std-keymap+ :read-only t) :pointer
  "Emacs standard keymap - default keymap of Readline.")

(defcvar ("emacs_meta_keymap" +emacs-meta-keymap+ :read-only t) :pointer
  "Emacs meta keymap.")

(defcvar ("emacs_ctlx_keymap" +emacs-ctlx-keymap+ :read-only t) :pointer
  "Emacs Ctlx keymap.")

(defcvar ("vi_insertion_keymap" +vi-insertion-keymap+ :read-only t) :pointer
  "Vi insertion keymap.")

(defcvar ("vi_movement_keymap" +vi-movement-keymap+ :read-only t) :pointer
  "Vi movement keymap.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                        ;;
;;                          Basic Functionality                           ;;
;;                                                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun readline (&key
                   prompt
                   already-prompted
                   num-chars
                   erase-empty-line
                   add-history)
  "Get a line from user with editing. If PROMPT supplied (and it's a string
designator), it will be printed before reading of input. Non-NIL value of
ALREADY-PROMPTED will tell Readline that the application has printed prompt
already. However PROMPT must be supplied in this case too, so redisplay
functions can update the display properly. If NUM-CHARS argument is a
positive number, Readline will return after accepting that many
characters. If ERASE-EMPTY-LINE is not NIL, READLINE will completely erase
the current line, including any prompt, any time a newline is typed as the
only character on an otherwise-empty line. The cursor is moved to the
beginning of the newly-blank line. If ADD-HISTORY supplied and its value is
not NIL, user's input will be added to history. However, blank lines don't
get into history anyway. Return value on success is the actual string and
NIL on failure."
  (setf *already-prompted*  already-prompted
        *num-chars-to-read* (if (and (integerp num-chars)
                                     (plusp    num-chars))
                                num-chars
                                0)
        *erase-empty-line* erase-empty-line)
  (let* ((prompt (if (and (not (null prompt))
                          (typep prompt 'string-designator))
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

(defun ensure-initialization ()
  "Makes sure that Readline is initialized. If it's not initialized yet,
initializes it."
  (unless (find :initialized +readline-state+)
    (initialize)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                        ;;
;;                       Hooks and Custom Functions                       ;;
;;                                                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun register-startup-hook (function)
  "Make Readline call FUNCTION just before it prints the prompt."
  (set-callback *startup-hook* function))

(defun register-pre-input-hook (function)
  "Make Readline call FUNCTION after prompt has been printed and just before
READLINE starts reading input characters."
  (set-callback *pre-input-hook* function))

(defun register-event-hook (function)
  "Make Readline call FUNCTION periodically when waiting for terminal
input. By default, this will be called at most ten times a second if there
is no keyboard input."
  (set-callback *event-hook* function))

(defun register-getc-function (function)
  "Register FUNCTION to get a character from the input stream. In general,
an application that registers getcfunction should consider registering input
available hook as well."
  (set-callback *getc-function* function))

(defun register-signal-event-hook (function)
  "If registered, this is the function to call if a read system call is
interrupted when Readline is reading terminal input."
  (set-callback *signal-event-hook* function))

(defun register-input-available-hook (function)
  "If registered, Readline will use this function's return value when it
needs to determine whether or not there is available input on the current
input source. NIL means that there is no available input."
  (set-callback *input-available-hook* function))

(defun register-redisplay-function (function)
  "Readline will call the function to update the display with the current
contents of the editing buffer. By default, it is set to RL-REDISPLAY, the
default Readline redisplay function."
  (set-callback *redisplay-function* function))

(defun register-prep-term-function (function)
  "Readline will call the function to initialize the terminal. The function
must be able to take at least one argument, a flag that says whether or not
to use eight-bit characters. By default, PREP-TERMINAL is used."
  (set-callback *prep-term-function* function ((x :boolean))))

(defun register-deprep-term-function (function)
  "Readline will call the function to reset the terminal. This function
should undo the effects of PREP-TERM-FUNCTION."
  (set-callback *deprep-term-function* function))


;; naming a funciton - rl_add_defun

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                        ;;
;;                           Work with Keymaps                            ;;
;;                                                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcfun ("rl_make_bare_keymap" make-bare-keymap) :pointer
  "Returns a new, empty keymap. The space for the keymap is allocated with
malloc(); the caller should free it by calling FREE-KEYMAP when done.")

(defcfun ("rl_copy_keymap" copy-keymap) :pointer
  "Return a new keymap which is a copy of map.")

(defcfun ("rl_make_keymap" make-keymap) :pointer
  "Return a new keymap with the printing characters bound to rl_insert, the
lowercase Meta characters bound to run their equivalents, and the Meta
digits bound to produce numeric arguments.")

(defcfun ("rl_free_keymap" free-keymap) :void
  "Free all storage associated with keymap."
  (keymap :pointer))

(defcfun ("rl_get_keymap" get-keymap) :pointer
  "Returns currently active keymap.")

(defcfun ("rl_set_keymap" set-keymap) :void
  "Makes KEYMAP the currently active keymap."
  (keymap :pointer))

(defcfun ("rl_get_keymap_by_name" get-keymap-by-name) :pointer
  "Return the keymap matching NAME. NAME is one which would be supplied in a
set keymap inputrc line.")

(defcfun ("rl_get_keymap_name" get-keymap-name) :string
  "Return the name matching KEYMAP. Name is one which would be supplied in a
set keymap inputrc line."
  (keymap :pointer))

(defmacro with-new-keymap (form &body body)
  "Create new keymap evaluating FORM, then free it when control flow leaves
BODY. MAKE-BARE-KEYMAP, COPY-KEYMAP, and MAKE-KEYMAP can be used to produce
new keymap."
  (with-gensyms (keymap)
    `(let ((,keymap ,form))
       ,@body
       (free-keymap ,keymap))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                        ;;
;;                              Binding Keys                              ;;
;;                                                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                        ;;
;;                Associating Function Names and Bindings                 ;;
;;                                                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; rl_named_function
;; rl_function_of_keyseq
;; rl_invoking_keyseqs
;; rl_invoking_keyseqs_in_map
;; rl_function_dumper
;; rl_list_funmap_names
;; rl_funmap_names
;; rl_add_funmap_entry

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                        ;;
;;                            Allowing Undoing                            ;;
;;                                                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; rl_begin_undo_group (macro may be useful here)
;; rl_end_undo_group   (macro may be useful here)
;; rl_add_undo
;; rl_free_undo_list
;; rl_do_undo
;; rl_modifying

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                        ;;
;;                               Redisplay                                ;;
;;                                                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                        ;;
;;                             Modifying Text                             ;;
;;                                                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; rl_insert_text
;; rl_delete_text
;; rl_copy_text
;; rl_kill_text
;; rl_push_macro_input

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                        ;;
;;                            Character Input                             ;;
;;                                                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(defcfun ("rl_prep_terminal" prep-terminal) :void
  "Modify the terminal settings for Readline's use, so READLINE can read a
single character at a time from the keyboard. The EIGHT-BIT-INPUT argument
should be non-NIL if Readline should read eight-bit input."
  (eight-bit-input :boolean))

(defcfun ("rl_deprep_terminal" deprep-terminal) :void
  "Undo the effects of PREP-TERMINAL, leaving the terminal in the state in
which it was before the most recent call to PREP-TERMINAL.")

(defun tty-set-default-bindings (keymap)
  "Read the operating system's terminal editing characters (as would be
displayed by stty) to their Readline equivalents. The bindings are performed
in KEYMAP."
  (ensure-initialization)
  (foreign-funcall "rl_tty_set_default_bindings"
                   :pointer keymap
                   :void))

(defcfun ("rl_tty_unset_default_bindings" tty-unset-default-bindings) :void
  "Reset the bindings manipulated by TTY-SET-DEFAULT-BINDINGS so that the
terminal editing characters are bound to INSERT. The bindings are performed
in KEYMAP."
  (keymap :pointer))

(defcfun ("rl_reset_terminal" reset-terminal) :boolean
  "Reinitialize Readline's idea of the terminal settings using terminal_name
as the terminal type (e.g., vt100)."
  (terminal :string))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                        ;;
;;                           Utility Functions                            ;;
;;                                                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcfun ("rl_replace_line" replace-line) :void
  "Replace the contents of *LINE-BUFFER* with TEXT. The point and mark are
preserved, if possible. If CLEAR-UNDO is non-zero, the undo list associated
with the current line is cleared."
  (text       :string)
  (clear-undo :boolean))

(defcfun ("rl_extend_line_buffer" extend-line-buffer) :void
  "Ensure that line buffer has enough space to hold LEN characters,
possibly reallocating it if necessary."
  (len :int))

(defcfun ("rl_initialize" initialize) :boolean
  "Initialize or re-initialize Readline's internal state. It's not strictly
necessary to call this; READLINE calls it before reading any input.")

(defcfun ("rl_ding" ding) :boolean
  "Ring the terminal bell, obeying the setting of bell-style.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                        ;;
;;                         Miscelaneous Functions                         ;;
;;                                                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun macro-dumper (&optional (readable t)) ; TODO: output redirection
  "Print the key sequences bound to macros and their values, using the
current keymap to *STANDARD-OUTPUT*. If READABLE is non-NIL (T is default),
the list is formatted in such a way that it can be made part of an inputrc
file and re-read."
  (ensure-initialization)
  (foreign-funcall "rl_macro_dumper"
                   :boolean readable
                   :void))

(defun variable-bind (variable value)
  "Make the Readline variable VARIABLE have VALUE. This behaves as if the
readline command 'set variable value' had been executed in an inputrc file."
  (ensure-initialization)
  (foreign-funcall "rl_variable_bind"
                   :string variable
                   :string value
                   :boolean))

(defun variable-value (variable)
  "Return a string representing the value of the Readline variable
VARIABLE. For boolean variables, this string is either 'on' or 'off'."
  (ensure-initialization)
  (foreign-funcall "rl_variable_value"
                   :string variable
                   :string))

(defun variable-dumper (&optional (readable t)) ; TODO: output redir
  "Print the readline variable names and their current values to
*STANDARD-OUTPUT*. If readable is not NIL (T is default), the list is
formatted in such a way that it can be made part of an inputrc file and
re-read."
  (ensure-initialization)
  (foreign-funcall "rl_variable_dumper"
                   :boolean readable
                   :void))

(defcfun ("rl_set_paren_blink_timeout" set-paren-blink-timeout) :int
  "Set the time interval (in microseconds) that Readline waits when showing
a balancing character when blink-matching-paren has been enabled. The
function returns previous value of the parameter."
  (micros :int))

(defcfun ("rl_clear_history" clear-history) :void
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
