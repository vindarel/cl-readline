;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp -*-
;;;
;;; cl-readline, bindings to GNU Readline library.
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

(in-package #:cl-readline)

(define-foreign-library readline
  ;; On OSX we first search readline, installed by brew install readline
  ;; because native system version of readline is a symlink to libedit.
  ;; Some people on the internet advice to "fix" it by running:
  ;; brew link --force readline
  ;; but it is a bad idea, because this command may break some system utilities,
  ;; depending on libedit's internals.
  (:darwin (:or "/usr/local/opt/readline/lib/libreadline.dylib"
                "/opt/homebrew/opt/readline/lib/libreadline.dylib"
                "libreadline.dylib"))
  (:unix   (:or "libreadline.so.6.3"
                "libreadline.so.6"
                "libreadline.so.7"
                "libreadline.so.8"
                "libreadline.so"))
  (t       (:default "libreadline")))

(use-foreign-library readline)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                        ;;
;;                           Readline Variables                           ;;
;;                                                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Descriptions here are from the official documentation for GNU Readline.
;; The documentation can be found at
;; http://cnswww.cns.cwru.edu/php/chet/readline/readline.html

(defcvar ("rl_line_buffer" *line-buffer*) :string
  "This is the line gathered so far. You are welcome to modify the contents
of the line, but remember about undoing. The function `extend-line-buffer'
is available to increase the memory allocated to `*line-buffer*'.")

(defcvar ("rl_point" *point*) :int
  "The offset of the current cursor position in `*line-buffer*' (the
point).")

(defcvar ("rl_end" +end+ :read-only t) :int
  "The number of characters present in `*line-buffer*'. When `*point*' is at
the end of the line, `*point*' and `+end+' are equal.")

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

(defcvar ("rl_dispatching" +dispatching+ :read-only t) :boolean
  "Set to a non-NIL value if a function is being called from a key binding;
NIL otherwise. Application functions can test this to discover whether they
were called directly or by Readline's dispatching mechanism.")

(defcvar ("rl_erase_empty_line" *erase-empty-line*) :boolean
  "Setting this to a non-NIL value causes Readline to completely erase the
current line, including any prompt, any time a newline is typed as the only
character on an otherwise-empty line. The cursor is moved to the beginning
of the newly-blank line.")

(defcvar ("rl_prompt" +prompt+ :read-only t) :string
  "The prompt Readline uses. This is set from the argument to `readline',
and should not be assigned to directly. The `set-prompt' function may be
used to modify the prompt string after calling `readline'.")

(defcvar ("rl_display_prompt" *display-prompt*) :string
  "The string displayed as the prompt. This is usually identical to
`+prompt+', but may be changed temporarily by functions that use the prompt
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

(defcvar ("rl_instream" *instream*) :pointer ;; not used
  "The stdio stream from which Readline reads input. If NULL, Readline
defaults to stdin.")

(defcvar ("rl_outstream" *outstream*) :pointer
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
character from the input stream. By default, it is set to `rl_getc', the
default Readline character input function (see section 2.4.8 Character
Input). In general, an application that sets `rl_getc_function' should
consider setting `rl_input_available_hook' as well.")

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
is set to `rl_redisplay', the default Readline redisplay function (see
section 2.4.6 Redisplay).")

(defcvar ("rl_prep_term_function" *prep-term-function*) :pointer
  "If non-zero, Readline will call indirectly through this pointer to
initialize the terminal. The function takes a single argument, an int flag
that says whether or not to use eight-bit characters. By default, this is
set to `rl_prep_terminal' (see section 2.4.9 Terminal Management).")

(defcvar ("rl_deprep_term_function" *deprep-term-function*) :pointer
  "If non-zero, Readline will call indirectly through this pointer to reset
the terminal. This function should undo the effects of
`rl_prep_term_function.' By default, this is set to
`rl_deprep_terminal' (see section 2.4.9 Terminal Management).")

(defcvar ("rl_executing_keymap" +executing-keymap+ :read-only t) :pointer
  "This symbol macro is evaluated to the keymap in which the currently
executing Readline function was found.")

(defcvar ("rl_binding_keymap" +binding-keymap+ :read-only t) :pointer
  "This symbol macro is evaluated to the keymap in which the last key
binding occurred.")

(defcvar ("rl_executing_macro" +executing-macro+ :read-only t) :string
  "This symbol macro is evaluated to the text of any currently-executing
macro.")

(defcvar ("rl_executing_key" +executing-key+ :read-only t) int-char
  "The key that caused the dispatch to the currently-executing Readline
function.")

(defcvar ("rl_executing_keyseq" +executing-keyseq+ :read-only t) :string
  "The full key sequence that caused the dispatch to the currently-executing
Readline function.")

(defcvar ("rl_key_sequence_length" +key-sequence-length+ :read-only t) :int
  "The number of characters in `+executing-keyseq+'.")

(defcvar ("rl_readline_state" +readline-state+ :read-only t) state
  "This symbol macro is evaluated to a list containing keywords that denote
state of Readline. For list of possible values see `+states+'.")

(defcvar ("rl_explicit_arg" +explicit-arg+ :read-only t) :boolean
  "Evaluated to T if an explicit numeric argument was specified by the
user. Only valid in a bindable command function.")

(defcvar ("rl_numeric_arg" +numeric-arg+ :read-only t) :int
  "Evaluated to the value of any numeric argument explicitly specified by
the user before executing the current Readline function. Only valid in a
bindable command function.")

(defcvar ("rl_editing_mode" +editing-mode+ :read-only t) editing-mode
  "Evaluated to keyword denoting actual editing mode: :EMACS or :VI.")

(defcvar ("rl_catch_signals" *catch-signals*) :boolean
  "If this variable is non-NIL, Readline will install signal handlers for
SIGINT, SIGQUIT, SIGTERM, SIGHUP, SIGALRM, SIGTSTP, SIGTTIN, and
SIGTTOU. The default value of `*catch-signals*' is T.")

(defcvar ("rl_catch_sigwinch" *catch-sigwinch*) :boolean
  "If this variable is set to a non-NIL value, Readline will install a
signal handler for SIGWINCH. The default value of `*catch-sigwinch*' is T.")

(defcvar ("rl_change_environment" *change-environment*) :boolean
  "If this variable is set to a non-NIL value, and Readline is handling
SIGWINCH, Readline will modify the LINES and COLUMNS environment variables
upon receipt of a SIGWINCH. The default value of `*change-environment*' is
T.")

(defcvar ("rl_attempted_completion_function"
          *attempted-completion-function*)
    :pointer
  "A pointer to an alternative function to create matches. The function is
called with TEXT, START, and END. START and END are indices in
`*line-buffer*' defining the boundaries of text, which is a character
string. If this function exists and returns NULL, or if this variable is set
to NULL, then `complete' will call the value of
`*completion-entry-function*' to generate matches, otherwise the array of
strings returned will be used. If this function sets the
`*attempted-completion-over*' variable to a non-NIL value, Readline will not
perform its default completion even if this function returns no matches.")

(defcvar ("rl_completion_display_matches_hook"
          *completion-display-matches-hook*)
    :pointer
  "If non-zero, then this is the address of a function to call when
completing a word would normally display the list of possible matches. This
function is called au lieu de Readline displaying the list. It takes three
arguments: (char **matches, int num_matches, int max_length) where matches
is the array of matching strings, `num_matches' is the number of strings in
that array, and `max_length' is the length of the longest string in that
array.")

(defcvar ("rl_basic_word_break_characters"
          *basic-word-break-characters*)
    :string
  "The basic list of characters that signal a break between words for the
completer routine. The default value of this variable is the characters
which break words for completion in Bash.")

(defcvar ("rl_basic_quote_characters" *basic-quote-characters*) :string
  "A list of quote characters which can cause a word break.")

(defcvar ("rl_completer_word_break_characters"
          *completer-word-break-characters*)
    :string
  "The list of characters that signal a break between words for
`complete-internal'. The default list is the value of
`*basic-word-break-characters*.'")

(defcvar ("rl_completion_query_items" *completion-query-items*) :int
  "Up to this many items will be displayed in response to a
`possible-completions' call. After that, Readline asks the user if she is
sure she wants to see them all. The default value is 100. A negative value
indicates that Readline should never ask the user.")

(defcvar ("rl_completion_append_character" *completion-append-character*)
    int-char
  "When a single completion alternative matches at the end of the command
line, this character is appended to the inserted completion text. The
default is a space character. Setting this to the null character prevents
anything being appended automatically. This can be changed in
application-specific completion functions to provide the 'most sensible word
separator character' according to an application-specific command line
syntax specification.")

(defcvar ("rl_ignore_completion_duplicates" *ignore-completion-duplicates*)
    :boolean
  "If non-NIL, then duplicates in the matches are removed. The default is
T.")

(defcvar ("rl_attempted_completion_over" *attempted-completion-over*)
    :boolean
  "If an application-specific completion function assigned to
`*attempted-completion-function*' sets this variable to a non-NIL value,
Readline will not perform its default filename completion even if the
application's completion function returns no matches. It should be set only
by an application's completion function.")

(defcvar ("rl_sort_completion_matches" *sort-completion-matches*)
    :boolean
  "If an application sets this variable to NIL, Readline will not sort the
list of completions (which implies that it cannot remove any duplicate
completions). The default value is T, which means that Readline will sort
the completions and, depending on the value of
`*ignore-completion-duplicates*', will attempt to remove duplicate
matches.")

(defcvar ("rl_completion_type" +completion-type+ :read-only t) completion-type
  "Set to a keyword describing the type of completion Readline is currently
attempting. Possible values are:

:STANDARD-COMPLETION tells Readline to do standard completion;

:DISPLAY-AND-PERFORM means to display all possible completions if there is
more than one, as well as performing partial completion;

:INSERT-ALL means insert all possible completions;

:LIST-ALL means list the possible completions;

:NOT-LIST-CMN-PREFIX is similar to :DISPLAY-AND-PERFORM but possible
completions are not listed if the possible completions share a common
prefix.")

(defcvar ("rl_inhibit_completion" *inhibit-completion*) :boolean
  "If this variable is non-NIL, completion is inhibited. The completion
character will be inserted as any other bound to self-insert.")

(defcvar ("history_base" +history-base+ :read-only t) :int
  "The logical offset of the first entry in the history list.")

(defcvar ("history_length" +history-length+ :read-only t) :int
  "The number of entries currently stored in the history list.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                        ;;
;;                          Basic Functionality                           ;;
;;                                                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun recent-history-line-satisfies-p (predicate)
  "Check if the most recent history line satisfies given predicate
PREDICATE. Return T if there is no history saved."
    (if (zerop +history-length+)
        t
        (funcall predicate
                 (foreign-string-to-lisp
                  (with-foreign-slots
                      ((line)
                       (foreign-funcall "history_get"
                                        :int (1- (+ +history-base+
                                                    +history-length+))
                                        :pointer)
                       (:struct history-entry))
                    line)))))

(defun readline (&key
                   prompt
                   already-prompted
                   num-chars
                   erase-empty-line
                   add-history
                   novelty-check)
  "Get a line from user with editing. PROMPT, if supplied, is printed before
reading of input. Non-NIL value of ALREADY-PROMPTED will tell Readline that
the application has printed prompt already. However, PROMPT must be supplied
in this case too, so redisplay functions can update the display properly. If
NUM-CHARS argument is a positive number, Readline will return after
accepting that many characters. If ERASE-EMPTY-LINE is not NIL, `readline'
will completely erase the current line, including any prompt, any time a
newline is typed as the only character on an otherwise-empty line. The
cursor is moved to the beginning of the newly-blank line. Supplying
ADD-HISTORY tells Readline that user's input should be added to
history. However, blank lines don't get into history anyway. NOVELTY-CHECK,
if given, must be a predicate that takes two strings: the actual line and
the most recent history line. Only when the predicate evaluates to non-NIL
value new line will be added to the history. Return value on success is the
actual string and NIL on failure."
  (setf *already-prompted*  already-prompted
        *num-chars-to-read* (or num-chars 0)
        *erase-empty-line*  erase-empty-line)
  (let* ((prompt (if prompt (string prompt) ""))
         (ptr (foreign-funcall "readline"
                               :string prompt
                               :pointer)))
    (unless (null-pointer-p ptr))
      (unwind-protect
           (let ((str (foreign-string-to-lisp ptr)))
             (when (and add-history
                        (not (emptyp str))
                        (or (not novelty-check)
                            (recent-history-line-satisfies-p
                             (curry novelty-check str))))
               (foreign-funcall "add_history"
                                :string str
                                :void))
             str)
        (foreign-funcall "free"
                         :pointer ptr
                         :void))))

(defun ensure-initialization ()
  "Make sure that Readline is initialized. If it's not initialized yet,
initialize it."
  (unless (find :initialized +readline-state+)
    (initialize)))

(defmacro with-possible-redirection (filename append &body body)
  "If FILENAME is not NIL, try to create C file named FILENAME,
temporarily reassign `*outstream*' to pointer to this file, perform BODY,
then close the file and assign `*outstream*' the old value. If APPEND is not
NIL, output will be appended to the file. Returns NIL on success and T on
failure."
  (with-gensyms (temp-outstream file-pointer body-fnc)
    `(flet ((,body-fnc ()
              ,@body))
       (if ,filename
           (let ((,temp-outstream *outstream*)
                 (,file-pointer (foreign-funcall "fopen"
                                                 :string ,filename
                                                 :string (if ,append "a" "w")
                                                 :pointer)))
             (if (null-pointer-p ,file-pointer)
                 t
                 (unwind-protect
                      (progn
                        (setf *outstream* ,file-pointer)
                        (,body-fnc))
                   (foreign-funcall "fclose"
                                    :pointer ,file-pointer
                                    :boolean)
                   (setf *outstream* ,temp-outstream))))
           (,body-fnc)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                        ;;
;;                       Hooks and Custom Functions                       ;;
;;                                                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun register-hook (hook function)
  "Register a hook. HOOK should be a keyword, one of the following:

:STARTUP hook is called just before READLINE prints the prompt.

:PRE-INPUT hook is called after prompt has been printed and just before
READLINE starts reading input characters.

:EVENT hook is called periodically when waiting for terminal input. By
default, this will be called at most ten times a second if there is no
keyboard input.

:SIGNAL hook is called when a read system call is interrupted when
READLINE is reading terminal input.

:INPUTP hook is called when Readline need to determine whether or not there
is available input on the current input source. If FUNCTION returns NIL, it
means that there is no available input.

:LSMATCHES hook is called to display list of completions. FUNCTION must be
able to take three arguments: list of completions, length of the list, and
length of the longest completion in the list. It's up to the function how to
display these completions.

Other values of HOOK will be ignored.

FUNCTION must be a function that takes no arguments and returns NIL on
success and T on failure. If FUNCTION is NIL, hook will be removed (or
default function will be used)."
  (let ((cb (if (and (eql hook :lsmatches)
                     function)
                (produce-callback
                 (lambda (matches num max-length)
                   (funcall function
                            (to-list-of-strings matches)
                            num
                            max-length))
                 :void
                 (:pointer :int :int))
                (produce-callback function :boolean))))
    (case hook
      (:startup   (setf *startup-hook*                    cb))
      (:pre-input (setf *pre-input-hook*                  cb))
      (:event     (setf *event-hook*                      cb))
      (:signal    (setf *signal-event-hook*               cb))
      (:inputp    (setf *input-available-hook*            cb))
      (:lsmatches (setf *completion-display-matches-hook* cb))))
  nil)

(defun register-function (func function)
  "Register a function. FUNC should be a keyword, one of the following:

:GETC function is used to get a character from the input stream, thus
FUNCTION should take pointer to C stream and return a character if this
function is desired to be registered. In general, an application that
registers :GETC function should consider registering :INPUTP hook as
well (see REGISTER-HOOK).

:REDISPLAY function is used to update the display with the current contents
of the editing buffer, thus FUNCTION should take no arguments and return NIL
on success and non-NIL of failure. By default, it is set to REDISPLAY, the
default Readline redisplay function.

:PREP-TERM function is used to initialize the terminal, so FUNCTION must be
able to take one argument, a flag that says whether or not to use eight-bit
characters. By default, PREP-TERMINAL is used.

:DEPREP-TERM function is used to reset the terminal. This function should
undo the effects of :PREP-TERM function.

:COMPLETE function is used to generate list of possible completions for
given partially entered word. The function must be able to take three
arguments: partially entered word, start index of the word in *LINE-BUFFER*
and end index of the word in the buffer. The function must return a list
where first element is the actual completion (or part of completion if two
or more completions share common prefix) and the rest arguments are possible
completions.

Other values of FUNC will be ignored.

FUNCTION must be a function, if FUNCTION is NIL, result is unpredictable."
  (case func
    (:getc        (setf *getc-function*
                        (produce-callback function int-char (:pointer))))
    (:redisplay   (setf *redisplay-function*
                        (produce-callback function :void)))
    (:prep-term   (setf *prep-term-function*
                        (produce-callback function :void (:boolean))))
    (:deprep-term (setf *deprep-term-function*
                        (produce-callback function :void)))
    (:complete    (setf *attempted-completion-function*
                        (produce-callback
                         (lambda (text start end)
                           (prog1
                               (to-array-of-strings
                                (funcall function text start end))
                             (setf *attempted-completion-over* t)))
                         :pointer
                         (:string :int :int)))))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                        ;;
;;                           Work with Keymaps                            ;;
;;                                                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-keymap (&optional bare)
  "Return a new keymap with self-inserting printing characters, the
lowercase Meta characters bound to run their equivalents, and the Meta
digits bound to produce numeric arguments. If BARE is supplied and it's not
NIL, empty keymap will be returned."
  (if bare
      (foreign-funcall "rl_make_bare_keymap"
                       :pointer)
      (foreign-funcall "rl_make_keymap"
                       :pointer)))

(defcfun ("rl_copy_keymap" copy-keymap) :pointer
  "Return a new keymap which is a copy of KEYMAP."
  (keymap :pointer))

(defcfun ("rl_free_keymap" free-keymap) :void
  "Free all storage associated with KEYMAP."
  (keymap :pointer))

(defcfun ("rl_get_keymap" get-keymap) :pointer
  "Return currently active keymap.")

(defcfun ("rl_set_keymap" set-keymap) :void
  "Make KEYMAP the currently active keymap."
  (keymap :pointer))

(defcfun ("rl_get_keymap_by_name" get-keymap-by-name) :pointer
  "Return the keymap matching NAME. NAME is one which would be supplied in a
set keymap inputrc line."
  (name :string))

(defcfun ("rl_get_keymap_name" get-keymap-name) :string
  "Return the name matching KEYMAP. Name is one which would be supplied in a
set keymap inputrc line."
  (keymap :pointer))

(defmacro with-new-keymap (form &body body)
  "Create new keymap evaluating FORM, bind symbol `keymap' to the result,
then free it when control flow leaves BODY. `make-keymap' and `copy-keymap'
can be used to produce new keymap."
  `(let ((keymap ,form))
     (unwind-protect
          (progn ,@body)
       (free-keymap keymap))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                        ;;
;;                              Binding Keys                              ;;
;;                                                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun add-defun (name function &optional key)
  "Add NAME to the list of named functions. Make FUNCTION be the function
that gets called. If KEY is not NIL and it's a character, then bind it to
function using `bind-key'. FUNCTION must be able to take two arguments:
integer representing its argument and character representing key that has
invoked it."
  (ensure-initialization)
  (foreign-funcall "rl_add_defun"
                   :string name
                   :pointer (produce-callback function
                                              :boolean
                                              (:int int-char))
                   :int (if key (char-code key) -1)))

(defun bind-key (key function &key keymap if-unbound)
  "Bind KEY to FUNCTION in the currently active keymap. If KEYMAP argument
supplied, binding takes place in specified keymap. If IF-UNBOUND is supplied
and it's not NIL, KEY will be bound to FUNCTION only if it's not already
bound."
  (ensure-initialization)
  (let ((cb (produce-callback function :boolean (:int int-char))))
    (cond ((and keymap if-unbound)
           (foreign-funcall "rl_bind_key_if_unbound_in_map"
                            int-char key
                            :pointer cb
                            :pointer keymap
                            :boolean))
          (keymap
           (foreign-funcall "rl_bind_key_in_map"
                            int-char key
                            :pointer cb
                            :pointer keymap
                            :boolean))
          (if-unbound
           (foreign-funcall "rl_bind_key_if_unbound"
                            int-char key
                            :pointer cb
                            :boolean))
          (t
           (foreign-funcall "rl_bind_key"
                            int-char key
                            :pointer cb
                            :boolean)))))

(defun unbind-key (key &optional keymap)
  "Unbind KEY in KEYMAP. If KEYMAP is not supplied or it's NIL, KEY will be
unbound in currently active keymap. The function returns NIL on success and
T on failure."
  (ensure-initialization)
  (if keymap
      (foreign-funcall "rl_unbind_key_in_map"
                       int-char key
                       :pointer keymap
                       :boolean)
      (foreign-funcall "rl_unbind_key"
                       int-char key
                       :boolean)))

(defun unbind-command (command keymap)
  "Unbind all keys that are bound to COMMAND in KEYMAP."
  (ensure-initialization)
  (foreign-funcall "rl_unbind_command_in_map"
                   :string command
                   :pointer keymap
                   :boolean))

(defun bind-keyseq (keyseq function &key keymap if-unbound)
  "Bind the key sequence represented by the string KEYSEQ to the function
FUNCTION, beginning in the current keymap. This makes new keymaps as
necessary. If KEYMAP is supplied and it's not NIL, initial bindings are
performed in KEYMAP. If IF-UNBOUND is supplied and it's not NIL, KEYSEQ will
be bound to FUNCTION only if it's not already bound. The return value is T
if KEYSEQ is invalid and NIL otherwise."
  (ensure-initialization)
  (let ((cb (produce-callback function :boolean (:int int-char))))
    (cond ((and keymap if-unbound)
           (foreign-funcall "rl_bind_keyseq_if_unbound_in_map"
                            :string  keyseq
                            :pointer cb
                            :pointer keymap
                            :boolean))
          (keymap
           (foreign-funcall "rl_bind_keyseq_in_map"
                            :string  keyseq
                            :pointer cb
                            :pointer keymap
                            :boolean))
          (if-unbound
           (foreign-funcall "rl_bind_keyseq_if_unbound"
                            :string  keyseq
                            :pointer cb
                            :boolean))
          (t
           (foreign-funcall "rl_bind_keyseq"
                            :string  keyseq
                            :pointer cb
                            :boolean)))))

(defun parse-and-bind (line)
  "Parse LINE as if it had been read from the inputrc file and perform any
key bindings and variable assignments found."
  (ensure-initialization)
  (foreign-funcall "rl_parse_and_bind"
                   :string line
                   :boolean))

(defun read-init-file (filename)
  "Read keybindings and variable assignments from FILENAME."
  (ensure-initialization)
  (foreign-funcall "rl_read_init_file"
                   :string filename
                   :boolean))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                        ;;
;;                Associating Function Names and Bindings                 ;;
;;                                                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun function-dumper (readable &optional filename append)
  "Print the Readline function names and the key sequences currently bound
to them to stdout. If READABLE is non-NIL, the list is formatted in such a
way that it can be made part of an inputrc file and re-read. If FILENAME is
supplied and it's a string or path, output will be redirected to the
file. APPEND allows to append text to the file instead of overwriting it."
  (ensure-initialization)
  (with-possible-redirection filename append
    (foreign-funcall "rl_function_dumper"
                     :boolean readable
                     :void)))

(defun list-funmap-names (&optional filename append)
  "Print the names of all bindable Readline functions to stdout. If FILENAME
is supplied and it's a string or path, output will be redirected to the
file. APPEND allows append text to the file instead of overwriting it."
  (ensure-initialization)
  (with-possible-redirection filename append
    (foreign-funcall "rl_list_funmap_names"
                     :void)))

(defun funmap-names ()
  "Return a list of known function names. The list is sorted."
  (ensure-initialization)
  (let ((ptr (foreign-funcall "rl_funmap_names"
                              :pointer)))
    (unless (null-pointer-p ptr)
      (unwind-protect
           (to-list-of-strings ptr)
        (foreign-funcall "free"
                         :pointer ptr
                         :void)))))

(defun add-funmap-entry (name function)
  "Add NAME to the list of bindable Readline command names, and make
FUNCTION the function to be called when name is invoked."
  (foreign-funcall "rl_add_funmap_entry"
                   :string name
                   :pointer (produce-callback function
                                              :boolean
                                              (:int int-char))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                        ;;
;;                            Allowing Undoing                            ;;
;;                                                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro undo-group (&body body)
  "All insertion and deletion inside this macro will be grouped together
into one undo operation."
  `(unwind-protect
        (progn
          (foreign-funcall "rl_begin_undo_group" :boolean)
          ,@body)
     (foreign-funcall "rl_end_undo_group" :boolean)))

(defcfun ("rl_add_undo" add-undo) :void
  "Remember how to undo an event (according to WHAT). The affected text runs
from START to END, and encompasses TEXT. Possible values of WHAT
include: :UNDO-DELETE, :UNDO-INSERT, :UNDO-BEGIN, and :UNDO-END."
  (what  undo-code)
  (start :int)
  (end   :int)
  (text  :string))

(defcfun ("rl_free_undo_list" free-undo-list) :void
  "Free the existing undo list.")

(defcfun ("rl_do_undo" do-undo) :boolean
  "Undo the first thing on the undo list. Returns NIL if there was nothing
to undo, T if something was undone.")

(defcfun ("rl_modifying" modifying) :boolean
  "Tell Readline to save the text between START and END as a single undo
unit. It is assumed that you will subsequently modify that text."
  (start :int)
  (end   :int))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                        ;;
;;                               Redisplay                                ;;
;;                                                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcfun ("rl_redisplay" redisplay) :void
  "Change what's displayed on the screen to reflect the current contents of
`*line-buffer*'.")

(defcfun ("rl_forced_update_display" forced-update-display) :boolean
  "Force the line to be updated and redisplayed, whether or not Readline
thinks the screen display is correct.")

(defun on-new-line (&optional with-prompt)
  "Tell the update functions that we have moved onto a new (empty) line,
usually after outputting a newline. When WITH-PROMPT is not NIL, Readline
will think that prompt is already displayed. This could be used by
applications that want to output the prompt string themselves, but still
need Readline to know the prompt string length for redisplay. This should be
used together with :ALREADY-PROMPTED keyword argument of `readline'."
  (if with-prompt
      (foreign-funcall "rl_on_new_line_with_prompt" :boolean)
      (foreign-funcall "rl_on_new_line" :boolean)))

(defcfun ("rl_reset_line_state" reset-line-state) :boolean
  "Reset the display state to a clean state and redisplay the current line
starting on a new line.")

(defcfun ("rl_crlf" crlf) :boolean
  "Move the cursor to the start of the next screen line.")

(defcfun ("rl_show_char" show-char) :boolean
  "Display character CHAR on outstream. If Readline has not been set to
display meta characters directly, this will convert meta characters to a
meta-prefixed key sequence. This is intended for use by applications which
wish to do their own redisplay."
  (char int-char))

(defmacro with-message (message save-prompt &body body)
  "Show message MESSAGE in the echo area while executing BODY. If
SAVE-PROMPT is not NIL, save prompt before showing the message and restore
it before clearing the message."
  `(unwind-protect
        (progn
          (when ,save-prompt
            (foreign-funcall "rl_save_prompt" :void))
          (foreign-funcall "rl_message"
                           :string ,message
                           :boolean)
          ,@body)
     (when ,save-prompt
       (foreign-funcall "rl_restore_prompt" :void))
     (foreign-funcall "rl_clear_message" :boolean)))

(defcfun ("rl_set_prompt" set-prompt) :boolean
  "Make Readline use PROMPT for subsequent redisplay. This calls
`expand-prompt' to expand the prompt and sets `+prompt+' to the result."
  (prompt :string))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                        ;;
;;                             Modifying Text                             ;;
;;                                                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcfun ("rl_insert_text" insert-text) :int
  "Insert TEXT into the line at the current cursor position. Return the
number of characters inserted."
  (text :string))

(defcfun ("rl_delete_text" delete-text) :int
  "Delete the text between START and END in the current line. Return the
number of characters deleted."
  (start :int)
  (end   :int))

(defcfun ("rl_kill_text" kill-text) :boolean
  "Copy the text between START and END in the current line to the kill ring,
appending or prepending to the last kill if the last command was a kill
command. The text is deleted. If START is less than END, the text is
appended, otherwise prepended. If the last command was not a kill, a new
kill ring slot is used."
  (start :int)
  (end   :int))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                        ;;
;;                            Character Input                             ;;
;;                                                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcfun ("rl_read_key" read-key) int-char
  "Return the next character available from Readline's current input
stream.")

(defcfun ("rl_stuff_char" stuff-char) :boolean
  "Insert CHAR into the Readline input stream. It will be «read» before
Readline attempts to read characters from the terminal with `read-key'. Up
to 512 characters may be pushed back. `stuff-char' returns T if the
character was successfully inserted; NIL otherwise."
  (char int-char))

(defcfun ("rl_execute_next" execute-next) :boolean
  "Make CHAR be the next command to be executed when `read-key' is called."
  (char int-char))

(defcfun ("rl_clear_pending_input" clear-pending-input) :boolean
  "Negate the effect of any previous call to `execute-next'. This works only
if the pending input has not already been read with `read-key'.")

(defcfun ("rl_set_keyboard_input_timeout" set-keyboard-input-timeout) :int
  "While waiting for keyboard input in `read-key', Readline will wait for U
microseconds for input before calling any function assigned to `event-hook'.
U must be greater than or equal to zero (a zero-length timeout is equivalent
to a poll). The default waiting period is one-tenth of a second. Return the
old timeout value."
  (u :int))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                        ;;
;;                          Terminal Management                           ;;
;;                                                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcfun ("rl_prep_terminal" prep-terminal) :void
  "Modify the terminal settings for Readline's use, so `readline' can read a
single character at a time from the keyboard. The EIGHT-BIT-INPUT argument
should be non-NIL if Readline should read eight-bit input."
  (eight-bit-input :boolean))

(defcfun ("rl_deprep_terminal" deprep-terminal) :void
  "Undo the effects of `prep-terminal', leaving the terminal in the state in
which it was before the most recent call to `prep-terminal'.")

(defun tty-set-default-bindings (keymap)
  "Read the operating system's terminal editing characters (as would be
displayed by stty) to their Readline equivalents. The bindings are performed
in KEYMAP."
  (ensure-initialization)
  (foreign-funcall "rl_tty_set_default_bindings"
                   :pointer keymap
                   :void))

(defcfun ("rl_tty_unset_default_bindings" tty-unset-default-bindings) :void
  "Reset the bindings manipulated by `tty-set-default-bindings' so that the
terminal editing characters are bound to `insert'. The bindings are
performed in KEYMAP."
  (keymap :pointer))

(defcfun ("rl_reset_terminal" reset-terminal) :boolean
  "Reinitialize Readline's idea of the terminal settings using TERMINAL as
the terminal type (e.g., vt100)."
  (terminal :string))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                        ;;
;;                           Utility Functions                            ;;
;;                                                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcfun ("rl_replace_line" replace-line) :void
  "Replace the contents of `*line-buffer*' with TEXT. The point and mark are
preserved, if possible. If CLEAR-UNDO is non-NIL, the undo list associated
with the current line is cleared."
  (text       :string)
  (clear-undo :boolean))

(defcfun ("rl_extend_line_buffer" extend-line-buffer) :void
  "Ensure that line buffer has enough space to hold LEN characters,
possibly reallocating it if necessary."
  (len :int))

(defcfun ("rl_initialize" initialize) :boolean
  "Initialize or re-initialize Readline's internal state. It's not strictly
necessary to call this; `readline' calls it before reading any input.")

(defcfun ("rl_ding" ding) :boolean
  "Ring the terminal bell, obeying the setting of bell-style.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                        ;;
;;                        Miscellaneous Functions                         ;;
;;                                                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun macro-dumper (readable &optional filename append)
  "Print the key sequences bound to macros and their values, using the
current keymap to stdout. If READABLE is non-NIL, the list is formatted in
such a way that it can be made part of an inputrc file and re-read. If
FILENAME is supplied and it's a string or path, output will be redirected to
the file. APPEND allows to append text to the file instead of overwriting
it."
  (ensure-initialization)
  (with-possible-redirection filename append
    (foreign-funcall "rl_macro_dumper"
                     :boolean readable
                     :void)))

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
VARIABLE. For Boolean variables, this string is either 'on' or 'off'."
  (ensure-initialization)
  (foreign-funcall "rl_variable_value"
                   :string variable
                   :string))

(defun variable-dumper (readable &optional filename append)
  "Print the readline variable names and their current values to stdout. If
readable is not NIL, the list is formatted in such a way that it can be made
part of an inputrc file and re-read. If FILENAME is supplied and it's a
string or path, output will be redirected to the file. APPEND allows to
append text to the file instead of overwriting it."
  (ensure-initialization)
  (with-possible-redirection filename append
    (foreign-funcall "rl_variable_dumper"
                     :boolean readable
                     :void)))

(defcfun ("rl_set_paren_blink_timeout" set-paren-blink-timeout) :int
  "Set the time interval (in microseconds) that Readline waits when showing
a balancing character when 'blink-matching-paren' has been enabled. The
function returns previous value of the parameter."
  (micros :int))

(defcfun ("rl_clear_history" clear-history) :void
  "Clear the history list by deleting all of the entries.")

(defcfun ("read_history" read-history) :int
  "Add the contents of filename to the history list, a line at a
time."
  (filename :string))

(defcfun ("write_history" write-history) :int
  "Write the current history to filename, overwriting filename if
necessary."
  (filename :string))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                        ;;
;;                            Signal Handling                             ;;
;;                                                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcfun ("rl_cleanup_after_signal" cleanup-after-signal) :void
  "This function will reset the state of the terminal to what it was before
`readline' was called, and remove the Readline signal handlers for all
signals, depending on the values of `*catch-signals*' and
`*catch-sigwinch*'.")

(defcfun ("rl_free_line_state" free-line-state) :void
  "This will free any partial state associated with the current input
line (undo information, any partial history entry, any partially-entered
keyboard macro, and any partially-entered numeric argument). This should be
called before `cleanup-after-signal.' The Readline signal handler for SIGINT
calls this to abort the current input line.")

(defcfun ("rl_reset_after_signal" reset-after-signal) :void
  "This will reinitialize the terminal and reinstall any Readline signal
handlers, depending on the values of `*catch-signals*' and
`*catch-sigwinch*'.")

(defcfun ("rl_echo_signal_char" echo-signal-char) :void
  "If an application wishes to install its own signal handlers, but still
have readline display characters that generate signals, calling this
function with SIG set to :SIGINT, :SIGQUIT, or :SIGTSTP will display the
character generating that signal."
  (sig unix-signal))

(defun resize-terminal ()
  "Update Readline's internal screen size by reading values from the
kernel."
  (ensure-initialization)
  (foreign-funcall "rl_resize_terminal"
                   :void))

(defcfun ("rl_set_screen_size" set-screen-size) :void
  "Set Readline's idea of the terminal size to ROWS rows and COLS
columns. If either rows or columns is less than or equal to 0, Readline's
idea of that terminal dimension is unchanged."
  (rows :int)
  (cols :int))

(defun get-screen-size ()
  "Return Readline's idea of the terminal's size. The function returns
multiple values: number of rows and columns."
  (ensure-initialization)
  (with-foreign-objects ((rows :int)
                         (cols :int))
    (foreign-funcall "rl_get_screen_size"
                     :pointer rows
                     :pointer cols
                     :void)
    (values (mem-ref rows :int)
            (mem-ref cols :int))))

(defcfun ("rl_reset_screen_size" reset-screen-size) :void
  "Cause Readline to reobtain the screen size and recalculate its
dimensions.")

(defcfun ("rl_set_signals" set-signals) :boolean
  "Install Readline's signal handler for SIGINT, SIGQUIT, SIGTERM, SIGHUP,
SIGALRM, SIGTSTP, SIGTTIN, SIGTTOU, and SIGWINCH, depending on the values of
`*catch-signals*' and `*catch-sigwinch*'.")

(defcfun ("rl_clear_signals" clear-signals) :boolean
  "Remove all of the Readline signal handlers installed by `set-signals'.")
