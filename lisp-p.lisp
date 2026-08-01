(in-package "LISP-P")

;;; Bounded limits used to reject "absurdly long" tokens, strings, and
;;; comments without ever accumulating their actual characters.  These are
;;; deliberately generous; anything beyond them is not a plausible symbol,
;;; string, or comment in a real program.
(defconstant +max-token-length+ 4096
  "Maximum length, in characters, of a single symbol/number token or
character-literal name.")

(defconstant +max-string-length+ 65536
  "Maximum length, in characters, of a single string literal.")

(defconstant +max-comment-length+ 65536
  "Maximum length, in characters, of a single line or block comment.")

(defparameter *whitespace-chars* '(#\Space #\Tab #\Newline #\Linefeed #\Page #\Return)
  "Characters treated as whitespace by the state machine.")

(defparameter *terminating-chars* '(#\" #\' #\( #\) #\, #\; #\` #\|)
  "Macro characters (other than #\\#, which is non-terminating) that end an
in-progress token without themselves needing to be consed onto it.")

;;; States in which it is legal for the stream to end.  Every other state
;;; represents an unterminated construct (open string, comment, escape,
;;; dispatch, or character literal) and must cause rejection at EOF.
;;; :NORMAL additionally requires LIST-DEPTH to be zero.
(defparameter *eof-ok-states* '(:normal :token :char-literal-rest :line-comment)
  "States that may legally be active when the stream reaches end-of-file
(besides requiring LIST-DEPTH to be zero).")

(defun whitespace-char-p (c)
  (member c *whitespace-chars* :test #'char=))

(defun terminating-char-p (c)
  (member c *terminating-chars* :test #'char=))

(defun lisp-p (stream)
  "Return T if STREAM contains a putative Lisp program, NIL otherwise.

This drives a state machine modeled on the Common Lisp reader algorithm
(CLHS 2.2) one character at a time, without consing or interning: state is
an enumerated/dispatch value, and nesting is tracked with two bounded
integer counters (LIST-DEPTH for parenthesized lists/vectors, COMMENT-DEPTH
for nested #| |# block comments) rather than any heap-allocated
representation of the parsed forms.

The stream is accepted iff, at end-of-file, the machine has returned to a
state where ending is legal (see *EOF-OK-STATES*) with LIST-DEPTH zero --
i.e. every list, string, block comment, multiple-escape, and character
literal that was opened has been properly closed.  A single token, line
comment, or the top level itself may legally end at EOF, mirroring how the
real reader treats EOF as an implicit terminator for those constructs.

The stream is rejected early if any single token, string, or comment
exceeds +MAX-TOKEN-LENGTH+, +MAX-STRING-LENGTH+, or +MAX-COMMENT-LENGTH+
respectively, even if it would otherwise be well-formed.

This does not validate full Lisp grammar (e.g. that a quote or backquote
is followed by a datum, or that dispatch macro characters like #b, #s,
or #+ are followed by well-formed content) -- only that reader-level
delimiters are balanced and bounded, per the project's design notes."
  (let ((state :normal)
        (list-depth 0)
        (comment-depth 0)
        (len 0)
        (pending nil))
    (loop
      (let ((c (if pending
                   (shiftf pending nil)
                   (read-char stream nil :eof))))
        (when (eq c :eof)
          (return (and (zerop list-depth)
                       (not (null (member state *eof-ok-states*))))))
        (ecase state
          (:normal
           (cond
             ((whitespace-char-p c))
             ((char= c #\() (incf list-depth))
             ((char= c #\))
              (if (zerop list-depth)
                  (return nil)
                  (decf list-depth)))
             ((char= c #\") (setf state :string len 0))
             ((char= c #\;) (setf state :line-comment len 0))
             ((char= c #\|) (setf state :multi-escape len 0))
             ((char= c #\#) (setf state :hash))
             ((terminating-char-p c))
             (t (setf state :token len 1))))

          (:token
           (cond
             ((or (whitespace-char-p c) (terminating-char-p c))
              (setf pending c state :normal))
             (t (incf len)
                (when (> len +max-token-length+) (return nil)))))

          (:string
           (cond
             ((char= c #\\) (setf state :string-escape))
             ((char= c #\") (setf state :normal))
             (t (incf len)
                (when (> len +max-string-length+) (return nil)))))

          (:string-escape
           (incf len)
           (when (> len +max-string-length+) (return nil))
           (setf state :string))

          (:multi-escape
           (cond
             ((char= c #\\) (setf state :multi-escape-escape))
             ((char= c #\|) (setf state :normal))
             (t (incf len)
                (when (> len +max-token-length+) (return nil)))))

          (:multi-escape-escape
           (incf len)
           (when (> len +max-token-length+) (return nil))
           (setf state :multi-escape))

          (:line-comment
           (cond
             ((char= c #\Newline) (setf state :normal))
             (t (incf len)
                (when (> len +max-comment-length+) (return nil)))))

          (:hash
           (cond
             ((char= c #\\) (setf state :char-literal len 0))
             ((char= c #\|) (setf state :block-comment comment-depth 1 len 0))
             (t (setf pending c state :normal))))

          (:char-literal
           ;; The character immediately following #\ is consumed
           ;; unconditionally, whatever its syntax type.
           (incf len)
           (setf state :char-literal-rest))

          (:char-literal-rest
           (cond
             ((or (whitespace-char-p c) (terminating-char-p c))
              (setf pending c state :normal))
             (t (incf len)
                (when (> len +max-token-length+) (return nil)))))

          (:block-comment
           (cond
             ((char= c #\#) (setf state :block-comment-hash))
             ((char= c #\|) (setf state :block-comment-bar))
             (t (incf len)
                (when (> len +max-comment-length+) (return nil)))))

          (:block-comment-hash
           (cond
             ((char= c #\|) (incf comment-depth) (setf state :block-comment))
             ((char= c #\#))
             (t (setf pending c state :block-comment))))

          (:block-comment-bar
           (cond
             ((char= c #\#)
              (decf comment-depth)
              (setf state (if (zerop comment-depth) :normal :block-comment)))
             ((char= c #\|))
             (t (setf pending c state :block-comment)))))))))
