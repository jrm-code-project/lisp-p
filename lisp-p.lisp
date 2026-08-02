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

(defconstant +max-list-depth+ 4096
  "Maximum nesting depth of parenthesized lists/vectors.  Also bounds the
size of the per-depth backquote-level and consing-dot tracking arrays.")

(defconstant +max-backquote-depth+ 256
  "Maximum number of consecutive backquote/comma prefix characters that may
apply to a single upcoming datum, before it is consumed.")

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
an enumerated/dispatch value, and nesting is tracked with bounded integer
counters (LIST-DEPTH for parenthesized lists/vectors, COMMENT-DEPTH for
nested #| |# block comments, BQ-PENDING for backquote/comma prefixes not
yet applied to a datum) plus three fixed-size, dynamic-extent (stack
allocated, not heap-consed) per-depth arrays -- BQ-LEVEL, HAS-DATUM, and
DOT-STATE -- rather than any heap-allocated representation of the parsed
forms.

Two additional reader-level rules are enforced beyond simple delimiter
balancing:

- A comma (or comma-at-sign, `,@`) is only valid while at least one
  backquote's worth of quasiquotation is still \"in effect\" at the current
  position; BQ-LEVEL/BQ-PENDING track that net level (incremented by `` ` ``,
  decremented by `,`) per list-nesting depth, and a comma seen when the net
  level is not positive is rejected.
- A lone `.` token (the consing dot) is only accepted when it obviously
  denotes a literal dotted-cons: inside a list, preceded by at least one
  datum already read in that list, itself preceded by no pending
  backquote/comma prefix, followed by exactly one more datum, and then the
  list's closing `)`.  Any other placement (`.` at top level, `(. a)`,
  `(a . )`, `(a . b c)`, two dots in one list, etc.) is rejected.

The stream is accepted iff, at end-of-file, the machine has returned to a
state where ending is legal (see *EOF-OK-STATES*) with LIST-DEPTH zero --
i.e. every list, string, block comment, multiple-escape, and character
literal that was opened has been properly closed.  A single token, line
comment, or the top level itself may legally end at EOF, mirroring how the
real reader treats EOF as an implicit terminator for those constructs.

The stream is rejected early if any single token, string, or comment
exceeds +MAX-TOKEN-LENGTH+, +MAX-STRING-LENGTH+, or +MAX-COMMENT-LENGTH+
respectively, even if it would otherwise be well-formed.

This does not validate full Lisp grammar (e.g. that dispatch macro
characters like #b, #s, or #+ are followed by well-formed content, or
that numeric tokens are well-formed) -- only that reader-level delimiters
are balanced and bounded, and that backquote/comma and consing-dot usage
are locally consistent, per the project's design notes."
  (let ((state :normal)
        (list-depth 0)
        (comment-depth 0)
        (len 0)
        (pending nil)
        (bq-pending 0)
        (bq-level (make-array (1+ +max-list-depth+)
                               :element-type 'fixnum :initial-element 0))
        (has-datum (make-array (1+ +max-list-depth+) :initial-element nil))
        (dot-state (make-array (1+ +max-list-depth+) :initial-element :none)))
    (declare (dynamic-extent bq-level has-datum dot-state))
    (labels ((record-datum (d)
               ;; A datum (atom, string, list, char literal) has just been
               ;; completed at nesting depth D.  Check it against any
               ;; consing dot pending at that depth, and mark that a datum
               ;; has now been seen there.
               (case (svref dot-state d)
                 (:after-dot (return-from record-datum nil))
                 (:dot-seen (setf (svref dot-state d) :after-dot)))
               (setf (svref has-datum d) t)
               t)
             (handle-dot ()
               ;; A lone "." token has just been read.  Only legal
               ;; mid-list, after at least one prior datum, with no
               ;; backquote/comma prefix pending, and not already used.
               (and (zerop bq-pending)
                    (plusp list-depth)
                    (svref has-datum list-depth)
                    (eq (svref dot-state list-depth) :none)
                    (progn (setf (svref dot-state list-depth) :dot-seen) t))))
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
               ((char= c #\()
                (when (>= list-depth +max-list-depth+) (return nil))
                (setf (svref has-datum (1+ list-depth)) nil
                      (svref dot-state (1+ list-depth)) :none
                      (aref bq-level (1+ list-depth))
                      (+ (aref bq-level list-depth) bq-pending))
                (setf bq-pending 0)
                (incf list-depth))
               ((char= c #\))
                (when (zerop list-depth) (return nil))
                (when (or (not (zerop bq-pending))
                          (eq (svref dot-state list-depth) :dot-seen))
                  (return nil))
                (decf list-depth)
                (unless (record-datum list-depth) (return nil))
                (setf bq-pending 0))
               ((char= c #\") (setf state :string len 0))
               ((char= c #\;) (setf state :line-comment len 0))
               ((char= c #\|) (setf state :multi-escape len 0))
               ((char= c #\#) (setf state :hash))
               ((char= c #\`)
                (incf bq-pending)
                (when (> bq-pending +max-backquote-depth+) (return nil)))
               ((char= c #\,)
                (unless (plusp (+ (aref bq-level list-depth) bq-pending))
                  (return nil))
                (decf bq-pending)
                (let ((c2 (read-char stream nil :eof)))
                  (unless (or (eq c2 :eof) (char= c2 #\@))
                    (setf pending c2))))
               ((char= c #\.) (setf state :dot-token len 1))
               ((terminating-char-p c))
               (t (setf state :token len 1))))

            (:dot-token
             (cond
               ((or (whitespace-char-p c) (terminating-char-p c))
                (unless (handle-dot) (return nil))
                (setf pending c state :normal))
               (t (setf state :token len 2))))

            (:token
             (cond
               ((or (whitespace-char-p c) (terminating-char-p c))
                (setf pending c state :normal)
                (unless (record-datum list-depth) (return nil))
                (setf bq-pending 0))
               (t (incf len)
                  (when (> len +max-token-length+) (return nil)))))

            (:string
             (cond
               ((char= c #\\) (setf state :string-escape))
               ((char= c #\")
                (setf state :normal)
                (unless (record-datum list-depth) (return nil))
                (setf bq-pending 0))
               (t (incf len)
                  (when (> len +max-string-length+) (return nil)))))

            (:string-escape
             (incf len)
             (when (> len +max-string-length+) (return nil))
             (setf state :string))

            (:multi-escape
             (cond
               ((char= c #\\) (setf state :multi-escape-escape))
               ((char= c #\|)
                (setf state :normal)
                (unless (record-datum list-depth) (return nil))
                (setf bq-pending 0))
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
                (setf pending c state :normal)
                (unless (record-datum list-depth) (return nil))
                (setf bq-pending 0))
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
               (t (setf pending c state :block-comment))))))))))
