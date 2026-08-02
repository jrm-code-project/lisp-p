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

(defconstant +max-hash-prefix+ 1000000
  "Cap on the value accumulated for a #N... decimal digit prefix (radix,
vector length, or label number). Prefixes larger than this are treated as
an unbounded/untracked value (rather than consed as a bignum) by whichever
check would have used them.")

(defconstant +max-label+ 8192
  "Maximum #N=/#N# label number tracked for forward-reference checking (in
a fixed-size bit vector). Labels numbered beyond this bound are accepted
without verification.")

(defconstant +max-pending-req+ 64
  "Maximum nesting depth of chained deferred-form obligations: dispatch
macros (#N=, #., #+, #-) whose value is itself the next object(s) to be
read, possibly themselves starting with another such dispatch.")

(defconstant +max-exponent-field+ 1000000
  "Saturating cap for the decimal value accumulated from a float token's
explicit exponent-field digits, so it can never require a bignum.")

(defconstant +max-single-float-exponent+ 38
  "Approximate maximum base-10 order of magnitude representable by
SINGLE-FLOAT -- and by a float with no exponent marker at all, since
*READ-DEFAULT-FLOAT-FORMAT* defaults to SINGLE-FLOAT -- consistent with
IEEE 754 single precision (max magnitude ~3.4028235e38).")

(defconstant +max-double-float-exponent+ 308
  "Approximate maximum base-10 order of magnitude representable by
DOUBLE-FLOAT (and, in most implementations, LONG-FLOAT), consistent with
IEEE 754 double precision (max magnitude ~1.7976931348623157e308).")

(defparameter *whitespace-chars* '(#\Space #\Tab #\Newline #\Linefeed #\Page #\Return)
  "Characters treated as whitespace by the state machine.")

(defparameter *terminating-chars* '(#\" #\' #\( #\) #\, #\; #\` #\|)
  "Macro characters (other than #\\#, which is non-terminating) that end an
in-progress token without themselves needing to be consed onto it.")

(defun sign-char-p (c)
  (or (char= c #\+) (char= c #\-)))

(defun decimal-digit-p (c)
  (digit-char-p c 10))

(defun exponent-marker-char-p (c)
  (member c '(#\d #\D #\e #\E #\f #\F #\l #\L #\s #\S) :test #'char=))

(defun double-format-exponent-marker-p (c)
  (member c '(#\d #\D #\l #\L) :test #'char=))

;;; States in which it is legal for the stream to end.  Every other state
;;; represents an unterminated construct (open string, comment, escape,
;;; dispatch, digit prefix, or character literal) and must cause rejection
;;; at EOF.  :NORMAL additionally requires LIST-DEPTH to be zero, and
;;; :TOKEN/:RADIX-NUMBER additionally require their in-progress numeric
;;; content to be well-formed so far.
(defparameter *eof-ok-states*
  '(:normal :token :char-literal-rest :line-comment :radix-number :bit-vector)
  "States that may legally be active when the stream reaches end-of-file
(besides requiring LIST-DEPTH to be zero).")

(defun whitespace-char-p (c)
  (member c *whitespace-chars* :test #'char=))

(defun terminating-char-p (c)
  (member c *terminating-chars* :test #'char=))

(defun lisp-p (stream)
  "Return T if STREAM (or STRING) contains a putative Lisp program, NIL
otherwise.  If STREAM is a string, it is converted to a stream via
WITH-INPUT-FROM-STRING and LISP-P is called recursively on that stream.

This drives a state machine modeled on the Common Lisp reader algorithm
(CLHS 2.2) one character at a time, without consing or interning: state is
an enumerated/dispatch value, and nesting is tracked with bounded integer
counters (LIST-DEPTH for parenthesized lists/vectors, COMMENT-DEPTH for
nested #| |# block comments, BQ-PENDING for backquote/comma prefixes not
yet applied to a datum) plus three fixed-size, dynamic-extent (stack
allocated, not heap-consed) per-depth arrays -- BQ-LEVEL, HAS-DATUM, and
DOT-STATE -- rather than any heap-allocated representation of the parsed
forms.

If STREAM is a STRING, this function does not itself avoid consing (the
string must already exist in memory, and WITH-INPUT-FROM-STRING allocates a
string-stream wrapper), but the recursive call that scans it obeys the
no-consing/no-interning constraints described below.

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

Numeric tokens are also checked for the one case CLHS 2.3.1.1 identifies as
genuinely invalid rather than a fallback to symbol-hood: a ratio token
(optional sign, then digits, a single slash, then digits, and nothing
else) whose denominator consists entirely of zero digits, e.g. \"5/0\" or
\"-5/000\".  CLHS notes such a ratio \"is not represented in any
implementation,\" and real readers (e.g. SBCL) signal READER-ERROR for it.
Every other token that merely looks like a malformed number -- \"1/2/3\",
\"1.2.3\", \"12A\", \".e5\", \"1+\" -- is, per the CLHS 2.2 reader
algorithm, simply read back as an ordinary symbol when it fails to have
valid number syntax, so LISP-P accepts those unconditionally; only the
zero-denominator ratio case is rejected.

Several dispatch-macro-character (#\\#...) forms are additionally checked:

- Radix-prefixed numbers, #b/#o/#x/#Nr (2 <= N <= 36): unlike a plain
  token, these never fall back to symbol-hood, so any character that is
  not a valid sign, digit-in-that-radix, or ratio slash is rejected
  immediately, and (as with plain ratios) an all-zero-digit denominator is
  rejected.
- #N( (explicit-length vector), #c(...) (complex number), and #s(...)
  (structure): the element count of the following list is checked against
  each dispatch's arity rule -- at most N elements for #N( (fewer is
  accepted; the reader pads by repeating the last element), exactly 2 for
  #c(...), and an odd count of at least 1 (a name plus zero or more
  even-numbered slot/value pairs) for #s(...).  The *types* of those
  elements (e.g. that #c's components are actually reals, or that #s's
  first element is a symbol naming a real structure type) are not
  checked.
- #N= (label definition) and #N# (label reference), for N < +MAX-LABEL+:
  a #N# that references a label never defined by a preceding #N= is
  rejected; redefining a label is permitted, matching real reader
  behavior.
- #* (bit-vector): only #\\0 and #\\1 may appear between #* and its
  terminator.
- #N=, #., #+, and #- each require one or more further data to follow
  (the labelled/eval'd/feature-guarded form(s)); reaching EOF before that
  requirement is satisfied is rejected.  #+/#- are approximated as always
  requiring exactly the feature-expression plus the guarded form (2 data)
  to follow, since real feature-conditional reading depends on the
  run-time *FEATURES* list, which a static validator has no access to.
- Floating-point tokens (CLHS 2.3.2.2: optional sign, digit+, optionally
  a decimal point and more digits, optionally an exponent marker letter
  followed by an optionally-signed digit+) have their base-10 order of
  magnitude -- computed purely by counting mantissa digits and the
  explicit exponent field, never via floating-point or bignum arithmetic
  -- checked against the range representable by the applicable float
  format: +MAX-SINGLE-FLOAT-EXPONENT+ (38) for the marker-less default
  format or an e/s/f marker, +MAX-DOUBLE-FLOAT-EXPONENT+ (308) for a d/l
  marker.  A value of exactly zero (all mantissa digits zero) is never
  rejected regardless of exponent, matching real underflow-to-zero
  behavior; likewise, an overly negative exponent (underflow) is never
  rejected, only overflow.  As with ratios, a token that merely looks
  float-ish but fails to match this grammar exactly is left to fall back
  to symbol-hood, with no exponent check applied.

Explicitly out of scope, and always accepted permissively (no validation
at all): #A array syntax (rank/dimension nesting is not tracked); #+/#-
feature-expression truth (approximated as described above, not actually
evaluated); the semantic validity of #s's structure-type name or #c's
component types (only element counts are checked, as described above);
and any unrecognized dispatch character, which is treated the same as an
ordinary #\\ macro character with no special following syntax."
  (when (stringp stream)
    (return-from lisp-p
      (with-input-from-string (s stream)
        (lisp-p s))))
  (let ((state :normal)
        (list-depth 0)
        (comment-depth 0)
        (len 0)
        (pending nil)
        (bq-pending 0)
        (bq-level (make-array (1+ +max-list-depth+)
                               :element-type 'fixnum :initial-element 0))
        (has-datum (make-array (1+ +max-list-depth+) :initial-element nil))
        (dot-state (make-array (1+ +max-list-depth+) :initial-element :none))
        ;; Per-depth bookkeeping for #N( vector-length, #c(...) complex,
        ;; and #s(...) structure arity checks.  LIST-KIND records which
        ;; (if any) of those dispatches opened the list at that depth;
        ;; LIST-LIMIT is #N('s declared length (NIL = unlimited, as for
        ;; a plain #( or "("); ELEM-COUNT counts direct children read so
        ;; far in that list frame.  NEXT-LIST-KIND/NEXT-LIST-LIMIT are a
        ;; one-shot pending value, set by #N(/#c(/#s( just before the
        ;; "(" they require, and applied (then reset) when that "(" is
        ;; processed.
        (list-kind (make-array (1+ +max-list-depth+) :initial-element :plain))
        (list-limit (make-array (1+ +max-list-depth+) :initial-element nil))
        (elem-count (make-array (1+ +max-list-depth+)
                                 :element-type 'fixnum :initial-element 0))
        (next-list-kind :plain)
        (next-list-limit nil)
        ;; Bounded forward-reference tracking for #N=/#N# labels: bit I
        ;; is set once #I= has been read.
        (label-defined (make-array +max-label+ :element-type 'bit :initial-element 0))
        ;; Stack of "how many more data are owed" counts, one frame per
        ;; currently-pending #N=, #., #+, or #- dispatch.  See
        ;; PUSH-PENDING-FORMS and SATISFY-ONE-FORM.
        (pending-req-stack (make-array +max-pending-req+
                                        :element-type 'fixnum :initial-element 0))
        (pending-req-top 0)
        ;; Digit-prefix accumulator for #N=, #N#, #Nr, #N(, #NA -- used
        ;; while STATE is :HASH-DIGITS.
        (hash-num 0)
        (hash-num-overflow nil)
        ;; Radix-number (#b, #o, #x, #Nr) sub-FSA, used while STATE is
        ;; :RADIX-NUMBER.  Unlike a plain token, an invalid character
        ;; here is rejected immediately rather than falling back to
        ;; symbol-hood, since a dispatch-prefixed radix number is never
        ;; read as anything but a number.
        (rn-state :rn-start)
        (rn-radix 10)
        (rn-denom-all-zero nil)
        ;; Numeric-token tracking for the token currently being
        ;; accumulated in state :TOKEN (started from :NORMAL or
        ;; :DOT-TOKEN).  NUM-STATE follows a small FSA that recognizes
        ;; only the exact ratio grammar (CLHS 2.3.2.1.2): optional
        ;; sign, digit+, a single slash, digit+, and nothing else.  Any
        ;; other token content (a plain symbol, or a malformed
        ;; almost-number like "1.2.3" or "12A") falls out of the FSA
        ;; into :NS-NONE and is never rejected -- per the CLHS 2.2
        ;; reader algorithm, a token that merely looks like a number
        ;; but does not have valid number syntax is simply read back as
        ;; a symbol, not an error.  The one case CLHS explicitly calls
        ;; out as invalid despite matching number syntax is a ratio
        ;; whose denominator is all zero digits (e.g. "5/0"), tracked by
        ;; NUM-DENOM-ALL-ZERO once NUM-STATE reaches :NS-DENOM.
        (num-state :ns-none)
        (num-denom-all-zero nil)
        ;; Float-token exponent-magnitude tracking, run in parallel with
        ;; NUM-STATE above on every character of the same token.
        ;; NF-STATE recognizes exact CLHS 2.3.2.2 float syntax; a token
        ;; that doesn't match it (or isn't a float attempt at all) is
        ;; never checked here, for the same falls-back-to-symbol reason
        ;; malformed ratios aren't rejected.  Once a token *does* match
        ;; float syntax (NF-STATE ends at :NF-FRAC or :NF-EXP-DIGIT),
        ;; FLOAT-TOKEN-OK-P computes its base-10 order of magnitude from
        ;; the mantissa digits (INT-DIGITS/INT-HAS-NONZERO/
        ;; INT-FIRST-NONZERO for the integer part; FRAC-DIGITS/
        ;; FRAC-HAS-NONZERO/FRAC-FIRST-NONZERO for the fractional part,
        ;; used only if the integer part was all zeros) plus the
        ;; explicit exponent field (EXP-SIGN * EXP-VALUE, saturating so
        ;; it never needs a bignum), and rejects it only if that combined
        ;; order exceeds the range representable by the applicable float
        ;; format (EXP-MARKER, or none for the default format).
        (nf-state :nf-none)
        (int-digits 0)
        (int-has-nonzero nil)
        (int-first-nonzero 0)
        (frac-digits 0)
        (frac-has-nonzero nil)
        (frac-first-nonzero 0)
        (exp-marker nil)
        (exp-sign 1)
        (exp-value 0))
    (declare (dynamic-extent bq-level has-datum dot-state list-kind list-limit
                             elem-count label-defined pending-req-stack))
    (labels ((note-int-digit (c)
               (incf int-digits)
               (when (and (not int-has-nonzero) (not (char= c #\0)))
                 (setf int-has-nonzero t int-first-nonzero int-digits)))
             (note-frac-digit (c)
               (incf frac-digits)
               (when (and (not frac-has-nonzero) (not (char= c #\0)))
                 (setf frac-has-nonzero t frac-first-nonzero frac-digits)))
             (note-exp-digit (c)
               (setf exp-value (min (+ (* exp-value 10) (digit-char-p c 10))
                                     +max-exponent-field+)))
             (numeric-token-start (c)
               ;; Called on the first character of a token (from
               ;; :NORMAL's #\. branch or its generic token branch).
               ;; Initializes both the ratio-only FSA (NUM-STATE) and
               ;; the float-magnitude FSA (NF-STATE) that run in
               ;; parallel over the same token.
               (setf num-state (cond ((decimal-digit-p c) :ns-num)
                                     ((sign-char-p c) :ns-sign)
                                     (t :ns-none)))
               (setf int-digits 0 int-has-nonzero nil int-first-nonzero 0
                     frac-digits 0 frac-has-nonzero nil frac-first-nonzero 0
                     exp-marker nil exp-sign 1 exp-value 0)
               (setf nf-state
                     (cond ((decimal-digit-p c) (note-int-digit c) :nf-int)
                           ((sign-char-p c) :nf-sign)
                           ((char= c #\.) :nf-dot-noint)
                           (t :nf-none))))
             (numeric-token-feed (c)
               ;; Called on each subsequent character of the token.
               (setf num-state
                     (case num-state
                       (:ns-sign (if (decimal-digit-p c) :ns-num :ns-none))
                       (:ns-num
                        (cond ((decimal-digit-p c) :ns-num)
                              ((char= c #\/) :ns-slash)
                              (t :ns-none)))
                       (:ns-slash
                        (if (decimal-digit-p c)
                            (progn (setf num-denom-all-zero (char= c #\0))
                                   :ns-denom)
                            :ns-none))
                       (:ns-denom
                        (cond ((decimal-digit-p c)
                               (setf num-denom-all-zero
                                     (and num-denom-all-zero (char= c #\0)))
                               :ns-denom)
                              (t :ns-none)))
                       (t :ns-none)))
               (setf nf-state
                     (case nf-state
                       (:nf-sign
                        (cond ((decimal-digit-p c) (note-int-digit c) :nf-int)
                              ((char= c #\.) :nf-dot-noint)
                              (t :nf-invalid)))
                       (:nf-int
                        (cond ((decimal-digit-p c) (note-int-digit c) :nf-int)
                              ((char= c #\.) :nf-int-dot)
                              ((exponent-marker-char-p c) (setf exp-marker c) :nf-exp-marker)
                              (t :nf-invalid)))
                       (:nf-int-dot
                        (cond ((decimal-digit-p c) (note-frac-digit c) :nf-frac)
                              ((exponent-marker-char-p c) (setf exp-marker c) :nf-exp-marker)
                              (t :nf-invalid)))
                       (:nf-dot-noint
                        (cond ((decimal-digit-p c) (note-frac-digit c) :nf-frac)
                              (t :nf-invalid)))
                       (:nf-frac
                        (cond ((decimal-digit-p c) (note-frac-digit c) :nf-frac)
                              ((exponent-marker-char-p c) (setf exp-marker c) :nf-exp-marker)
                              (t :nf-invalid)))
                       (:nf-exp-marker
                        (cond ((sign-char-p c) (setf exp-sign (if (char= c #\-) -1 1)) :nf-exp-sign)
                              ((decimal-digit-p c) (note-exp-digit c) :nf-exp-digit)
                              (t :nf-invalid)))
                       (:nf-exp-sign
                        (cond ((decimal-digit-p c) (note-exp-digit c) :nf-exp-digit)
                              (t :nf-invalid)))
                       (:nf-exp-digit
                        (cond ((decimal-digit-p c) (note-exp-digit c) :nf-exp-digit)
                              (t :nf-invalid)))
                       (t :nf-invalid)))
               t)
             (numeric-token-ok-p ()
               ;; Reject only a fully-formed ratio token whose
               ;; denominator consists entirely of zero digits.
               (not (and (eq num-state :ns-denom) num-denom-all-zero)))
             (float-token-ok-p ()
               ;; Only a token that fully matches float grammar
               ;; (NF-STATE ended at a valid terminal) is checked at
               ;; all; anything else -- a plain symbol, integer, ratio,
               ;; or malformed float-ish token -- is left alone.
               (if (not (member nf-state '(:nf-frac :nf-exp-digit)))
                   t
                   (let* ((zero-valued (and (not int-has-nonzero) (not frac-has-nonzero)))
                          (mag-order (cond (int-has-nonzero (- int-digits int-first-nonzero))
                                           (frac-has-nonzero (- frac-first-nonzero))
                                           (t 0)))
                          (effective (+ mag-order (* exp-sign exp-value)))
                          (bound (if (and exp-marker (double-format-exponent-marker-p exp-marker))
                                     +max-double-float-exponent+
                                     +max-single-float-exponent+)))
                     (or zero-valued (<= effective bound)))))
             (record-datum (d)
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
                    (progn (setf (svref dot-state list-depth) :dot-seen) t)))
             (push-pending-forms (n)
               ;; A dispatch (#N=, #., #+, or #-) has just been seen
               ;; that requires N more data to follow before it is
               ;; itself complete.
               (when (>= pending-req-top +max-pending-req+)
                 (return-from push-pending-forms nil))
               (setf (aref pending-req-stack pending-req-top) n)
               (incf pending-req-top)
               t)
             (satisfy-one-form ()
               ;; Any datum has just completed; it satisfies one
               ;; obligation of the innermost pending deferred-form
               ;; frame, cascading outward through any frames that
               ;; become fully satisfied as a result.
               (loop while (plusp pending-req-top)
                     do (let ((idx (1- pending-req-top)))
                          (decf (aref pending-req-stack idx))
                          (if (zerop (aref pending-req-stack idx))
                              (decf pending-req-top)
                              (return)))))
             (note-child-completed (parent-depth)
               ;; A child datum has just completed at PARENT-DEPTH;
               ;; update its running element count and, if that list was
               ;; opened by #N( or #c(, check the incremental arity
               ;; limit those dispatches impose.  (#s(...)'s parity
               ;; check happens once, at list-close, since it depends on
               ;; the final count.)
               (let ((kind (svref list-kind parent-depth)))
                 (incf (aref elem-count parent-depth))
                 (case kind
                   (:vector
                    (when (and (svref list-limit parent-depth)
                               (> (aref elem-count parent-depth) (svref list-limit parent-depth)))
                      (return-from note-child-completed nil)))
                   (:complex
                    (when (> (aref elem-count parent-depth) 2)
                      (return-from note-child-completed nil))))
                 t))
             (finish-datum (d)
               ;; A complete datum (list, string, token, char literal,
               ;; multi-escape symbol, radix number, or bit-vector) has
               ;; just been read at nesting depth D: apply consing-dot
               ;; bookkeeping, clear one level of pending deferred-form
               ;; obligation, and update the enclosing list's element
               ;; count against any #N(/#c(/#s( arity constraint.
               (and (record-datum d)
                    (progn (satisfy-one-form) t)
                    (note-child-completed d))))
      (loop
        (let ((c (if pending
                     (shiftf pending nil)
                     (read-char stream nil :eof))))
          (when (eq c :eof)
            (let ((ok (and (zerop list-depth)
                           (not (null (member state *eof-ok-states*))))))
              (when (and ok (eq state :token))
                (setf ok (and (numeric-token-ok-p) (float-token-ok-p)
                              (finish-datum list-depth))))
              (when (and ok (eq state :radix-number))
                (setf ok (and (member rn-state '(:rn-num :rn-denom))
                              (not (and (eq rn-state :rn-denom) rn-denom-all-zero))
                              (finish-datum list-depth))))
              (when (and ok (eq state :bit-vector))
                (setf ok (finish-datum list-depth)))
              (return (and ok (zerop pending-req-top)))))
          (ecase state
            (:normal
             (cond
               ((whitespace-char-p c))
               ((char= c #\()
                (when (>= list-depth +max-list-depth+) (return nil))
                (setf (svref has-datum (1+ list-depth)) nil
                      (svref dot-state (1+ list-depth)) :none
                      (aref bq-level (1+ list-depth))
                      (+ (aref bq-level list-depth) bq-pending)
                      (svref list-kind (1+ list-depth)) next-list-kind
                      (svref list-limit (1+ list-depth)) next-list-limit
                      (aref elem-count (1+ list-depth)) 0)
                (setf next-list-kind :plain next-list-limit nil)
                (setf bq-pending 0)
                (incf list-depth))
               ((char= c #\))
                (when (zerop list-depth) (return nil))
                (when (or (not (zerop bq-pending))
                          (eq (svref dot-state list-depth) :dot-seen))
                  (return nil))
                (case (svref list-kind list-depth)
                  (:complex (unless (= (aref elem-count list-depth) 2) (return nil)))
                  (:structure (let ((n (aref elem-count list-depth)))
                                (when (or (zerop n) (evenp n)) (return nil)))))
                (decf list-depth)
                (unless (finish-datum list-depth) (return nil))
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
               ((char= c #\.) (setf state :dot-token len 1) (numeric-token-start c))
               ((terminating-char-p c))
               (t (setf state :token len 1) (numeric-token-start c))))

            (:dot-token
             (cond
               ((or (whitespace-char-p c) (terminating-char-p c))
                (unless (handle-dot) (return nil))
                (setf pending c state :normal))
               (t (setf state :token len 2) (numeric-token-feed c))))

            (:token
             (cond
               ((or (whitespace-char-p c) (terminating-char-p c))
                (unless (and (numeric-token-ok-p) (float-token-ok-p)) (return nil))
                (setf pending c state :normal)
                (unless (finish-datum list-depth) (return nil))
                (setf bq-pending 0))
               (t (incf len)
                  (when (> len +max-token-length+) (return nil))
                  (numeric-token-feed c))))

            (:string
             (cond
               ((char= c #\\) (setf state :string-escape))
               ((char= c #\")
                (setf state :normal)
                (unless (finish-datum list-depth) (return nil))
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
                (unless (finish-datum list-depth) (return nil))
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
               ((decimal-digit-p c)
                (setf state :hash-digits hash-num (digit-char-p c 10) hash-num-overflow nil))
               ((char-equal c #\b) (setf state :radix-number rn-state :rn-start rn-radix 2 len 0))
               ((char-equal c #\o) (setf state :radix-number rn-state :rn-start rn-radix 8 len 0))
               ((char-equal c #\x) (setf state :radix-number rn-state :rn-start rn-radix 16 len 0))
               ((char= c #\*) (setf state :bit-vector len 0))
               ((char-equal c #\c)
                (let ((c2 (read-char stream nil :eof)))
                  (cond ((eq c2 :eof) (return nil))
                        ((char= c2 #\() (setf next-list-kind :complex pending c2 state :normal))
                        (t (return nil)))))
               ((char-equal c #\s)
                (let ((c2 (read-char stream nil :eof)))
                  (cond ((eq c2 :eof) (return nil))
                        ((char= c2 #\() (setf next-list-kind :structure pending c2 state :normal))
                        (t (return nil)))))
               ((sign-char-p c)
                ;; The +/- itself was fully consumed as part of the #+/#-
                ;; dispatch; do not feed it back into :NORMAL, or it would
                ;; be misread as the start of the following form's token.
                (unless (push-pending-forms 2) (return nil))
                (setf state :normal))
               ((char= c #\.)
                ;; Likewise: the "." was consumed by "#.", not a bare
                ;; consing-dot candidate, so go straight to :NORMAL.
                (unless (push-pending-forms 1) (return nil))
                (setf state :normal))
               (t (setf pending c state :normal))))

            (:hash-digits
             (cond
               ((decimal-digit-p c)
                (unless hash-num-overflow
                  (let ((v (+ (* hash-num 10) (digit-char-p c 10))))
                    (if (> v +max-hash-prefix+)
                        (setf hash-num-overflow t)
                        (setf hash-num v)))))
               ((char= c #\=)
                (when (and (not hash-num-overflow) (< hash-num +max-label+))
                  (setf (sbit label-defined hash-num) 1))
                (unless (push-pending-forms 1) (return nil))
                (setf state :normal))
               ((char= c #\#)
                (when (and (not hash-num-overflow) (< hash-num +max-label+)
                           (zerop (sbit label-defined hash-num)))
                  (return nil))
                (unless (finish-datum list-depth) (return nil))
                (setf state :normal))
               ((char-equal c #\r)
                (if (or hash-num-overflow (< hash-num 2) (> hash-num 36))
                    (return nil)
                    (setf state :radix-number rn-state :rn-start rn-radix hash-num len 0)))
               ((char= c #\()
                (setf next-list-kind :vector
                      next-list-limit (if hash-num-overflow nil hash-num)
                      pending c state :normal))
               ((char-equal c #\a)
                ;; #NA array syntax: dimension/rank nesting is out of
                ;; scope; accept permissively with no further checks.
                (setf state :normal))
               (t (setf pending c state :normal))))

            (:radix-number
             (cond
               ((or (whitespace-char-p c) (terminating-char-p c))
                (unless (and (member rn-state '(:rn-num :rn-denom))
                             (not (and (eq rn-state :rn-denom) rn-denom-all-zero)))
                  (return nil))
                (setf pending c state :normal)
                (unless (finish-datum list-depth) (return nil))
                (setf bq-pending 0))
               (t
                (incf len)
                (when (> len +max-token-length+) (return nil))
                (setf rn-state
                      (case rn-state
                        (:rn-start
                         (cond ((digit-char-p c rn-radix) :rn-num)
                               ((sign-char-p c) :rn-sign)
                               (t (return nil))))
                        (:rn-sign
                         (cond ((digit-char-p c rn-radix) :rn-num)
                               (t (return nil))))
                        (:rn-num
                         (cond ((digit-char-p c rn-radix) :rn-num)
                               ((char= c #\/) :rn-slash)
                               (t (return nil))))
                        (:rn-slash
                         (cond ((digit-char-p c rn-radix)
                                (setf rn-denom-all-zero (char= c #\0))
                                :rn-denom)
                               (t (return nil))))
                        (:rn-denom
                         (cond ((digit-char-p c rn-radix)
                                (setf rn-denom-all-zero (and rn-denom-all-zero (char= c #\0)))
                                :rn-denom)
                               (t (return nil))))
                        (t (return nil)))))))

            (:bit-vector
             (cond
               ((or (whitespace-char-p c) (terminating-char-p c))
                (setf pending c state :normal)
                (unless (finish-datum list-depth) (return nil))
                (setf bq-pending 0))
               ((or (char= c #\0) (char= c #\1))
                (incf len)
                (when (> len +max-token-length+) (return nil)))
               (t (return nil))))

            (:char-literal
             ;; The character immediately following #\ is consumed
             ;; unconditionally, whatever its syntax type.
             (incf len)
             (setf state :char-literal-rest))

            (:char-literal-rest
             (cond
               ((or (whitespace-char-p c) (terminating-char-p c))
                (setf pending c state :normal)
                (unless (finish-datum list-depth) (return nil))
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
