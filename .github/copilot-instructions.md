# lisp-p

A Common Lisp ASDF system that validates whether a stream contains a Lisp
program. The project is currently a minimal stub: `lisp-p` (the exported
function) always returns `nil` and has no implementation yet.

## Design

`lisp-p` implements the validation as a **state machine modeled on the
Common Lisp reader algorithm**, but with two hard constraints that
distinguish it from a normal reader:

- **No consing / allocation** — it must not build lists, strings, or other
  heap objects while scanning input.
- **No interning** — it must not create or look up symbols (no `intern`,
  no package operations).

The machine consumes the input stream one character at a time, transitioning
between reader states (e.g. top-level, in a list, in a string, in a
character escape, after a reader macro character, in a comment, etc.),
mirroring the states the standard Lisp reader passes through when parsing
tokens, lists, strings, and macro characters (see CLHS 2.2, "Reader
Algorithm"). The input is accepted iff, at end-of-file, all parentheses and
other delimiters are balanced back to the top level (i.e. the state machine
has returned to its initial/top-level state with no open lists, strings, or
other unterminated constructs).

When implementing or extending the state machine, prefer representing state
as an enumerated/dispatch value (not a data structure that would require
allocation) and drive transitions purely from the character stream and
current state — do not build any representation of the parsed form.

### Expected states

The reader algorithm distinguishes characters by syntax type (constituent,
terminating/non-terminating macro character, single escape `\`, multiple
escape `|`, whitespace). The states below are the ones needed to track that
without consing:

- **Top level** — the accept state. Only valid at EOF.
- **In a list** — reading between `(` and `)` (also entered for the `(` that
  closes a `#(` vector literal or `#'(lambda ...)`, since they share the same
  closing delimiter). Tracked with a single integer *depth counter*, not a
  stack, since every open list is closed by the same `)` character and
  there's nothing per-frame to remember.
- **In a token** — accumulating a symbol or number. Ends on whitespace or a
  terminating macro character; the terminator itself starts a new
  transition (e.g. a token followed directly by `(` ends the token and opens
  a list) rather than being consumed as part of the token.
- **In a string** — between unescaped `"` characters.
- **In a single escape** — immediately after a `\`; the next character is
  consumed literally regardless of its own syntax type, then control
  returns to whatever state was active before the `\`.
- **In a multiple escape** — between unescaped `|` characters (e.g.
  `|foo bar|` symbols); like a string but delimited by `|`, and does not
  nest. A `\` inside still single-escapes the next character.
- **In a line comment** — after an unquoted `;`, through end-of-line or EOF.
- **In a block comment** — between `#|` and `|#`. These *do* nest, so this
  state needs its own depth counter (separate from the list-depth counter),
  incremented on nested `#|` and decremented on `|#`.
- **After `#`** — dispatch-macro-character state; the next character
  determines how to continue (e.g. `#\` enters character-literal-name
  reading, `#|` enters a block comment, `#(` enters a list, `#:` starts an
  uninterned-symbol token, etc.). This state exists only to look at one more
  character before dispatching, so it does not itself need a counter.
- **In a character literal name** — after `#\`; accumulates constituent
  characters as a bounded token (same length limit as a normal token) since
  named characters like `#\Newline` are multi-character but still just a
  token boundary problem, not a semantic one.

Only two independent nesting counters are required: one for list depth and
one for block-comment depth. Everything else (string, multiple-escape,
single-escape, line comment, token, dispatch) is a simple on/off flag or a
one-shot lookahead, because CL only has one list-closing delimiter and only
block comments support nesting.

### Length limits

The machine also rejects putative programs containing extraordinarily long
symbols, strings, or comments. Enforce this with bounded counters (not by
accumulating the actual characters, which would require allocation) that
fail the input once a token/string/comment exceeds its length limit. Reset
each counter when its corresponding state is (re-)entered (e.g. entering a
new token, a new string, a new line/block comment), since the limit applies
per-instance, not cumulatively across the whole stream.

## Structure

- `lisp-p.asd` — ASDF system definition. Components are wired with explicit
  `:depends-on` (not `:serial t`), so any new file added to the system must
  declare its dependency on `package` (and on each other, as needed).
- `package.lisp` — defines the `LISP-P` package (`:use "COMMON-LISP"`) and
  exports the public symbol `LISP-P`.
- `lisp-p.lisp` — implementation, `(in-package "LISP-P")`.

## Conventions

- Package and export names are upper-case strings (e.g. `"LISP-P"`), matching
  the style already used in `package.lisp`.
- When adding new source files, add a corresponding `(:file ...)` entry to
  `lisp-p.asd` with an explicit `:depends-on` list rather than relying on load
  order.

## Working with SBCL

When invoking `sbcl` in this environment, pass `--no-userinit`. The user's
personal `~/.sbclrc` redefines `[` as a vector-literal reader macro, which
breaks unrelated code that uses `[` as a plain symbol/reader character (e.g.
`double-precision[]`-style names in some dependencies). Loading the system
via Quicklisp typically looks like:

```
sbcl --no-userinit --non-interactive --load "$HOME\quicklisp\setup.lisp" --eval "(ql:quickload :lisp-p)"
```

There is currently no test system or CI workflow defined for `lisp-p` — no
`*-tests` component in `lisp-p.asd` and no `.github/workflows`. If tests are
added, define a `test-system` in the `.asd` and wire it into
`asdf:test-system`.
