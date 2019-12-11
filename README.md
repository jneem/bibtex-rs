# A modern BibTex implementation

This repository contains (or will contain, eventually, hopefully) a
reimplementation of the [BibTex](https://ctan.org/pkg/bibtex?lang=en),
the venerable citation manager.

# Eventually, hopefully?

Yes. So far, we just have a mostly-complete BibTex parser. There's a lot more
left to do (mainly, a `.bst`-file interpreter).

This repository does not currently contain any binaries: it consists only
of [rust](https://www.rust-lang.org/)-language libraries ("crates", in rust-speak).
Eventually, our goal is to have a drop-in replacement for the bibtex binary,
along with some other tools.

# Why reimplement BibTex?

If you use TeX or LaTeX on a regular basis, BibTex is everywhere. You use it
because your co-authors do, or because Google Scholar or mathscinet export
references in BibTex format.  But BibTex is also extremely, ahem, *venerable*.
Its only interface is a command-line batch program, and so integration into
other workflows is achieved by shelling out and parsing the error messages.
Wouldn't it be great if there were a modern BibTex engine that was modular
enough to embed it into an editor (with support for accurate syntax
highlighting and incremental execution), while retaining 100% compatibility
with old databases? This repository doesn't contain that modern engine yet,
but maybe one day it will.

# Why reimplement BibTex *again*?

There are plenty of other BibTex parsers around. Thirty seconds of searching
found two ([here](https://github.com/jackweinbender/bibtex-rs) and
[here](https://github.com/charlesvdv/nom-bibtex)) implementations just in rust.
There are two things about this implementation that are a little bit special,
though. Firstly, we aim for compatibility with BibTex: we will accept exactly
the same files and report errors in exactly the same locations. This is a
little more tricky than it sounds, because BibTex doesn't have a formally
specified grammar.  Secondly, we (eventually, hopefully) aim to provide a full
implementation, including a parser for `.aux` files and an interpreter for
`.bst` files. Most reimplementations of BibTex just parse `.bib` files.

# Known incompatiblities

This section lists situations where we our behavior intentionally differs from
that of BibTex. All of these concern differences in error messages, and not
in the behavior of the parser on correct input.

- When we encounter an I/O error reading the input, we report an error. BibTex
  treats an I/O error the same as it does an EOF.
- When we, as part of an error message, display part of the input file, our
  display will match the input file more closely than BibTex's. This is because
  BibTex sometimes changes parts of the input to lowercase before printing the
  error message.
