# Linear Temporal Logic Visualizer (v2)

✨ [quickstrom.github.io/ltl-visualizer-2/](https://quickstrom.github.io/ltl-visualizer-2/) ✨

This is an interactive visualizer for linear temporal logic (LTL), made as a
companion tool for the article [Specifying State Machines with Temporal Logic
](https://wickstrom.tech/programming/2021/05/03/specifying-state-machines-with-temporal-logic.html). Use this to play around with formulae and traces to get a better sense of how the temporal operators work.

## Usage

The rough workflow for this little application goes as follows:

1. Add formulae (see section on [Syntax](#syntax) below)
2. The atomic propositions in your formula are automatically shown above the formula
3. Toggle the truth of an atomic proposition in each respective state (click the circles!)
4. See how the truth of more complex formulae (e.g. with temporal operators) are affected when changing the atomic propositions
5. (Repeat and learn LTL for great good!)

**NOTE:** The last state in the trace is considered repeating forever
(also called a _lasso_). We can't practically have an infinite number
of checkboxes on a web page, but we want to mess around with infinite
traces nonetheless!

### Syntax

The most basic syntactic construct is the atomic proposition. It's denoted by a
single uppercase letter (A-Z), and it represents something that is true or false
in a given state. Here's an atomic proposition:

```js
A
```

Operators are of two kinds:

* Unary operators (prefix):
  - `not`
  - `next`
  - `always`
  - `eventually`
* Binary operators (infix):
  - `&&`
  - `||`
  - `=>` (implication)
  - `until`
  
All binary operators are right-associative. For example, `X until Y =>
next Z` is parsed as `X until (Y => (next Z))`.

Here are some examples:

```js
next A 
A || B
C && D
A => eventually B
always next C
A until B && eventually C
eventually always D
```

### Sharing

TODO

## Hacking

All dependencies are supplied by the Nix shell.

- Build: `dune build` (and open `_build/default/bin/index.html`)
- Build & watch: `dune build --watch`
- Deploy to GitHub Pages: `./deploy.sh`

## License

[Mozilla Public License Version 2.0](LICENSE)
