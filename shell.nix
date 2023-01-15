{ pkgs ? import <nixpkgs> { } }:

pkgs.mkShell {
  OCAMLRUNPARAM = "b";
  buildInputs = with pkgs.ocamlPackages; [
    pkgs.fswatch
    pkgs.ghp-import
    pkgs.ocaml
    pkgs.ocamlformat
    pkgs.opam
    findlib
    dune_2
    merlin
    utop
    ocaml-lsp
    menhir
    # library deps
    core_kernel
    ppx_jane
    ppx_deriving
    js_of_ocaml
    js_of_ocaml-ppx
    angstrom
    virtual_dom
    incr_dom
    qcheck
  ];
}
