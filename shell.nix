{ pkgs ? import <nixpkgs> { } }:

pkgs.mkShell {
  OCAMLRUNPARAM = "b";
  buildInputs = with pkgs.ocamlPackages; [
    pkgs.ocaml
    pkgs.ocamlformat
    pkgs.opam
    findlib
    dune_2
    merlin
    utop
    ocaml-lsp
    # library deps
    core
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
