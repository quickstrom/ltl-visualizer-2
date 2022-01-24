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
    # library deps
    ppx_jane
    js_of_ocaml
    js_of_ocaml-ppx
    virtual_dom
    incr_dom
  ];
}
