#!/usr/bin/env sh

ocamlfind ocamlopt -I _build -package base -c cmd.ml -o _build/cmd.cmx
ocamlfind ocamlopt -I _build -package unix -package stdio -package base -c printer.ml -o _build/printer.cmx
ocamlfind ocamlopt -I _build -package unix -package stdio -package base -c app.ml -o _build/app.cmx
ocamlfind ocamlopt -I _build -package unix -package stdio -package re -package base -c project.ml -o _build/project.cmx
ocamlfind ocamlopt -I _build -package unix -package stdio -package base -c testing.ml -o _build/testing.cmx
ocamlfind ocamlopt -I _build -package unix -package stdio -package re -package base -c cbt.ml -o _build/cbt.cmx
ocamlfind ocamlopt -I _build -linkpkg -package unix -package stdio -package re -package base -o cbt cmd.cmx printer.cmx app.cmx project.cmx testing.cmx cbt.cmx
