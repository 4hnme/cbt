# 'Caml Build Tool
simple build tool for small OCaml projects

## Usage
- `cbt init <project name>` to create a new project
- `cbt build` to build it
- `cbt install` to install it on your system<br><br>
it is that simple<br>
to see more details on each command, add `help` after it
> note: good luck with naming your project "help"!

## Project configuration
to configure your project, simply **declare** your modules inside `proj.cbt` file:
```
#  module       modules to link with     external packages
cbt         ; modulename1, modulename2 ; base, stdio, unix
modulename1 ; modulename2,             ; base, unix
modulename2 ; _                        ; base, unix
```
the first line declares the main module that will be compiled into an executable file on build, and the project name gets derived from it

### But why?
why not use [dune](https://github.com/ocaml/dune)? because i felt like it was an overkill for majority of projects i'm making

### LSP integration
at the moment [ocaml-lsp](https://github.com/ocaml/ocaml-lsp) doesn't fully support not dune-based project, although it kinda does (see [pull request](https://github.com/ocaml/ocaml-lsp/pull/1173)). in order for LSP to function properly, in project's root you will need to create:
- a `.merlin` file by following [the guide](https://github.com/ocaml/merlin/wiki/Project-configuration) or by using `cbt drop-merlin` command which will generate it for you automatically
- an empty `dune-workspace` file
> note: this will become obsolete once the aforementioned pull request is added into ocaml-lsp's release version
