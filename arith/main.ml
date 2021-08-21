open Format
open Support.Error
open Support.Pervasive
open Syntax
open Core

let searchpath = ref [""]

let argspecs = [
    "-I",
    Arg.String (fun f -> searchpath := f :: !searchpath),
    "Append a directory to the search path"
]

let parseArgs () =
