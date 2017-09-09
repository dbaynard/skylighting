{-# LANGUAGE OverloadedStrings #-}
-- | Provides syntax highlighting definitions.
-- THIS FILE IS AUTOMATICALLY GENERATED. DO NOT EDIT IT MANUALLY.
module Skylighting.Syntax (defaultSyntaxMap) where
import qualified Data.Map as Map
import Skylighting.Types
import qualified Skylighting.Syntax.Sed
import qualified Skylighting.Syntax.SqlMysql
import qualified Skylighting.Syntax.Modula2
import qualified Skylighting.Syntax.Pascal
import qualified Skylighting.Syntax.Sgml
import qualified Skylighting.Syntax.Makefile
import qualified Skylighting.Syntax.Prolog
import qualified Skylighting.Syntax.Noweb
import qualified Skylighting.Syntax.Fasm
import qualified Skylighting.Syntax.Nasm
import qualified Skylighting.Syntax.Relaxng
import qualified Skylighting.Syntax.Yacc
import qualified Skylighting.Syntax.Dot
import qualified Skylighting.Syntax.Php
import qualified Skylighting.Syntax.Ada
import qualified Skylighting.Syntax.Zsh
import qualified Skylighting.Syntax.Bash
import qualified Skylighting.Syntax.Objectivec
import qualified Skylighting.Syntax.Xul
import qualified Skylighting.Syntax.AlertIndent
import qualified Skylighting.Syntax.Pike
import qualified Skylighting.Syntax.Mathematica
import qualified Skylighting.Syntax.Markdown
import qualified Skylighting.Syntax.Cmake
import qualified Skylighting.Syntax.Alert
import qualified Skylighting.Syntax.Rhtml
import qualified Skylighting.Syntax.Coldfusion
import qualified Skylighting.Syntax.Julia
import qualified Skylighting.Syntax.Mips
import qualified Skylighting.Syntax.Modula3
import qualified Skylighting.Syntax.Boo
import qualified Skylighting.Syntax.Roff
import qualified Skylighting.Syntax.Scala
import qualified Skylighting.Syntax.Actionscript
import qualified Skylighting.Syntax.Relaxngcompact
import qualified Skylighting.Syntax.Lex
import qualified Skylighting.Syntax.Xslt
import qualified Skylighting.Syntax.Latex
import qualified Skylighting.Syntax.Rest
import qualified Skylighting.Syntax.Abc
import qualified Skylighting.Syntax.Texinfo
import qualified Skylighting.Syntax.Gnuassembler
import qualified Skylighting.Syntax.Fortran
import qualified Skylighting.Syntax.Apache
import qualified Skylighting.Syntax.Clojure
import qualified Skylighting.Syntax.Ocaml
import qualified Skylighting.Syntax.Ruby
import qualified Skylighting.Syntax.Curry
import qualified Skylighting.Syntax.Metafont
import qualified Skylighting.Syntax.Jsp
import qualified Skylighting.Syntax.Html
import qualified Skylighting.Syntax.Tcl
import qualified Skylighting.Syntax.Ini
import qualified Skylighting.Syntax.Javadoc
import qualified Skylighting.Syntax.Glsl
import qualified Skylighting.Syntax.Json
import qualified Skylighting.Syntax.Djangotemplate
import qualified Skylighting.Syntax.Cs
import qualified Skylighting.Syntax.Maxima
import qualified Skylighting.Syntax.Elixir
import qualified Skylighting.Syntax.Erlang
import qualified Skylighting.Syntax.Opencl
import qualified Skylighting.Syntax.Fsharp
import qualified Skylighting.Syntax.Verilog
import qualified Skylighting.Syntax.Octave
import qualified Skylighting.Syntax.Gcc
import qualified Skylighting.Syntax.Yaml
import qualified Skylighting.Syntax.Monobasic
import qualified Skylighting.Syntax.Cpp
import qualified Skylighting.Syntax.Haxe
import qualified Skylighting.Syntax.Changelog
import qualified Skylighting.Syntax.Dockerfile
import qualified Skylighting.Syntax.Kotlin
import qualified Skylighting.Syntax.Doxygen
import qualified Skylighting.Syntax.Diff
import qualified Skylighting.Syntax.R
import qualified Skylighting.Syntax.Perl
import qualified Skylighting.Syntax.Lilypond
import qualified Skylighting.Syntax.Hamlet
import qualified Skylighting.Syntax.Objectivecpp
import qualified Skylighting.Syntax.Bibtex
import qualified Skylighting.Syntax.C
import qualified Skylighting.Syntax.Xml
import qualified Skylighting.Syntax.Lua
import qualified Skylighting.Syntax.Idris
import qualified Skylighting.Syntax.D
import qualified Skylighting.Syntax.Awk
import qualified Skylighting.Syntax.Coffee
import qualified Skylighting.Syntax.Asn1
import qualified Skylighting.Syntax.Javascript
import qualified Skylighting.Syntax.Dtd
import qualified Skylighting.Syntax.Llvm
import qualified Skylighting.Syntax.Postscript
import qualified Skylighting.Syntax.Purebasic
import qualified Skylighting.Syntax.Haskell
import qualified Skylighting.Syntax.Vhdl
import qualified Skylighting.Syntax.Scheme
import qualified Skylighting.Syntax.Pure
import qualified Skylighting.Syntax.Python
import qualified Skylighting.Syntax.Go
import qualified Skylighting.Syntax.M4
import qualified Skylighting.Syntax.Mandoc
import qualified Skylighting.Syntax.Modelines
import qualified Skylighting.Syntax.Isocpp
import qualified Skylighting.Syntax.LiterateCurry
import qualified Skylighting.Syntax.Agda
import qualified Skylighting.Syntax.Xorg
import qualified Skylighting.Syntax.Doxygenlua
import qualified Skylighting.Syntax.Eiffel
import qualified Skylighting.Syntax.Css
import qualified Skylighting.Syntax.LiterateHaskell
import qualified Skylighting.Syntax.Sql
import qualified Skylighting.Syntax.Java
import qualified Skylighting.Syntax.Tcsh
import qualified Skylighting.Syntax.Ats
import qualified Skylighting.Syntax.SqlPostgresql
import qualified Skylighting.Syntax.Sci
import qualified Skylighting.Syntax.Asp
import qualified Skylighting.Syntax.Matlab
import qualified Skylighting.Syntax.Mediawiki
import qualified Skylighting.Syntax.Rust
import qualified Skylighting.Syntax.Email
import qualified Skylighting.Syntax.Commonlisp

-- | Default mapping from short names to syntax definitions.
defaultSyntaxMap :: SyntaxMap
defaultSyntaxMap = Map.fromList [
     ("sed", Skylighting.Syntax.Sed.syntax)
  ,  ("SQL (MySQL)", Skylighting.Syntax.SqlMysql.syntax)
  ,  ("Modula-2", Skylighting.Syntax.Modula2.syntax)
  ,  ("Pascal", Skylighting.Syntax.Pascal.syntax)
  ,  ("SGML", Skylighting.Syntax.Sgml.syntax)
  ,  ("Makefile", Skylighting.Syntax.Makefile.syntax)
  ,  ("Prolog", Skylighting.Syntax.Prolog.syntax)
  ,  ("noweb", Skylighting.Syntax.Noweb.syntax)
  ,  ("Intel x86 (FASM)", Skylighting.Syntax.Fasm.syntax)
  ,  ("Intel x86 (NASM)", Skylighting.Syntax.Nasm.syntax)
  ,  ("RELAX NG", Skylighting.Syntax.Relaxng.syntax)
  ,  ("Yacc/Bison", Skylighting.Syntax.Yacc.syntax)
  ,  ("dot", Skylighting.Syntax.Dot.syntax)
  ,  ("PHP/PHP", Skylighting.Syntax.Php.syntax)
  ,  ("Ada", Skylighting.Syntax.Ada.syntax)
  ,  ("Zsh", Skylighting.Syntax.Zsh.syntax)
  ,  ("Bash", Skylighting.Syntax.Bash.syntax)
  ,  ("Objective-C", Skylighting.Syntax.Objectivec.syntax)
  ,  ("XUL", Skylighting.Syntax.Xul.syntax)
  ,  ("Alerts_indent", Skylighting.Syntax.AlertIndent.syntax)
  ,  ("Pike", Skylighting.Syntax.Pike.syntax)
  ,  ("Mathematica", Skylighting.Syntax.Mathematica.syntax)
  ,  ("Markdown", Skylighting.Syntax.Markdown.syntax)
  ,  ("CMake", Skylighting.Syntax.Cmake.syntax)
  ,  ("Alerts", Skylighting.Syntax.Alert.syntax)
  ,  ("Ruby/Rails/RHTML", Skylighting.Syntax.Rhtml.syntax)
  ,  ("ColdFusion", Skylighting.Syntax.Coldfusion.syntax)
  ,  ("Julia", Skylighting.Syntax.Julia.syntax)
  ,  ("MIPS Assembler", Skylighting.Syntax.Mips.syntax)
  ,  ("Modula-3", Skylighting.Syntax.Modula3.syntax)
  ,  ("Boo", Skylighting.Syntax.Boo.syntax)
  ,  ("Roff", Skylighting.Syntax.Roff.syntax)
  ,  ("Scala", Skylighting.Syntax.Scala.syntax)
  ,  ("ActionScript 2.0", Skylighting.Syntax.Actionscript.syntax)
  ,  ("RelaxNG-Compact", Skylighting.Syntax.Relaxngcompact.syntax)
  ,  ("Lex/Flex", Skylighting.Syntax.Lex.syntax)
  ,  ("xslt", Skylighting.Syntax.Xslt.syntax)
  ,  ("LaTeX", Skylighting.Syntax.Latex.syntax)
  ,  ("reStructuredText", Skylighting.Syntax.Rest.syntax)
  ,  ("ABC", Skylighting.Syntax.Abc.syntax)
  ,  ("Texinfo", Skylighting.Syntax.Texinfo.syntax)
  ,  ("GNU Assembler", Skylighting.Syntax.Gnuassembler.syntax)
  ,  ("Fortran", Skylighting.Syntax.Fortran.syntax)
  ,  ("Apache Configuration", Skylighting.Syntax.Apache.syntax)
  ,  ("Clojure", Skylighting.Syntax.Clojure.syntax)
  ,  ("Objective Caml", Skylighting.Syntax.Ocaml.syntax)
  ,  ("Ruby", Skylighting.Syntax.Ruby.syntax)
  ,  ("Curry", Skylighting.Syntax.Curry.syntax)
  ,  ("Metapost/Metafont", Skylighting.Syntax.Metafont.syntax)
  ,  ("JSP", Skylighting.Syntax.Jsp.syntax)
  ,  ("HTML", Skylighting.Syntax.Html.syntax)
  ,  ("Tcl/Tk", Skylighting.Syntax.Tcl.syntax)
  ,  ("INI Files", Skylighting.Syntax.Ini.syntax)
  ,  ("Javadoc", Skylighting.Syntax.Javadoc.syntax)
  ,  ("GLSL", Skylighting.Syntax.Glsl.syntax)
  ,  ("JSON", Skylighting.Syntax.Json.syntax)
  ,  ("Django HTML Template", Skylighting.Syntax.Djangotemplate.syntax)
  ,  ("C#", Skylighting.Syntax.Cs.syntax)
  ,  ("Maxima", Skylighting.Syntax.Maxima.syntax)
  ,  ("Elixir", Skylighting.Syntax.Elixir.syntax)
  ,  ("Erlang", Skylighting.Syntax.Erlang.syntax)
  ,  ("OpenCL", Skylighting.Syntax.Opencl.syntax)
  ,  ("FSharp", Skylighting.Syntax.Fsharp.syntax)
  ,  ("Verilog", Skylighting.Syntax.Verilog.syntax)
  ,  ("Octave", Skylighting.Syntax.Octave.syntax)
  ,  ("GCCExtensions", Skylighting.Syntax.Gcc.syntax)
  ,  ("YAML", Skylighting.Syntax.Yaml.syntax)
  ,  ("MonoBasic", Skylighting.Syntax.Monobasic.syntax)
  ,  ("C++", Skylighting.Syntax.Cpp.syntax)
  ,  ("Haxe", Skylighting.Syntax.Haxe.syntax)
  ,  ("ChangeLog", Skylighting.Syntax.Changelog.syntax)
  ,  ("Dockerfile", Skylighting.Syntax.Dockerfile.syntax)
  ,  ("Kotlin", Skylighting.Syntax.Kotlin.syntax)
  ,  ("Doxygen", Skylighting.Syntax.Doxygen.syntax)
  ,  ("Diff", Skylighting.Syntax.Diff.syntax)
  ,  ("R Script", Skylighting.Syntax.R.syntax)
  ,  ("Perl", Skylighting.Syntax.Perl.syntax)
  ,  ("LilyPond", Skylighting.Syntax.Lilypond.syntax)
  ,  ("Hamlet", Skylighting.Syntax.Hamlet.syntax)
  ,  ("Objective-C++", Skylighting.Syntax.Objectivecpp.syntax)
  ,  ("BibTeX", Skylighting.Syntax.Bibtex.syntax)
  ,  ("C", Skylighting.Syntax.C.syntax)
  ,  ("XML", Skylighting.Syntax.Xml.syntax)
  ,  ("Lua", Skylighting.Syntax.Lua.syntax)
  ,  ("Idris", Skylighting.Syntax.Idris.syntax)
  ,  ("D", Skylighting.Syntax.D.syntax)
  ,  ("AWK", Skylighting.Syntax.Awk.syntax)
  ,  ("CoffeeScript", Skylighting.Syntax.Coffee.syntax)
  ,  ("ASN.1", Skylighting.Syntax.Asn1.syntax)
  ,  ("JavaScript", Skylighting.Syntax.Javascript.syntax)
  ,  ("DTD", Skylighting.Syntax.Dtd.syntax)
  ,  ("LLVM", Skylighting.Syntax.Llvm.syntax)
  ,  ("PostScript", Skylighting.Syntax.Postscript.syntax)
  ,  ("PureBasic", Skylighting.Syntax.Purebasic.syntax)
  ,  ("Haskell", Skylighting.Syntax.Haskell.syntax)
  ,  ("VHDL", Skylighting.Syntax.Vhdl.syntax)
  ,  ("Scheme", Skylighting.Syntax.Scheme.syntax)
  ,  ("Pure", Skylighting.Syntax.Pure.syntax)
  ,  ("Python", Skylighting.Syntax.Python.syntax)
  ,  ("Go", Skylighting.Syntax.Go.syntax)
  ,  ("GNU M4", Skylighting.Syntax.M4.syntax)
  ,  ("Troff Mandoc", Skylighting.Syntax.Mandoc.syntax)
  ,  ("Modelines", Skylighting.Syntax.Modelines.syntax)
  ,  ("ISO C++", Skylighting.Syntax.Isocpp.syntax)
  ,  ("Literate Curry", Skylighting.Syntax.LiterateCurry.syntax)
  ,  ("Agda", Skylighting.Syntax.Agda.syntax)
  ,  ("x.org Configuration", Skylighting.Syntax.Xorg.syntax)
  ,  ("DoxygenLua", Skylighting.Syntax.Doxygenlua.syntax)
  ,  ("Eiffel", Skylighting.Syntax.Eiffel.syntax)
  ,  ("CSS", Skylighting.Syntax.Css.syntax)
  ,  ("Literate Haskell", Skylighting.Syntax.LiterateHaskell.syntax)
  ,  ("SQL", Skylighting.Syntax.Sql.syntax)
  ,  ("Java", Skylighting.Syntax.Java.syntax)
  ,  ("Tcsh", Skylighting.Syntax.Tcsh.syntax)
  ,  ("ATS", Skylighting.Syntax.Ats.syntax)
  ,  ("SQL (PostgreSQL)", Skylighting.Syntax.SqlPostgresql.syntax)
  ,  ("scilab", Skylighting.Syntax.Sci.syntax)
  ,  ("ASP", Skylighting.Syntax.Asp.syntax)
  ,  ("Matlab", Skylighting.Syntax.Matlab.syntax)
  ,  ("MediaWiki", Skylighting.Syntax.Mediawiki.syntax)
  ,  ("Rust", Skylighting.Syntax.Rust.syntax)
  ,  ("Email", Skylighting.Syntax.Email.syntax)
  ,  ("Common Lisp", Skylighting.Syntax.Commonlisp.syntax) ]