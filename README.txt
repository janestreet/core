
              README: library "Core"
              *************************
 Copyright   (C)   2008-2009  Jane Street Holding, LLC (1)
=====================================================
                 Author: Lots of people 
                ======================
                 New York, 2009-10-07
                 ====================

1  Directory contents
*=*=*=*=*=*=*=*=*=*=*



  ------------------------------------------------------------------------
  |     CHANGES       |              History of code changes             |
  ------------------------------------------------------------------------
  |    COPYRIGHT      |                 Notes on copyright               |
  ------------------------------------------------------------------------
  |     INSTALL       |            Short notes on compiling and          |
  |                   |               installing the library             |
  ------------------------------------------------------------------------
  |     LICENSE       |        "GNU LESSER GENERAL PUBLIC LICENSE"       |
  ------------------------------------------------------------------------
  |   MLton-LICENSE   |   "MLton LICENSE, for extended/union_find.ml"    |
  ------------------------------------------------------------------------
  |     Makefile      |                    Top Makefile                  |
  ------------------------------------------------------------------------
  |  OCamlMakefile    |        Generic Makefile for OCaml-projects       |
  ------------------------------------------------------------------------
  |    OMakefile      |                  Ignore this file                |
  ------------------------------------------------------------------------
  |      README       |                     This file                    |
  ------------------------------------------------------------------------
  |     VERSION       |                  Current version                 |
  ------------------------------------------------------------------------
  |       lib/        |                  The Core library                |
  ------------------------------------------------------------------------
  |    lib_test/      |             Test applications for Core           |
  ------------------------------------------------------------------------
  |    extended/      |             The Core_extended library            |
  ------------------------------------------------------------------------
  |  extended_test/   |        Test applications for Core_extended       |
  ------------------------------------------------------------------------

2  What is "Core"?
*=*=*=*=*=*=*=*=*=*=*
  Core is Jane Street Capital's Ocaml standard library overlay.  It
  provides tail recursive versions of non tail recursive functions in
  the standard library, changes the signature of many of the standard
  modules, and adds new functionality.

  Core_extended adds new functionality, but is only code reviewed on an ad-hoc
  basis.

3  How can you use it?
*=*=*=*=*=*=*=*=*=*=*=
  At the top of your source, add "open Core.Std;;".
  For access to all of Core_extended as well, put "open Core_extended.Std;;" instead.

4  Conventions
*=*=*=*=*=*=*=*=*=*=*=
  
  For modules that overwrite the name of standard library modules, the
  standard library version is available as "Caml.<module>".  For
  example, "Caml.List" is the standard libary version of list.
  
  Functions that throw exceptions in non-exceptional cases have been
  replaced with optional return values.  The exception throwing
  version is available with the "_exn" suffix.
  
  In cases when a function takes two arguments of the same type,
  labeled arguments are used to make it clear which argument is which.
  
  For many modules with type t, t is the first argument to functions
  in that module.  This means currying requires "(fun t -> <function>
  t)" when the rest of the arguments aren't labeled, but provides
  consistency.
  
  Types are sexpabable.

5  List of modules
*=*=*=*=*=*=*=*=*=*=*=
  CRv2 rdouglass: TODO any release now...

6  An acknowledgement
*=*=*=*=*=*=*=*=*=*=*=
The core algorithm of extended/patience_diff was inspired by the excellent version written by Bram Cohen as seen in
Canonical's Bazaar version 1.14.1.

7  Contact information
*=*=*=*=*=*=*=*=*=*=*=
  
  In the case of bugs, feature requests and similar, you can contact us here:
     opensource@janestreet.com
   Up-to-date information concerning this library should be available here:
     http://www.janestreet.com/ocaml
   Enjoy!!
   
--------------------------------------
  
  
 (1) http://www.janestreet.com
