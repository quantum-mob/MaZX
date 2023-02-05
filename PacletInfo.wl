(* Paclet Info File *)
(* $Date: 2023-02-03 09:28:08+09 $ *)
(* $Revision: 1.12 $ *)

Paclet[
  "Name" -> "MaZX",
  "Version" -> "0.3.9", (* draft *)
  "WolframVersion" -> "12.1+",
  "Updating" -> Automatic,
  "Extensions" -> {
    { "Kernel",
      "Root" -> "Kernel",
      "Context" -> { "MaZX`" }
      (* Context specifies the package context or list of contexts.
         The list is also used by FindFile.
         The list also causes documentation links to be added to usage
         messages when documentation is present. *)
     },
    { "Documentation",
      "Language" -> "English",
      "MainPage" -> "Guides/MaZX" }
   },
  "Description" -> "Mathematica package for ZX-calculus",
  "Creator" -> "Mahn-Soo Choi (Korea University)",
  "URL" -> "https://github.com/quantum-mob/MaZX",
  "Category" -> { "Physics", "Quantum Physics", "Quantum Computing" },
  "Keywords" -> { "quantum information", "quantum computation" },
  "Support" -> "quantum.mob21@gmail.com"
 ]
