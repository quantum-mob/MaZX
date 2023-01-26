(* -*- mode:math -*- *)
Get["Q3`"];
Q3Assert["2.9.3"];

BeginPackage["MaZX`", {"Q3`"}]

Unprotect["`*"];
ClearAll["`*"];

`MaZX`$Version = StringJoin[
  "Solovay/", $Input, " v",
  StringSplit["$Revision: 2.39 $"][[2]], " (",
  StringSplit["$Date: 2023-01-26 19:04:01+09 $"][[2]], ") ",
  "Mahn-Soo Choi"
 ];

{ MaZXInfo, MaZXUpdate, MaZXCheckUpdate };

{ MaZXGeneral };

{ PhaseValue };

{ ZXDiagram, ZXObject,
  ZXMultiply, ZXBraKet };

{ ZXSpeciesQ,
  ZSpider, ZSpiders, ZSpiderQ,
  XSpider, XSpiders, XSpiderQ,
  Hadamard, Hadamards, HadamardQ,
  Diamond, Diamonds, DiamondQ };

{ ZXSpiders, ZXLinks };

{ ToZBasis, ToXBasis };

{ ZXForm };

{ Layers };


{ $Z, $X, $H, $B, $i, $o };


Begin["`Private`"]

ClearAll["`*"];

MaZXGeneral::base = "Something is wrong. You are trying Base[``], whish should not occur in ZX calculs."

MaZXGeneral::local = "You are using a beta version of MaZX locally installed in `1`."

MaZXGeneral::setup = "The MaZX application has not been installed properly. Go to `` for the installation guide."


(**** <MaZXInfo> ****)

MaZXInfo::usage = "MaZXInfo[] prints the information about the MaZX release."

MaZXInfo[] := Module[
  { pac = PacletObject @ "MaZX" },
  If[ FailureQ @ pac,
    Message[MaZXGeneral::setup,
      Hyperlink["https://github.com/quantum-mob/MaZX/blob/main/INSTALL.md"]
     ];
    Return[pac]
   ];
  
  If[ Not @ StringContainsQ[
      pac @ "Location",
      FileNameJoin @ {"Paclets", "Repository", "MaZX-"}
     ],
    Message[MaZXGeneral::local, pac @ "Location"]
   ];

  StringJoin["MaZX v", pac @ "Version", " Mahn-Soo Choi"]
 ]


MaZXUpdate::usage = "MaZXUpdate[] installs the latest update of the package."

MaZXUpdate[opts___?OptionQ] := (
  PrintTemporary["Installing an update ..."];
  PacletDataRebuild[];
  Q3`Private`serverEnsure[];
  PacletInstall["MaZX", opts, UpdatePacletSites -> True]
 )


MaZXCheckUpdate::usage = "MaZXCheckUpdate[] checks if there is a newer release of MaZX in the GitHub repository."

MaZXCheckUpdate[] := Module[
  { pac, new },
  PrintTemporary["Checking for updates ..."];
  PacletDataRebuild[];
  Q3`Private`serverEnsure[];
  pac = PacletFind["MaZX"];
  new = PacletFindRemote["MaZX", UpdatePacletSites -> True];
  If[ pac=={}, Return[$Failed], pac = Q3`Private`pacletVersion[pac] ];
  If[ new=={}, Return[$Failed], new = Q3`Private`pacletVersion[new] ];
  If[ OrderedQ @ {
      Q3`Private`versionNumber @ new,
      Q3`Private`versionNumber @ pac },
    Print["You are using the latest release v", pac, " of MaZX."],
    PrintTemporary["MaZX,v", new, " is now available; ",
      "you are using v", pac, "."];
    MaZXUpdate[]
   ]
 ]

(**** </MaZXInfo> ****)


ZSpider::usage = "ZSpider ..."

XSpider::usage = "XSpider ..."

ZSpider /:
Let[ZSpider, {ls__}] := (
  Clear[ls];
  Scan[setSpecies, {ls}];
  Scan[setZSpider, {ls}];
 )

XSpider /:
Let[XSpider, {ls__}] := (
  Clear[ls];
  Scan[setSpecies, {ls}];
  Scan[setXSpider, {ls}];
 )

Unprotect[Let];

Let[Hadamard, {ls__}] := (
  Clear[ls];
  Scan[setSpecies, {ls}];
  Scan[setHadamard, {ls}];
 )

Let[Diamond, {ls__}] := (
  Clear[ls];
  Scan[setSpecies, {ls}];
  Scan[setDiamond, {ls}];
 )

Protect[Let];

setSpecies[Z_Symbol] := (
  ClearAttributes[Z, Attributes @ Z];
  SetAttributes[Z, {NHoldAll, ReadProtected}];

  ZXSpeciesQ[Z] ^= True;
  ZXSpeciesQ[Z[___]] ^= True;
  ZXSpeciesQ[Z[___][___]] ^= True;

  Z /: Base @ Z[k___] = Z[k];
  Z /: Base @ Z := (Message[MaZXGeneral::base, Z]; Z[0]);

  Z[k___] := ReleaseHold @ Thread[Hold[Z][k]] /; AnyTrue[{k}, MatchQ[_List]];
  Format[Z[k___]] := Interpretation[Subscript[Z, k], Z[k]];
 )

setSpider[S_Symbol] := (
  S /: Base @ S[k___][___] = S[k];
  
  Format[S[k___][phi_]] :=
    Interpretation[TraditionalForm @ Subscript[S, k][phi], S[k][phi]];
 )

setZSpider[Z_Symbol] := (
  setSpider[Z];
  
  ZSpiderQ[Z] ^= True;
  ZSpiderQ[Z[___]] ^= True;
  ZSpiderQ[Z[___][___]] ^= True;
 )

setXSpider[X_Symbol] := (
  setSpider[X];
  
  XSpiderQ[X] ^= True;
  XSpiderQ[X[___]] ^= True;
  XSpiderQ[X[___][___]] ^= True;
 )

setDiamond[B_Symbol] := (
  DiamondQ[B] ^= True;
  DiamondQ[B[___]] ^= True;
 )

setHadamard[H_Symbol] := (
  HadamardQ[H] ^= True;
  HadamardQ[H[___]] ^= True;
 )


ZXSpeciesQ[_] = False;
ZSpiderQ[_] = XSpiderQ[] = HadamardQ[_] = DiamondQ[_] = False;
ZXSpiderQ[S_] := Or[ZSpiderQ @ S, XSpiderQ @ S]


PhaseValue::usage = "PhaseValue[spider] returns the phase value of spider."

PhaseValue[_?ZSpiderQ[___][phi_]] = phi

PhaseValue[_?XSpiderQ[___][phi_]] = phi

PhaseValue[_] = Nothing


ZXLinks::usage = "ZXLinks[expr] returns an association of ZX links in expression expr."

ZXLinks[expr_] := Union @ ReplaceAll[
  Cases[Flatten @ {expr}, _Rule],
  op_?ZXSpeciesQ :> Base[op]
 ]


ZXSpiders::usage = "ZXSpiders[expr] returns the association of all Z and X spiders in ZX expression expr."

ZXSpiders[ZXObject[assc_Association, ___?OptionQ]] := assc["Spiders"]

ZXSpiders[objs:{ZXObject[_Association, ___?OptionQ]...}] :=
  Merge[Map[ZXSpiders, objs], First]

ZXSpiders[expr_] := Module[
  { spiders },
  spiders = Merge[
    Cases[ Flatten @ {expr},
      Z_?ZXSpiderQ :> Rule[Base @ Z, PhaseValue @ Z],
      Infinity, Heads -> False ],
    Identity
   ];
  checkSpiders[spiders];
  KeySort @ Map[First, spiders /. {} -> {0}]
 ]

checkSpiders[spiders_Association] := Module[
  { new },
  new = Select[Union /@ spiders, (Length[#] < 1)&];
  If[Length[new] > 0, Message[ZXDiagram::none, Normal @ new]];
  new = Select[Union /@ spiders, (Length[#] > 1)&];
  If[Length[new] > 0, Message[ZXDiagram::many, Normal @ new]];
 ]


Diamonds::usage = "Diamonds returns the list of all diamonds in ZX expression expr."

Diamonds[expr_] :=
  Union @ Cases[{expr}, Z_?DiamondQ :> Base[Z], Infinity, Heads -> False]


Hadamards::usage = "Hadamards returns the list of all Hadamard gates in ZX expression expr."

Hadamards[expr_] :=
  Union @ Cases[{expr}, Z_?HadamardQ :> Base[Z], Infinity, Heads -> False]


checkHadamards[g_Graph][hh_List] := Module[
  { dd },
  dd = Transpose @ {
    VertexInDegree[g, #]& /@ hh,
    VertexInDegree[g, #]& /@ hh };
  dd = AssociationThread[hh -> dd];
  If[ AnyTrue[dd, (# != 1)&, 2],
    dd = Select[dd, AnyTrue[#, (# != 1)&]&];
    Message[ZXDiagram::hadamard, Normal @ dd]
   ];
  dd
 ]


(**** <ZXDiagram> ****)

ZXDiagram::usage = "ZXDiagram[{spec}] constructs the ZX diagram and stores it as ZXObject."

ZXDiagram::none = "No phase value for some spiders: ``. Zero is assumed."

ZXDiagram::many = "Different phase values for the same spiders: ``. The first value is taken for each spider."

ZXDiagram::hadamard = "Wrong arities for some Hadamard gates: ``. Every Hadamard gate should have one and only one input and output link."

(* NOTE: The ZX expressions needs to be enclosed in a List since they include
   Rules, which may not be distinguished from options. *)
ZXDiagram[past___ZXObject, spec_List, opts___?OptionQ] := Module[
  { data },
  data = Association[
    "Spiders" -> ZXSpiders @ Join[
      KeyValueMap[#1[#2]&, ZXSpiders @ {past}],
      spec /. Rule -> List ],
    "Hadamards" -> Hadamards @ {spec /. Rule -> List},
    "Diamonds" -> Diamonds @ {spec /. Rule -> List},
    "Links" -> ZXLinks[spec]
   ];
  Join[ZXObject[data, opts], past]
 ]

(**** </ZXDiagram> ****)


(**** <ZXObject> ****)

ZXObject::usage = "ZXObject[...] ..."

Format @ ZXObject[data_Association, opts___?OptionQ] :=
  Graph @ ZXObject[data, opts]

ZXObject /:
Join[obj:ZXObject[_Association, ___?OptionQ]..] := Module[
  { new = Merge[First /@ {obj}, Union] },
  new = MapAt[Merge[#, Identity]&, new, 1];
  checkSpiders[new @ "Spiders"];
  ZXObject[
    MapAt[Catenate, MapAt[Map[First], new, 1], {{2}, {3}, {4}}],
    Sequence @@ Flatten[Rest /@ {obj} /. ZXObject -> List]
   ]
 ]

ZXObject /:
Graph[ZXObject[data_Association, opts___?OptionQ], more___?OptionQ] :=
  Module[
    { graph, gates, sizes, labels, zz, xx, hh, bb, ss },
    graph = Graph[
      Join[Keys @ data @ "Spiders", data @ "Hadamards", data @ "Diamonds"],
      data @ "Links"
     ];
    sizes = Map[
      (# -> If[MemberQ[AdjacencyList[graph, #], #], 0.01, 0.4])&,
      VertexList[graph]
     ];

    zz = KeySelect[data @ "Spiders", ZSpiderQ];
    xx = KeySelect[data @ "Spiders", XSpiderQ];

    checkHadamards[graph][data @ "Hadamards"];

    labels = Join[
      Normal @ Map[Placed[#, Center]&, data @ "Spiders"],
      (# -> Placed["H", Center])& /@ data["Hadamards"]
     ];

    gates = Union[ Keys @ data @ "Spiders",
      data @ "Hadamards", data @ "Diamonds" ];
    
    Graph[ VertexList @ graph, EdgeList @ graph, more,
      Sequence @@ FilterRules[{opts}, Options @ Graph],
      VertexShapeFunction -> Join[
        Thread[data["Diamonds"] -> "Diamond"],
        Thread[data["Hadamards"] -> "Square"] ], 
      VertexStyle -> Join[
        Thread[Keys[zz] -> Green],
        Thread[Keys[xx] -> Red], 
        Thread[data["Hadamards"] -> Yellow],
        Thread[data["Diamonds"] -> Black],
        Thread[Complement[VertexList @ graph, gates] ->
            Directive[Transparent, EdgeForm[]]] ],
      VertexSize -> sizes, 
      VertexLabels -> labels,
      EdgeStyle -> Arrowheads[{{0.05, 0.58}}],
      ImageSize -> Medium,
      GraphLayout -> "SpringElectricalEmbedding"
     ]
   ]


ZXObject /:
Basis[obj:ZXObject[data_Association, ___?OptionQ], ___] := Module[
  { graph = Graph @ obj,
    vv, in, out },
  vv = VertexList[graph];
  in = Select[vv, VertexInDegree[graph, #] == 0 &];
  out = Select[vv, VertexOutDegree[graph, #] == 0 &];
  in = Sort @ Flatten @ Map[outgoingEdges[graph], in];
  out = Sort @ Flatten @ Map[incomingEdges[graph], out];
  Association[
    "In" -> Map[Ket[in -> #]&, Tuples[{0, 1}, Length @ in]],
    "Out" -> Map[Ket[out -> #]&, Tuples[{0, 1}, Length @ out]]
   ]
 ]

ZXObject /:
Basis[obj_ZXObject, ___] := obj (* fallback *)


ZXObject /:
Matrix[obj_ZXObject, ___] := Matrix @ ExpressionFor[obj]


ZXObject /:
ExpressionFor[obj:ZXObject[data_Association, ___]] := Module[
  { graph = Graph @ obj,
    layers, rules },
  rules = Normal @ AssociationMap[
    (First[#] -> First[#][Last @ #])&,
    data @ "Spiders"
   ];
  layers = Layers[graph] /. rules;
  Apply[ ZXMultiply,
    theExpression[graph] /@ Flatten[Reverse @ Values @ layers]
   ] * Power[Sqrt[2], Length @ data @ "Diamonds"] // Garner
 ]

ZXObject /:
ExpressionFor[obj_ZXObject, ___] := obj (* fallback *)

theExpression[g_Graph][v_?ZSpiderQ[k___][p_]] := With[
  { in = incomingEdges[g][v @ k],
    out = outgoingEdges[g][v @ k] },
  ZXMultiply[Ket[out -> 0], Bra[in -> 0]] +
    ZXMultiply[Ket[out -> 1], Bra[in -> 1]] * Exp[I*p]
 ]

theExpression[g_Graph][v_?XSpiderQ[k___][p_]] := With[
  { in = incomingEdges[g][v @ k],
    out = outgoingEdges[g][v @ k] },
  ZXMultiply[Ket[ out -> 2], Bra[in -> 2]] +
    ZXMultiply[Ket[ out -> 3], Bra[in -> 3]] * Exp[I*p]
 ]

theExpression[g_Graph][v_?HadamardQ] := With[
  { in = incomingEdges[g][v],
    out = outgoingEdges[g][v] },
  ZXMultiply[Ket[out -> 2], Bra[in -> 0]] +
    ZXMultiply[Ket[out -> 3], Bra[in -> 1]]
 ]

theExpression[g_Graph][v_?DiamondQ[___]] = Sqrt[2]

theExpression[g_Graph][v_] := Module[
  { in = incomingEdges[g][v],
    out = outgoingEdges[g][v] },
  Which[
    in == {}, (* input vertex *)
    ZXMultiply[Ket[out -> 0], Bra[v -> 0]] +
      ZXMultiply[Ket[out -> 1], Bra[v -> 1]],
    out == {}, (* output vertex *)
    ZXMultiply[Ket[v -> 0], Bra[in -> 0]] +
      ZXMultiply[Ket[v -> 1], Bra[in -> 1]],
    True, 1 ]
 ]

(**** </ZXObject> ****)


(**** <ZXMultiply> ****)

ZXMultiply::usage = "ZXMultiply[a, b, ...] ..."

SetAttributes[ZXMultiply, Listable]

Unprotect[$GarnerPatterns];
AddGarnerPatterns[_ZXMultiply];
Protect[$GarnerPatterns];

Format[ZXMultiply[a_Ket, b_Bra]] := Interpretation[
  Row @ {a, b} /. {2 -> "+", 3 -> "-"},
  ZXMultiply[a, b]
 ]

Format[ZXMultiply[a_Ket, b:Bra[]]] := Interpretation[
  a /. {2 -> "+", 3 -> "-"},
  ZXMultiply[a, b]
 ]

Format[ZXMultiply[a:Ket[], b_Bra]] := Interpretation[
  b /. {2 -> "+", 3 -> "-"},
  ZXMultiply[a, b]
 ]

ZXMultiply /: NonCommutativeQ[_ZXMultiply] = True

ZXMultiply /: Matrix[ZXMultiply[Ket[a_], Bra[b_]], ___] :=
  Dyad[theKet @@ Values[a], theKet @@ Values[b]]

theKet[] = {1}

theKet[0] = TheKet[0]

theKet[1] = TheKet[1]

theKet[2] = {1, 1} / Sqrt[2] (* Ket[+] *)

theKet[3] = {1, -1} / Sqrt[2] (* Ket[-] *)

theKet[mm:(0|1|2|3|4)..] := CircleTimes @@ Map[theKet, {mm}]


ZXMultiply[pre___, z_?CommutativeQ, post___] := ZXMultiply[pre, post]

ZXMultiply[pre___, z_?CommutativeQ op_, post___] := z * ZXMultiply[pre, op, post]

ZXMultiply[pre___, expr_Plus, post___] := Total @ ZXMultiply[pre, List @@ expr, post]

ZXMultiply[pre___, Ket[a_], Ket[b_], post___] :=
  ZXMultiply[pre, Ket @ KeySort @ Join[a, b], post]

ZXMultiply[pre___, Bra[a_], Bra[b_], post___] :=
  ZXMultiply[pre, Bra @ KeySort @ Join[a, b], post]

ZXMultiply[pre___, Bra[a_], Ket[b_], post___] := With[
  { kk = Intersection[Keys @ a, Keys @ b] },
  ZXMultiply[pre,
    Ket @ KeyDrop[b, Keys @ a], Bra @ KeyDrop[a, Keys @ b], post] *
    Apply[Times, ZXBraKet[Lookup[a, kk], Lookup[b, kk]]]
 ]

HoldPattern @ ZXMultiply[pre___, ZXMultiply[ops__], post___] := ZXMultiply[pre, ops, post]


ZXBraKet::usage = "ZXBraKet[a, b] returns the ..."

SetAttributes[ZXBraKet, Listable]

ZXBraKet[0, 2] = ZXBraKet[2, 0] = 1 / Sqrt[2]

ZXBraKet[1, 2] = ZXBraKet[2, 1] = 1 / Sqrt[2]

ZXBraKet[0, 3] = ZXBraKet[3, 0] = 1 / Sqrt[2]

ZXBraKet[1, 3] = ZXBraKet[3, 1] = -1 / Sqrt[2]

ZXBraKet[a_, a_] = 1

ZXBraKet[a_, b_] = 0

(**** </ZXMultiply> ****)


(**** <ToZBasis> <ToXBasis> ****)

ToZBasis::usage = "ToZBasis[expr] ..."

ToXBasis::usage = "ToXBasis[expr] ..."

ToZBasis[Bra[a_Association], kk_] := Dagger @ ToZBasis[Ket @ a, kk]

ToXBasis[Bra[a_Association], kk_] := Dagger @ ToXBasis[Ket @ a, kk]


ToZBasis[Ket[a_Association], All] := ToZBasis[Ket @ a, Keys @ a]

ToXBasis[Ket[a_Association], All] := ToXBasis[Ket @ a, Keys @ a]


ToZBasis[Ket[a_Association], kk_List] := Module[
  { bb = Select[KeyTake[a, kk], (# > 1)&],
    aa },
  aa = KeyDrop[a, Keys @ bb];
  bb = Map[Ket, Thread[Keys[bb] -> 0]] +
    Map[Ket, Thread[Keys[bb] -> 1]] * (1 - 2*Boole[OddQ @ Values @ bb]);
  Garner @ CircleTimes[Ket @ aa,  Apply[Sequence, bb / Sqrt[2]]]
 ]

ToXBasis[Ket[a_Association], kk_List] := Module[
  { bb = Select[KeyTake[a, kk], (# < 2)&],
    aa },
  aa = KeyDrop[a, Keys @ bb];
  bb = Map[Ket, Thread[Keys[bb] -> 2]] +
    Map[Ket, Thread[Keys[bb] -> 3]] * (1 - 2*Boole[OddQ @ Values @ bb]);
  Garner @ CircleTimes[Ket @ aa,  Apply[Sequence, bb / Sqrt[2]]]
 ]


ToZBasis[expr_, kk_] := expr /. {
  v:Ket[_Association] :> ToZBasis[v, kk],
  v:Bra[_Association] :> ToZBasis[v, kk]
 } // Garner

ToXBasis[expr_, kk_] := expr /. {
  v:Ket[_Association] :> ToXBasis[v, kk],
  v:Bra[_Association] :> ToXBasis[v, kk]
 } // Garner


ToZBasis[expr_] := ToZBasis[expr, All]

ToXBasis[expr_] := ToXBasis[expr, All]

(**** </ToXBasis> </ToZBasis> ****)


(**** <Layers> ****)

Layers::usage = "Layers[graph] returns the list of layers.\nSee also LayeredGraphPlot, LayeredGraphPlot3D."

Layers[graph_Graph] := <||> /; VertexList[graph] <= 1

Layers[graph_Graph] := Module[
  { vv = VertexList @ graph,
    in, layers },
  in = Select[vv, VertexInDegree[graph, #] == 0&];
  layers = Sort /@ DeleteCases[
    NestList[VertexOutComponent[graph, #, {1}]&, in, Length @ vv],
    {}
   ];
  layers = FoldPairList[
    {Complement[#2, #1], Union[#2, #1]}&,
    {}, Reverse @ layers ];
  AssociationThread[Range[Length @ layers] -> Reverse[layers]]
 ]

incomingEdges[g_Graph][v_] :=
  Map[EdgeIndex[g, #]&, EdgeList[g, DirectedEdge[_, v]]]

outgoingEdges[g_Graph][v_] :=
  Map[EdgeIndex[g, #]&, EdgeList[g, DirectedEdge[v, _]]]

(**** </Layers> ****)


(**** <ZXForm> ****)

ZXForm::usage = "ZXForm[qc] converts quantum circuit qc to a ZXObject.\nNote that it only supports gates acting on up to two qubits."

ZXForm[QuantumCircuit[spec___, opts___?OptionQ], more___?OptionQ] := Module[
  { aa = Qubits @ {spec},
    vv, ee },
  aa = AssociationThread[aa -> Range[Length @ aa]];
  vv = MapIndexed[zxcGate[aa], Flatten @ {spec}];
  ee = Flatten @ ReplaceAll[ vv,
    { Rule -> List,
      _?DiamondQ -> Nothing,
      sp_?ZXSpiderQ :> Base[sp] } ];
  ee = KeyValueMap[
    Chain[$i @ #1, Sequence @@ #2, $o @ #1]&,
    GroupBy[ee, First @* Base]
   ];
  ZXDiagram[Flatten @ {vv, ee}, more, opts]
 ]

zxcGate[aa_Association][q_?QubitQ, {t_Integer}] := With[
  { k = theSpacetime[aa][q, t] },
  Switch[ FlavorLast[q],
    3, $Z[k][Pi],
    1, $X[k][Pi],
    2, {$Z[k][Pi], $Z[k][Pi]},
    6, $H[k],
    _, 1
   ]
 ]

zxcGate[ss_Association][CNOT[{a_?QubitQ} -> {1}, {b_?QubitQ}], {t_Integer}] :=
  { $Z[theSpacetime[ss][a, t]][0] ->
      $X[theSpacetime[ss][b, t]][0], $B[t] }

theSpacetime[aa_Association][q_?QubitQ, t_Integer] :=
  Sequence[aa @ FlavorMute @ q, t]

(**** </ZXForm> ****)


(**** Epilog ****)

$Z::usage = "$Z is a symbol reserved for the Z spider in the ZX-calculus. See also \[FormalCapitalZ]."

$X::usage = "$X is a symbol reserved for the X spider in the ZX-calculus. See also \[FormalCapitalX]."

$H::usage = "$H is a symbol reserved for the Hadamard gate in the ZX-calculus. See also \[FormalCapitalH]."

$B::usage = "$B is a symbol reserved for the ZX diamond in the ZX-calculus. See also \[FormalCapitalB]."

$i::usage = "$i is a symbol reserved for inputs in a ZX diagram."

$o::usage = "$o is a symbol reserved for outputs in a ZX diagram."

Let[ZSpider, $Z, Global`Z];
Let[XSpider, $X, Global`X];
Let[Diamond, $B, Global`B];
Let[Hadamard, $H, Global`H];
Let[Species, $i, $o];

SetAttributes[Evaluate @ Names["`*"], ReadProtected];

End[]

SetAttributes[Evaluate @ Protect["`*"], ReadProtected];

EndPackage[]
