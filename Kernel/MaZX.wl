(* -*- mode:math -*- *)
Get["Q3`"];
Q3Assert["2.9.6"];

BeginPackage["MaZX`", {"Q3`"}]

Unprotect["`*"];
ClearAll["`*"];

`MaZX`$Version = StringJoin[
  "Solovay/", $Input, " v",
  StringSplit["$Revision: 4.32 $"][[2]], " (",
  StringSplit["$Date: 2023-01-30 23:08:01+09 $"][[2]], ") ",
  "Mahn-Soo Choi"
 ];

{ MaZXInfo, MaZXUpdate, MaZXCheckUpdate };

{ MaZXGeneral };

{ PhaseValue };

{ ZXDiagram, ZXObject,
  ZXMultiply, ZXBraKet };

{ ZXSpeciesQ, ZXSpiderQ,
  ZSpider, ZSpiders, ZSpiderQ,
  XSpider, XSpiders, XSpiderQ,
  Hadamard, ZXHadamards, ZXHadamardQ,
  Diamond, ZXDiamonds, ZXDiamondQ };

{ ZXSpiders, ZXLinks };

{ ToZBasis, ToXBasis };

{ ZXForm };

{ ZXLayers };

{ ZXTimes };


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
  S[___][Nothing] = Nothing;
  
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
  ZXDiamondQ[B] ^= True;
  ZXDiamondQ[B[___]] ^= True;
 )

setHadamard[H_Symbol] := (
  ZXHadamardQ[H] ^= True;
  ZXHadamardQ[H[___]] ^= True;
 )


ZXSpeciesQ[_] = False;
ZSpiderQ[_] = XSpiderQ[_] = ZXHadamardQ[_] = ZXDiamondQ[_] = False;
ZXSpiderQ[S_] := Or[ZSpiderQ @ S, XSpiderQ @ S]


PhaseValue::usage = "PhaseValue[spider] returns the phase value of spider."

PhaseValue[_?ZSpiderQ[___][phi_]] = phi

PhaseValue[_?XSpiderQ[___][phi_]] = phi

PhaseValue[_] = Nothing


ZXInputs::usage = "ZXInputs[...] ..."

ZXOutputs::usage = "ZXOutputs[...] ..."

ZXInputs[obj_ZXObject] := ZXInputs[minimalGraph @ obj]

ZXInputs[graph_Graph] := Sort @ Select[
  DeleteCases[VertexList @ graph, _?ZXSpeciesQ],
  VertexInDegree[graph, #] == 0 &
 ]

ZXOutputs[obj_ZXObject] := ZXOutputs[minimalGraph @ obj]

ZXOutputs[graph_Graph] := Sort @ Select[
  DeleteCases[VertexList @ graph, _?ZXSpeciesQ],
  VertexOutDegree[graph, #] == 0 &
 ]


ZXLinks::usage = "ZXLinks[expr] returns an association of ZX links in expression expr."

ZXLinks[expr_] := ReplaceAll[
  Cases[Flatten @ {expr}, _Rule],
  op_?ZXSpeciesQ :> Base[op]
 ]


ZSpiders::usage = "ZSpiders[obj] returns the association of all Z spiders in ZX expression expr."

ZSpiders[obj:ZXObject[_List, _List, ___?OptionQ]] :=
  Cases[VertexList @ obj, _?ZSpiderQ]


XSpiders::usage = "XSpiders[expr] returns the list of all X spiders in ZX expression expr."

ZSpiders[obj:ZXObject[_List, _List, ___?OptionQ]] :=
  Union @ Cases[VertexList @ obj, _?ZSpiderQ]

ZSpiders[expr_] :=
  Union @ Cases[{expr}, _?ZSpiderQ, Infinity, Heads -> False]


XSpiders::usage = "XSpiders[expr] returns the list of all X spiders in ZX expression expr."

XSpiders[obj:ZXObject[_List, _List, ___?OptionQ]] :=
  Union @ Cases[VertexList @ obj, _?XSpiderQ]

XSpiders[expr_] :=
  Union @ Cases[{expr}, _?XSpiderQ, Infinity, Heads -> False]


ZXSpiders::usage = "ZXSpiders[expr] returns the list of all Z and X spiders in ZX expression expr."

ZXSpiders[obj:ZXObject[_List, _List, ___?OptionQ]] :=
  Union @ Cases[VertexList @ obj, _?ZXSpiderQ]

ZXSpiders[expr_] :=
  Union @ Cases[{expr}, _?ZXSpiderQ, Infinity, Heads -> False]


ZXDiamonds::usage = "ZXDiamonds returns the list of all diamonds in ZX expression expr."

ZXDiamonds[obj:ZXObject[_List, _List, ____?OptionQ]] :=
  Union @ Cases[VertexList @ obj, _?ZXDiamondQ]

ZXDiamonds[expr_] :=
  Union @ Cases[{expr}, _?ZXDiamondQ, Infinity, Heads -> False]


ZXHadamards::usage = "ZXHadamards returns the list of all Hadamard gates in ZX expression expr."

ZXHadamards[obj:ZXObject[_List, _List, ____?OptionQ]] :=
  Union @ Cases[VertexList @ obj, _?ZXHadamardQ]

ZXHadamards[expr_] :=
  Union @ Cases[{expr}, _?ZXHadamardQ, Infinity, Heads -> False]

checkHadamards[g_Graph][hh_List] := Module[
  { dd },
  dd = Transpose @ {
    VertexInDegree[g, #]& /@ hh,
    VertexOutDegree[g, #]& /@ hh };
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

ZXDiagram[spec_List, opts___?OptionQ] := Module[
  { args, past, rules, vtxes, edges },
  past = Cases[spec, _ZXObject, Infinity, Heads -> False];
  args = DeleteCases[spec, _ZXObject, Infinity, Heads -> False];
  args = Flatten[args /. Rule -> flatChain /. flatChain -> Chain];
  rules= ruleSpiders[{past, args}];
  edges = Cases[args, _Rule] /. rules;
  vtxes = Union @ Flatten[
    args /. {_?ZXObject -> Nothing, Rule -> List} /. rules
   ];
  Join[ZXObject[vtxes, edges, opts], Sequence @@ past]
 ]

ruleSpiders[expr_] := Module[
  { spiders },
  spiders = Merge[
    Cases[
      Flatten[expr /. obj_ZXObject :> ZXSpiders[obj]],
      v_?ZXSpiderQ :> Rule[Base[v], Base[v][PhaseValue @ v]],
      Infinity, Heads -> False ],
    Identity
   ];
  checkSpiders[spiders];
  Prepend[
    Normal @ KeySort @ Map[First, spiders /. {} -> {0}],
    Global`v:(_Symbol?ZXSpiderQ[___][_]) :> Global`v
   ]
 ]

checkSpiders[spiders_Association] := Module[
  { new },
  new = Select[Union /@ spiders, (Length[#] < 1)&];
  If[Length[new] > 0, Message[ZXDiagram::none, Normal @ new]];
  new = Select[Union /@ spiders, (Length[#] > 1)&];
  If[Length[new] > 0, Message[ZXDiagram::many, Normal @ new]];
 ]

flatChain::ugate = "flatChain is a wrapper for Chain."

SetAttributes[flatChain, Flat]

(**** </ZXDiagram> ****)


(**** <ZXObject> ****)

ZXObject::usage = "ZXObject[...] ..."

Format[obj:ZXObject[_List, _List, ___?OptionQ]] :=
  Interpretation[Graph @ obj, obj]

ZXObject /:
Graph[obj_ZXObject, ___?OptionQ] = obj (* fallback *)

ZXObject /:
Graph[obj:ZXObject[vv_List, ee_List, opts___?OptionQ], more___?OptionQ] :=
  Module[
    { rules = ruleSpiders[vv],
      zz, xx, hh, bb, rr },

    zz = Select[vv, ZSpiderQ];
    xx = Select[vv, XSpiderQ];

    hh = Select[vv, ZXHadamardQ];
    checkHadamards[minimalGraph @ obj][hh];

    bb = Select[vv, ZXDiamondQ];

    rr = DeleteCases[vv, _?ZXSpeciesQ];
    
    Graph[ vv, ee,
      Sequence @@ FilterRules[{more} /. rules, Options @ Graph],
      Sequence @@ FilterRules[{opts} /. rules, Options @ Graph],
      VertexShapeFunction -> Join[
        Thread[bb -> "Diamond"],
        Thread[hh -> "Square"] ], 
      VertexStyle -> Join[
        Thread[zz -> Green],
        Thread[xx -> Red], 
        Thread[hh -> Yellow],
        Thread[bb -> Black],
        Thread[rr -> Directive[EdgeForm[], Transparent]] ],
      VertexSize -> Join[
       Thread[zz -> 0.4],
       Thread[xx -> 0.4],
       Thread[hh -> 0.4],
       Thread[bb -> 0.1],
       Thread[rr -> 0.01] ], 
      VertexLabels -> Join[
        (# -> Placed[PhaseValue @ #, Center])& /@ Join[zz, xx],
        (# -> Placed["H", Center])& /@ hh ],
      EdgeStyle -> Arrowheads[{{Medium, 0.6}}],
      ImageSize -> Medium
     ]
   ]

(* Minimal graph with only skeleton. *)
minimalGraph[ZXObject[vv_List, ee_List, ___?OptionQ], more___?OptionQ] :=
  Graph[DeleteCases[vv, _?ZXDiamondQ], ee, more];


ZXObject /:
Join[obj:ZXObject[_List, _List, ___?OptionQ]..] := Module[
  { data, opts },
  {data, opts} = Transpose @ Map[
    TakeDrop[#, 2]&,
    {obj} /. ZXObject -> List
   ];
  data = Flatten /@ Transpose @ data;
  opts = Flatten @ opts;
  ZXObject[Union @ First @ data, Last @ data, Sequence @@ opts]
 ]

(**** </ZXObject> ****)


(**** <Basis> ****)

ZXObject /:
Basis[obj:ZXObject[_List, _List, ___?OptionQ], ___] := With[
  { in = ZXInputs[obj],
    out = ZXOutputs[obj] },
  Association[
    "In" -> Map[Ket[in -> #]&, Tuples[{0, 1}, Length @ in]],
    "Out" -> Map[Ket[out -> #]&, Tuples[{0, 1}, Length @ out]]
   ]
 ]

ZXObject /:
Basis[obj_ZXObject, ___] := obj (* fallback *)


ZXObject /:
VertexList[ZXObject[vv_List, ee_List, ___?OptionQ]] :=
  VertexList[Graph[vv, ee]]

ZXObject /:
VertexList[obj_ZXObject] = obj (* fallback *)

(**** </Basis> ****)


(**** <ExpressionFor> ****)

ZXObject /:
ExpressionFor[obj:ZXObject[_List, _List, ___]] := Module[
  { graph = minimalGraph @ obj },
  Apply[ ZXMultiply,
    Map[theExpression[graph], Flatten @ Reverse @ ZXLayers @ graph]
   ] * Power[Sqrt[2], Length @ ZXDiamonds @ obj] // Garner
 ]

ZXObject /:
ExpressionFor[obj_ZXObject, ___] := obj (* fallback *)

theExpression[g_Graph][v:_?ZSpiderQ[k___][p_]] := With[
  { in = incomingEdges[g][v],
    out = outgoingEdges[g][v] },
  ZXMultiply[Ket[out -> 0], Bra[in -> 0]] +
    ZXMultiply[Ket[out -> 1], Bra[in -> 1]] * Exp[I*p]
 ]

theExpression[g_Graph][v:_?XSpiderQ[k___][p_]] := With[
  { in = incomingEdges[g][v],
    out = outgoingEdges[g][v] },
  ZXMultiply[Ket[ out -> 2], Bra[in -> 2]] +
    ZXMultiply[Ket[ out -> 3], Bra[in -> 3]] * Exp[I*p]
 ]

theExpression[g_Graph][v_?ZXHadamardQ] := With[
  { in = incomingEdges[g][v],
    out = outgoingEdges[g][v] },
  ZXMultiply[Ket[out -> 2], Bra[in -> 0]] +
    ZXMultiply[Ket[out -> 3], Bra[in -> 1]]
 ]

theExpression[g_Graph][v_?ZXDiamondQ] = Sqrt[2]

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

(**** </ExpressionFor> ****)


(**** <Matrix> ****)

(* 2023-01-26: This method is slow. *)
(*
ZXObject /:
Matrix[obj_ZXObject, ___] := Matrix @ ExpressionFor[obj]
 *)

ZXObject /:
Matrix[obj:ZXObject[_List, _List, ___?OptionQ], ___] := Module[
  { graph = minimalGraph[obj],
    clusters },
  clusters = Subgraph[graph, #]& /@ WeaklyConnectedComponents[graph];
  ( CircleTimes @@ Map[theMatrixByCluster, clusters] ) *
    Power[Sqrt[2], Length @ ZXDiamonds @ obj]
 ]


theMatrixByCluster[graph_Graph] := Module[
  { in = ZXInputs[graph],
    out = ZXOutputs[graph],
    ops = Flatten @ ZXLayers[graph],
    dic },

  dic = FoldList[SymmetricDifference, in,
    Union @@@ Map[theInOutEdges[graph], ops]];
  dic = Reverse @ Riffle[dic, ops];
  Dot @@ Successive[theMatrix[graph], dic, 3, 2]
 ]


theH[obs_, oo_, ii_, ibs_] := Module[
  { cc = Intersection[obs, ibs],
    aa, bb, pp },
  pp = Pattern[#, Blank[]] & /@ Symbol /@
    (StringJoin["x", #] & /@ ToString /@ Range[Length @ cc]);
  aa = Thread[FirstPosition[obs, #] & /@ cc -> pp];
  bb = Thread[FirstPosition[ibs, #] & /@ cc -> pp];
  ArrayReshape[
    SparseArray[
      { Join[
          ReplacePart[Table[1, Length@obs], aa],
          ReplacePart[Table[1, Length@ibs], bb]
         ] -> 1,
        Join[
          ReplacePart[Table[1, Length@obs], aa],
          ReplacePart[Table[2, Length@ibs], bb]
         ] -> 1,
        Join[
          ReplacePart[Table[2, Length@obs], aa],
          ReplacePart[Table[1, Length@ibs], bb]
         ] -> 1,
        Join[
          ReplacePart[Table[2, Length@obs], aa],
          ReplacePart[Table[2, Length@ibs], bb]
         ] -> -1
       },
      Table[2, Length@obs + Length@ibs]
     ] / Sqrt[2],
    Power[2, {Length@obs, Length@ibs}]
   ]
 ]

theZ[obs_, oo_, phi_, ii_, ibs_] := Module[
  { cc = Intersection[obs, ibs],
    aa, bb, pp },
  pp = Pattern[#, Blank[]] & /@ Symbol /@
    (StringJoin["x", #] & /@ ToString /@ Range[Length @ cc]);
  aa = Thread[FirstPosition[obs, #] & /@ cc -> pp];
  bb = Thread[FirstPosition[ibs, #] & /@ cc -> pp];
  ArrayReshape[
    SparseArray[
      Join[
        ReplacePart[Table[1, Length@obs], aa],
        ReplacePart[Table[1, Length@ibs], bb]
       ] -> 1,
      Table[2, Length@obs + Length@ibs]
     ] + SparseArray[
       Join[
         ReplacePart[Table[2, Length@obs], aa],
         ReplacePart[Table[2, Length@ibs], bb]
        ] -> 1,
       Table[2, Length@obs + Length@ibs]
      ] * Exp[I*phi],
    Power[2, {Length@obs, Length@ibs}]
   ]
 ]

theX[obs_, oo_, phi_, ii_, ibs_] := Dot[
  ThePauli @@
    ReplacePart[Table[0, Length@obs], FirstPosition[obs, #] & /@ oo -> 6],
  theZ[obs, oo, phi, ii, ibs],
  ThePauli @@
    ReplacePart[Table[0, Length@ibs], FirstPosition[ibs, #] & /@ ii -> 6]
 ]


theMatrix[g_Graph][obs_, v_?ZSpiderQ, ibs_] := With[
  { in = incomingEdges[g][v],
    out = outgoingEdges[g][v] },
  theZ[obs, out, PhaseValue[v], in, ibs]
 ]

theMatrix[g_Graph][obs_, v_?XSpiderQ, ibs_] := With[
  { in = incomingEdges[g][v],
    out = outgoingEdges[g][v] },
  theX[obs, out, PhaseValue[v], in, ibs]
 ]


theMatrix[g_Graph][obs_, v_?ZXHadamardQ, ibs_] := With[
  { in = First @ incomingEdges[g][v],
    out = First @ outgoingEdges[g][v] },
  theH[obs, out, in, ibs]
 ]

theMatrix[g_Graph][_, v_?ZXDiamondQ, _] = {{Sqrt[2]}}

theMatrix[g_Graph][obs_, v_, ibs_] := With[
  { in = incomingEdges[g][v],
    out = outgoingEdges[g][v] },
  Which[
    in == {}, (* input vertex *)
    theZ[obs, out, 0, {v}, ibs],
    out == {}, (* output vertex *)
    theZ[obs, {v}, 0, in, ibs],
    True,
    theZ[obs, out, 0, in, ibs]
   ]
 ]

(**** </Matrix> ****)


(**** <ZXTimes> ****)
ZXTimes::nomtch = "There is a mismatch between two index sets `` and ``."

SetAttributes[ZXTimes, {Flat, OneIdentity}];

(* NOTE: TensorProduct is rather slow; otherwise, using TensorProduct and
   TensorContract may make this code simpler . *)
ZXTimes[
  {a_?VectorQ, A_?MatrixQ, b_?VectorQ},
  {c_?VectorQ, B_?MatrixQ, d_?VectorQ} ] := Module[
    { AA, BB, bb, bc, cc, pb, pc, d1, d2, d3, d4 },
    {d1, d2} = Dimensions[A];
    {d3, d4} = Dimensions[B];

    bc = Intersection[b, c];
    bb = Complement[b, c];
    cc = Complement[c, b];

    pb = Join[ {1},
      1 + PermutationList[FindPermutation[b, Join[bb, bc]], Length[b]] ];
    pc = PermutationList[FindPermutation[c, Join[bc, cc]], Length[c]+1];

    AA = ArrayReshape[
      Transpose[ArrayReshape[A, Join[{d1}, Table[2, Length @ b]]], pb],
      {d1, Power[2, Length @ bb], Power[2, Length @ bc]}
     ];
    BB = ArrayReshape[
      Transpose[ArrayReshape[B, Join[Table[2, Length @ c], {d4}]], pc],
      {Power[2, Length @ bc], Power[2, Length @ cc], d4}
     ];
    { Join[a, cc],
      ArrayReshape[ Transpose[AA . BB, 2 <-> 3],
        {d1 * Power[2, Length @ cc], d4 * Power[2, Length @ bb]}],
      Join[bb, d] }
   ]

(**** </ZXTimes> ****)


(**** <ZXMultiply> ****)

ZXMultiply::usage = "ZXMultiply[a,b,\[Ellipsis]] represents the non-commutative multiplication of operators a, b, \[Ellipsis] for ZX expressions."

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


ZXMultiply[] = 1

ZXMultiply[Ket[], Bra[]] = 1


ZXMultiply[pre___, z_?CommutativeQ, post___] := z * ZXMultiply[pre, post]

ZXMultiply[pre___, z_?CommutativeQ op_, post___] :=
  z * ZXMultiply[pre, op, post]

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


(**** <ZXLayers> ****)

ZXLayers::usage = "ZXLayers[graph] returns the list of layers.\nSee also LayeredGraphPlot, LayeredGraphPlot3D."

ZXLayers[graph_Graph] := <||> /; VertexList[graph] <= 1

ZXLayers[graph_Graph] := Module[
  { vv = VertexList @ graph,
    in, layers },
  in = Select[vv, VertexInDegree[graph, #] == 0&];
  layers = Sort /@ DeleteCases[
    NestList[VertexOutComponent[graph, #, {1}]&, in, Length @ vv],
    {}
   ];
  DeleteCases[
    Reverse @ FoldPairList[
      {Complement[#2, #1], Union[#2, #1]}&,
      {}, Reverse @ layers ],
    {} ]
 ]

ZXLayers[obj_ZXObject] := With[
  { graph = minimalGraph @ obj },
  Map[ Graph[#, ImageSize -> 75] -> ZXLayers[#] &,
    Map[Subgraph[graph, #]&, WeaklyConnectedComponents @ graph] ]
 ]

incomingEdges::usage = "incomingEdges[g][v] returns the list of edges incoming to vertex v in graph g."

incomingEdges[obj_ZXObject][v_] := incomingEdges[minimalGraph @ obj][v]

incomingEdges[g_Graph][v_] := incomingEdges[g][{v}]

incomingEdges[g_Graph][vv_List] := Sort @ Map[EdgeIndex[g, #]&] @
  EdgeList[g, DirectedEdge[_, Alternatives @@ vv]]


outgoingEdges::usage = "outgoingEdges[g][v] returns the list of edges outgoing from vertex v in graph g."

outgoingEdges[obj_ZXObject][v_] := outgoingEdges[minimalGraph @ obj][v]

outgoingEdges[g_Graph][v_] := outgoingEdges[g][{v}]

outgoingEdges[g_Graph][vv_List] := Sort @ Map[EdgeIndex[g, #]&] @
  EdgeList[g, DirectedEdge[Alternatives @@ vv, _]]


theInOutEdges::usage = "theInOutEdges[...] ..."

theInOutEdges[g_Graph][v_?ZXSpeciesQ] :=
  {outgoingEdges[g][v], incomingEdges[g][v]}

theInOutEdges[g_Graph][v_] :=
  {outgoingEdges[g][v], {v}} /;
  VertexInDegree[g, v] == 0 (* input *)

theInOutEdges[g_Graph][v_] :=
  {{v}, incomingEdges[g][v]} /;
  VertexOutDegree[g, v] == 0 (* output *)

(**** </ZXLayers> ****)


(**** <ZXForm> ****)

ZXForm::usage = "ZXForm[qc] converts quantum circuit qc to a ZXObject.\nNote that it only supports gates acting on up to two qubits."

ZXForm[QuantumCircuit[spec___, opts___?OptionQ], more___?OptionQ] := Module[
  { aa = Qubits @ {spec},
    vv, ee },
  aa = AssociationThread[aa -> Range[Length @ aa]];
  vv = MapIndexed[zxcGate[aa], Flatten @ {spec}];
  ee = Flatten @ ReplaceAll[ vv,
    { Rule -> List,
      _?ZXDiamondQ -> Nothing,
      v_?ZXSpiderQ :> Base[v] } ];
  ee = KeyValueMap[
    Chain[$i @ #1, Sequence @@ #2, $o @ #1]&,
    KeyTake[GroupBy[ee, First @* Base], Values @ aa]
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

zxcGate[ss_Association][CZ[{a_?QubitQ}, {b_?QubitQ}], {t_Integer}] := 
  { Chain[
      $Z[theSpacetime[ss][a, t]][0],
      $H[Unique[], t],
      $Z[theSpacetime[ss][b, t]][0] ],
    $B[t] }

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
