(* -*- mode:math -*- *)
BeginPackage["MaZX`", {"Q3`"}]

`ZXVille`$Version = StringJoin[
  $Input, " v",
  StringSplit["$Revision: 1.10 $"][[2]], " (",
  StringSplit["$Date: 2023-02-23 08:45:52+09 $"][[2]], ") ",
  "Mahn-Soo Choi"
 ];


{ ZXForm };


Begin["`Private`"]

(**** <ZXForm> ****)

ZXForm::usage = "ZXForm[qc] converts quantum circuit qc to a ZXObject.\nNote that it only supports gates acting on up to two qubits."

ZXForm::unknown = "The current version of ZXForm does not recognize gate ``."

ZXForm[QuantumCircuit[spec___, opts___?OptionQ], more___?OptionQ] := Module[
  { aa = Qubits @ {spec},
    zx, vv, ee },
  aa = AssociationThread[aa -> Range[Length @ aa]];
  zx = FoldPairList[theZXForm[aa], 1, {spec}];

  vv = Union @ Flatten[zx /. {Rule -> List, _?ZXDiamondQ -> Nothing}];

  dd = Union @ Cases[Flatten @ zx, _?ZXDiamondQ];

  ee = SortBy[#, Last @* Base]& /@ GroupBy[vv, First @* Base];
  ee = KeyValueMap[
    Chain[$i @ #1, Sequence @@ #2, $o @ #1]&,
    KeyTake[ee, Values @ aa]
   ];
  ee = Flatten @ Join[ee, Cases[Flatten @ zx, _Rule]];
  
  vv = Union @ Join[$i @ Values @ aa, $o @ Values @ aa, vv];
  ZXObject[ Join[vv, dd], ee, more, opts,
    VertexCoordinates -> getVertexCoordinates[vv] ]
 ]

theZXForm[aa_Association][t_Integer, gate_] :=
  zxcGate[aa, t][gate] + {0, 1}

theZXForm[aa_Association][t_Integer, {spec__, ___?OptionQ}] := Module[
  { gate, time },
  {gate, time} = Transpose @ Map[zxcGate[aa, t], Flatten @ {spec}];
  {Flatten @ gate, Max[time] + 1}
 ]

getVertexCoordinates[vv_List] := With[
  { tmax = 1 + Max @ Cases[vv, v_?ZXSpeciesQ :> Last[Base @ v]] },
  Join[
    Cases[vv, $i[k_] -> $i[k] -> {0, 1-k}],
    Cases[vv, $o[k_] -> $o[k] -> {tmax, 1-k}],
    Cases[vv, v_?ZXSpeciesQ :> v -> {Last @ Base @ v, 1-First[Base @ v]}]
   ]
 ]

(**** </ZXForm> ****)

spacetime::usage = "spacetime[ss][q, t] ..."

spacetime[ss_Association][q_?QubitQ, t_Integer] :=
  Sequence[ss @ FlavorMute @ q, t]

space[ss_Association][q_?QubitQ] := ss[FlavorMute @ q]


(**** <zxcGate> ****)

zxcGate::usage = "zxcGate[aa, t][gate] returns a pair {expr, tmax} of ZX expression expr and maxitum time slice tmax."

zxcGate[ss_Association, t_Integer][q_?QubitQ[k___, +C[n_Integer]]] :=
  zxcGate[ss, t][Phase[+2*Pi*Power[2, n], q[k,3]]]

zxcGate[ss_Association, t_Integer][q_?QubitQ[k___, -C[n_Integer]]] :=
  zxcGate[ss, t][Phase[-2*Pi*Power[2, n], q[k,3]]]

zxcGate[ss_Association, t_Integer][q_?QubitQ] := With[
  { k = ss @ FlavorMute[q] },
  Switch[ FlavorLast[q],
    3, {$Z[k,t][Pi], t},
    1, {$X[k,t][Pi], t},
    2, {{$Z[k,t][-Pi/2], $X[k,t+1][Pi], $Z[k,t+2][Pi/2]}, t+2},
    6, {$H[k,t], t},
    7, {$Z[k,t][Pi/2], t},
    8, {$Z[k,t][Pi/4], t},
    9, {$Z[k,t][Pi/8], t},
    -7, {$Z[k,t][-Pi/2], t},
    -8, {$Z[k,t][-Pi/4], t},
    -9, {$Z[k,t][-Pi/8], t},
    _, zxcGate[ss, t][q]
   ]
 ]

zxcGate[ss_Association, t_Integer] @
  Phase[phi_, q_?QubitQ, ___?OptionQ] :=
  With[
    { k = ss @ FlavorMute[q] },
    Switch[ FlavorLast[q],
      3, {$Z[k,t][phi], t},
      1, {$X[k,t][phi], t},
      2, {{$Z[k,t][-Pi/2], $X[k,t+1][phi], $Z[k,t+2][Pi/2]}, t+2},
      _, zxcGate[ss, t][Phase[phi, q]]
     ]
   ]

zxcGate[ss_Association, t_Integer][CZ[{a_?QubitQ}, {b_?QubitQ}]] := 
  { { Chain[
        $Z[spacetime[ss][a, t]][0],
        $H[space[ss][a]+1/2, t],
        $Z[spacetime[ss][b, t]][0] ],
      $B[t] },
    t }

zxcGate[ss_Association, t_Integer][CX[{a_?QubitQ} -> {1}, {b_?QubitQ}]] :=
  { { $Z[spacetime[ss][a, t]][0] ->
        $X[spacetime[ss][b, t]][0], $B[t] },
    t }

zxcGate[ss_Association, t_Integer] @
  ControlledGate[{c_?QubitQ} -> {v_}, Phase[phi_, q_?QubitQ], ___?OptionQ] :=
  With[
    { p = ss @ FlavorMute[c],
      k = ss @ FlavorMute[q] },
    Switch[ FlavorLast[q],
      3,
      { { $Z[k,t+1][-phi/2], $Z[k,t+3][phi/2],
          $Z[p,t+1][phi/2] -> $X[k,t][0],
          $Z[p,t+1][phi/2] -> $X[k,t+2][0],
          If[v == 0,{$X[p,t][Pi], $X[p,t+2][Pi]}, Nothing], 
          $B[p,t], $B[k,t] },
        t+3 },
      1,
      { { $X[k,t+1][-phi/2], $X[k,t+3][phi/2],
          $Z[p,t+1][phi/2] -> $H[p+1/2,t+1/2],
          $H[p+1/2,t+1/2] -> $Z[k,t][0],
          $Z[p,t+1][phi/2] -> $H[p+1/2,t+3/2],
          $H[p+1/2,t+3/2] -> $Z[k,t+2][0],
          If[v == 0,{$X[p,t][Pi], $X[p,t+2][Pi]}, Nothing], 
          $B[p,t], $B[k,t] },
        t+3 },
      _, zxcGate[ss, t][ControlledGate[{c}->{1}, Phase[phi, q]]]
     ]
   ]

zxcGate[_Association, t_Integer][gate_] := (
  Message[ZXForm::unknown, gate];
  {{}, t}
 )

(**** </zxcGate> ****)

End[]

EndPackage[]
