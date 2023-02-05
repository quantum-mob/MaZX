(* -*- mode:math -*- *)
BeginPackage["MaZX`", {"Q3`"}]

`ZXCafe`$Version = StringJoin[
  $Input, " v",
  StringSplit["$Revision: 1.2 $"][[2]], " (",
  StringSplit["$Date: 2023-02-05 10:26:20+09 $"][[2]], ") ",
  "Mahn-Soo Choi"
 ];


{ ZXStandardForm };


Begin["`Private`"]

(**** <ZXStandardForm> ****)

ZXStandardForm::usage = "ZXStandardForm[obj] convert ZX diagram obj into the standard form."

ZXStandardForm[ZXObject[vv_List, ee_List, opts___?OptionQ],
  more___?OptionQ] := Module[
    { zx, ff, bb, gg },
    zx = Cases[ ee,
      HoldPattern[_?ZSpiderQ -> _?ZSpiderQ] |
      HoldPattern[_?XSpiderQ -> _?XSpiderQ]
     ];
    gg = WeaklyConnectedGraphComponents @ Graph[zx];
    rr = Flatten[zxsfMergeRules /@ VertexList /@ gg];
    {ff, bb} = Transpose @ zxsfTrimEdges[Complement[ee, zx] /. rr];

    bb = Total[bb];
    If[ bb > 1,
      Print["The result needs to be divied by ", Power[Sqrt[2], bb], "."]
     ];
    
    ZXObject[vv /. rr, Flatten @ ff, more, opts] 
   ]

(**** </ZXStandardForm> ****)


zxsfMergeRules[ss:{__?ZXSpiderQ}] := With[
  { phi = Total[PhaseValue /@ ss] },
  Thread[ ss -> Base[First @ ss][Total[PhaseValue /@ ss]] ]
 ]

zxsfTrimEdges[ee:{___Rule}] := KeyValueMap[
  Which[
    #2 == 1, {#1, 0},
    OddQ[#2], {#1, #2-1},
    True, {{}, #2}
   ]&,
  Counts[ee]
 ]


End[]

EndPackage[]
