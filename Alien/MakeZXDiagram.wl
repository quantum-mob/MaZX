(* -*- mode:math -*- *)
(* Mahn-Soo Choi *)
(* $Date: 2023-01-24 07:43:02+09 $ *)
(* $Revision: 1.1 $ *)

GenerateZXGraph[zxExpression_] := 
 Module[{zSpiders, xSpiders, hadamardGates, diamonds, wires, graph, 
   vertexSizes, emptyVertices},
  zSpiders = 
   First /@ 
    Cases[{zxExpression}, _\[FormalCapitalZ], Infinity, Heads -> True];
  xSpiders = 
   First /@ 
    Cases[{zxExpression}, _\[FormalCapitalX], Infinity, Heads -> True];
  hadamardGates = 
   First /@ 
    Cases[{zxExpression}, _\[FormalCapitalH], Infinity, Heads -> True];
  diamonds = 
   First /@ 
    Cases[{zxExpression}, _\[FormalCapitalB], Infinity, Heads -> True];
  wires = 
   Cases[{zxExpression}, _\[FormalCapitalW], Infinity, 
     Heads -> True] /. (\[FormalCapitalW] -> DirectedEdge);
  graph = 
   Graph[Join[zSpiders, xSpiders, hadamardGates, diamonds], wires];
  vertexSizes = (# -> 
       If[MemberQ[AdjacencyList[graph, #], #], 0.01, 0.4]) & /@ 
    VertexList[graph];
  emptyVertices = 
   Complement[VertexList[graph], 
    Join[zSpiders, xSpiders, hadamardGates, diamonds]];
  Graph[Join[zSpiders, xSpiders, hadamardGates, diamonds], wires, 
   VertexShapeFunction -> 
    Join[Thread[diamonds -> "Diamond"], 
     Thread[hadamardGates -> "Square"]], 
   VertexStyle -> 
    Join[Thread[zSpiders -> Green], Thread[xSpiders -> Red], 
     Thread[hadamardGates -> Yellow], Thread[diamonds -> Black], 
     Thread[emptyVertices -> Directive[Transparent, EdgeForm[]]]], 
   VertexSize -> vertexSizes, 
   GraphLayout -> "SpringElectricalEmbedding", 
   EdgeShapeFunction -> 
    GraphElementData["ShortFilledArrow", "ArrowSize" -> 0.05]]]

GenerateZXGraphWithPhases[zxExpression_] := 
 Module[{zSpiders, zSpiderPhases, xSpiders, xSpiderPhases, 
   hadamardGates, diamonds, wires, graph, vertexSizes, 
   emptyVertices},
  zSpiders = 
   First /@ 
    Cases[{zxExpression}, _\[FormalCapitalZ], Infinity, Heads -> True];
  zSpiderPhases = 
   Last /@ Cases[{zxExpression}, _\[FormalCapitalZ], Infinity, 
     Heads -> True];
  xSpiders = 
   First /@ 
    Cases[{zxExpression}, _\[FormalCapitalX], Infinity, Heads -> True];
  xSpiderPhases = 
   Last /@ Cases[{zxExpression}, _\[FormalCapitalX], Infinity, 
     Heads -> True];
  hadamardGates = 
   First /@ 
    Cases[{zxExpression}, _\[FormalCapitalH], Infinity, Heads -> True];
  diamonds = 
   First /@ 
    Cases[{zxExpression}, _\[FormalCapitalB], Infinity, Heads -> True];
  wires = 
   Cases[{zxExpression}, _\[FormalCapitalW], Infinity, 
     Heads -> True] /. (\[FormalCapitalW] -> DirectedEdge);
  graph = 
   Graph[Join[zSpiders, xSpiders, hadamardGates, diamonds], wires];
  vertexSizes = (# -> 
       If[MemberQ[AdjacencyList[graph, #], #], 0.01, 0.4]) & /@ 
    VertexList[graph];
  emptyVertices = 
   Complement[VertexList[graph], 
    Join[zSpiders, xSpiders, hadamardGates, diamonds]];
  Graph[Join[zSpiders, xSpiders, hadamardGates, diamonds], wires, 
   VertexShapeFunction -> 
    Join[Thread[diamonds -> "Diamond"], 
     Thread[hadamardGates -> "Square"]], 
   VertexStyle -> 
    Join[Thread[zSpiders -> Green], Thread[xSpiders -> Red], 
     Thread[hadamardGates -> Yellow], Thread[diamonds -> Black], 
     Thread[emptyVertices -> Directive[Transparent, EdgeForm[]]]], 
   VertexLabels -> 
    Join[(First[#] -> Placed[Last[#], Center] & /@ 
       Thread[zSpiders -> zSpiderPhases]), (First[#] -> 
         Placed[Last[#], Center] & /@ 
       Thread[xSpiders -> xSpiderPhases]), (# -> 
         Placed["H", Center]) & /@ hadamardGates], 
   VertexSize -> vertexSizes, VertexLabelStyle -> Bold, 
   GraphLayout -> "SpringElectricalEmbedding", 
   EdgeShapeFunction -> 
    GraphElementData["ShortFilledArrow", "ArrowSize" -> 0.05]]]

zSpiderToMatrix[inputArity_Integer, outputArity_Integer, phase_] := 
 SparseArray[{{1, 1} -> 
    1, {Power[2, outputArity], Power[2, inputArity]} -> 
    Exp[I*phase]}]

xSpiderToMatrix[inputArity_Integer, outputArity_Integer, phase_] := 
 Module[{antecedent, subsequent, hadamardGate},
  hadamardGate = (1/Sqrt[2])*{{1, 1}, {1, -1}};
  antecedent = 
   Which[inputArity == 0, IdentityMatrix[Power[2, inputArity]], 
    inputArity == 1, hadamardGate, inputArity >= 2, 
    Nest[KroneckerProduct[#, hadamardGate] &, 
     KroneckerProduct[hadamardGate, hadamardGate], inputArity - 2]];
  subsequent = 
   Which[outputArity == 0, IdentityMatrix[Power[2, outputArity]], 
    outputArity == 1, hadamardGate, outputArity >= 2, 
    Nest[KroneckerProduct[#, hadamardGate] &, 
     KroneckerProduct[hadamardGate, hadamardGate], outputArity - 2]];
  antecedent . 
   SparseArray[{{1, 1} -> 
      1, {Power[2, inputArity], Power[2, outputArity]} -> 
      Exp[I*phase]}] . subsequent]

findLayers[graph_Graph] := Module[{initialLayer, layers},
  initialLayer = 
   Select[VertexList[graph], VertexInDegree[graph, #] == 0 &];
  If[Length[VertexList[graph]] > 1,
   layers = 
    Association[
     Prepend[(# -> 
          Nest[DeleteDuplicates[
             Catenate[
              Module[{vertex = #}, 
                 VertexOutComponent[graph, vertex, {1}]] & /@ #]] &, 
           initialLayer, #]) & /@ Range[Length[VertexList[graph]]], 
      0 -> initialLayer]];
   layers = 
    Delete[layers, 
     Position[Values[layers], i_ /; Length[i] == 0, {1}, 
      Heads -> False]];
   layers = 
    Complement[#, 
       Flatten[Join[
         Drop[Values[layers], 
          Delete[Flatten[Position[Values[layers], #]], 0]]]]] & /@ 
     layers, <||>]]

combineLayer[layer_List, matrixConversions_Association] := 
 Module[{generators},
  generators = 
   matrixConversions[#] & /@ 
    Intersection[layer, Keys[matrixConversions]];
  Which[Length[generators] == 1, SparseArray[generators[[1]]], 
   Length[generators] > 1, KroneckerProduct[Delete[generators, 0]], 
   Length[generators] == 0, 1]]

combineSubgraph[subgraph_Graph, matrixConversions_Association] := 
 Module[{layerMatrices, combinedSpiders, spidersToCombine, Ket0, Ket1,
    Bra0, Bra1},
  layerMatrices = 
   DeleteCases[
    combineLayer[#, matrixConversions] & /@ 
     Values[findLayers[subgraph]], _Integer];
  Ket0 = {{1}, {0}};
  Ket1 = {{0}, {1}};
  Bra0 = ConjugateTranspose[Ket0];
  Bra1 = ConjugateTranspose[Ket1];
  If[Length[layerMatrices] > 0,
   combinedSpiders = layerMatrices[[1]];
   spidersToCombine = Rest[layerMatrices];
   While[Length[spidersToCombine] > 0,
    combinedSpiders = 
     Which[Dimensions[combinedSpiders][[2]] < 
       Dimensions[spidersToCombine[[1]]][[1]], 
      SparseArray[
        PadRight[#, Dimensions[spidersToCombine[[1]]][[1]]] & /@ 
         combinedSpiders] . spidersToCombine[[1]], 
      Dimensions[combinedSpiders][[2]] > 
       Dimensions[spidersToCombine[[1]]][[1]], 
      combinedSpiders . 
       Join[spidersToCombine[[1]], 
        Table[Table[0, 
          Dimensions[spidersToCombine[[1]]][[
           2]]], (Dimensions[combinedSpiders][[2]] - 
           Dimensions[spidersToCombine[[1]]][[1]])]], 
      Dimensions[combinedSpiders][[2]] == 
       Dimensions[spidersToCombine[[1]]][[1]], 
      combinedSpiders . spidersToCombine[[1]], True, 
      combinedSpiders];
    spidersToCombine = Rest[spidersToCombine]],
   combinedSpiders = 
    Which[StringMatchQ[ToString[VertexList[subgraph][[1]]], 
      "d" ~~ __], {Sqrt[2]}, 
     StringMatchQ[ToString[EdgeList[subgraph][[1, 1]]], "i" ~~ __] && 
      StringMatchQ[ToString[EdgeList[subgraph][[1, 2]]], "i" ~~ __] &&
       EdgeList[subgraph][[1, 1]] =!= EdgeList[subgraph][[1, 2]], 
     KroneckerProduct[Bra0, Bra0] + KroneckerProduct[Bra1, Bra1], 
     StringMatchQ[ToString[EdgeList[subgraph][[1, 1]]], "o" ~~ __] && 
      StringMatchQ[ToString[EdgeList[subgraph][[1, 2]]], "o" ~~ __] &&
       EdgeList[subgraph][[1, 1]] =!= EdgeList[subgraph][[0, 1]], 
     KroneckerProduct[Ket0, Ket0] + KroneckerProduct[Ket1, Ket1], 
     EdgeList[subgraph][[1, 1]] == EdgeList[subgraph][[1, 2]], {2}, 
     True, {1}]];
  SparseArray[combinedSpiders]]

ConvertToMatrix[zxExpression_] := 
 Module[{hadamardGate, subgraphs, generatorsToMatrices, 
   convertedSubgraphs},
  hadamardGate = (1/Sqrt[2])*{{1, 1}, {1, -1}};
  subgraphs = 
   WeaklyConnectedGraphComponents[GenerateZXGraph[zxExpression]];
  generatorsToMatrices = 
   Association[
    Select[Flatten[zxExpression] /. CircleTimes -> List, 
      MemberQ[{\[FormalCapitalX], \[FormalCapitalZ], \
\[FormalCapitalH], \[FormalCapitalB]}, 
        Head[#]] &] /. {\[FormalCapitalX][xspider_, 
        inputArity_Integer, outputArity_Integer, 
        phase_] :> (xspider -> 
         xSpiderToMatrix[inputArity, outputArity, 
          phase]), \[FormalCapitalZ][zspider_, inputArity_Integer, 
        outputArity_Integer, 
        phase_] :> (zspider -> 
         zSpiderToMatrix[inputArity, outputArity, 
          phase]), \[FormalCapitalH][
        h_] :> (h -> hadamardGate), \[FormalCapitalB][
        d_] :> (d -> Sqrt[2])}];
  convertedSubgraphs = 
   combineSubgraph[subgraphs[[#]], generatorsToMatrices] & /@ 
    Range[Length[subgraphs]];
  If[Length[convertedSubgraphs] > 1, 
   KroneckerProduct[Delete[convertedSubgraphs, 0]], 
   convertedSubgraphs[[1]]]]

ZXDiagramObject /: 
 MakeBoxes[diagram : ZXDiagramObject[zxExpression_], format_] := 
 Module[{icon, zSpiderCount, xSpiderCount, hadamardGateCount, 
   diamondCount, wireCount},
  zSpiderCount = 
   Length[First /@ 
     Cases[{zxExpression}, _\[FormalCapitalZ], Infinity, 
      Heads -> True]];
  xSpiderCount = 
   Length[First /@ 
     Cases[{zxExpression}, _\[FormalCapitalX], Infinity, 
      Heads -> True]];
  hadamardGateCount = 
   Length[First /@ 
     Cases[{zxExpression}, _\[FormalCapitalH], Infinity, 
      Heads -> True]];
  diamondCount = 
   Length[First /@ 
     Cases[{zxExpression}, _\[FormalCapitalB], Infinity, 
      Heads -> True]];
  wireCount = 
   Length[Cases[{zxExpression}, _\[FormalCapitalW], Infinity, 
     Heads -> True]];
  BoxForm`ArrangeSummaryBox["ZXDiagramObject", diagram, 
   GenerateZXGraph[
    zxExpression], {{BoxForm`SummaryItem[{"Z Spiders: ", 
       zSpiderCount}]}, {BoxForm`SummaryItem[{"X Spiders: ", 
       xSpiderCount}]}, {BoxForm`SummaryItem[{"Hadamard Gates: ", 
       hadamardGateCount}]}}, {{BoxForm`SummaryItem[{"Diamonds: ", 
       diamondCount}]}, {BoxForm`SummaryItem[{"Wires: ", 
       wireCount}]}}, format, "Interpretable" -> Automatic]]

ZXDiagramObject[zxExpression_]["OperatorForm"] := zxExpression

ZXDiagramObject[zxExpression_]["ListForm"] := 
 Flatten[zxExpression] /. CircleTimes -> List

ZXDiagramObject[zxExpression_]["MatrixForm"] := 
 Normal[ConvertToMatrix[zxExpression]]

ZXDiagramObject[zxExpression_]["ZSpiders"] := 
 Cases[{zxExpression}, _\[FormalCapitalZ], Infinity, Heads -> True]

ZXDiagramObject[zxExpression_]["ZSpiderCount"] := 
 Length[Cases[{zxExpression}, _\[FormalCapitalZ], Infinity, 
   Heads -> True]]

ZXDiagramObject[zxExpression_]["XSpiders"] := 
 Cases[{zxExpression}, _\[FormalCapitalX], Infinity, Heads -> True]

ZXDiagramObject[zxExpression_]["XSpiderCount"] := 
 Length[Cases[{zxExpression}, _\[FormalCapitalX], Infinity, 
   Heads -> True]]

ZXDiagramObject[zxExpression_]["HadamardGates"] := 
 Cases[{zxExpression}, _\[FormalCapitalH], Infinity, Heads -> True]

ZXDiagramObject[zxExpression_]["HadamardGateCount"] := 
 Length[Cases[{zxExpression}, _\[FormalCapitalH], Infinity, 
   Heads -> True]]

ZXDiagramObject[zxExpression_]["Diamonds"] := 
 Cases[{zxExpression}, _\[FormalCapitalB], Infinity, Heads -> True]

ZXDiagramObject[zxExpression_]["DiamondCount"] := 
 Length[Cases[{zxExpression}, _\[FormalCapitalB], Infinity, 
   Heads -> True]]

ZXDiagramObject[zxExpression_]["Wires"] := 
 Cases[{zxExpression}, _\[FormalCapitalW], Infinity, Heads -> True]

ZXDiagramObject[zxExpression_]["WireCount"] := 
 Cases[{zxExpression}, _\[FormalCapitalW], Infinity, Heads -> True]

ZXDiagramObject[zxExpression_]["UnlabeledGraph"] := 
 GenerateZXGraph[zxExpression]

ZXDiagramObject[zxExpression_]["LabeledGraph"] := 
 GenerateZXGraphWithPhases[zxExpression]

ZXDiagramObject[zxExpression_]["Properties"] := {"OperatorForm", 
  "ListForm", "MatrixForm", "ZSpiders", "ZSpiderCount", "XSpiders", 
  "XSpiderCount", "HadamardGates", "HadamardGateCount", "Diamonds", 
  "DiamondCount", "Wires", "WireCount", "UnlabeledGraph", 
  "LabeledGraph"}

MakeZXDiagram[components_List] := Module[{oldComponents, operatorForm},
  oldComponents = components;
  operatorForm = oldComponents;
  While[Length[oldComponents] > 1,
   operatorForm = 
    operatorForm /. 
     oldComponents -> (CircleTimes[First[oldComponents], 
        Rest[oldComponents]]);
   oldComponents = Rest[oldComponents]];
  operatorForm = (operatorForm /. 
     oldComponents -> First[oldComponents]);
  ZXDiagramObject[operatorForm]]

MakeZXDiagram[<|"Diamonds" -> diamondCount_Integer, 
   "HadamardGates" -> hadamardGateCount_Integer, 
   "Wires" -> wires_List, "XSpiders" -> xSpiders_List, 
   "ZSpiders" -> zSpiders_List|>] := 
 Module[{validWires, zSpiderInputArities, zSpiderOutputArities, 
    zSpiderPhases, zSpiderOperators, xSpiderInputArities, 
    xSpiderOutputArities, xSpiderPhases, xSpiderOperators, 
    hadamardGateOperators, diamondOperators, wireOperators},
   validWires = False;
   If[wires =!= {}, 
    If[Length[Transpose[wires]] == 2, validWires = True]];
   If[wires === {}, validWires = True];
   If[validWires,
    zSpiderInputArities = zSpiders[[1]];
    zSpiderOutputArities = zSpiders[[2]];
    zSpiderPhases = zSpiders[[3]];
    zSpiderOperators = 
     Table[\[FormalCapitalZ][ToExpression["z" <> ToString[i]], 
       zSpiderInputArities[[i]], zSpiderOutputArities[[i]], 
       Which[NumberQ[zSpiderPhases[[i]]], 
        Mod[zSpiderPhases[[i]], 2*Pi], True, zSpiderPhases[[i]]]], {i,
        Length[zSpiderInputArities]}];
    xSpiderInputArities = xSpiders[[1]];
    xSpiderOutputArities = xSpiders[[2]];
    xSpiderPhases = xSpiders[[3]];
    xSpiderOperators = 
     Table[\[FormalCapitalX][ToExpression["x" <> ToString[i]], 
       xSpiderInputArities[[i]], xSpiderOutputArities[[i]], 
       Which[NumberQ[xSpiderPhases[[i]]], 
        Mod[xSpiderPhases[[i]], 2*Pi], True, xSpiderPhases[[i]]]], {i,
        Length[xSpiderInputArities]}];
    hadamardGateOperators = 
     Table[\[FormalCapitalH][ToExpression["h" <> ToString[i]]], {i, 
       hadamardGateCount}];
    diamondOperators = 
     Table[\[FormalCapitalB][ToExpression["d" <> ToString[i]]], {i, 
       diamondCount}];
    wireOperators = 
     Table[\[FormalCapitalW][First[wires[[i]]], Last[wires[[i]]]], {i,
        Length[wires]}];
    MakeZXDiagram[
     Join[zSpiderOperators, xSpiderOperators, hadamardGateOperators, 
      diamondOperators, wireOperators]]]] /; (Length[zSpiders[[1]]] ==
      Length[zSpiders[[2]]] && 
    Length[zSpiders[[2]]] == Length[zSpiders[[3]]] && 
    Length[xSpiders[[1]]] == Length[xSpiders[[2]]] && 
    Length[xSpiders[[2]]] == Length[xSpiders[[3]]])

MakeZXDiagram[assoc_Association, rest___] := 
 MakeZXDiagram[
   KeySortBy[assoc, 
    Position[{"Diamonds", "HadamardGates", "Wires", "XSpiders", 
       "ZSpiders"}, #] &], 
   rest] /; (Keys[assoc] != {"Diamonds", "HadamardGates", "Wires", 
      "XSpiders", "ZSpiders"} && 
    Sort[Keys[assoc]] == {"Diamonds", "HadamardGates", "Wires", 
      "XSpiders", "ZSpiders"})
