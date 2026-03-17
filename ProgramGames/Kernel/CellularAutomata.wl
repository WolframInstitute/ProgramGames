(* ::Package:: *)

Package["WolframInstitute`ProgramGames`"]


(* ::Section::Closed:: *)
(*Exported Symbols*)


PackageExport["CellularAutomatonClassify"]
PackageExport["CellularAutomatonMaxIndex"]


(* ::Section::Closed:: *)
(*Usage Messages*)


CellularAutomatonMaxIndex::usage = "CellularAutomatonMaxIndex[k, r] returns the total number of CA rules for k colors and radius r: k^(k^(2r+1)).";

CellularAutomatonClassify::usage = "CellularAutomatonClassify[k, r] classifies CA rules by behavioral equivalence, \
returning unique representatives and groups. Options: \"Steps\", \"Depth\", \"Sample\".";


(* ::Section::Closed:: *)
(*CellularAutomatonMaxIndex*)


CellularAutomatonMaxIndex[k_Integer, r_] := With[
	{numNeighborhoods = k^(Round[2 r] + 1)},
	k^numNeighborhoods - 1
]


(* ::Section::Closed:: *)
(*CellularAutomatonClassify*)


Options[CellularAutomatonClassify] = {
	"Steps" -> 10,
	"Depth" -> 4,
	"Sample" -> Automatic
};

CellularAutomatonClassify[k_Integer, r_, opts : OptionsPattern[]] :=
	Module[{t, depth, sample, rNum, rDen, totalRules, sampleCount, resultJSON, result},
		t = OptionValue["Steps"];
		depth = OptionValue["Depth"];
		sample = OptionValue["Sample"];
		{rNum, rDen} = If[IntegerQ[r], {r, 1}, Through[{Numerator, Denominator}[r]]];
		totalRules = CellularAutomatonMaxIndex[k, r] + 1;
		sampleCount = Replace[sample, {
			Automatic :> If[totalRules > 10000000, 100000, 0],
			n_Integer :> n,
			"all" :> 0
		}];
		resultJSON = CAClassifyWL[k, rNum, rDen, t, depth, sampleCount];
		If[FailureQ[resultJSON],
			Message[CellularAutomatonClassify::rust, ToString[resultJSON, InputForm]];
			Return[$Failed]
		];
		If[MatchQ[resultJSON, _Missing],
			Message[CellularAutomatonClassify::rust, "CA classify export missing from the loaded Rust manifest. Run ProgramGamesSetup[] to rebuild."];
			Return[$Failed]
		];
		If[!StringQ[resultJSON],
			Message[CellularAutomatonClassify::rust, "CA classify function not available. Run ProgramGamesSetup[] to rebuild."];
			Return[$Failed]
		];
		result = ImportString[resultJSON, "RawJSON"];
		<|
			"Representatives" -> result["representatives"],
			"Groups" -> result["groups"],
			"UniqueCount" -> result["unique_behaviors"],
			"TotalRules" -> result["total_rules"],
			"SampledRules" -> result["sampled_rules"],
			"ReductionFactor" -> result["reduction_factor"]
		|>
	]

CellularAutomatonClassify::rust = "`1`";
