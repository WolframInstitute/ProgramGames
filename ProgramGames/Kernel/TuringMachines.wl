(* ::Package:: *)

Package["WolframInstitute`ProgramGames`"]


(* ::Section::Closed:: *)
(*Exported Symbols*)


PackageExport["TuringMachineProgramSearch"]
PackageExport["TuringMachineClassify"]


(* ::Section::Closed:: *)
(*Usage Messages*)


TuringMachineProgramSearch::usage = "TuringMachineProgramSearch[s, k] finds all halting TMs \
in the (s,k) space. Options: \"MaxSteps\", \"Depth\", \"Sample\".";

TuringMachineClassify::usage = "TuringMachineClassify[s, k] classifies halting TMs by \
behavior, returning unique representatives and groups.";


(* ::Section::Closed:: *)
(*TuringMachineProgramSearch*)


Options[TuringMachineProgramSearch] = {
	"MaxSteps" -> 500,
	"Depth" -> 4,
	"Sample" -> Automatic,
	"Seed" -> 42,
	"GPU" -> True
};

TuringMachineProgramSearch[s_Integer, k_Integer, opts : OptionsPattern[]] :=
	Module[{maxsteps, depth, sample, maxIdx, sampleCount, resultJSON},
		maxsteps = OptionValue["MaxSteps"];
		depth = OptionValue["Depth"];
		sample = OptionValue["Sample"];
		maxIdx = TuringMachineMaxIndex[s, k];
		sampleCount = Replace[sample, {
			Automatic :> If[maxIdx + 1 > 10000000, 100000, 0],
			n_Integer :> n,
			"all" :> 0
		}];
		resultJSON = TMSearchWL[s, k, maxsteps, depth, sampleCount,
			TrueQ[OptionValue["GPU"]], OptionValue["Seed"]];
		If[FailureQ[resultJSON], Return[{}]];
		ImportString[resultJSON, "RawJSON"]
	]


(* ::Section::Closed:: *)
(*TuringMachineClassify*)


Options[TuringMachineClassify] = {
	"MaxSteps" -> 500,
	"Depth" -> 4,
	"Sample" -> Automatic,
	"Seed" -> 42,
	"GPU" -> True
};

TuringMachineClassify[s_Integer, k_Integer, opts : OptionsPattern[]] :=
	Module[{maxsteps, depth, ids, resultJSON, result},
		maxsteps = OptionValue["MaxSteps"];
		depth = OptionValue["Depth"];
		ids = TuringMachineProgramSearch[s, k,
			"MaxSteps" -> maxsteps, "Depth" -> depth,
			"Sample" -> OptionValue["Sample"],
			"Seed" -> OptionValue["Seed"],
			"GPU" -> OptionValue["GPU"]];
		If[Length[ids] == 0, Return[$Failed]];
		resultJSON = TMClassifyWL[s, k, maxsteps, depth,
			ExportString[ids, "RawJSON"], TrueQ[OptionValue["GPU"]]];
		If[FailureQ[resultJSON], Return[$Failed]];
		result = ImportString[resultJSON, "RawJSON"];
		<|
			"Representatives" -> result["representatives"],
			"Groups" -> result["groups"],
			"UniqueCount" -> result["unique_behaviors"],
			"TotalHalting" -> result["total_halting"],
			"ReductionFactor" -> result["reduction_factor"]
		|>
	]
