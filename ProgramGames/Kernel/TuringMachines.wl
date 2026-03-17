(* ::Package:: *)

(* ::Section::Closed:: *)
(*PackageExported*)


PackageExported[
	{
		TuringMachineProgramSearch,
		TuringMachineClassify
	}
];


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
	"Sample" -> Automatic
};

TuringMachineProgramSearch[s_Integer, k_Integer, opts : OptionsPattern[]] :=
	Module[{maxsteps, depth, sample, maxIdx, mode, args, stdout, lines},
		maxsteps = OptionValue["MaxSteps"];
		depth = OptionValue["Depth"];
		sample = OptionValue["Sample"];
		maxIdx = TuringMachineMaxIndex[s, k];
		mode = Replace[sample, {
			Automatic :> If[maxIdx + 1 > 10000000, 100000, "all"],
			n_Integer :> n,
			"all" :> "all"
		}];
		args = {"search",
			"--states", ToString[s], "--symbols", ToString[k],
			"--max-steps", ToString[maxsteps], "--depth", ToString[depth],
			"--format", "ids", "--gpu"};
		If[IntegerQ[mode],
			AppendTo[args, "--sample"]; AppendTo[args, ToString[mode]]
		];
		stdout = iRunBinary[args];
		If[stdout === $Failed, Return[{}]];
		lines = StringSplit[stdout, "\n"];
		ToExpression /@ Select[lines, StringMatchQ[#, DigitCharacter ..] &]
	]


(* ::Section::Closed:: *)
(*TuringMachineClassify*)


Options[TuringMachineClassify] = {
	"MaxSteps" -> 500,
	"Depth" -> 4,
	"Sample" -> Automatic
};

TuringMachineClassify[s_Integer, k_Integer, opts : OptionsPattern[]] :=
	Module[{maxsteps, depth, ids, stdout, result},
		maxsteps = OptionValue["MaxSteps"];
		depth = OptionValue["Depth"];
		ids = TuringMachineProgramSearch[s, k,
			"MaxSteps" -> maxsteps, "Depth" -> depth,
			"Sample" -> OptionValue["Sample"]];
		If[Length[ids] == 0, Return[$Failed]];
		stdout = iRunBinary[
			{"classify",
				"--states", ToString[s], "--symbols", ToString[k],
				"--max-steps", ToString[maxsteps], "--depth", ToString[depth],
				"--gpu", "--ids-file", "-"},
			StringRiffle[ToString /@ ids, "\n"]
		];
		If[stdout === $Failed, Return[$Failed]];
		result = ImportString[stdout, "RawJSON"];
		<|
			"Representatives" -> result["representatives"],
			"Groups" -> result["groups"],
			"UniqueCount" -> result["unique_behaviors"],
			"TotalHalting" -> result["total_halting"],
			"ReductionFactor" -> result["reduction_factor"]
		|>
	]
