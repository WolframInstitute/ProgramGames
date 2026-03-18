(* ::Package:: *)

Package["WolframInstitute`ProgramGames`"]


(* ::Section::Closed:: *)
(*Exported Symbols*)


PackageExport["FiniteStateMachineClassify"]
PackageExport["FiniteStateMachineMaxIndex"]


(* ::Section::Closed:: *)
(*Usage Messages*)


FiniteStateMachineMaxIndex::usage = "FiniteStateMachineMaxIndex[s, k] returns the total number of FSMs for s states and k actions: s^(s*k) * k^s.";

FiniteStateMachineClassify::usage = "FiniteStateMachineClassify[s, k] classifies FSMs by behavioral equivalence, \
returning unique representatives and groups. Options: \"Depth\", \"Sample\".";


(* ::Section::Closed:: *)
(*FiniteStateMachineMaxIndex*)


FiniteStateMachineMaxIndex[s_Integer, k_Integer] := s^(s k) k^s


(* ::Section::Closed:: *)
(*FiniteStateMachineClassify*)


Options[FiniteStateMachineClassify] = {
	"Depth" -> 4,
	"Sample" -> Automatic
};

FiniteStateMachineClassify[s_Integer, k_Integer, opts : OptionsPattern[]] :=
	Module[{depth, sample, totalFSMs, sampleCount, resultJSON, result},
		depth = OptionValue["Depth"];
		sample = OptionValue["Sample"];
		totalFSMs = FiniteStateMachineMaxIndex[s, k];
		sampleCount = Replace[sample, {
			Automatic :> If[totalFSMs > 10000000, 100000, 0],
			n_Integer :> n,
			"all" :> 0
		}];
		resultJSON = FSMClassifyWL[s, k, depth, sampleCount];
		If[FailureQ[resultJSON],
			Message[FiniteStateMachineClassify::rust, ToString[resultJSON, InputForm]];
			Return[$Failed]
		];
		If[MatchQ[resultJSON, _Missing],
			Message[FiniteStateMachineClassify::rust, "FSM classify export missing. Run ProgramGamesSetup[] to rebuild."];
			Return[$Failed]
		];
		If[!StringQ[resultJSON],
			Message[FiniteStateMachineClassify::rust, "FSM classify function not available. Run ProgramGamesSetup[] to rebuild."];
			Return[$Failed]
		];
		result = ImportString[resultJSON, "RawJSON"];
		<|
			"Representatives" -> result["representatives"],
			"Groups" -> result["groups"],
			"UniqueCount" -> result["unique_behaviors"],
			"CanonicalCount" -> result["canonical_count"],
			"TotalRules" -> result["total_rules"],
			"SampledRules" -> result["sampled_rules"],
			"ReductionFactor" -> result["reduction_factor"]
		|>
	]

FiniteStateMachineClassify::rust = "`1`";
