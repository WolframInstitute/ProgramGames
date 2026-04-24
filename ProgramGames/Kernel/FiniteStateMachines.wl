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
returning unique representatives and groups. Options: \"Depth\", \"Sample\", \"Seed\", \"FullStatesOnly\", \"GPU\". \
\"FullStatesOnly\" -> True (default) drops FSMs whose BFS from the start state doesn't reach all s states, \
matching the WL notebook's VertexCount[FSMGraph[#]] == s filter. \
\"GPU\" -> True (default) runs Phases 1 and 2 on Metal when available, falling back to CPU on any error \
or when sampling (only exhaustive enumeration uses the GPU path).";


(* ::Section::Closed:: *)
(*FiniteStateMachineMaxIndex*)


FiniteStateMachineMaxIndex[s_Integer, k_Integer] := s^(s k) k^s


(* ::Section::Closed:: *)
(*FiniteStateMachineClassify*)


Options[FiniteStateMachineClassify] = {
	"Depth" -> 12,
	"Sample" -> Automatic,
	"Seed" -> 42,
	"FullStatesOnly" -> True,
	"GPU" -> True
};

FiniteStateMachineClassify[s_Integer, k_Integer, opts : OptionsPattern[]] :=
	Module[{depth, sample, totalFSMs, sampleCount, fullStatesOnly, useGpu, resultJSON, result},
		depth = OptionValue["Depth"];
		sample = OptionValue["Sample"];
		fullStatesOnly = If[TrueQ[OptionValue["FullStatesOnly"]], 1, 0];
		useGpu = If[TrueQ[OptionValue["GPU"]], 1, 0];
		totalFSMs = FiniteStateMachineMaxIndex[s, k];
		sampleCount = Replace[sample, {
			Automatic :> If[totalFSMs > 10000000, 100000, 0],
			n_Integer :> n,
			"all" :> 0
		}];
		resultJSON = FSMClassifyWL[s, k, depth, sampleCount, OptionValue["Seed"], fullStatesOnly, useGpu];
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
