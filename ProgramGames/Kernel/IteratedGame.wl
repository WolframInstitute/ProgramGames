(* ::Package:: *)

Package["WolframInstitute`ProgramGames`"]


(* ::Section::Closed:: *)
(*Exported Symbols*)


PackageExport["ProgramIteratedGame"]
PackageExport["ProgramIteratedGameCross"]
PackageExport["IteratedGameFiniteStateMachine"]
PackageExport["IteratedGameCellularAutomaton"]
PackageExport["IteratedGameTuringMachine"]


(* ::Section::Closed:: *)
(*Usage Messages*)


ProgramIteratedGame::usage = "ProgramIteratedGame[{strategy1, strategy2}] plays an iterated game between two strategies \
and returns the move history as a list of {moveA, moveB} pairs. \
Strategies: {\"TM\",id,s,k}, {\"TM\",id,s,k,maxSteps}, {\"FSM\",id,s,k}, {\"CA\",rule,k,r,t}, or any mix. \
Options: \"Rounds\" (default 100), \"InitialHistory\" (default {}), \"NumActions\" (default 2).";

IteratedGameFiniteStateMachine::usage = "IteratedGameFiniteStateMachine[ids] plays iterated games \
between all pairs of FSM strategies and returns an Association of pairwise move histories.";

IteratedGameCellularAutomaton::usage = "IteratedGameCellularAutomaton[rules] plays iterated games \
between all pairs of CA strategies and returns an Association of pairwise move histories.";

IteratedGameTuringMachine::usage = "IteratedGameTuringMachine[ids, s, k] plays iterated games \
between all pairs of TM strategies and returns an Association of pairwise move histories.";

ProgramIteratedGameCross::usage = "ProgramIteratedGameCross[leftStrategies, rightStrategies] plays \
iterated games between every left strategy and every right strategy in a single batched call, \
returning an Association with full move histories for all L\[Times]R pairs.";

ProgramIteratedGame::err = "Game failed: ``";
ProgramIteratedGameCross::err = "Game failed: ``";
IteratedGameFiniteStateMachine::err = "Game failed: ``";
IteratedGameCellularAutomaton::err = "Game failed: ``";
IteratedGameTuringMachine::err = "Game failed: ``";


(* ::Section::Closed:: *)
(*ProgramIteratedGame*)


Options[ProgramIteratedGame] = {"Rounds" -> 100, "InitialHistory" -> {}, "NumActions" -> 2};

ProgramIteratedGame[{s1_List, s2_List}, opts : OptionsPattern[]] :=
	Module[{ndjson, initJSON, resultJSON, result},
		ndjson = StringJoin[StrategyToJSON[s1], "\n", StrategyToJSON[s2]];
		initJSON = ExportString[OptionValue["InitialHistory"], "RawJSON", "Compact" -> True];
		resultJSON = IteratedGameWL[OptionValue["Rounds"], ndjson, initJSON,
			OptionValue["NumActions"]];
		If[FailureQ[resultJSON], Return[$Failed]];
		result = ImportString[resultJSON, "RawJSON"];
		If[KeyExistsQ[result, "error"],
			Message[ProgramIteratedGame::err, result["error"]]; Return[$Failed]];
		result["history"]
	]


(* ::Section::Closed:: *)
(*ProgramIteratedGameCross*)


Options[ProgramIteratedGameCross] = {"Rounds" -> 100, "InitialHistory" -> {}, "NumActions" -> 2};

ProgramIteratedGameCross[leftSpecs_List, rightSpecs_List, opts : OptionsPattern[]] :=
	Module[{leftNdjson, rightNdjson, initJSON, resultJSON, result},
		leftNdjson = StringRiffle[StrategyToJSON /@ leftSpecs, "\n"];
		rightNdjson = StringRiffle[StrategyToJSON /@ rightSpecs, "\n"];
		initJSON = ExportString[OptionValue["InitialHistory"], "RawJSON", "Compact" -> True];
		resultJSON = IteratedGameCrossWL[
			OptionValue["Rounds"], leftNdjson, rightNdjson, initJSON,
			OptionValue["NumActions"]];
		If[FailureQ[resultJSON], Return[$Failed]];
		result = ImportString[resultJSON, "RawJSON"];
		If[KeyExistsQ[result, "error"],
			Message[ProgramIteratedGameCross::err, result["error"]]; Return[$Failed]];
		iParseIteratedGameCrossResult[result]
	]


(* ::Section::Closed:: *)
(*Helper: Parse Tournament Result*)


iParseIteratedGameResult[result_] :=
	Module[{strategies, games},
		strategies = result["strategies"];
		games = Association[
			{iParseLabel[#["label_a"]], iParseLabel[#["label_b"]]} -> #["history"] & /@ result["games"]
		];
		<|
			"Strategies" -> (iParseLabel /@ strategies),
			"Games" -> games,
			"Rounds" -> result["rounds"],
			"NumStrategies" -> result["num_strategies"],
			"NumPairs" -> result["num_pairs"]
		|>
	]

iParseIteratedGameCrossResult[result_] :=
	Module[{leftStrategies, rightStrategies, games},
		leftStrategies = result["left_strategies"];
		rightStrategies = result["right_strategies"];
		games = Association[
			{iParseLabel[#["label_a"]], iParseLabel[#["label_b"]]} -> #["history"] & /@ result["games"]
		];
		<|
			"LeftStrategies" -> (iParseLabel /@ leftStrategies),
			"RightStrategies" -> (iParseLabel /@ rightStrategies),
			"Games" -> games,
			"Rounds" -> result["rounds"],
			"NumLeft" -> result["num_left"],
			"NumRight" -> result["num_right"],
			"NumPairs" -> result["num_pairs"]
		|>
	]


(* ::Section::Closed:: *)
(*IteratedGameFiniteStateMachine*)


Options[IteratedGameFiniteStateMachine] = {
	"States" -> 2, "Colors" -> 2, "Rounds" -> 100,
	"InitialHistory" -> {}, "NumActions" -> 2
};

IteratedGameFiniteStateMachine[fsmIds_List, opts : OptionsPattern[]] :=
	Module[{s, k, specs, ndjson, initJSON, resultJSON, result},
		s = OptionValue["States"];
		k = OptionValue["Colors"];
		specs = {"FSM", #, s, k} & /@ fsmIds;
		ndjson = StringRiffle[StrategyToJSON /@ specs, "\n"];
		initJSON = ExportString[OptionValue["InitialHistory"], "RawJSON", "Compact" -> True];
		resultJSON = IteratedGameTournamentWL[OptionValue["Rounds"], ndjson, initJSON,
			OptionValue["NumActions"]];
		If[FailureQ[resultJSON], Return[$Failed]];
		result = ImportString[resultJSON, "RawJSON"];
		If[KeyExistsQ[result, "error"],
			Message[IteratedGameFiniteStateMachine::err, result["error"]]; Return[$Failed]];
		iParseIteratedGameResult[result]
	]


(* ::Section::Closed:: *)
(*IteratedGameCellularAutomaton*)


Options[IteratedGameCellularAutomaton] = {
	"Colors" -> 2, "Radius" -> 1, "Steps" -> 10, "Rounds" -> 100,
	"InitialHistory" -> {}, "NumActions" -> 2
};

IteratedGameCellularAutomaton[caRules_List, opts : OptionsPattern[]] :=
	Module[{k, r, specs, ndjson, initJSON, resultJSON, result},
		k = OptionValue["Colors"];
		r = OptionValue["Radius"];
		specs = {"CA", #, k, r, OptionValue["Steps"]} & /@ caRules;
		ndjson = StringRiffle[StrategyToJSON /@ specs, "\n"];
		initJSON = ExportString[OptionValue["InitialHistory"], "RawJSON", "Compact" -> True];
		resultJSON = IteratedGameTournamentWL[OptionValue["Rounds"], ndjson, initJSON,
			OptionValue["NumActions"]];
		If[FailureQ[resultJSON], Return[$Failed]];
		result = ImportString[resultJSON, "RawJSON"];
		If[KeyExistsQ[result, "error"],
			Message[IteratedGameCellularAutomaton::err, result["error"]]; Return[$Failed]];
		iParseIteratedGameResult[result]
	]


(* ::Section::Closed:: *)
(*IteratedGameTuringMachine*)


Options[IteratedGameTuringMachine] = {
	"MaxSteps" -> 500, "Rounds" -> 100,
	"InitialHistory" -> {}, "NumActions" -> 2
};

IteratedGameTuringMachine[tmIds_List, s_Integer, k_Integer, opts : OptionsPattern[]] :=
	Module[{maxSteps, specs, ndjson, initJSON, resultJSON, result},
		maxSteps = OptionValue["MaxSteps"];
		specs = {"TM", #, s, k, maxSteps} & /@ tmIds;
		ndjson = StringRiffle[StrategyToJSON /@ specs, "\n"];
		initJSON = ExportString[OptionValue["InitialHistory"], "RawJSON", "Compact" -> True];
		resultJSON = IteratedGameTournamentWL[OptionValue["Rounds"], ndjson, initJSON,
			OptionValue["NumActions"]];
		If[FailureQ[resultJSON], Return[$Failed]];
		result = ImportString[resultJSON, "RawJSON"];
		If[KeyExistsQ[result, "error"],
			Message[IteratedGameTuringMachine::err, result["error"]]; Return[$Failed]];
		iParseIteratedGameResult[result]
	]
