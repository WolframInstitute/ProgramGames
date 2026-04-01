(* ::Package:: *)

Package["WolframInstitute`ProgramGames`"]


(* ::Section::Closed:: *)
(*Exported Symbols*)


PackageExport["ProgramIteratedGame"]
PackageExport["IteratedGameFiniteStateMachine"]
PackageExport["IteratedGameCellularAutomaton"]
PackageExport["IteratedGameTuringMachine"]


(* ::Section::Closed:: *)
(*Usage Messages*)


ProgramIteratedGame::usage = "ProgramIteratedGame[{strategy1, strategy2}] plays an iterated game between two strategies \
and returns the move history as a list of {moveA, moveB} pairs. \
Strategies: {\"TM\",id,s,k}, {\"TM\",id,s,k,maxSteps}, {\"FSM\",id,s,k}, {\"CA\",rule,k,r,t}, or any mix. \
Options: \"Rounds\" (default 100), \"InitialHistory\" (default {}).";

IteratedGameFiniteStateMachine::usage = "IteratedGameFiniteStateMachine[ids] plays iterated games \
between all pairs of FSM strategies and returns an Association of pairwise move histories.";

IteratedGameCellularAutomaton::usage = "IteratedGameCellularAutomaton[rules] plays iterated games \
between all pairs of CA strategies and returns an Association of pairwise move histories.";

IteratedGameTuringMachine::usage = "IteratedGameTuringMachine[ids, s, k] plays iterated games \
between all pairs of TM strategies and returns an Association of pairwise move histories.";

ProgramIteratedGame::err = "Game failed: ``";
IteratedGameFiniteStateMachine::err = "Game failed: ``";
IteratedGameCellularAutomaton::err = "Game failed: ``";
IteratedGameTuringMachine::err = "Game failed: ``";


(* ::Section::Closed:: *)
(*ProgramIteratedGame*)


Options[ProgramIteratedGame] = {"Rounds" -> 100, "InitialHistory" -> {}};

ProgramIteratedGame[{s1_List, s2_List}, opts : OptionsPattern[]] :=
	Module[{ndjson, initJSON, resultJSON, result},
		ndjson = StringJoin[StrategyToJSON[s1], "\n", StrategyToJSON[s2]];
		initJSON = ExportString[OptionValue["InitialHistory"], "RawJSON", "Compact" -> True];
		resultJSON = IteratedGameWL[OptionValue["Rounds"], ndjson, initJSON];
		If[FailureQ[resultJSON], Return[$Failed]];
		result = ImportString[resultJSON, "RawJSON"];
		If[KeyExistsQ[result, "error"],
			Message[ProgramIteratedGame::err, result["error"]]; Return[$Failed]];
		result["history"]
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


(* ::Section::Closed:: *)
(*IteratedGameFiniteStateMachine*)


Options[IteratedGameFiniteStateMachine] = {
	"States" -> 2, "Colors" -> 2, "Rounds" -> 100, "InitialHistory" -> {}
};

IteratedGameFiniteStateMachine[fsmIds_List, opts : OptionsPattern[]] :=
	Module[{s, k, specs, ndjson, initJSON, resultJSON, result},
		s = OptionValue["States"];
		k = OptionValue["Colors"];
		specs = {"FSM", #, s, k} & /@ fsmIds;
		ndjson = StringRiffle[StrategyToJSON /@ specs, "\n"];
		initJSON = ExportString[OptionValue["InitialHistory"], "RawJSON", "Compact" -> True];
		resultJSON = IteratedGameTournamentWL[OptionValue["Rounds"], ndjson, initJSON];
		If[FailureQ[resultJSON], Return[$Failed]];
		result = ImportString[resultJSON, "RawJSON"];
		If[KeyExistsQ[result, "error"],
			Message[IteratedGameFiniteStateMachine::err, result["error"]]; Return[$Failed]];
		iParseIteratedGameResult[result]
	]


(* ::Section::Closed:: *)
(*IteratedGameCellularAutomaton*)


Options[IteratedGameCellularAutomaton] = {
	"Colors" -> 2, "Radius" -> 1, "Steps" -> 10, "Rounds" -> 100, "InitialHistory" -> {}
};

IteratedGameCellularAutomaton[caRules_List, opts : OptionsPattern[]] :=
	Module[{k, r, specs, ndjson, initJSON, resultJSON, result},
		k = OptionValue["Colors"];
		r = OptionValue["Radius"];
		specs = {"CA", #, k, r, OptionValue["Steps"]} & /@ caRules;
		ndjson = StringRiffle[StrategyToJSON /@ specs, "\n"];
		initJSON = ExportString[OptionValue["InitialHistory"], "RawJSON", "Compact" -> True];
		resultJSON = IteratedGameTournamentWL[OptionValue["Rounds"], ndjson, initJSON];
		If[FailureQ[resultJSON], Return[$Failed]];
		result = ImportString[resultJSON, "RawJSON"];
		If[KeyExistsQ[result, "error"],
			Message[IteratedGameCellularAutomaton::err, result["error"]]; Return[$Failed]];
		iParseIteratedGameResult[result]
	]


(* ::Section::Closed:: *)
(*IteratedGameTuringMachine*)


Options[IteratedGameTuringMachine] = {
	"MaxSteps" -> 500, "Rounds" -> 100, "InitialHistory" -> {}
};

IteratedGameTuringMachine[tmIds_List, s_Integer, k_Integer, opts : OptionsPattern[]] :=
	Module[{maxSteps, specs, ndjson, initJSON, resultJSON, result},
		maxSteps = OptionValue["MaxSteps"];
		specs = {"TM", #, s, k, maxSteps} & /@ tmIds;
		ndjson = StringRiffle[StrategyToJSON /@ specs, "\n"];
		initJSON = ExportString[OptionValue["InitialHistory"], "RawJSON", "Compact" -> True];
		resultJSON = IteratedGameTournamentWL[OptionValue["Rounds"], ndjson, initJSON];
		If[FailureQ[resultJSON], Return[$Failed]];
		result = ImportString[resultJSON, "RawJSON"];
		If[KeyExistsQ[result, "error"],
			Message[IteratedGameTuringMachine::err, result["error"]]; Return[$Failed]];
		iParseIteratedGameResult[result]
	]
