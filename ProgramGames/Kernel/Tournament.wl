(* ::Package:: *)

Package["WolframInstitute`ProgramGames`"]


(* ::Section::Closed:: *)
(*Exported Symbols*)


PackageExport["TuringMachineTournament"]
PackageExport["CellularAutomatonTournament"]
PackageExport["FiniteStateMachineTournament"]
PackageExport["ProgramTournament"]
PackageExport["PayoffToString"]
PackageExport["StrategyToJSON"]


(* ::Section::Closed:: *)
(*Usage Messages*)


TuringMachineTournament::usage = "TuringMachineTournament[ids, s, k] runs a GPU-accelerated \
round-robin tournament among TM ids. Options: \"MaxSteps\", \"Rounds\", \"Game\".";

CellularAutomatonTournament::usage = "CellularAutomatonTournament[rules] runs a tournament \
among cellular automaton rules. Options: \"K\", \"R\", \"T\", \"Rounds\", \"Game\".";

FiniteStateMachineTournament::usage = "FiniteStateMachineTournament[ids] runs a tournament \
among FSM ids. Options: \"S\", \"K\", \"Rounds\", \"Game\".";

ProgramTournament::usage = "ProgramTournament[strategies] runs a mixed-type tournament. \
Strategies: {\"TM\",id,s,k}, {\"FSM\",id,s,k}, {\"CA\",rule,k,r,t}. \
Options: \"Rounds\", \"Game\".";

PayoffToString::usage = "PayoffToString[game] converts a WL payoff array to a comma-separated string.";

StrategyToJSON::usage = "StrategyToJSON[spec] converts a WL strategy spec to JSON.";


(* ::Section::Closed:: *)
(*PayoffToString*)


PayoffToString[game_List] := StringRiffle[ToString /@ Flatten[game], ","]
PayoffToString[game_String] := game


(* ::Section::Closed:: *)
(*StrategyToJSON*)


StrategyToJSON[{"TM", id_Integer, s_Integer, k_Integer}] :=
	ExportString[<|"type" -> "tm", "id" -> id, "s" -> s, "k" -> k|>,
		"RawJSON", "Compact" -> True]
StrategyToJSON[{"TM", id_Integer, s_Integer, k_Integer, ms_Integer}] :=
	ExportString[<|"type" -> "tm", "id" -> id, "s" -> s, "k" -> k,
		"max_steps" -> ms|>, "RawJSON", "Compact" -> True]
StrategyToJSON[{"FSM", id_Integer, s_Integer, k_Integer}] :=
	ExportString[<|"type" -> "fsm", "id" -> id, "s" -> s, "k" -> k|>,
		"RawJSON", "Compact" -> True]
StrategyToJSON[{"CA", rule_Integer, k_Integer, r_, t_Integer}] :=
	ExportString[<|"type" -> "ca", "rule" -> rule, "k" -> k,
		"r" -> N[r], "t" -> t|>, "RawJSON", "Compact" -> True]
StrategyToJSON[{id_Integer, s_Integer, k_Integer}] :=
	StrategyToJSON[{"TM", id, s, k}]


(* ::Section::Closed:: *)
(*iParsePairwise*)


iParsePairwise[pairwise_List, keyA_String, keyB_String] :=
	Association[
		{#[keyA], #[keyB]} -> {#["score_a"], #["score_b"]} & /@ pairwise
	]


(* ::Section::Closed:: *)
(*TuringMachineTournament*)


Options[TuringMachineTournament] = {
	"MaxSteps" -> 500,
	"Rounds" -> 100,
	"Game" -> "pd",
	"GPU" -> True
};

TuringMachineTournament[tmIds_List, s_Integer, k_Integer, opts : OptionsPattern[]] :=
	Module[{gameStr, resultJSON, result},
		gameStr = PayoffToString[OptionValue["Game"]];
		resultJSON = TMTournamentWL[s, k,
			OptionValue["MaxSteps"], OptionValue["Rounds"],
			gameStr, ExportString[tmIds, "RawJSON"], TrueQ[OptionValue["GPU"]]];
		If[FailureQ[resultJSON], Return[$Failed]];
		result = ImportString[resultJSON, "RawJSON"];
		<|
			"IDs" -> result["ids"],
			"Scores" -> result["scores"],
			"Pairwise" -> iParsePairwise[result["pairwise"], "id_a", "id_b"],
			"Ranking" -> result["ranking"],
			"Rounds" -> result["rounds"],
			"Game" -> result["game"],
			"NumMachines" -> result["num_machines"],
			"NumPairs" -> result["num_pairs"]
		|>
	]


(* ::Section::Closed:: *)
(*ProgramTournament*)


Options[ProgramTournament] = {
	"Rounds" -> 100,
	"Game" -> "pd"
};

ProgramTournament[strategies_List, opts : OptionsPattern[]] :=
	Module[{ndjson, gameStr, resultJSON, result},
		ndjson = StringRiffle[StrategyToJSON /@ strategies, "\n"];
		gameStr = PayoffToString[OptionValue["Game"]];
		resultJSON = ProgramTournamentWL[
			OptionValue["Rounds"], gameStr, ndjson];
		If[FailureQ[resultJSON], Return[$Failed]];
		result = ImportString[resultJSON, "RawJSON"];
		<|
			"Strategies" -> result["strategies"],
			"Labels" -> Lookup[result["ranking"], "label"],
			"Scores" -> result["scores"],
			"Pairwise" -> iParsePairwise[result["pairwise"], "label_a", "label_b"],
			"Ranking" -> result["ranking"],
			"Rounds" -> result["rounds"],
			"Game" -> result["game"],
			"NumStrategies" -> result["num_strategies"],
			"NumPairs" -> result["num_pairs"]
		|>
	]


(* ::Section::Closed:: *)
(*CellularAutomatonTournament*)


Options[CellularAutomatonTournament] = {
	"K" -> 2, "R" -> 1, "T" -> 10,
	"Rounds" -> 100, "Game" -> "pd"
};

CellularAutomatonTournament[caRules_List, opts : OptionsPattern[]] :=
	ProgramTournament[
		{"CA", #, OptionValue["K"], OptionValue["R"], OptionValue["T"]} & /@ caRules,
		"Rounds" -> OptionValue["Rounds"],
		"Game" -> OptionValue["Game"]
	]


(* ::Section::Closed:: *)
(*FiniteStateMachineTournament*)


Options[FiniteStateMachineTournament] = {
	"S" -> 2, "K" -> 2,
	"Rounds" -> 100, "Game" -> "pd"
};

FiniteStateMachineTournament[fsmIds_List, opts : OptionsPattern[]] :=
	ProgramTournament[
		{"FSM", #, OptionValue["S"], OptionValue["K"]} & /@ fsmIds,
		"Rounds" -> OptionValue["Rounds"],
		"Game" -> OptionValue["Game"]
	]
