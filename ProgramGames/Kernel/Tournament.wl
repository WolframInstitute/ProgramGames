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

CellularAutomatonTournament::usage = "CellularAutomatonTournament[rules] runs a GPU-accelerated \
tournament among cellular automaton rules. Options: \"Colors\", \"Radius\", \"Steps\", \"Rounds\", \"Game\", \"GPU\".";

FiniteStateMachineTournament::usage = "FiniteStateMachineTournament[ids] runs a GPU-accelerated \
tournament among FSM ids. Options: \"States\", \"Colors\", \"Rounds\", \"Game\", \"GPU\".";

ProgramTournament::usage = "ProgramTournament[strategies] runs a mixed-type tournament. \
Strategies: {\"TM\",id,s,k}, {\"TM\",id,s,k,maxSteps}, {\"FSM\",id,s,k}, {\"CA\",rule,k,r,t}. \
For TMs, an optional 5th element specifies maxSteps (default 500). Larger TMs (e.g. 3-state) may need higher values like 5000. \
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
(*iParseLabel*)


iParseLabel[label_String] := Which[
	StringMatchQ[label, "tm(" ~~ __ ~~ ")#" ~~ __],
		With[{parts = StringCases[label,
			"tm(" ~~ Shortest[s__] ~~ "," ~~ Shortest[k__] ~~ ")#" ~~ id__ :>
				{"TM", {ToExpression[id], ToExpression[s], ToExpression[k]}}]},
			If[Length[parts] > 0, First[parts], label]
		],
	StringMatchQ[label, "fsm(" ~~ __ ~~ ")#" ~~ __],
		With[{parts = StringCases[label,
			"fsm(" ~~ Shortest[s__] ~~ "," ~~ Shortest[k__] ~~ ")#" ~~ id__ :>
				{"FSM", {ToExpression[id], ToExpression[s], ToExpression[k]}}]},
			If[Length[parts] > 0, First[parts], label]
		],
	StringMatchQ[label, "ca(" ~~ __ ~~ ")#" ~~ __],
		With[{parts = StringCases[label,
			"ca(" ~~ Shortest[k__] ~~ "," ~~ Shortest[r__] ~~ "," ~~ Shortest[t__] ~~ ")#" ~~ rule__ :>
				{"CA", {ToExpression[rule], ToExpression[k], Rationalize[ToExpression[r]]}}]},
			If[Length[parts] > 0, First[parts], label]
		],
	True, label
]


(* ::Section::Closed:: *)
(*iParsePairwise*)


iParsePairwise[pairwise_List, keyA_String, keyB_String, parseLabel_: Identity] :=
	Association[
		{parseLabel[#[keyA]], parseLabel[#[keyB]]} -> {#["score_a"], #["score_b"]} & /@ pairwise
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
			"Strategies" -> (iParseLabel /@ result["strategies"]),
			"Labels" -> (iParseLabel /@ Lookup[result["ranking"], "label"]),
			"Scores" -> result["scores"],
			"Pairwise" -> iParsePairwise[result["pairwise"], "label_a", "label_b", iParseLabel],
			"Ranking" -> Map[MapAt[iParseLabel, #, Key["label"]] &, result["ranking"]],
			"Rounds" -> result["rounds"],
			"Game" -> result["game"],
			"NumStrategies" -> result["num_strategies"],
			"NumPairs" -> result["num_pairs"]
		|>
	]


(* ::Section::Closed:: *)
(*CellularAutomatonTournament*)


Options[CellularAutomatonTournament] = {
	"Colors" -> 2, "Radius" -> 1, "Steps" -> 10,
	"Rounds" -> 100, "Game" -> "pd", "GPU" -> True
};

CellularAutomatonTournament[caRules_List, opts : OptionsPattern[]] :=
	Module[{k, r, rNum, rDen, gameStr, resultJSON, result},
		k = OptionValue["Colors"];
		r = OptionValue["Radius"];
		{rNum, rDen} = If[IntegerQ[r], {r, 1}, Through[{Numerator, Denominator}[r]]];
		gameStr = PayoffToString[OptionValue["Game"]];
		resultJSON = CATournamentWL[k, rNum, rDen,
			OptionValue["Steps"], OptionValue["Rounds"],
			gameStr, ExportString[caRules, "RawJSON"],
			TrueQ[OptionValue["GPU"]]];
		If[FailureQ[resultJSON], Return[$Failed]];
		result = ImportString[resultJSON, "RawJSON"];
		<|
			"Strategies" -> (iParseLabel /@ result["strategies"]),
			"Labels" -> (iParseLabel /@ Lookup[result["ranking"], "label"]),
			"Scores" -> result["scores"],
			"Pairwise" -> iParsePairwise[result["pairwise"], "label_a", "label_b", iParseLabel],
			"Ranking" -> Map[MapAt[iParseLabel, #, Key["label"]] &, result["ranking"]],
			"Rounds" -> result["rounds"],
			"Game" -> result["game"],
			"NumStrategies" -> result["num_strategies"],
			"NumPairs" -> result["num_pairs"],
			"GPU" -> TrueQ[result["gpu"]]
		|>
	]


(* ::Section::Closed:: *)
(*FiniteStateMachineTournament*)


Options[FiniteStateMachineTournament] = {
	"States" -> 2, "Colors" -> 2,
	"Rounds" -> 100, "Game" -> "pd", "GPU" -> True
};

FiniteStateMachineTournament[fsmIds_List, opts : OptionsPattern[]] :=
	Module[{s, k, gameStr, resultJSON, result},
		s = OptionValue["States"];
		k = OptionValue["Colors"];
		gameStr = PayoffToString[OptionValue["Game"]];
		resultJSON = FSMTournamentWL[s, k,
			OptionValue["Rounds"],
			gameStr, ExportString[fsmIds, "RawJSON"],
			TrueQ[OptionValue["GPU"]]];
		If[FailureQ[resultJSON], Return[$Failed]];
		result = ImportString[resultJSON, "RawJSON"];
		<|
			"Strategies" -> (iParseLabel /@ result["strategies"]),
			"Labels" -> (iParseLabel /@ Lookup[result["ranking"], "label"]),
			"Scores" -> result["scores"],
			"Pairwise" -> iParsePairwise[result["pairwise"], "label_a", "label_b", iParseLabel],
			"Ranking" -> Map[MapAt[iParseLabel, #, Key["label"]] &, result["ranking"]],
			"Rounds" -> result["rounds"],
			"Game" -> result["game"],
			"NumStrategies" -> result["num_strategies"],
			"NumPairs" -> result["num_pairs"],
			"GPU" -> TrueQ[result["gpu"]]
		|>
	]
