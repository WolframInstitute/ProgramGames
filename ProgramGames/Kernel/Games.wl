(* ::Package:: *)

Package["WolframInstitute`ProgramGames`"]


(* ::Section::Closed:: *)
(*Exported Symbols*)


PackageExport["EnumerateGames"]
PackageExport["GameWinner"]
PackageExport["GameSpaceSurvey"]


(* ::Section::Closed:: *)
(*Usage Messages*)


EnumerateGames::usage = "EnumerateGames[values, numActions] enumerates all possible \
symmetric two-player games with the given payoff values and number of actions. \
Each game is a numActions\[Times]numActions\[Times]2 payoff array.";

GameWinner::usage = "GameWinner[tournamentResult] returns the ID of the top-ranked \
strategy. GameWinner[tournamentResult, n] returns the top n.";

GameSpaceSurvey::usage = "GameSpaceSurvey[fsmIds, games, opts] runs a tournament for \
each game and returns an association mapping each game to its winner(s). \
Options: \"States\", \"Colors\", \"Rounds\", \"GPU\", \"TopN\".";


(* ::Section::Closed:: *)
(*EnumerateGames*)


(* Enumerate all numActions x numActions x 2 payoff arrays from the given values.
   For 2-action games with values {-1,0,1}: 3^8 = 6561 games. *)
EnumerateGames[values_List, numActions_Integer : 2] :=
	Module[{nEntries, tuples},
		nEntries = numActions^2 * 2; (* each cell has 2 payoffs *)
		tuples = Tuples[values, nEntries];
		(* Reshape each flat tuple into {{a,b},{c,d},...} then into matrix *)
		Partition[Partition[#, 2], numActions] & /@ tuples
	]


(* ::Section::Closed:: *)
(*GameWinner*)


GameWinner[result_Association] :=
	With[{ranking = result["Ranking"]},
		If[Length[ranking] == 0, Return[<||>]];
		<|
			"ByTotal" -> First[SortBy[ranking, -#["total"] &]]["label"],
			"ByMean" -> First[SortBy[ranking, -#["mean"] &]]["label"],
			"ByMedian" -> First[SortBy[ranking, -#["median"] &]]["label"]
		|>
	]


(* ::Section::Closed:: *)
(*GameSpaceSurvey*)


Options[GameSpaceSurvey] = {
	"States" -> 2, "Colors" -> 2,
	"Rounds" -> 100, "GPU" -> True
};

GameSpaceSurvey[fsmIds_List, games_List, opts : OptionsPattern[]] :=
	Module[{s, k, rounds, gpu, winners, nGames, t0, progress},
		s = OptionValue["States"];
		k = OptionValue["Colors"];
		rounds = OptionValue["Rounds"];
		gpu = OptionValue["GPU"];
		nGames = Length[games];
		t0 = AbsoluteTime[];
		progress = "";

		Monitor[
			winners = Table[
				progress = Row[{
					ProgressIndicator[i, {1, nGames}], "  ",
					"Game ", i, "/", nGames,
					"  (", Round[AbsoluteTime[] - t0, 0.1], "s)"}];
				With[{result = FiniteStateMachineTournament[fsmIds,
						"States" -> s, "Colors" -> k,
						"Rounds" -> rounds,
						"Game" -> PayoffToString[games[[i]]],
						"GPU" -> gpu],
					game = games[[i]]},
					Append[GameWinner[result], "Game" -> game]
				],
				{i, nGames}
			],
			progress
		];

		<|
			"Winners" -> winners,
			"NumGames" -> nGames,
			"NumStrategies" -> Length[fsmIds],
			"Time" -> AbsoluteTime[] - t0
		|>
	]
