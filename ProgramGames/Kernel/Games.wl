(* ::Package:: *)

Package["WolframInstitute`ProgramGames`"]


(* ::Section::Closed:: *)
(*Exported Symbols*)


PackageExport["EnumerateGames"]
PackageExport["GameWinner"]
PackageExport["GameSpaceSurvey"]
PackageExport["GameRankingClasses"]


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

GameRankingClasses::usage = "GameRankingClasses[fsmIds, games, opts] groups games by \
identical tournament ranking. Uses 8 basis tournaments (GPU-accelerated) and linear \
algebra to reconstruct rankings for all games at once. \
Returns <|\"Classes\"->..., \"NumClasses\"->..., ...|>. \
Options: \"States\", \"Colors\", \"Rounds\", \"GPU\".";


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
		If[Length[ranking] < 2, Return[<||>]];
		Module[{hasTie, sorted},
			hasTie[key_] := (
				sorted = SortBy[ranking, -#[key] &];
				sorted[[1]][key] == sorted[[2]][key]
			);
			(* Discard game entirely if any metric has a tie at the top *)
			If[hasTie["total"] || hasTie["mean"] || hasTie["median"],
				<||>,
				<|
					"ByTotal" -> First[SortBy[ranking, -#["total"] &]]["label"],
					"ByMean" -> First[SortBy[ranking, -#["mean"] &]]["label"],
					"ByMedian" -> First[SortBy[ranking, -#["median"] &]]["label"]
				|>
			]
		]
	]


(* ::Section::Closed:: *)
(*GameSpaceSurvey*)


(* Parse Rust label "fsm(s,k)#id" into {FSM, {id, s, k}} *)
parseLabel[label_String] :=
	With[{m = StringCases[label,
		"fsm(" ~~ s : DigitCharacter .. ~~ "," ~~ k : DigitCharacter .. ~~
		")#" ~~ id : DigitCharacter .. :>
			{"FSM", {ToExpression[id], ToExpression[s], ToExpression[k]}}
	]},
		If[Length[m] > 0, First[m], label]
	]

Options[GameSpaceSurvey] = {
	"States" -> 2, "Colors" -> 2,
	"Rounds" -> 100, "GPU" -> True
};

(* Batched GPU path: sends all games to Rust in a single call *)
GameSpaceSurvey[fsmIds_List, games_List, opts : OptionsPattern[]] :=
	Module[{s, k, rounds, gpu, nGames, t0, gameStrings, resultJSON,
		result, winners, progress = 0, tmpCell},
		s = OptionValue["States"];
		k = OptionValue["Colors"];
		rounds = OptionValue["Rounds"];
		gpu = OptionValue["GPU"];
		nGames = Length[games];
		t0 = AbsoluteTime[];

		(* Convert all games to payoff strings *)
		gameStrings = PayoffToString /@ games;

		(* Print status before blocking Rust call *)
		tmpCell = PrintTemporary[
			Style[
				StringJoin["Running ", ToString[nGames], " games \[Times] ",
					ToString[Length[fsmIds]], " strategies",
					If[TrueQ[gpu], " (GPU)", " (CPU)"], "..."],
				Gray
			]
		];

		(* Single batched Rust call for all games *)
		resultJSON = FSMGameSurveyWL[s, k, rounds,
			ExportString[gameStrings, "RawJSON"],
			ExportString[fsmIds, "RawJSON"],
			TrueQ[gpu]];

		NotebookDelete[tmpCell];
		If[FailureQ[resultJSON], Return[$Failed]];

		result = ImportString[resultJSON, "RawJSON"];

		(* Parse results into winners list *)
		winners = Monitor[
			Table[
				progress = i;
				With[{r = result["results"][[i]]},
					If[KeyExistsQ[r, "ByTotal"],
						<|
							"ByTotal" -> parseLabel[r["ByTotal"]],
							"ByMean" -> parseLabel[r["ByMean"]],
							"ByMedian" -> parseLabel[r["ByMedian"]],
							"Game" -> games[[i]]
						|>,
						Nothing
					]
				],
				{i, nGames}
			],
			Row[{"Parsing results: ", ProgressIndicator[progress, {0, nGames}],
				" ", progress, "/", nGames}]
		];

		<|
			"Winners" -> winners,
			"NumGames" -> nGames,
			"NumStrategies" -> Length[fsmIds],
			"Time" -> AbsoluteTime[] - t0,
			"GPU" -> TrueQ[result["gpu"]]
		|>
	]


(* ::Section::Closed:: *)
(*GameRankingClasses*)


(* Action sequences in iterated games are independent of payoff values:
   payoffs affect only the final scoring, not the strategies' decisions.
   We run 8 "basis" tournaments (one per entry in the 2x2x2 payoff tensor)
   and reconstruct the score matrix for ANY game via linear combination.
   For 2-action games this reduces 6561 tournaments to 8 GPU calls
   plus a single matrix multiply. *)

Options[GameRankingClasses] = {
	"States" -> 2, "Colors" -> 2,
	"Rounds" -> 100, "GPU" -> True
};

GameRankingClasses[fsmIds_List, games_List, opts : OptionsPattern[]] :=
	Module[{s, k, rounds, gpu, n, nGames, t0,
		basisGames, basisResults, basisTotals, basisScoreMatrices,
		coeffMatrix, allTotals, allRankings,
		groups, classSummary, strategyLabels, tmpCell},

		s = OptionValue["States"];
		k = OptionValue["Colors"];
		rounds = OptionValue["Rounds"];
		gpu = TrueQ[OptionValue["GPU"]];
		n = Length[fsmIds];
		nGames = Length[games];
		t0 = AbsoluteTime[];

		(* 8 basis games: standard basis for the 2x2x2 payoff tensor.
		   Basis i has payoff 1 in position i of Flatten[game], 0 elsewhere. *)
		basisGames = Table[
			ReplacePart[ConstantArray[0, {2, 2, 2}],
				{Quotient[i - 1, 4] + 1,
				 Quotient[Mod[i - 1, 4], 2] + 1,
				 Mod[i - 1, 2] + 1} -> 1],
			{i, 8}
		];

		(* Run 8 GPU-accelerated FSM tournaments *)
		tmpCell = PrintTemporary[
			Style[StringJoin["Running 8 basis tournaments (",
				ToString[n], " strategies, GPU=", ToString[gpu], ")..."], Gray]
		];
		basisResults = Table[
			FiniteStateMachineTournament[fsmIds,
				"States" -> s, "Colors" -> k,
				"Rounds" -> rounds,
				"Game" -> basisGames[[i]],
				"GPU" -> gpu],
			{i, 8}
		];
		NotebookDelete[tmpCell];

		If[AnyTrue[basisResults, # === $Failed &],
			Return[$Failed]];

		strategyLabels = basisResults[[1]]["Strategies"];

		(* Total-score vectors for each basis tournament *)
		basisTotals = Total[#["Scores"], {2}] & /@ basisResults;
		basisScoreMatrices = #["Scores"] & /@ basisResults;

		(* Coefficient matrix: each game's flattened payoffs *)
		coeffMatrix = Flatten /@ games;

		(* Matrix multiply: total scores for ALL games at once *)
		allTotals = coeffMatrix . basisTotals;

		(* Rank strategies per game (descending total, ties broken by index) *)
		allRankings = Ordering[#, All, Greater] & /@ allTotals;

		(* Group games by identical ranking permutation *)
		groups = GroupBy[Range[nGames], allRankings[[#]] &];

		(* Summary sorted by class size *)
		classSummary = ReverseSortBy[
			KeyValueMap[
				<|"Ranking" -> #1,
				  "RankingLabels" -> strategyLabels[[#1]],
				  "Size" -> Length[#2],
				  "GameIndices" -> #2,
				  "Fraction" -> N[Length[#2] / nGames]|> &,
				groups
			],
			#["Size"] &
		];

		<|
			"Classes" -> classSummary,
			"NumClasses" -> Length[groups],
			"NumGames" -> nGames,
			"NumStrategies" -> n,
			"StrategyLabels" -> strategyLabels,
			"BasisTotals" -> basisTotals,
			"BasisScores" -> basisScoreMatrices,
			"AllTotals" -> allTotals,
			"AllRankings" -> allRankings,
			"Time" -> AbsoluteTime[] - t0,
			"GPU" -> gpu
		|>
	]
