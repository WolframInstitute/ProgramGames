(* ::Package:: *)

Package["WolframInstitute`ProgramGames`"]


(* ::Section::Closed:: *)
(*Exported Symbols*)


PackageExport["ComplexityMeasure"]
PackageExport["ComplexityLandscapeSearch"]


(* ::Section::Closed:: *)
(*Scoped Symbols*)


PackageScope["iFSMDecode"]
PackageScope["iFSMSimulateGame"]
PackageScope["iLempelZivComplexity"]


(* ::Section::Closed:: *)
(*Usage Messages*)


ComplexityMeasure::usage = "ComplexityMeasure[fsmIds, opts] computes a complexity value \
for each FSM strategy. Returns an Association <|id -> complexity, ...|>. \
Uses Rust/GPU when available for fast computation. \
Options: \"States\", \"Colors\", \"Rounds\", \"GPU\", \
  \"Method\" \[Dash] one of: \
    \"LempelZiv\" (default, GPU: mean LZ76 factorization complexity of action sequences), \
    \"CountDistinct\" (GPU: counts distinct behavior sequences per FSM).";

ComplexityLandscapeSearch::usage = "ComplexityLandscapeSearch[fsmIds, games, opts] \
searches the game space and classifies games by the complexity of their winning \
strategies into \"Complex\", \"Simple\", and \"Neutral\". \
Default fast path: 1 batched GPU call (GameSpaceSurvey) + 1 reference tournament = 2 GPU calls. \
Set \"DetailedScores\"->True for full per-game per-FSM scores via 8 basis tournaments. \
Returns <|\"ComplexGames\"->..., \"SimpleGames\"->..., \"NeutralGames\"->..., \
\"WinnerData\"->..., \"Statistics\"->...|>. \
Options: \
  \"States\" (default 2), \"Colors\" (default 2), \"Rounds\" (default 100), \"GPU\" (default True), \
  \"Threshold\" ({0.25, 0.75} quantile cutoffs for Simple/Complex), \
  \"DetailedScores\" (False; True enables per-game per-FSM score matrices), \
  \"ComplexityData\" (None; pass an Association <|id->complexity,...|> to use \
    pre-computed complexity data such as compdata from Working-08. \
    When None, computes ScoreCompression automatically).";

ComplexityMeasure::method = "Unknown complexity method \"`1`\". Use \
\"LempelZiv\" or \"CountDistinct\".";


(* ::Section::Closed:: *)
(*FSM Decode*)


(* Decode an FSM index into output and transition tables.
   Matches the Rust decode_fsm in strategy.rs.
   Returns <|"Outputs" -> {action per state},
             "Transitions" -> {{next_state per input} per state}|>
   All values 0-indexed. *)

iFSMDecode[0, s_Integer, k_Integer] :=
	<|"Outputs" -> ConstantArray[0, s],
	  "Transitions" -> ConstantArray[0, {s, k}]|>

iFSMDecode[id_Integer, s_Integer, k_Integer] :=
	Module[{actionBlock = k^s, transCode, outputCode},
		{transCode, outputCode} = QuotientRemainder[id - 1, actionBlock];
		<|
			"Outputs" -> IntegerDigits[outputCode, k, s],
			"Transitions" -> Partition[
				If[s == 1,
					ConstantArray[0, s * k],
					IntegerDigits[transCode, s, s * k]
				], k]
		|>
	]


(* ::Section::Closed:: *)
(*FSM Simulate*)


(* Simulate a game between two decoded FSMs for the given number of rounds.
   Returns a list of {action1, action2} pairs, one per round.
   Protocol (matching Rust FSM tournament):
     Round 1: output from initial state (state 0), no transition.
     Round r >= 2: transition on opponent's previous action, then output. *)

iFSMSimulateGame[fsm1_Association, fsm2_Association, rounds_Integer] :=
	Module[{state1 = 0, state2 = 0, a1, a2,
	        result = ConstantArray[0, {rounds, 2}]},
		(* Round 1: initial outputs, no transition *)
		a1 = fsm1["Outputs"][[1]];
		a2 = fsm2["Outputs"][[1]];
		result[[1]] = {a1, a2};
		(* Rounds 2+: transition then output *)
		Do[
			With[{s1 = state1, s2 = state2},
				state1 = fsm1["Transitions"][[s1 + 1, a2 + 1]];
				state2 = fsm2["Transitions"][[s2 + 1, a1 + 1]]
			];
			a1 = fsm1["Outputs"][[state1 + 1]];
			a2 = fsm2["Outputs"][[state2 + 1]];
			result[[r]] = {a1, a2},
			{r, 2, rounds}
		];
		result
	]


(* ::Section::Closed:: *)
(*Lempel-Ziv Complexity*)


(* LZ76 factorization complexity: at each position, find the longest
   substring that appeared earlier in the sequence. The new factor is
   that match plus one new symbol. Matches the Rust lz76_complexity. *)

iLempelZivComplexity[{}] := 0

iLempelZivComplexity[seq_List] :=
	Module[{n = Length[seq], c = 0, pos = 1, longest, matchLen, refPos},
		While[pos <= n,
			longest = 0;
			Do[
				matchLen = 0;
				While[pos + matchLen <= n && refPos + matchLen < pos &&
					seq[[refPos + matchLen]] === seq[[pos + matchLen]],
					matchLen++
				];
				If[matchLen > longest, longest = matchLen],
				{refPos, 1, pos - 1}
			];
			c++;
			pos += longest + 1
		];
		c
	]


(* ::Section::Closed:: *)
(*ComplexityMeasure*)


Options[ComplexityMeasure] = {
	"States" -> 2, "Colors" -> 2,
	"Rounds" -> 100, "GPU" -> True,
	"Method" -> "LempelZiv"
};

ComplexityMeasure[fsmIds_List, opts : OptionsPattern[]] :=
	Module[{method, s, k, rounds, gpu, n, resultJSON, result},
		method = OptionValue["Method"];
		s = OptionValue["States"];
		k = OptionValue["Colors"];
		rounds = OptionValue["Rounds"];
		gpu = TrueQ[OptionValue["GPU"]];
		n = Length[fsmIds];

		If[!MatchQ[method, "LempelZiv" | "CountDistinct"],
			Message[ComplexityMeasure::method, method];
			Return[$Failed]
		];

		(* Single Rust/GPU call computes both LZ and CountDistinct *)
		resultJSON = FSMComplexityWL[s, k, rounds,
			ExportString[fsmIds, "RawJSON"], gpu];
		If[FailureQ[resultJSON], Return[$Failed]];
		result = ImportString[resultJSON, "RawJSON"];
		If[KeyExistsQ[result, "error"],
			Message[ComplexityMeasure::method, result["error"]];
			Return[$Failed]
		];

		Switch[method,
			"LempelZiv",
				AssociationThread[fsmIds, result["lz"]],
			"CountDistinct",
				AssociationThread[fsmIds, result["count_distinct"]]
		]
	]


(* ::Section::Closed:: *)
(*ComplexityLandscapeSearch*)


(* Two execution paths:
   Fast (default): 1 reference tournament + 1 batched GameSpaceSurvey call = 2 GPU calls.
   Detailed ("DetailedScores"->True): 8 basis tournaments via GameRankingClasses = 8 GPU calls,
     but provides per-FSM per-game scores for payoff-complexity scatter plots. *)

Options[ComplexityLandscapeSearch] = {
	"States" -> 2, "Colors" -> 2,
	"Rounds" -> 100, "GPU" -> True,
	"ComplexityData" -> None,
	"Threshold" -> {0.25, 0.75},
	"DetailedScores" -> False
};

ComplexityLandscapeSearch[fsmIds_List, games_List, opts : OptionsPattern[]] :=
	Module[{s, k, rounds, gpu, thresholds, detailed,
	        n, nGames, t0, userComplexity,
	        complexities, winnerComplexities, qLow, qHigh,
	        winnerData, gameClasses, result},

		s = OptionValue["States"];
		k = OptionValue["Colors"];
		rounds = OptionValue["Rounds"];
		gpu = TrueQ[OptionValue["GPU"]];
		userComplexity = OptionValue["ComplexityData"];
		thresholds = OptionValue["Threshold"];
		detailed = TrueQ[OptionValue["DetailedScores"]];
		n = Length[fsmIds];
		nGames = Length[games];
		t0 = AbsoluteTime[];

		(* Determine winners *)
		If[detailed,
			result = iDetailedPath[fsmIds, games, s, k, rounds, gpu],
			result = iFastPath[fsmIds, games, s, k, rounds, gpu]
		];
		If[result === $Failed, Return[$Failed]];

		(* Complexity: use supplied data or compute ScoreCompression *)
		complexities = If[AssociationQ[userComplexity],
			userComplexity,
			result["Complexities"]
		];

		(* Thresholds from winner complexity distribution, not the full pool *)
		winnerComplexities = DeleteMissing[
			Lookup[complexities, #["WinnerId"]& /@ result["Winners"], Missing[]]
		];
		If[Length[winnerComplexities] == 0, winnerComplexities = {0}];
		qLow = Quantile[winnerComplexities, thresholds[[1]]];
		qHigh = Quantile[winnerComplexities, thresholds[[2]]];

		(* Classify every game by its winner's complexity *)
		winnerData = Table[
			With[{w = result["Winners"][[i]]},
				With[{comp = Lookup[complexities, w["WinnerId"], Missing[]]},
					Append[w, <|
						"WinnerComplexity" -> comp,
						"Class" -> Which[
							MissingQ[comp], "Neutral",
							comp >= qHigh, "Complex",
							comp <= qLow, "Simple",
							True, "Neutral"
						]
					|>]
				]
			],
			{i, Length[result["Winners"]]}
		];

		gameClasses = GroupBy[winnerData, #["Class"] &];

		Join[
			<|
				"ComplexGames" -> Lookup[gameClasses, "Complex", {}],
				"SimpleGames" -> Lookup[gameClasses, "Simple", {}],
				"NeutralGames" -> Lookup[gameClasses, "Neutral", {}],
				"TiedGames" -> nGames - Length[result["Winners"]],
				"WinnerData" -> winnerData,
				"Thresholds" -> <|"Low" -> qLow, "High" -> qHigh|>,
				"Statistics" -> <|
					"NumGames" -> nGames,
					"NumClassified" -> Length[result["Winners"]],
					"NumTied" -> nGames - Length[result["Winners"]],
					"NumStrategies" -> n,
					"NumComplex" -> Length[Lookup[gameClasses, "Complex", {}]],
					"NumSimple" -> Length[Lookup[gameClasses, "Simple", {}]],
					"NumNeutral" -> Length[Lookup[gameClasses, "Neutral", {}]],
					"ComplexFraction" -> iSafeFraction[Lookup[gameClasses, "Complex", {}], result["Winners"]],
					"SimpleFraction" -> iSafeFraction[Lookup[gameClasses, "Simple", {}], result["Winners"]],
					"NeutralFraction" -> iSafeFraction[Lookup[gameClasses, "Neutral", {}], result["Winners"]],
					"MeanWinnerComplexity" -> N[Mean[winnerComplexities]],
					"WinnerComplexityRange" -> MinMax[winnerComplexities]
				|>,
				"Time" -> AbsoluteTime[] - t0,
				"GPU" -> gpu
			|>,
			KeyDrop[result, {"Complexities", "Winners"}]
		]
	]

iSafeFraction[subset_, total_] :=
	If[Length[total] > 0, N[Length[subset] / Length[total]], 0.]


(* ::Section::Closed:: *)
(*Fast Path: GameSpaceSurvey + single reference tournament*)


iFastPath[fsmIds_List, games_List, s_, k_, rounds_, gpu_] :=
	Module[{n = Length[fsmIds], complexities,
	        surveyResult, winners},

		(* 1 GPU call: LempelZiv complexity via Rust *)
		complexities = ComplexityMeasure[fsmIds,
			"States" -> s, "Colors" -> k, "Rounds" -> rounds,
			"GPU" -> gpu, "Method" -> "LempelZiv"];
		If[complexities === $Failed, Return[$Failed]];

		(* 1 GPU call: batched winner determination for ALL games *)
		surveyResult = GameSpaceSurvey[fsmIds, games,
			"States" -> s, "Colors" -> k, "Rounds" -> rounds, "GPU" -> gpu];
		If[surveyResult === $Failed, Return[$Failed]];

		(* Parse winners into uniform format *)
		winners = Table[
			With[{w = surveyResult["Winners"][[i]]},
				<|"Game" -> w["Game"],
				  "WinnerId" -> w["ByTotal"][[2, 1]]|>
			],
			{i, Length[surveyResult["Winners"]]}
		];

		<|"Complexities" -> complexities, "Winners" -> winners|>
	]


(* ::Section::Closed:: *)
(*Detailed Path: GameRankingClasses (8 basis tournaments)*)


iDetailedPath[fsmIds_List, games_List, s_, k_, rounds_, gpu_] :=
	Module[{n = Length[fsmIds], nGames = Length[games],
	        rankingResult, allTotals, allRankings,
	        complexities, winners},

		(* 8 GPU calls: basis tournaments + linear algebra *)
		rankingResult = GameRankingClasses[fsmIds, games,
			"States" -> s, "Colors" -> k, "Rounds" -> rounds, "GPU" -> gpu];
		If[rankingResult === $Failed, Return[$Failed]];

		allTotals = rankingResult["AllTotals"];
		allRankings = rankingResult["AllRankings"];

		(* Complexity via Rust/GPU *)
		complexities = ComplexityMeasure[fsmIds,
			"States" -> s, "Colors" -> k, "Rounds" -> rounds,
			"GPU" -> gpu, "Method" -> "LempelZiv"];
		If[complexities === $Failed, Return[$Failed]];

		(* Build winner list with scores *)
		winners = Table[
			With[{winnerIdx = allRankings[[g, 1]],
			      winnerId = fsmIds[[allRankings[[g, 1]]]]},
				<|"Game" -> games[[g]],
				  "GameIndex" -> g,
				  "WinnerId" -> winnerId,
				  "WinnerIndex" -> winnerIdx,
				  "WinnerScore" -> allTotals[[g, winnerIdx]]|>
			],
			{g, nGames}
		];

		(* AllMeans = AllTotals / n. No need to reconstruct per-opponent matrices.
		   BasisScores are available in Rankings for per-opponent reconstruction
		   if needed: perOpponent[[g]] = Sum[coeffs[[g,b]] * BasisScores[[b]], {b,8}] *)

		<|"Complexities" -> complexities,
		  "Winners" -> winners,
		  "AllTotals" -> allTotals,
		  "AllMeans" -> N[allTotals / n],
		  "Rankings" -> rankingResult|>
	]
