(* ::Package:: *)

Package["WolframInstitute`ProgramGames`"]


(* ::Section::Closed:: *)
(*Exported Symbols*)


PackageExport["RuleArrayTournament"]
PackageExport["RuleArrayEvolve"]
PackageExport["RuleArrayPlot"]
PackageExport["RuleArrayExplain"]
PackageExport["RuleArraySpaceTable"]
PackageExport["RuleArrayStrategy"]


(* ::Section::Closed:: *)
(*Usage Messages*)


RuleArrayTournament::usage = "RuleArrayTournament[ruleArrays] runs a tournament among rule array strategies. \
Each rule array is a list of CA rule numbers applied step-by-step (inhomogeneous CA). \
Options: \"Colors\", \"Radius\", \"Steps\", \"Rounds\", \"Game\".";

RuleArrayEvolve::usage = "RuleArrayEvolve[baseRules, steps, fitnessFunc] evolves rule arrays using \
hill-climbing mutation. Returns a list of {ruleArray, fitness} pairs. \
Options: \"Population\", \"Generations\", \"Colors\", \"Radius\".";

RuleArrayPlot::usage = "RuleArrayPlot[ruleArray, baseRules] shows a color-coded visualization \
of a rule array strategy. Options: \"Radius\", \"ImageSize\".";

RuleArrayExplain::usage = "RuleArrayExplain[ruleArray] displays a table explaining each rule \
in the rule array: its binary encoding, neighborhood outputs, and logical character.";

RuleArraySpaceTable::usage = "RuleArraySpaceTable[k, r] displays a table showing the rule array \
space size for each step count and its feasibility for exhaustive or evolutionary exploration.";

RuleArrayStrategy::usage = "RuleArrayStrategy[ruleArray, k, r] creates a strategy spec \
for use with ProgramTournament. Returns {\"RA\", ruleArray, k, r, Length[ruleArray]}.";


(* ::Section::Closed:: *)
(*RuleArraySpaceTable*)


Options[RuleArraySpaceTable] = {"MaxSteps" -> 12};

RuleArraySpaceTable[k_Integer : 2, r_ : 1/2, opts : OptionsPattern[]] :=
	Module[{nRules, maxT, rows, header},
		nRules = k^(k^(Round[2 r] + 1));
		maxT = OptionValue["MaxSteps"];

		rows = Table[
			With[{space = nRules^t,
				feasibility = Which[
					nRules^t <= 256, "exhaustive — classify + full tournament",
					nRules^t <= 10000, "exhaustive — classify + tournament on reps",
					nRules^t <= 100000, "exhaustive classify, sample tournament",
					nRules^t <= 10000000, "sample classify + tournament",
					True, "evolutionary / random sample only"
				]},
				{t,
					If[space > 10^9, ShortNum[space],
						NumberForm[space, DigitBlock -> 3]],
					feasibility}
			],
			{t, 1, maxT}
		];

		header = Style[#, Bold] & /@ {"Steps (t)",
			Row[{"Space (", nRules, Superscript["", "t"], ")"}], "Feasibility"};

		Column[{
			Style[Row[{"Rule Array Space (k=", k, ", r=", r,
				", ", nRules, " rules per step)"}], Bold, 13],
			Spacer[5],
			Grid[Prepend[rows, header],
				Frame -> All,
				Background -> {None, {LightBlue, None}},
				Alignment -> {{Center, Right, Left}},
				Spacings -> {2, 0.6},
				BaseStyle -> {FontFamily -> "Source Code Pro", FontSize -> 11}
			]
		}, Spacer[3]]
	]


(* ::Section::Closed:: *)
(*RuleArrayStrategy*)


RuleArrayStrategy[rules_List, k_Integer : 2, r_ : 1/2] :=
	{"RA", rules, k, r, Length[rules]}


(* ::Section::Closed:: *)
(*RuleArrayExplain*)


Options[RuleArrayExplain] = {"Radius" -> 1/2, "Colors" -> 2};

(* Descriptions for the 16 r=1/2 k=2 rules *)
$RuleR12Names = <|
	0 -> "always 0 (cooperate)",
	1 -> "AND",
	2 -> "left AND NOT right",
	3 -> "identity of left",
	4 -> "NOT left AND right",
	5 -> "identity of right",
	6 -> "XOR",
	7 -> "OR",
	8 -> "NOR",
	9 -> "XNOR (equality)",
	10 -> "NOT right",
	11 -> "left OR NOT right",
	12 -> "NOT left",
	13 -> "NOT left OR right",
	14 -> "NAND",
	15 -> "always 1 (defect)"
|>;

RuleArrayExplain[ruleArray_List, opts : OptionsPattern[]] :=
	Module[{k, r, twoR, nInputs, ruleData, header, rows, pipeline},
		k = OptionValue["Colors"];
		r = OptionValue["Radius"];
		twoR = Round[2 r];
		nInputs = k^(twoR + 1);

		ruleData[rule_] := Lookup[$RuleR12Names, rule, "rule " <> ToString[rule]];

		(* Table rows *)
		rows = MapIndexed[
			With[{step = #2[[1]], rule = #1, bits = IntegerDigits[#1, k, nInputs],
				name = ruleData[#1],
				nbhd = If[OddQ[#2[[1]]], "center + right", "left + center"]},
				{
					Style[step, Bold],
					Style[rule, Bold],
					StringJoin[ToString /@ bits],
					nbhd,
					name
				}
			] &,
			ruleArray
		];

		header = Style[#, Bold] & /@ {"Step", "Rule", "Binary", "Looks at", "Effect"};

		Grid[Prepend[rows, header],
			Frame -> All,
			Background -> {None, {LightBlue, None}},
			Alignment -> {{Center, Center, Center, Left, Left}},
			Spacings -> {1.5, 0.6},
			BaseStyle -> {FontFamily -> "Source Code Pro", FontSize -> 11}
		]
	]


(* ::Section::Closed:: *)
(*RuleArrayTournament*)


Options[RuleArrayTournament] = {
	"Colors" -> 2, "Radius" -> 1/2, "Steps" -> 10,
	"Rounds" -> 100, "Game" -> "pd"
};

RuleArrayTournament[ruleArrays_List, opts : OptionsPattern[]] :=
	Module[{k, r, rNum, rDen, t, gameStr, resultJSON, result},
		k = OptionValue["Colors"];
		r = OptionValue["Radius"];
		t = OptionValue["Steps"];
		{rNum, rDen} = If[IntegerQ[r], {r, 1}, Through[{Numerator, Denominator}[r]]];
		gameStr = PayoffToString[OptionValue["Game"]];
		resultJSON = RuleArrayTournamentWL[k, rNum, rDen, t,
			OptionValue["Rounds"],
			gameStr, ExportString[ruleArrays, "RawJSON"], False];
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
(*RuleArrayEvolve*)


Options[RuleArrayEvolve] = {
	"Population" -> 50,
	"Generations" -> 200,
	"Colors" -> 2,
	"Radius" -> 1/2,
	"Steps" -> 10,
	"Rounds" -> 100,
	"Game" -> "pd",
	"MutationRate" -> 1,
	"Seed" -> Automatic
};

RuleArrayEvolve[baseRules_List, opts : OptionsPattern[]] :=
	Module[{pop, nPop, nGen, k, r, t, rounds, game, nRules,
		mutRate, seed, fitness, generation, best, mutate},
		nPop = OptionValue["Population"];
		nGen = OptionValue["Generations"];
		k = OptionValue["Colors"];
		r = OptionValue["Radius"];
		t = OptionValue["Steps"];
		rounds = OptionValue["Rounds"];
		game = OptionValue["Game"];
		mutRate = OptionValue["MutationRate"];
		seed = OptionValue["Seed"];
		nRules = Length[baseRules];

		If[seed =!= Automatic, SeedRandom[seed]];

		(* Mutation: flip mutRate random positions to different rules *)
		mutate[ra_List] := Module[{m = ra, pos, old, new},
			Do[
				pos = RandomInteger[{1, Length[m]}];
				old = m[[pos]];
				new = RandomChoice[DeleteCases[baseRules, old]];
				m[[pos]] = new,
				{mutRate}
			];
			m
		];

		(* Fitness: total score in round-robin tournament *)
		fitness[population_List] := Module[{result, ranking},
			result = RuleArrayTournament[population,
				"Colors" -> k, "Radius" -> r, "Steps" -> t,
				"Rounds" -> rounds, "Game" -> game];
			If[result === $Failed, Return[ConstantArray[0, Length[population]]]];
			(* Extract total scores in original order *)
			ranking = result["Ranking"];
			Table[
				With[{idx = i},
					SelectFirst[ranking, MatchQ[#["label"], _] &, <|"total" -> 0|>]["total"]
				],
				{i, Length[population]}
			]
		];

		(* Initialize random population *)
		pop = Table[
			Table[RandomChoice[baseRules], {t}],
			{nPop}
		];

		(* Evolve: each generation, try mutations, keep improvements *)
		Do[
			Module[{scores, sortedIdx, topHalf, newPop},
				(* Evaluate fitness *)
				scores = Module[{result, ranking, labels, scoreMap},
					result = RuleArrayTournament[pop,
						"Colors" -> k, "Radius" -> r, "Steps" -> t,
						"Rounds" -> rounds, "Game" -> game];
					If[result === $Failed, ConstantArray[0, nPop],
						ranking = result["Ranking"];
						(* Scores come back sorted by ranking, map back to pop order *)
						Table[ranking[[i]]["total"], {i, Length[ranking]}]
					]
				];

				(* Sort by fitness (higher is better for PD where scores are negative) *)
				sortedIdx = Ordering[scores, All, Greater];

				(* Keep top half, mutate to fill bottom half *)
				topHalf = Ceiling[nPop / 2];
				newPop = Join[
					pop[[sortedIdx[[;; topHalf]]]],
					mutate /@ pop[[sortedIdx[[;; topHalf]]]]
				][[;; nPop]];

				pop = newPop;

				If[Mod[gen, 10] == 0 || gen == nGen,
					Print["  Gen ", gen, ": best=", Max[scores],
						" mean=", NumberForm[Mean[N@scores], {4, 1}]]
				];
			],
			{gen, nGen}
		];

		(* Final evaluation *)
		Module[{finalResult},
			finalResult = RuleArrayTournament[pop,
				"Colors" -> k, "Radius" -> r, "Steps" -> t,
				"Rounds" -> rounds, "Game" -> game];
			<|
				"Population" -> pop,
				"Tournament" -> finalResult,
				"BaseRules" -> baseRules
			|>
		]
	]


(* ::Section::Closed:: *)
(*RuleArrayPlot*)


Options[RuleArrayPlot] = {
	"Radius" -> 1/2,
	"Colors" -> 2,
	"InitialState" -> Automatic,
	"Width" -> 15,
	ImageSize -> 500
};

(* Run the rule array on an initial state, return all rows *)
iRuleArrayEvolve[ruleArray_List, initialState_List, k_Integer, r_] :=
	Module[{state, states, width, twoR, neighborhood},
		twoR = Round[2 r];
		neighborhood = twoR + 1;
		width = Length[initialState];
		state = initialState;
		states = {state};

		Do[
			Module[{rule, newState},
				rule = If[step <= Length[ruleArray], ruleArray[[step]], Last[ruleArray]];
				(* Decode rule to lookup table *)
				newState = Table[
					Module[{left, center, right, input, table},
						left = state[[Mod[j - 2, width] + 1]];
						center = state[[j]];
						right = state[[Mod[j, width] + 1]];
						table = IntegerDigits[rule, k, k^neighborhood];
						input = If[twoR == 1,
							(* Alternating neighborhood *)
							If[OddQ[step],
								center k + right,   (* odd step: center, right *)
								left k + center     (* even step: left, center *)
							],
							(* r=1: full neighborhood *)
							left k^2 + center k + right
						];
						table[[k^neighborhood - input]]
					],
					{j, width}
				];
				state = newState;
				AppendTo[states, state];
			],
			{step, Length[ruleArray]}
		];

		states
	]

RuleArrayPlot[ruleArray_List, opts : OptionsPattern[]] :=
	Module[{k, r, width, init, states, uniqueRules, ruleColors,
		stateGrid, ruleStrip, combined, t},
		k = OptionValue["Colors"];
		r = OptionValue["Radius"];
		width = OptionValue["Width"];
		t = Length[ruleArray];

		(* Initial state: center cell = 1 *)
		init = Replace[OptionValue["InitialState"], {
			Automatic :> CenterArray[1, width],
			s_List :> s
		}];
		width = Length[init];

		(* Run the evolution *)
		states = iRuleArrayEvolve[ruleArray, init, k, r];

		(* Rule color map *)
		uniqueRules = Sort @ DeleteDuplicates[ruleArray];
		ruleColors = AssociationThread[
			uniqueRules,
			ColorData["DarkRainbow"] /@ Subdivide[0, 1, Max[Length[uniqueRules] - 1, 1]]
		];

		(* Layout: t+1 rows total (init + t steps), drawn top to bottom.
		   Row 0 (top) = initial state, label "init"
		   Row s (s=1..t) = state after applying ruleArray[[s]], label = rule number
		   Rule label + background tint on each output row *)
		Graphics[{
			Table[
				With[{y = t - row, rowData = states[[row + 1]]},
					{
						(* Rule-colored background tint for step rows *)
						If[row >= 1,
							{Opacity[0.15], Lookup[ruleColors, ruleArray[[row]], White],
								Rectangle[{0, y}, {width, y + 1}]},
							(* Init row: light gray background *)
							{Opacity[0.06], GrayLevel[0.5],
								Rectangle[{0, y}, {width, y + 1}]}
						],
						(* Black cells *)
						Table[
							If[rowData[[col]] == 1,
								{Black, Rectangle[{col - 1, y}, {col, y + 1}]},
								{}
							],
							{col, width}
						]
					}
				],
				{row, 0, t}
			],
			(* Grid lines *)
			Table[{GrayLevel[0.8], Thin, Line[{{0, y}, {width, y}}]}, {y, 0, t + 1}],
			Table[{GrayLevel[0.8], Thin, Line[{{col, 0}, {col, t + 1}}]}, {col, 0, width}],
			(* Rule labels: each label sits at the OUTPUT row of that rule *)
			Table[
				With[{y = t - step},
					{Lookup[ruleColors, ruleArray[[step]], Gray],
						EdgeForm[GrayLevel[0.4]],
						Rectangle[{-1.8, y}, {-0.2, y + 1}],
						White,
						Text[Style[ruleArray[[step]], Bold,
							FontSize -> Max[7, Min[14, 250 / t]]],
							{-1, y + 0.5}]}
				],
				{step, t}
			],
			(* "init" label *)
			{GrayLevel[0.6], EdgeForm[GrayLevel[0.4]],
				Rectangle[{-1.8, t}, {-0.2, t + 1}],
				White,
				Text[Style["init", Italic, FontSize -> Max[7, Min[11, 200 / t]]],
					{-1, t + 0.5}]},
			(* Highlight the OUTPUT CELL and show value *)
			With[{outputVal = Last[Last[states]]},
				{Red, AbsoluteThickness[2],
					EdgeForm[{Red, AbsoluteThickness[2]}],
					FaceForm[None],
					Rectangle[{width - 1, 0}, {width, 1}],
					Black,
					Text[Style[Row[{"\[UpArrow] output = ", outputVal}], FontSize -> 9],
						{width - 0.5, -0.5}]
				}
			]
		},
			PlotRange -> All,
			PlotLabel -> Style[Row[{"Rule Array (", t, " steps, width ", width, ")"}], 11],
			ImageSize -> OptionValue[ImageSize],
			PlotRangePadding -> {{Scaled[0.02], Scaled[0.02]}, {Scaled[0.05], Scaled[0.02]}}
		]
	]
