(* ::Package:: *)

Package["WolframInstitute`ProgramGames`"]


(* ::Section::Closed:: *)
(*Visualization — survey and classification tables*)


(* ::Section::Closed:: *)
(*Exported Symbols*)


PackageExport["SpaceSurveyTable"]
PackageExport["ClassificationTable"]
PackageExport["ShortNum"]


(* ::Section::Closed:: *)
(*Usage Messages*)


ShortNum::usage = "ShortNum[n] formats a large number with K/M/B/T suffix.";

SpaceSurveyTable::usage = "SpaceSurveyTable[] generates a table of TM space sizes, \
halting counts, and halting rates. Accepts a list of {s,k} pairs.";

ClassificationTable::usage = "ClassificationTable[] generates a table showing \
behavioral classification results: unique behaviors, reduction factors, and tournament pair counts.";


(* ::Section::Closed:: *)
(*ShortNum*)


ShortNum[n_?NumericQ] := Which[
	Abs[n] >= 10^12, Row[{NumberForm[N[n / 10^12], {3, 1}], "T"}],
	Abs[n] >= 10^9,  Row[{NumberForm[N[n / 10^9], {3, 1}], "B"}],
	Abs[n] >= 10^6,  Row[{NumberForm[N[n / 10^6], {3, 1}], "M"}],
	Abs[n] >= 10^3,  Row[{NumberForm[N[n / 10^3], {3, 1}], "K"}],
	True, n
]


(* ::Section::Closed:: *)
(*SpaceSurveyTable*)


Options[SpaceSurveyTable] = {
	"MaxSteps" -> 500,
	"Depth" -> 4,
	"Sample" -> 100000
};


$DefaultSpaces = {{2, 2}, {3, 2}, {2, 3}, {4, 2}, {3, 3}, {5, 2}};


SpaceSurveyTable[opts : OptionsPattern[]] := SpaceSurveyTable[$DefaultSpaces, opts]
SpaceSurveyTable[spaces_List, opts : OptionsPattern[]] :=
	Module[{results, maxsteps, depth, sample},
		maxsteps = OptionValue["MaxSteps"];
		depth = OptionValue["Depth"];
		sample = OptionValue["Sample"];

		results = Table[
			Module[{s = sp[[1]], k = sp[[2]], maxIdx, ids, timing, halting, searched, rate},
				maxIdx = TuringMachineMaxIndex[s, k];
				timing = First @ AbsoluteTiming[
					ids = TuringMachineProgramSearch[s, k,
						"MaxSteps" -> maxsteps, "Depth" -> depth,
						"Sample" -> If[maxIdx + 1 > 10000000, sample, "all"]];
				];
				halting = Length[ids];
				searched = If[maxIdx + 1 > 10000000, sample, maxIdx + 1];
				rate = N[halting / searched];
				<|"Space" -> StringForm["TM(``,``)", s, k],
					"Size" -> maxIdx + 1, "Searched" -> searched,
					"Depth" -> depth, "Halting" -> halting,
					"HaltingRate" -> rate, "Time" -> timing|>
			],
			{sp, spaces}
		];

		Grid[
			Prepend[
				Table[
					With[{r = row, rate = row["HaltingRate"]},
						{r["Space"],
						If[r["Size"] > 10^7,
							Row[{NumberForm[N[r["Size"]], {3, 1}], " (",
								NumberForm[r["Searched"], DigitBlock -> 3], " sampled)"}],
							NumberForm[r["Size"], DigitBlock -> 3]],
						r["Depth"],
						NumberForm[r["Halting"], DigitBlock -> 3],
						If[r["Size"] > 10^7,
							Row[{"\[TildeTilde]", ShortNum[Round[r["Size"] (1 - rate)]]}],
							NumberForm[r["Size"] - r["Halting"], DigitBlock -> 3]],
						NumberForm[100 rate, {4, 1}],
						NumberForm[r["Time"], {3, 2}]}
					],
					{row, results}
				],
				Style[#, Bold] & /@ {
					"Space", "Size", "Depth", "Halting", "Non-halting", "Halting %", "Time (s)"}
			],
			Frame -> All,
			Alignment -> {{Left, Right, Center, Right, Right, Right, Right}},
			Background -> {None, {LightBlue, None}},
			Spacings -> {2, 0.8},
			BaseStyle -> {FontFamily -> "Source Code Pro", FontSize -> 12}
		]
	]


(* ::Section::Closed:: *)
(*ClassificationTable*)


Options[ClassificationTable] = {
	"MaxSteps" -> 500,
	"Depth" -> 4,
	"Sample" -> 100000
};


ClassificationTable[opts : OptionsPattern[]] := ClassificationTable[$DefaultSpaces, opts]
ClassificationTable[spaces_List, opts : OptionsPattern[]] :=
	Module[{results, maxsteps, depth, sample},
		maxsteps = OptionValue["MaxSteps"];
		depth = OptionValue["Depth"];
		sample = OptionValue["Sample"];

		results = Table[
			Module[{s = sp[[1]], k = sp[[2]], c, timing, maxIdx},
				maxIdx = TuringMachineMaxIndex[s, k];
				timing = First @ AbsoluteTiming[
					c = TuringMachineClassify[s, k,
						"MaxSteps" -> maxsteps, "Depth" -> depth,
						"Sample" -> If[maxIdx + 1 > 10000000, sample, "all"]];
				];
				If[c === $Failed,
					<|"Space" -> StringForm["TM(``,``)", s, k],
						"SpaceSize" -> maxIdx + 1,
						"Halting" -> "?", "Unique" -> "?", "Reduction" -> "?",
						"UniquePairs" -> "?", "FullPairs" -> "?",
						"Time" -> timing, "Method" -> "failed"|>,
					<|"Space" -> StringForm["TM(``,``)", s, k],
						"SpaceSize" -> maxIdx + 1,
						"Halting" -> c["TotalHalting"],
						"Unique" -> c["UniqueCount"],
						"Reduction" -> c["ReductionFactor"],
						"UniquePairs" -> c["UniqueCount"] (c["UniqueCount"] - 1),
						"FullPairs" -> c["TotalHalting"] (c["TotalHalting"] - 1),
						"Time" -> timing,
						"Method" -> If[maxIdx + 1 > 10000000, "sampled", "exhaustive"]|>
				]
			],
			{sp, spaces}
		];

		Grid[
			Prepend[
				Table[
					With[{r = row},
						{r["Space"],
						If[NumberQ[r["SpaceSize"]],
							NumberForm[r["SpaceSize"], DigitBlock -> 3], "?"],
						If[IntegerQ[r["Halting"]],
							NumberForm[r["Halting"], DigitBlock -> 3], r["Halting"]],
						If[IntegerQ[r["Unique"]],
							NumberForm[r["Unique"], DigitBlock -> 3], r["Unique"]],
						If[NumberQ[r["Reduction"]],
							Row[{NumberForm[r["Reduction"], {4, 1}], "x"}], r["Reduction"]],
						If[IntegerQ[r["UniquePairs"]] && IntegerQ[r["FullPairs"]],
							Row[{ShortNum[r["UniquePairs"]], " vs ",
								ShortNum[r["FullPairs"]]}], "?"],
						If[NumberQ[r["Time"]],
							Row[{NumberForm[r["Time"], {3, 2}], "s"}], "?"],
						If[StringQ[r["Method"]], r["Method"], "?"]}
					],
					{row, results}
				],
				Style[#, Bold] & /@ {
					"Space", "Total TMs", "Halting", "Unique", "Reduction",
					"Tournament pairs", "Time", "Method"}
			],
			Frame -> All,
			Alignment -> {{Left, Right, Right, Right, Right, Center, Right, Left}},
			Background -> {None, {LightBlue, None}},
			Spacings -> {2, 0.8},
			BaseStyle -> {FontFamily -> "Source Code Pro", FontSize -> 12}
		]
	]
