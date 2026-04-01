(* ::Package:: *)

Package["WolframInstitute`ProgramGames`"]

PackageImport["GeneralUtilities`"]


(* ::Section::Closed:: *)
(*Exported Symbols*)


PackageExport["TuringMachineMaxIndex"]
PackageExport["ProgramGamesSetup"]
PackageExport["ProgramGamesParallelLoad"]


(* ::Section::Closed:: *)
(*Scoped Symbols*)


PackageScope["$ProgramGamesPackageDirectory"]
PackageScope["$ProgramGamesRequiredFunctionNames"]
PackageScope["functions"]
PackageScope["iCargoBuildAndLoad"]
PackageScope["iLoadFunctions"]
PackageScope["iMissingFunctionNames"]
PackageScope["TMSearchWL"]
PackageScope["TMClassifyWL"]
PackageScope["TMTournamentWL"]
PackageScope["ProgramTournamentWL"]
PackageScope["TMMaxIndexWL"]
PackageScope["CAClassifyWL"]
PackageScope["CATournamentWL"]
PackageScope["FSMClassifyWL"]
PackageScope["FSMTournamentWL"]
PackageScope["FSMGameSurveyWL"]
PackageScope["FSMMultiTournamentWL"]
PackageScope["FSMComplexityWL"]
PackageScope["RuleArrayTournamentWL"]
PackageScope["IteratedGameWL"]
PackageScope["IteratedGameTournamentWL"]


(* ::Section::Closed:: *)
(*Usage Messages*)


TuringMachineMaxIndex::usage = "TuringMachineMaxIndex[s, k] returns the maximum TM index for (s,k) space: (2sk)^(sk) - 1.";

ProgramGamesSetup::usage = "ProgramGamesSetup[] installs required dependencies and builds the Rust library.";

ProgramGamesParallelLoad::usage = "ProgramGamesParallelLoad[] restarts parallel kernels and loads ProgramGames on all of them.";


(* ::Section::Closed:: *)
(*Directory Setup*)


$ProgramGamesPackageDirectory = DirectoryName[$InputFileName];

$ProgramGamesRequiredFunctionNames = {
	"tm_search_wl",
	"tm_classify_wl",
	"tm_tournament_wl",
	"program_tournament_wl",
	"tm_max_index_wl",
	"ca_classify_wl",
	"ca_tournament_wl",
	"fsm_classify_wl",
	"fsm_tournament_wl",
	"fsm_game_survey_wl",
	"fsm_multi_tournament_wl",
	"fsm_complexity_wl",
	"rule_array_tournament_wl",
	"iterated_game_wl",
	"iterated_game_tournament_wl"
};

iMissingFunctionNames[funcs_Association] :=
	Select[$ProgramGamesRequiredFunctionNames, !KeyExistsQ[funcs, #] &];


(* ::Section::Closed:: *)
(*TuringMachineMaxIndex*)


TuringMachineMaxIndex[s_Integer, k_Integer] := (2 s k)^(s k) - 1


(* ::Section::Closed:: *)
(*Setup*)


ProgramGamesSetup[] := (
	PacletInstall[
		"https://www.wolframcloud.com/obj/nikm/ExternalEvaluate.paclet",
		ForceVersionInstall -> True
	];
	PacletInstall[
		"https://www.wolframcloud.com/obj/nikm/PacletExtensions.paclet",
		ForceVersionInstall -> True
	];
	Needs["ExtensionCargo`"];
	SetEnvironment[
		"PATH" -> Environment["PATH"] <> ":" <> FileNameJoin[{$HomeDirectory, ".cargo", "bin"}]
	];
	With[{result = ExtensionCargo`CargoBuild[PacletObject["WolframInstitute/ProgramGames"]]},
		(* Directly reload functions from the newly built library.
		   We cannot just do functions=. here because the initial
		   SetDelayed (functions := functions = ...) was already
		   replaced by Set on first evaluation, so Unset would
		   leave functions permanently undefined. *)
		functions = iLoadFunctions[];
		result
	]
)


(* ::Section::Closed:: *)
(*Parallel Loading*)


ProgramGamesParallelLoad[] := (
	CloseKernels[];
	LaunchKernels[];
	ParallelEvaluate[
		Quiet[PacletDataRebuild[], PacletDataRebuild::lock];
		<< WolframInstitute`TuringMachine`;
		<< WolframInstitute`ProgramGames`
	]
)

ProgramGamesParallelLoad[n_Integer] := (
	CloseKernels[];
	LaunchKernels[n];
	ParallelEvaluate[
		Quiet[PacletDataRebuild[], PacletDataRebuild::lock];
		<< WolframInstitute`TuringMachine`;
		<< WolframInstitute`ProgramGames`
	]
)


(* ::Section::Closed:: *)
(*Cargo Library Loading*)


iCargoBuildAndLoad[paclet_] := Replace[
	ExtensionCargo`CargoBuild[paclet], {
		f : Except[{__ ? FileExistsQ}] :> (
			Function @ Function @ Failure["CargoBuildError", <|
				"MessageTemplate" -> "Cargo build failed",
				"Return" -> f
			|>]
		),
		files_ :> Replace[
			ExtensionCargo`CargoLoad[files, "Functions"],
			f : Except[_ ? AssociationQ] :>
				Function @ Function @ Failure["CargoLoadError", <|
					"MessageTemplate" -> "Cargo load failed",
					"Return" -> f
				|>]
		]
	}
];

iLoadFunctions[] := With[{paclet = PacletObject["WolframInstitute/ProgramGames"]},
	(
		If[!PacletObjectQ[PacletObject["PacletExtensions"]],
			PacletInstall["https://www.wolframcloud.com/obj/nikm/PacletExtensions.paclet"]
		];
		Needs["ExtensionCargo`"];
		SetEnvironment[
			"PATH" -> Environment["PATH"] <> ":" <> FileNameJoin[{$HomeDirectory, ".cargo", "bin"}]
		];
		(* Do not trust CargoLoad just because it returns an Association.
		   ResourceDefinition.nb builds can embed stale cargo manifests/targets
		   that are missing newer exports such as the CA functions. *)
		Replace[
			ExtensionCargo`CargoLoad[
				paclet,
				"Functions"
			], {
				funcs_ ? AssociationQ /; iMissingFunctionNames[funcs] === {} :> funcs,
				_ :> iCargoBuildAndLoad[paclet]
			}
		]
	)
] // Replace[{
	funcs_ ? AssociationQ :>
		With[{
			wrapped = Association @ KeyValueMap[
				#1 -> Composition[
					Replace[LibraryFunctionError[error_, code_] :>
						Failure["RustError", <|
							"MessageTemplate" -> "Rust error: `` (``)",
							"MessageParameters" -> {error, code},
							"Error" -> error, "ErrorCode" -> code, "Function" -> #1
						|>]
					],
					#2
				] &,
				funcs
			],
			missing = iMissingFunctionNames[funcs]
		},
			Join[
				wrapped,
				AssociationThread[
					missing,
					With[{name = #},
						Function[Failure["MissingCargoFunction", <|
							"MessageTemplate" -> "Rust function `` is missing from the loaded cargo manifest.",
							"MessageParameters" -> {name},
							"Function" -> name
						|>]]
					] & /@ missing
				]
			]
		]
}]

functions := functions = iLoadFunctions[]


(* ::Section::Closed:: *)
(*Function Bindings*)


TMSearchWL := functions["tm_search_wl"]
TMClassifyWL := functions["tm_classify_wl"]
TMTournamentWL := functions["tm_tournament_wl"]
ProgramTournamentWL := functions["program_tournament_wl"]
TMMaxIndexWL := functions["tm_max_index_wl"]
CAClassifyWL := functions["ca_classify_wl"]
CATournamentWL := functions["ca_tournament_wl"]
FSMClassifyWL := functions["fsm_classify_wl"]
FSMTournamentWL := functions["fsm_tournament_wl"]
FSMGameSurveyWL := functions["fsm_game_survey_wl"]
FSMMultiTournamentWL := functions["fsm_multi_tournament_wl"]
FSMComplexityWL := functions["fsm_complexity_wl"]
RuleArrayTournamentWL := functions["rule_array_tournament_wl"]
IteratedGameWL := functions["iterated_game_wl"]
IteratedGameTournamentWL := functions["iterated_game_tournament_wl"]
