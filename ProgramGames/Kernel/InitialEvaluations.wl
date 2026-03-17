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
PackageScope["functions"]
PackageScope["TMSearchWL"]
PackageScope["TMClassifyWL"]
PackageScope["TMTournamentWL"]
PackageScope["ProgramTournamentWL"]
PackageScope["TMMaxIndexWL"]


(* ::Section::Closed:: *)
(*Usage Messages*)


TuringMachineMaxIndex::usage = "TuringMachineMaxIndex[s, k] returns the maximum TM index for (s,k) space: (2sk)^(sk) - 1.";

ProgramGamesSetup::usage = "ProgramGamesSetup[] installs required dependencies and builds the Rust library.";

ProgramGamesParallelLoad::usage = "ProgramGamesParallelLoad[] restarts parallel kernels and loads ProgramGames on all of them.";


(* ::Section::Closed:: *)
(*Directory Setup*)


$ProgramGamesPackageDirectory = DirectoryName[$InputFileName];


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
	ExtensionCargo`CargoBuild[PacletObject["WolframInstitute/ProgramGames"]]
)


(* ::Section::Closed:: *)
(*Parallel Loading*)


ProgramGamesParallelLoad[] := (
	CloseKernels[];
	LaunchKernels[];
	ParallelEvaluate[
		<< WolframInstitute`TuringMachine`;
		<< WolframInstitute`ProgramGames`
	]
)


(* ::Section::Closed:: *)
(*Cargo Library Loading*)


functions := functions = (
	If[!PacletObjectQ[PacletObject["PacletExtensions"]],
		PacletInstall["https://www.wolframcloud.com/obj/nikm/PacletExtensions.paclet"]
	];
	Needs["ExtensionCargo`"];
	SetEnvironment[
		"PATH" -> Environment["PATH"] <> ":" <> FileNameJoin[{$HomeDirectory, ".cargo", "bin"}]
	];
	Replace[
		ExtensionCargo`CargoLoad[
			PacletObject["WolframInstitute/ProgramGames"],
			"Functions"
		],
		Except[_ ? AssociationQ] :> Replace[
			ExtensionCargo`CargoBuild[PacletObject["WolframInstitute/ProgramGames"]], {
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
		]
	]
) // Replace[{
	funcs_ ? AssociationQ :>
		Association @ KeyValueMap[
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
		]
}]


(* ::Section::Closed:: *)
(*Function Bindings*)


TMSearchWL := functions["tm_search_wl"]
TMClassifyWL := functions["tm_classify_wl"]
TMTournamentWL := functions["tm_tournament_wl"]
ProgramTournamentWL := functions["program_tournament_wl"]
TMMaxIndexWL := functions["tm_max_index_wl"]
