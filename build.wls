#!/usr/bin/env wolframscript

(* Install build dependencies if needed *)
If[!PacletObjectQ[PacletObject["PacletExtensions"]],
    PacletInstall["https://www.wolframcloud.com/obj/nikm/PacletExtensions.paclet"]
];

<< ExtensionCargo`

scriptDir = DirectoryName[$InputFileName];
publisher = "WolframInstitute";
name = "ProgramGames";
localPacletDir = FileNameJoin[{scriptDir, name}];

SetDirectory[scriptDir];

(* Ensure cargo is in PATH *)
SetEnvironment[
    "PATH" -> Environment["PATH"] <> ":" <> FileNameJoin[{$HomeDirectory, ".cargo", "bin"}]
];

(* Build Rust library *)
Print["Building Rust library..."]
PacletDirectoryLoad[localPacletDir];
paclet = SelectFirst[
    PacletFind[publisher <> "/" <> name],
    ExpandFileName[#["Location"]] === ExpandFileName[localPacletDir] &
];

If[MissingQ[paclet],
    Print["Could not resolve local paclet at ", localPacletDir];
    Exit[1]
];

buildResult = CargoBuild[paclet];
If[!MatchQ[buildResult, {__ ? FileExistsQ}],
    Print["CargoBuild failed: ", InputForm[buildResult]];
    Exit[1]
];

Print["Collecting binaries..."]
Quiet[DeleteDirectory[FileNameJoin[{name, "Binaries"}], DeleteContents -> True]];
CargoCollect[scriptDir, FileNameJoin[{name, "Binaries"}]];

(* Create paclet archive *)
Print["Creating paclet archive..."]
pacletFile = CreatePacletArchive[name];
Echo[StringTemplate["Paclet `` has size ``"][pacletFile, FileSize[pacletFile]]];

PacletDirectoryUnload[localPacletDir];

(* Install locally *)
PacletInstall[pacletFile, ForceVersionInstall -> True];
Print["Done. Paclet installed."]
