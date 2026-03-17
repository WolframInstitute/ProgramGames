#!/usr/bin/env wolframscript

(* Install build dependencies if needed *)
If[!PacletObjectQ[PacletObject["PacletExtensions"]],
    PacletInstall["https://www.wolframcloud.com/obj/nikm/PacletExtensions.paclet"]
];

<< ExtensionCargo`

publisher = "WolframInstitute"
name = "ProgramGames"

(* Ensure cargo is in PATH *)
SetEnvironment[
    "PATH" -> Environment["PATH"] <> ":" <> FileNameJoin[{$HomeDirectory, ".cargo", "bin"}]
];

(* Build Rust library *)
Print["Building Rust library..."]
RunProcess[
    {"cargo", "build", "--release"},
    ProcessDirectory -> FileNameJoin[{name, "Lib", "tm-search"}]
] // (Print["cargo exit code: ", #["ExitCode"]]; #) &;

(* Load paclet and collect binaries *)
PacletDirectoryLoad[name]
paclet = PacletObject[publisher <> "/" <> name]

Print["Collecting binaries..."]
Quiet[DeleteDirectory[FileNameJoin[{name, "Binaries"}], DeleteContents -> True]];
CargoCollect[ParentDirectory[paclet["Location"]], FileNameJoin[{name, "Binaries"}]]

(* Create paclet archive *)
Print["Creating paclet archive..."]
pacletFile = CreatePacletArchive[name]
Echo[StringTemplate["Paclet `` has size ``"][pacletFile, FileSize[pacletFile]]]

PacletDirectoryUnload[name]

(* Install locally *)
PacletInstall[pacletFile, ForceVersionInstall -> True]
Print["Done. Paclet installed."]
