#!/usr/bin/env wolframscript

<< ExtensionCargo`

publisher = "WolframInstitute"
name = "ProgramGames"

SetDirectory[DirectoryName[$InputFileName]];

(* Ensure cargo is in PATH *)
SetEnvironment[
    "PATH" -> Environment["PATH"] <> ":" <> FileNameJoin[{$HomeDirectory, ".cargo", "bin"}]
];

(* Build Rust library from the OUTER workspace.
   This puts artifacts in ProgramGames/target/ (outside the paclet dir)
   so they don't bloat the archive. *)
Print["Building Rust library..."]
RunProcess[{"cargo", "build", "--release"}] //
    (Print["  cargo exit code: ", #["ExitCode"]]; #) &;

(* Load paclet and collect binaries *)
PacletDirectoryLoad[name]
paclet = PacletObject[publisher <> "/" <> name]

Print["Collecting binaries..."]
Quiet[DeleteDirectory[FileNameJoin[{name, "Binaries"}], DeleteContents -> True]];
CargoCollect[ParentDirectory[paclet["Location"]], FileNameJoin[{name, "Binaries"}]]

(* Clean any inner build artifacts before archiving *)
Quiet[DeleteDirectory[FileNameJoin[{name, "build"}], DeleteContents -> True]];
Quiet[DeleteDirectory[FileNameJoin[{name, "Lib", "target"}], DeleteContents -> True]];

(* Create paclet archive *)
Print["Creating paclet archive..."]
buildDir = "build";
If[!DirectoryQ[buildDir], CreateDirectory[buildDir]];
pacletFile = CreatePacletArchive[name, buildDir]
Echo[StringTemplate["Paclet `` has size ``"][pacletFile, FileSize[pacletFile]]]

PacletDirectoryUnload[name]

(* Install locally *)
PacletInstall[pacletFile, ForceVersionInstall -> True]
Print["Done. Paclet installed: ", pacletFile]
