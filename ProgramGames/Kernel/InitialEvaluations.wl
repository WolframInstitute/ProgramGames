(* ::Package:: *)

(* ::Section::Closed:: *)
(*PackageExported*)


PackageExported[
	{
		ProgramGamesBinary,
		ProgramGamesBuild,
		TuringMachineMaxIndex
	}
];


(* ::Section::Closed:: *)
(*PackageScoped*)


PackageScoped[
	{
		$ProgramGamesPackageDirectory,
		$ProgramGamesLibDirectory,
		$TMSearchBin,
		iRunBinary
	}
];


(* ::Section::Closed:: *)
(*Usage Messages*)


ProgramGamesBinary::usage = "ProgramGamesBinary[] returns the path to the tm-search binary, or $Failed if not found.";
ProgramGamesBuild::usage = "ProgramGamesBuild[] builds the tm-search binary from source using cargo.";
TuringMachineMaxIndex::usage = "TuringMachineMaxIndex[s, k] returns the maximum TM index for (s,k) space: (2sk)^(sk) - 1.";


(* ::Section::Closed:: *)
(*Directory Setup*)


$ProgramGamesPackageDirectory = DirectoryName[$InputFileName];
$ProgramGamesLibDirectory = FileNameJoin[{DirectoryName[$ProgramGamesPackageDirectory], "Lib"}];


(* ::Section::Closed:: *)
(*TuringMachineMaxIndex*)


TuringMachineMaxIndex[s_Integer, k_Integer] := (2 s k)^(s k) - 1


(* ::Section::Closed:: *)
(*Binary Discovery*)


ProgramGamesBinary[] := Module[{candidate},
	candidate = FileNameJoin[{$ProgramGamesLibDirectory, "tm-search", "target", "release", "tm-search"}];
	If[FileExistsQ[candidate], Return[candidate]];
	With[{which = Quiet @ RunProcess[{"which", "tm-search"}]},
		If[AssociationQ[which] && which["ExitCode"] === 0,
			Return[StringTrim[which["StandardOutput"]]]
		]
	];
	$Failed
]


ProgramGamesBuild[] := Module[{srcDir, proc},
	srcDir = FileNameJoin[{$ProgramGamesLibDirectory, "tm-search"}];
	If[!DirectoryQ[srcDir], Return[$Failed]];
	proc = RunProcess[{"cargo", "build", "--release"}, ProcessDirectory -> srcDir];
	If[proc["ExitCode"] === 0,
		$TMSearchBin = ProgramGamesBinary[],
		$Failed
	]
]


(* ::Section::Closed:: *)
(*Cached Binary Path*)


$TMSearchBin := $TMSearchBin = ProgramGamesBinary[]


(* ::Section::Closed:: *)
(*Internal Binary Runner*)


iRunBinary[args_List] := iRunBinary[args, ""]

iRunBinary[args_List, stdin_String] := Module[{proc, stdout},
	If[$TMSearchBin === $Failed, Return[$Failed]];
	proc = If[stdin === "",
		RunProcess[Prepend[args, $TMSearchBin]],
		RunProcess[Prepend[args, $TMSearchBin], "StandardOutput", stdin]
	];
	stdout = If[StringQ[proc], proc, proc["StandardOutput"]];
	If[(!StringQ[proc]) && AssociationQ[proc] && proc["ExitCode"] =!= 0,
		Return[$Failed]
	];
	stdout
]
