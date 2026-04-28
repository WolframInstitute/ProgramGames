(* ::Package:: *)

Package["WolframInstitute`ProgramGames`"]


(* ::Section::Closed:: *)
(*Exported Symbols*)


PackageExport["FSMEvolveVsOne"]
PackageExport["FSMEvolveVsMany"]
PackageExport["FSMEvolveCoadapt"]
PackageExport["FSMEvolveEnsemble"]
PackageExport["FSMEvolveStepRust"]
PackageExport["FSMEvolveTrajectoryRust"]
PackageExport["FSMScoreVsOne"]
PackageExport["FSMScoreVsMany"]
PackageExport["FSMMutate"]
(* Deprecated aliases for prior names *)
PackageExport["FSMEvolveSingle"]
PackageExport["FSMEvolvePopulation"]


(* ::Section::Closed:: *)
(*Scoped Symbols*)


PackageScope["FSMEvolveSingleWL"]
PackageScope["FSMEvolvePopulationWL"]
PackageScope["FSMEvolveCoadaptWL"]
PackageScope["FSMEvolveStepWL"]
PackageScope["FSMEvolveTrajectoryWL"]
PackageScope["FSMEvolveEnsembleWL"]
PackageScope["FSMScoreVsOneWL"]
PackageScope["FSMScoreVsManyWL"]
PackageScope["FSMMutateWL"]


(* ::Section::Closed:: *)
(*Usage Messages*)


FSMEvolveVsOne::usage = "FSMEvolveVsOne[start, opp, opts] hill-climbs FSM start against fixed opponent opp; both are {id, s, k}. Options: \"Rounds\" (1000), \"EvolveSteps\" (200), \"Seed\" (42), \"Game\" (Automatic = Matching Pennies, or comma-separated payoff string).";

FSMEvolveVsMany::usage = "FSMEvolveVsMany[start, opps, opts] hill-climbs start vs a population of opponents (each {id, s, k}); fitness = mean payoff averaged across opponents.";

FSMEvolveCoadapt::usage = "FSMEvolveCoadapt[startA, startB, opts] alternates mutating A and B; each accepts only if its own score did not drop. Returns trajectory of {fsmA, fsmB, scoreA, scoreB}.";

FSMEvolveEnsemble::usage = "FSMEvolveEnsemble[opps, n, opts] runs n independent random-start hill-climbs in parallel against opponents opps. Or FSMEvolveEnsemble[opps, starts_List, opts] using user-specified starts. Returns a list of FitnessCurves.";

FSMEvolveStepRust::usage = "FSMEvolveStepRust[fsm, fit, opps, opts] applies one mutation step against opps (single or list); returns {fsm, fit, accepted, candFit}.";

FSMEvolveTrajectoryRust::usage = "FSMEvolveTrajectoryRust[start, opps, opts] runs an n-step hill-climb against opps (single or list); returns the same Association schema as FSMEvolveVsOne / FSMEvolveVsMany.";

FSMScoreVsOne::usage = "FSMScoreVsOne[fsm, opp, opts] returns the mean payoff of fsm vs opp over Rounds rounds.";

FSMScoreVsMany::usage = "FSMScoreVsMany[fsm, opps, opts] returns the mean payoff of fsm averaged over the list of opponents opps.";

FSMEvolveVsOne::err = "Evolution failed: ``";
FSMEvolveVsMany::err = "Evolution failed: ``";
FSMEvolveCoadapt::err = "Evolution failed: ``";
FSMEvolveEnsemble::err = "Evolution failed: ``";
FSMEvolveStepRust::err = "Step failed: ``";
FSMEvolveTrajectoryRust::err = "Evolution failed: ``";
FSMScoreVsOne::err = "Score failed: ``";
FSMScoreVsMany::err = "Score failed: ``";

FSMMutate::usage = "FSMMutate[fsm, opts] applies one mutation step to FSM = {id, s, k} and returns the mutated {id, s, k}. Options: \"Kind\" -> \"Either\" (default, MutateFSM), \"Edge\" (MutateFSMEdge), or \"StateAction\" (MutateFSMStateAction); \"Seed\" -> 42.";
FSMMutate::err = "Mutation failed: ``";


(* ::Section::Closed:: *)
(*Default Game (Matching Pennies)*)


$DefaultEvolveGame = "1,-1,-1,1,-1,1,1,-1";


iGameString[Automatic] := $DefaultEvolveGame;
iGameString[s_String] := s;
iGameString[m_List] :=
	StringRiffle[ToString /@ Round @ Flatten @ m, ","];


(* ::Section::Closed:: *)
(*Function bindings*)


FSMEvolveSingleWL := functions["fsm_evolve_single_wl"]
FSMEvolvePopulationWL := functions["fsm_evolve_population_wl"]
FSMEvolveCoadaptWL := functions["fsm_evolve_coadapt_wl"]
FSMEvolveStepWL := functions["fsm_evolve_step_wl"]
FSMEvolveTrajectoryWL := functions["fsm_evolve_trajectory_wl"]
FSMEvolveEnsembleWL := functions["fsm_evolve_ensemble_wl"]
FSMScoreVsOneWL := functions["fsm_score_vs_one_wl"]
FSMScoreVsManyWL := functions["fsm_score_vs_many_wl"]
FSMMutateWL := functions["fsm_mutate_wl"]


(* ::Section::Closed:: *)
(*Helpers*)


iFsmSpecJSON[{id_, s_, k_}] :=
	ExportString[
		<|"id" -> ToString[id], "s" -> s, "k" -> k|>,
		"RawJSON", "Compact" -> True];

iFsmSpecsJSON[specs_List] :=
	ExportString[
		(<|"id" -> ToString[#[[1]]], "s" -> #[[2]], "k" -> #[[3]]|>) & /@ specs,
		"RawJSON", "Compact" -> True];

iParseFsmSpec[entry_Association] :=
	{ToExpression[entry["id"]], entry["s"], entry["k"]};

iTrajToAssoc[traj_List] :=
	Module[{fsms, fits, accepts, candFits, breakthroughs},
		fsms     = iParseFsmSpec /@ traj;
		fits     = #["fitness"] & /@ traj;
		accepts  = #["accepted"] & /@ traj;
		candFits = #["candidate_fitness"] & /@ traj;
		breakthroughs = Flatten @ Position[Differences[fits], _?Positive, {1}, Heads -> False];
		<|
			"Trajectory"   -> Transpose[{fsms, fits, accepts, candFits}],
			"FitnessCurve" -> fits,
			"Accepted"     -> accepts,
			"Breakthroughs"-> breakthroughs
		|>
	];


(* ::Section::Closed:: *)
(*FSMEvolveVsOne (was FSMEvolveSingle)*)


Options[FSMEvolveVsOne] = {
	"Rounds" -> 1000, "EvolveSteps" -> 200, "Seed" -> 42, "Game" -> Automatic
};

FSMEvolveVsOne[start : {_, _Integer, _Integer}, opp : {_, _Integer, _Integer},
		opts : OptionsPattern[]] :=
	Module[{json, parsed, base},
		json = FSMEvolveSingleWL[
			iFsmSpecJSON[start],
			iFsmSpecJSON[opp],
			iGameString[OptionValue["Game"]],
			OptionValue["Rounds"],
			OptionValue["EvolveSteps"],
			OptionValue["Seed"]
		];
		If[FailureQ[json], Return[$Failed]];
		parsed = ImportString[json, "RawJSON"];
		If[KeyExistsQ[parsed, "error"],
			Message[FSMEvolveVsOne::err, parsed["error"]]; Return[$Failed]];
		base = iTrajToAssoc[parsed["trajectory"]];
		Join[base, <|"Opponent" -> opp, "Start" -> start|>]
	];

(* Deprecated alias *)
FSMEvolveSingle = FSMEvolveVsOne;


(* ::Section::Closed:: *)
(*FSMEvolveVsMany (was FSMEvolvePopulation)*)


Options[FSMEvolveVsMany] = {
	"Rounds" -> 1000, "EvolveSteps" -> 200, "Seed" -> 42, "Game" -> Automatic
};

FSMEvolveVsMany[start : {_, _Integer, _Integer}, opps_List,
		opts : OptionsPattern[]] :=
	Module[{json, parsed, base},
		json = FSMEvolvePopulationWL[
			iFsmSpecJSON[start],
			iFsmSpecsJSON[opps],
			iGameString[OptionValue["Game"]],
			OptionValue["Rounds"],
			OptionValue["EvolveSteps"],
			OptionValue["Seed"]
		];
		If[FailureQ[json], Return[$Failed]];
		parsed = ImportString[json, "RawJSON"];
		If[KeyExistsQ[parsed, "error"],
			Message[FSMEvolveVsMany::err, parsed["error"]]; Return[$Failed]];
		base = iTrajToAssoc[parsed["trajectory"]];
		Join[base, <|
			"Opponents" -> opps,
			"OpponentCount" -> Length[opps],
			"Start" -> start
		|>]
	];

(* Deprecated alias *)
FSMEvolvePopulation = FSMEvolveVsMany;


(* ::Section::Closed:: *)
(*FSMEvolveCoadapt*)


Options[FSMEvolveCoadapt] = {
	"Rounds" -> 1000, "EvolveSteps" -> 200, "Seed" -> 42, "Game" -> Automatic
};

FSMEvolveCoadapt[startA : {_, _Integer, _Integer}, startB : {_, _Integer, _Integer},
		opts : OptionsPattern[]] :=
	Module[{json, parsed, traj, history},
		json = FSMEvolveCoadaptWL[
			iFsmSpecJSON[startA],
			iFsmSpecJSON[startB],
			iGameString[OptionValue["Game"]],
			OptionValue["Rounds"],
			OptionValue["EvolveSteps"],
			OptionValue["Seed"]
		];
		If[FailureQ[json], Return[$Failed]];
		parsed = ImportString[json, "RawJSON"];
		If[KeyExistsQ[parsed, "error"],
			Message[FSMEvolveCoadapt::err, parsed["error"]]; Return[$Failed]];
		traj = parsed["trajectory"];
		history = Map[
			{
				{ToExpression[#["id_a"]], #["s_a"], #["k_a"]},
				{ToExpression[#["id_b"]], #["s_b"], #["k_b"]},
				#["score_a"],
				#["score_b"],
				Replace[#["mutator"], {Null -> None, m_String :> m}],
				#["cand_score_a"],
				#["cand_score_b"],
				TrueQ[#["accepted"]]
			} &,
			traj
		];
		Module[{rejA, rejB, attemptsA, attemptsB, acceptsA, acceptsB, rateA, rateB},
			rejA = Cases[
				MapIndexed[{#2[[1]] - 1, #1} &, history],
				{step_, {_, _, _, _, "A", csa_, _, False}} :> {step, csa}
			];
			rejB = Cases[
				MapIndexed[{#2[[1]] - 1, #1} &, history],
				{step_, {_, _, _, _, "B", _, csb_, False}} :> {step, csb}
			];
			attemptsA = Count[history[[All, 5]], "A"];
			attemptsB = Count[history[[All, 5]], "B"];
			acceptsA = Count[history, {_, _, _, _, "A", _, _, True}];
			acceptsB = Count[history, {_, _, _, _, "B", _, _, True}];
			rateA = If[attemptsA > 0, N[acceptsA / attemptsA], Indeterminate];
			rateB = If[attemptsB > 0, N[acceptsB / attemptsB], Indeterminate];
			<|
				"Trajectory" -> history,
				"ScoreCurveA" -> history[[All, 3]],
				"ScoreCurveB" -> history[[All, 4]],
				"RejectedA" -> rejA,
				"RejectedB" -> rejB,
				"AcceptanceRateA" -> rateA,
				"AcceptanceRateB" -> rateB,
				"StartA" -> startA,
				"StartB" -> startB
			|>
		]
	];


(* ::Section::Closed:: *)
(*FSMScoreVsOne / FSMScoreVsMany*)


Options[FSMScoreVsOne] = {"Rounds" -> 1000, "Game" -> Automatic};

FSMScoreVsOne[fsm : {_, _Integer, _Integer}, opp : {_, _Integer, _Integer},
		opts : OptionsPattern[]] :=
	Module[{r},
		r = FSMScoreVsOneWL[
			iFsmSpecJSON[fsm],
			iFsmSpecJSON[opp],
			iGameString[OptionValue["Game"]],
			OptionValue["Rounds"]
		];
		If[FailureQ[r], Return[$Failed]];
		r
	];

Options[FSMScoreVsMany] = {"Rounds" -> 1000, "Game" -> Automatic};

FSMScoreVsMany[fsm : {_, _Integer, _Integer}, opps_List,
		opts : OptionsPattern[]] :=
	Module[{r},
		r = FSMScoreVsManyWL[
			iFsmSpecJSON[fsm],
			iFsmSpecsJSON[opps],
			iGameString[OptionValue["Game"]],
			OptionValue["Rounds"]
		];
		If[FailureQ[r], Return[$Failed]];
		r
	];


(* ::Section::Closed:: *)
(*FSMEvolveStepRust*)


Options[FSMEvolveStepRust] = {
	"Rounds" -> 1000, "Seed" -> 42, "Game" -> Automatic
};

FSMEvolveStepRust[fsm : {_, _Integer, _Integer}, fit_?NumericQ,
		opps : {{_, _Integer, _Integer} ..},
		opts : OptionsPattern[]] :=
	Module[{json, parsed, nextFsm},
		json = FSMEvolveStepWL[
			iFsmSpecJSON[fsm],
			N[fit],
			iFsmSpecsJSON[opps],
			iGameString[OptionValue["Game"]],
			OptionValue["Rounds"],
			OptionValue["Seed"]
		];
		If[FailureQ[json], Return[$Failed]];
		parsed = ImportString[json, "RawJSON"];
		If[KeyExistsQ[parsed, "error"],
			Message[FSMEvolveStepRust::err, parsed["error"]]; Return[$Failed]];
		nextFsm = iParseFsmSpec[parsed];
		{nextFsm, parsed["fitness"], parsed["accepted"], parsed["candidate_fitness"]}
	];

FSMEvolveStepRust[fsm : {_, _Integer, _Integer}, fit_?NumericQ,
		opp : {_, _Integer, _Integer}, opts : OptionsPattern[]] :=
	FSMEvolveStepRust[fsm, fit, {opp}, opts];


(* ::Section::Closed:: *)
(*FSMEvolveTrajectoryRust*)


Options[FSMEvolveTrajectoryRust] = {
	"Rounds" -> 1000, "EvolveSteps" -> 200, "Seed" -> 42, "Game" -> Automatic
};

FSMEvolveTrajectoryRust[start : {_, _Integer, _Integer},
		opps : {{_, _Integer, _Integer} ..},
		opts : OptionsPattern[]] :=
	Module[{json, parsed, base},
		json = FSMEvolveTrajectoryWL[
			iFsmSpecJSON[start],
			iFsmSpecsJSON[opps],
			iGameString[OptionValue["Game"]],
			OptionValue["Rounds"],
			OptionValue["EvolveSteps"],
			OptionValue["Seed"]
		];
		If[FailureQ[json], Return[$Failed]];
		parsed = ImportString[json, "RawJSON"];
		If[KeyExistsQ[parsed, "error"],
			Message[FSMEvolveTrajectoryRust::err, parsed["error"]]; Return[$Failed]];
		base = iTrajToAssoc[parsed["trajectory"]];
		Join[base, If[Length[opps] == 1,
			<|"Opponent" -> First[opps], "Start" -> start|>,
			<|"Opponents" -> opps, "OpponentCount" -> Length[opps], "Start" -> start|>
		]]
	];

FSMEvolveTrajectoryRust[start : {_, _Integer, _Integer},
		opp : {_, _Integer, _Integer}, opts : OptionsPattern[]] :=
	FSMEvolveTrajectoryRust[start, {opp}, opts];


(* ::Section::Closed:: *)
(*FSMEvolveEnsemble*)


Options[FSMEvolveEnsemble] = {
	"Rounds" -> 1000, "EvolveSteps" -> 200, "Seed" -> 42, "Game" -> Automatic,
	"States" -> 3
};

(* FSMEvolveEnsemble[opps, n_Integer, opts] -- random starts. State count
   comes from "States" option (default 3); action count is 2 (existing
   convention, matching the FSM evolve pipeline). *)
FSMEvolveEnsemble[opps : {{_, _Integer, _Integer} ..}, n_Integer,
		opts : OptionsPattern[]] :=
	Module[{s, k, max, seed, starts},
		seed = OptionValue["Seed"];
		s = OptionValue["States"];
		k = 2;
		max = FiniteStateMachineMaxIndex[s, k];
		SeedRandom[seed];
		starts = {#, s, k} & /@ RandomSample[Range[0, max - 1], Min[n, max]];
		FSMEvolveEnsemble[opps, starts, opts]
	];

FSMEvolveEnsemble[opps : {{_, _Integer, _Integer} ..},
		starts : {{_, _Integer, _Integer} ..},
		opts : OptionsPattern[]] :=
	Module[{json, parsed, trajs},
		json = FSMEvolveEnsembleWL[
			iFsmSpecsJSON[starts],
			iFsmSpecsJSON[opps],
			iGameString[OptionValue["Game"]],
			OptionValue["Rounds"],
			OptionValue["EvolveSteps"],
			OptionValue["Seed"]
		];
		If[FailureQ[json], Return[$Failed]];
		parsed = ImportString[json, "RawJSON"];
		If[KeyExistsQ[parsed, "error"],
			Message[FSMEvolveEnsemble::err, parsed["error"]]; Return[$Failed]];
		trajs = parsed["trajectories"];
		(* Each trajectory: list of {fitness, ...} entries; return FitnessCurves *)
		Map[Function[traj, #["fitness"] & /@ traj], trajs]
	];


(* ::Section::Closed:: *)
(*FSMMutate*)


Options[FSMMutate] = {"Kind" -> "Either", "Seed" -> 42};

FSMMutate[fsm : {_, _Integer, _Integer}, opts : OptionsPattern[]] :=
	Module[{kindStr, kind, json, parsed},
		kindStr = OptionValue["Kind"];
		kind = Switch[kindStr,
			"StateAction", 0,
			"Edge", 1,
			_, 2
		];
		json = FSMMutateWL[
			iFsmSpecJSON[fsm],
			kind,
			OptionValue["Seed"]
		];
		If[FailureQ[json], Return[$Failed]];
		parsed = ImportString[json, "RawJSON"];
		If[KeyExistsQ[parsed, "error"],
			Message[FSMMutate::err, parsed["error"]]; Return[$Failed]];
		iParseFsmSpec[parsed]
	];
