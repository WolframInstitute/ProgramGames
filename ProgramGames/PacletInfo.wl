(* ::Package:: *)

PacletObject[
  <|
    "Name" -> "WolframInstitute/ProgramGames",
    "Description" -> "Fast iterated game tournaments between Turing machines, finite state machines, and cellular automata",
    "Creator" -> "Wolfram Institute",
    "License" -> "MIT",
    "PublisherID" -> "WolframInstitute",
    "Version" -> "1.0.0",
    "WolframVersion" -> "14.3+",
    "Dependencies" -> {"PacletExtensions"},
    "PrimaryContext" -> "WolframInstitute`ProgramGames`",
    "Extensions" -> {
      {
        "Cargo",
        "Root" -> "Lib"
      },
      {
        "Build",
        "Actions" -> {"CargoBuild"}
      },
      {
        "Kernel",
        "Root" -> "Kernel",
        "Context" -> {"WolframInstitute`ProgramGames`"},
        "Symbols" -> {
          "WolframInstitute`ProgramGames`ProgramGamesSetup",
          "WolframInstitute`ProgramGames`TuringMachineProgramSearch",
          "WolframInstitute`ProgramGames`TuringMachineClassify",
          "WolframInstitute`ProgramGames`TuringMachineMaxIndex",
          "WolframInstitute`ProgramGames`TuringMachineTournament",
          "WolframInstitute`ProgramGames`CellularAutomatonClassify",
          "WolframInstitute`ProgramGames`CellularAutomatonMaxIndex",
          "WolframInstitute`ProgramGames`CellularAutomatonTournament",
          "WolframInstitute`ProgramGames`FiniteStateMachineClassify",
          "WolframInstitute`ProgramGames`FiniteStateMachineMaxIndex",
          "WolframInstitute`ProgramGames`FiniteStateMachineTournament",
          "WolframInstitute`ProgramGames`RuleArrayTournament",
          "WolframInstitute`ProgramGames`RuleArrayEvolve",
          "WolframInstitute`ProgramGames`RuleArrayPlot",
          "WolframInstitute`ProgramGames`RuleArrayExplain",
          "WolframInstitute`ProgramGames`RuleArraySpaceTable",
          "WolframInstitute`ProgramGames`RuleArrayStrategy",
          "WolframInstitute`ProgramGames`ProgramTournament",
          "WolframInstitute`ProgramGames`SpaceSurveyTable",
          "WolframInstitute`ProgramGames`ClassificationTable",
          "WolframInstitute`ProgramGames`ShortNum",
          "WolframInstitute`ProgramGames`PayoffToString",
          "WolframInstitute`ProgramGames`StrategyToJSON",
          "WolframInstitute`ProgramGames`EnumerateGames",
          "WolframInstitute`ProgramGames`GameWinner",
          "WolframInstitute`ProgramGames`GameSpaceSurvey",
          "WolframInstitute`ProgramGames`GameRankingClasses",
          "WolframInstitute`ProgramGames`ComplexityMeasure",
          "WolframInstitute`ProgramGames`ComplexityLandscapeSearch"
        }
      },
      {
        "Binaries"
      }
    }
  |>
]
