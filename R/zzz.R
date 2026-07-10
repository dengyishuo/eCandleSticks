## Suppress R CMD check NOTEs for variables used inside aes() and data.frame columns
utils::globalVariables(c(
  # ggplot2 aes() column names used in drawCSP* functions
  "Date", "Close", "Open", "High", "Low",
  "GapLevel", "HammerLevel", "HaramiLevel", "InsideDayLevel",
  "InvertedHammerLevel", "KickingLevel", "LongCandleLevel",
  "LongCandleBodyLevel", "MarkLevel", "MarubozuLevel",
  "NLongBlackLevel", "NLongWhiteLevel", "OutsideDayLevel",
  "PatternLevel", "PiercingPatternLevel", "ShortCandleLevel",
  "ShortCandleBodyLevel", "StomachLevel", "TasukiGapLevel",
  "ThreeBlackCrowsLevel", "ThreeInsideLevel", "ThreeOutsideLevel",
  # dplyr .data pronoun
  ".data"
))
