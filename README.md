# eCandleSticks

An R package for Japanese candlestick pattern recognition and visualization. Identifies **63 pattern types** (100+ distinct signals) from OHLC price data, covering virtually the entire TA-Lib CDL catalogue. Two-layer API: `addCSP*()` for detection and `drawCSP*()` for chart annotation.

## Installation

```r
# Development version from GitHub
devtools::install_github("dengyishuo/eCandleSticks")
```

## Quick Start

```r
library(eCandleSticks)
library(quantmod)

getSymbols("AAPL", src = "yahoo", from = "2023-01-01", to = "2025-01-01")

# Draw candlestick chart
chart <- eCandleSticks(AAPL)

# Detect Doji patterns (returns xts by default)
doji <- addCSPDoji(AAPL)

# Or return as tibble / data.frame
doji_tbl <- addCSPDoji(AAPL, output = "tibble")

# Overlay pattern markers on the chart
chart <- drawCSPDoji(chart, doji)
print(chart$combined_plot)
```

## API Design

| Layer | Function pattern | Input | Output |
|-------|-----------------|-------|--------|
| Detection | `addCSP*(x, ..., output = )` | xts / data.frame / tibble | xts / tibble / data.frame |
| Visualization | `drawCSP*(chart, pattern_result, ...)` | eCandleSticks chart object | Modified chart object |

The `output` parameter controls the return format of all `addCSP*()` functions:

```r
addCSPEngulfing(AAPL, output = "xts")        # default
addCSPEngulfing(AAPL, output = "tibble")
addCSPEngulfing(AAPL, output = "data.frame")
```

## Pattern Reference

### Single-Candle Patterns (1 bar)

| Function | Signals | Direction |
|----------|---------|-----------|
| `addCSPDoji` | `Doji`, `DragonflyDoji`, `GravestoneDoji` | neutral |
| `addCSPHammer` | `Hammer` | bull |
| `addCSPHangingMan` | `HangingMan` | bear |
| `addCSPInvertedHammer` | `InvertedHammer` | bull |
| `addCSPShootingStar` | `ShootingStar` | bear |
| `addCSPLongCandle` | `LongWhiteCandle`, `LongBlackCandle` | bull/bear |
| `addCSPLongCandleBody` | `LongWhiteCandleBody`, `LongBlackCandleBody` | bull/bear |
| `addCSPShortCandle` | `ShortWhiteCandle`, `ShortBlackCandle` | bull/bear |
| `addCSPShortCandleBody` | `ShortWhiteCandleBody`, `ShortBlackCandleBody` | bull/bear |
| `addCSPMarubozu` | `WhiteMarubozu`, `BlackMarubozu` | bull/bear |
| `addCSPBeltHold` | `BullBeltHold`, `BearBeltHold` | bull/bear |
| `addCSPClosingMarubozu` | `BullClosingMarubozu`, `BearClosingMarubozu` | bull/bear |
| `addCSPHighWave` | `HighWave` | neutral |
| `addCSPLongLeggedDoji` | `LongLeggedDoji` | neutral |
| `addCSPRickshawMan` | `RickshawMan` | neutral |
| `addCSPSpinningTop` | `BullSpinningTop`, `BearSpinningTop` | neutral |
| `addCSPTakuri` | `Takuri` | bull |

### Two-Candle Patterns (2 bars)

| Function | Signals | Direction |
|----------|---------|-----------|
| `addCSPEngulfing` | `Bull.Engulfing`, `Bear.Engulfing` | bull/bear |
| `addCSPHarami` | `Bull.Harami`, `Bear.Harami` | bull/bear |
| `addCSPKicking` | `Bull.Kicking`, `Bear.Kicking` | bull/bear |
| `addCSPGap` | `GapUp`, `GapDown` | — |
| `addCSPDarkCloudCover` | `DarkCloudCover` | bear |
| `addCSPPiercingPattern` | `PiercingPattern` | bull |
| `addCSPInsideDay` | `InsideDay` | neutral |
| `addCSPOutsideDay` | `OutsideDay` | neutral |
| `addCSPStomach` | `AboveTheStomach`, `BelowTheStomach` | bull/bear |
| `addCSPTasukiGap` | `UpsideTasukiGap`, `DownsideTasukiGap` | bull/bear |
| `addCSPCounterAttack` | `BullCounterAttack`, `BearCounterAttack` | bull/bear |
| `addCSPDojiStar` | `BullDojiStar`, `BearDojiStar` | bull/bear |
| `addCSPHomingPigeon` | `HomingPigeon` | bull |
| `addCSPInNeck` | `InNeck` | bear |
| `addCSPMatchingLow` | `MatchingLow` | bull |
| `addCSPOnNeck` | `OnNeck` | bear |
| `addCSPSeparatingLines` | `BullSeparatingLines`, `BearSeparatingLines` | bull/bear |
| `addCSPThrusting` | `Thrusting` | bear |

### Three-Candle Patterns (3 bars)

| Function | Signals | Direction |
|----------|---------|-----------|
| `addCSPStar` | `MorningStar`, `EveningStar` | bull/bear |
| `addCSPThreeWhiteSoldiers` | `ThreeWhiteSoldiers` | bull |
| `addCSPThreeBlackCrows` | `ThreeBlackCrows` | bear |
| `addCSPThreeInside` | `ThreeInsideUp`, `ThreeInsideDown` | bull/bear |
| `addCSPThreeOutside` | `ThreeOutsideUp`, `ThreeOutsideDown` | bull/bear |
| `addCSPThreeLineStrike` | `Bull.ThreeLineStrike`, `Bear.ThreeLineStrike` | bull/bear |
| `addCSPThreeMethods` | `RisingThreeMethods`, `FallingThreeMethods` | bull/bear |
| `addCSPAbandonedBaby` | `BullAbandonedBaby`, `BearAbandonedBaby` | bull/bear |
| `addCSPAdvanceBlock` | `AdvanceBlock` | bear |
| `addCSPGapSideSideWhite` | `BullGapSideSideWhite`, `BearGapSideSideWhite` | bull/bear |
| `addCSPIdentical3Crows` | `Identical3Crows` | bear |
| `addCSPStalledPattern` | `StalledPattern` | bear |
| `addCSPStickSandwich` | `StickSandwich` | bull |
| `addCSPThreeStarsInSouth` | `ThreeStarsInSouth` | bull |
| `addCSPTristar` | `BullTristar`, `BearTristar` | bull/bear |
| `addCSPTwoCrows` | `TwoCrows` | bear |
| `addCSPUnique3River` | `Unique3River` | bull |
| `addCSPUpsideGap2Crows` | `UpsideGap2Crows` | bear |

### Four-Candle Patterns (4 bars)

| Function | Signals | Direction |
|----------|---------|-----------|
| `addCSPConcealBabySwallow` | `ConcealBabySwallow` | bull |

### Five-Candle Patterns (5 bars)

| Function | Signals | Direction |
|----------|---------|-----------|
| `addCSPBreakaway` | `BullBreakaway`, `BearBreakaway` | bull/bear |
| `addCSPLadderBottom` | `LadderBottom` | bull |

### N-Candle Patterns (parameterized)

| Function | Parameter | Signals |
|----------|-----------|---------|
| `addCSPNHigherClose` | `N` | `N HigherClose` |
| `addCSPNLowerClose` | `N` | `N LowerClose` |
| `addCSPNLongWhiteCandles` | `N` | `N LongWhiteCandles` |
| `addCSPNLongBlackCandles` | `N` | `N LongBlackCandles` |
| `addCSPNLongWhiteCandleBodies` | `N` | `N LongWhiteCandleBodies` |
| `addCSPNLongBlackCandleBodies` | `N` | `N LongBlackCandleBodies` |
| `addCSPNBlended` | `N` | Blended OHLC columns |

## Trend Detection Utilities

Trend context can be combined with pattern signals to filter for higher-probability setups:

```r
# Bearish Engulfing in an uptrend only
engulfing <- addCSPEngulfing(AAPL)
trend     <- TrendDetectionChannel(AAPL)

bear_signals <- engulfing[, "Bear.Engulfing"] & trend[, "UpTrend"]
```

| Function | Method |
|----------|--------|
| `TrendDetectionChannel` | Price channel breakout |
| `TrendDetectionSMA` | Simple moving average |
| `TrendDetectionEMA` | Exponential moving average |
| `DonchianChannel` | Donchian channel bounds |

## roxygen2 Families

Functions are grouped by two `@family` dimensions for easy navigation in the documentation:

- **By candle count**: `pattern-1bar`, `pattern-2bar`, `pattern-3bar`, `pattern-4bar`, `pattern-5bar`, `pattern-nbar`
- **By direction**: `pattern-bull`, `pattern-bear`

## Links

- **Website**: [xquant.shop](https://xquant.shop)
- **WeChat Public Account**: i锐角
- **WeChat Public Account**: xquant-shop

## License

GPL-3 © Andreas Voellenklee, Deng Yishuo
