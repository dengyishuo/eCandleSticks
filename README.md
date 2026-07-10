# eCandleSticks

An R package for Japanese candlestick pattern recognition and visualization. Identifies 34 pattern types (57+ distinct signals) from OHLC price data, with a two-layer API: `addCSP*()` for detection and `drawCSP*()` for chart annotation.

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

| Function | Signals |
|----------|---------|
| `addCSPDoji` | `Doji`, `DragonflyDoji`, `GravestoneDoji` |
| `addCSPHammer` | `Hammer` |
| `addCSPHangingMan` | `HangingMan` |
| `addCSPInvertedHammer` | `InvertedHammer` |
| `addCSPShootingStar` | `ShootingStar` |
| `addCSPLongCandle` | `LongWhiteCandle`, `LongBlackCandle` |
| `addCSPLongCandleBody` | `LongWhiteCandleBody`, `LongBlackCandleBody` |
| `addCSPShortCandle` | `ShortWhiteCandle`, `ShortBlackCandle` |
| `addCSPShortCandleBody` | `ShortWhiteCandleBody`, `ShortBlackCandleBody` |
| `addCSPMarubozu` | `WhiteMarubozu`, `BlackMarubozu` |

### Two-Candle Patterns (2 bars)

| Function | Signals |
|----------|---------|
| `addCSPEngulfing` | `Bull.Engulfing`, `Bear.Engulfing` |
| `addCSPHarami` | `Bull.Harami`, `Bear.Harami` |
| `addCSPKicking` | `Bull.Kicking`, `Bear.Kicking` |
| `addCSPGap` | `GapUp`, `GapDown` |
| `addCSPDarkCloudCover` | `DarkCloudCover` |
| `addCSPPiercingPattern` | `PiercingPattern` |
| `addCSPInsideDay` | `InsideDay` |
| `addCSPOutsideDay` | `OutsideDay` |
| `addCSPStomach` | `AboveTheStomach`, `BelowTheStomach` |
| `addCSPTasukiGap` | `UpsideTasukiGap`, `DownsideTasukiGap` |

### Three-Candle Patterns (3 bars)

| Function | Signals |
|----------|---------|
| `addCSPStar` | `MorningStar`, `EveningStar` |
| `addCSPThreeWhiteSoldiers` | `ThreeWhiteSoldiers` |
| `addCSPThreeBlackCrows` | `ThreeBlackCrows` |
| `addCSPThreeInside` | `ThreeInsideUp`, `ThreeInsideDown` |
| `addCSPThreeOutside` | `ThreeOutsideUp`, `ThreeOutsideDown` |
| `addCSPThreeLineStrike` | `Bull.ThreeLineStrike`, `Bear.ThreeLineStrike` |
| `addCSPThreeMethods` | `RisingThreeMethods`, `FallingThreeMethods` |

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

- **By candle count**: `pattern-1bar`, `pattern-2bar`, `pattern-3bar`, `pattern-nbar`
- **By direction**: `pattern-bull`, `pattern-bear`

## Links

- **Website**: [xquant.shop](https://xquant.shop)
- **WeChat Public Account**: i锐角
- **WeChat Public Account**: xquant-shop

## License

GPL-3 © Andreas Voellenklee, Deng Yishuo
