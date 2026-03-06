# Data

## Source

Both datasets are downloaded from Kaggle:
[**NBA Players Stats since 1950**](https://www.kaggle.com/datasets/drgilermo/nba-players-stats) by Gilermo.

They are included directly in this repository so the analysis is fully reproducible without requiring a Kaggle account.

---

## Files

### `seasons_stats.csv`

Season-level player statistics aggregated from Basketball-Reference, covering every NBA season from **1950 to 2021** (the 2022 rows in the raw file are duplicates of 2021 and are removed during cleaning).

Key columns used in the analysis:

| Column | Description |
|---|---|
| `Player` | Player name (Hall of Famers marked with `*`) |
| `Year` | Season end year |
| `Pos` | Position (standardised to C / PF / SF / SG / PG) |
| `Age` | Player age |
| `Tm` | Team abbreviation (`TOT` for players traded mid-season) |
| `G` | Games played |
| `MP` | Total minutes played |
| `PTS`, `AST`, `TRB`, `STL`, `BLK`, `TOV`, `PF` | Counting stats |
| `FT`, `FGA` | Free throws made, field goals attempted |
| `eFG.` | Effective field goal percentage |
| `ORB.` | Offensive rebounding percentage |
| `TOV.` | Turnover percentage |
| `X3P.` | Three-point percentage |
| `FT.` | Free throw percentage |

### `player_data.csv`

Player biographical data, one row per player.

| Column | Description |
|---|---|
| `Player` | Player name |
| `Ht` | Height in feet-inches format (e.g. `"6-5"`) — converted to cm |
| `Wt` | Weight in lbs — converted to kg |
| `born` | Birth year |
| `college` | College attended |

---

## Cleaning Notes

- `Ht` is converted from `"feet-inches"` strings to centimetres using `measurements::conv_unit()`.
- `Wt` is converted from pounds to kilograms.
- Players with missing `Wt` in `player_data.csv` are removed before merging.
- Duplicate player entries in `player_data.csv` are resolved by keeping the first occurrence.
- Missing `Ht` / `Wt` values after merging are imputed with the positional mean.
- `X3P.` and `FT.` NAs are replaced with 0 (player did not attempt that type of shot).
- For players traded mid-season, only the `TOT` (combined) row is retained.
