# Variable Glossary

## Derived Variables (added during cleaning)

| Variable | Formula | Description |
|---|---|---|
| `HallOfFame` | `if_else(str_detect(Player, "\\*$"), "Yes", "No")` | Whether the player is a Hall of Famer |
| `MpG` | `MP / G` | Minutes per game |
| `PpG` | `PTS / G` | Points per game |
| `ApG` | `AST / G` | Assists per game |
| `RpG` | `TRB / G` | Rebounds per game |
| `TOpG` | `TOV / G` | Turnovers per game |
| `BpG` | `BLK / G` | Blocks per game |
| `SpG` | `STL / G` | Steals per game |
| `FpG` | `PF / G` | Personal fouls per game |
| `FTr` | `FT / FGA` | Free Throw Rate — one of Oliver's Four Factors |

## Oliver's Four Factors

Dean Oliver identified four statistical factors that best explain winning in basketball. In this project they are used as clustering variables:

| Factor | Variable | Description |
|---|---|---|
| Shooting efficiency | `eFG%` | Effective Field Goal % — adjusts for the extra value of 3-pointers |
| Turnovers | `TOV%` | Turnover % — estimated percentage of possessions ending in a turnover |
| Rebounding | `ORB%` | Offensive Rebound % — percentage of available offensive rebounds grabbed |
| Free throws | `FTr%` | Free Throw Rate — free throws attempted relative to field goals attempted |

## Position Codes

| Code | Position |
|---|---|
| `C` | Centre |
| `PF` | Power Forward |
| `SF` | Small Forward |
| `SG` | Shooting Guard |
| `PG` | Point Guard |

Multi-position labels (e.g. `"PF-C"`, `"SG-SF"`) are mapped to a single primary position — see `R/helpers.R` → `standardise_positions()`.
