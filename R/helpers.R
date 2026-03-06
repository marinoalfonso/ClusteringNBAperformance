# =============================================================================
# R/helpers.R
# Utility functions for ClusteringNBAperformance
# =============================================================================


# -----------------------------------------------------------------------------
# convert_height()
# Convert a height string in "feet-inches" format to centimetres.
#
# @param x  Character string, e.g. "6-5"
# @return   Numeric value in cm (rounded to nearest integer)
# -----------------------------------------------------------------------------
convert_height <- function(x) {
  x     <- as.character(x)
  split <- strsplit(x, "-")
  feet  <- as.numeric(split[[1]][1])
  inch  <- as.numeric(split[[1]][2])
  round(
    measurements::conv_unit(feet, "ft", "cm") +
    measurements::conv_unit(inch, "inch", "cm"),
    0
  )
}


# -----------------------------------------------------------------------------
# standardise_positions()
# Map multi-position labels (e.g. "PF-C") to a single primary position.
#
# @param df  data.frame containing a column named "Pos"
# @return    Same data.frame with Pos standardised
# -----------------------------------------------------------------------------
standardise_positions <- function(df) {
  dplyr::mutate(df, Pos = dplyr::case_when(
    Pos == "PF-C"  ~ "PF",
    Pos == "C-F"   ~ "C",
    Pos == "SF-SG" ~ "SF",
    Pos == "C-PF"  ~ "C",
    Pos == "SG-SF" ~ "SG",
    Pos == "PF-SF" ~ "PF",
    Pos == "SF-PF" ~ "SF",
    Pos == "SG-PG" ~ "SG",
    Pos == "SF-PG" ~ "SF",
    Pos == "C-SF"  ~ "C",
    Pos == "PG-SG" ~ "PG",
    Pos == "PG-SF" ~ "PG",
    Pos == "SG-PF" ~ "SG",
    Pos == "SF-C"  ~ "SF",
    Pos == "F-C"   ~ "PF",
    Pos == "F-G"   ~ "SF",
    Pos == "G-F"   ~ "SF",
    Pos == "F"     ~ "PF",
    Pos == "G"     ~ "SG",
    TRUE           ~ Pos
  ))
}


# -----------------------------------------------------------------------------
# keep_tot_rows()
# For players traded mid-season, keep only their combined "TOT" row and
# discard individual team rows.
#
# @param df  data.frame with columns Year, Player, Tm
# @return    Filtered data.frame
# -----------------------------------------------------------------------------
keep_tot_rows <- function(df) {
  df %>%
    dplyr::group_by(Year, Player) %>%
    dplyr::mutate(count_tot = sum(Tm == "TOT")) %>%
    dplyr::filter(count_tot == 0 | (count_tot > 0 & Tm == "TOT")) %>%
    dplyr::select(-count_tot) %>%
    as.data.frame()
}


# -----------------------------------------------------------------------------
# impute_physical()
# Replace missing Ht / Wt values with the positional mean.
#
# @param df  data.frame with columns Pos, Ht, Wt
# @return    data.frame with imputed Ht and Wt
# -----------------------------------------------------------------------------
impute_physical <- function(df) {
  df %>%
    dplyr::group_by(Pos) %>%
    dplyr::mutate(
      MeanWt = mean(Wt, na.rm = TRUE),
      MeanHt = mean(Ht, na.rm = TRUE)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      Wt = ifelse(is.na(Wt), MeanWt, Wt),
      Ht = ifelse(is.na(Ht), MeanHt, Ht)
    ) %>%
    dplyr::select(-MeanWt, -MeanHt) %>%
    as.data.frame()
}


# -----------------------------------------------------------------------------
# cluster_profiles()
# Compute the mean of each clustering variable per cluster.
#
# @param players_df   data.frame with player-level data and a Cluster column
# @return             Summarised data.frame (one row per cluster)
# -----------------------------------------------------------------------------
cluster_profiles <- function(players_df) {
  players_df %>%
    dplyr::group_by(Cluster) %>%
    dplyr::select(-Player, -Pos) %>%
    dplyr::summarise_all(mean)
}
