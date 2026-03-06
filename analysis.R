# =============================================================================
# analysis.R
# Clustering NBA Player Performance — 2000–2021
# Author: Alfonso Marino
# =============================================================================
# This script performs:
#   1. Data loading & cleaning
#   2. Physical attribute analysis (height / weight trends by position)
#   3. Four Factors analysis by position
#   4. K-Means clustering on 2020/21 season player performance
# =============================================================================

library(tidyverse)
library(ggplot2)
library(measurements)
library(factoextra)
library(GGally)

source("R/helpers.R")

# -----------------------------------------------------------------------------
# 1. DATA LOADING
# -----------------------------------------------------------------------------

nba         <- read.csv("data/seasons_stats.csv")
player_stat <- read.csv("data/player_data.csv")


# -----------------------------------------------------------------------------
# 2. INITIAL EXPLORATION
# -----------------------------------------------------------------------------

glimpse(nba)
glimpse(player_stat)


# -----------------------------------------------------------------------------
# 3. CLEANING — player_stat
# -----------------------------------------------------------------------------

# Check and count NA values
player_stat %>%
  summarise_all(list(~ sum(is.na(.))))

# Rows with missing weight
player_stat %>% filter(is.na(Wt))

# Remove rows with missing weight
player_stat <- player_stat %>% drop_na(Wt)

# Remove duplicates (keep first occurrence per player)
player_stat[duplicated(player_stat), ]
player_stat <- player_stat %>% distinct(Player, .keep_all = TRUE)

# Convert height from feet-inches to cm, weight from lbs to kg
player_stat <- player_stat %>%
  rowwise() %>%
  mutate(
    Ht = convert_height(Ht),
    Wt = round(conv_unit(Wt, "lbs", "kg"))
  ) %>%
  ungroup()

player_stat %>% select(Ht, Wt) %>% head(10)


# -----------------------------------------------------------------------------
# 4. CLEANING — nba (seasons_stats)
# -----------------------------------------------------------------------------

# Remove 2022 rows (duplicate of 2021 in the raw file)
nba <- nba %>% filter(Year < 2022)

# Add Hall of Fame indicator based on asterisk in player name
nba %>%
  filter(str_detect(Player, ".*\\*$")) %>%
  select(Player) %>%
  head(10)

nba <- nba %>%
  mutate(HallOfFame = if_else(str_detect(Player, ".*\\*$"), "Yes", "No"))

# Remove asterisks from player names
nba$Player        <- gsub("\\*$", "", nba$Player)
player_stat$Player <- gsub("\\*$", "", player_stat$Player)

# Add per-game statistics and Free Throw Rate (one of Oliver's Four Factors)
nba <- nba %>%
  mutate(
    MpG  = round(MP  / G, 3),
    PpG  = round(PTS / G, 3),
    ApG  = round(AST / G, 3),
    RpG  = round(TRB / G, 3),
    TOpG = round(TOV / G, 3),
    BpG  = round(BLK / G, 3),
    SpG  = round(STL / G, 3),
    FpG  = round(PF  / G, 3),
    .before = 9
  ) %>%
  mutate(FTr = round(FT / FGA, 3), .before = 30)

# Standardise multi-position labels to a single primary position
# SF = Small Forward | SG = Shooting Guard | PF = Power Forward
# PG = Point Guard   | C  = Centre
unique(nba$Pos)

nba <- nba %>%
  mutate(Pos = case_when(
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

table(nba$Pos)


# -----------------------------------------------------------------------------
# 5. MERGING
# -----------------------------------------------------------------------------

player_stat <- player_stat %>%
  select(Player, Ht, Wt) %>%
  as.data.frame()

nba <- left_join(nba, player_stat, by = "Player", relationship = "many-to-many")

nba <- nba %>%
  select(Year:Tm, HallOfFame:Wt, everything()) %>%
  distinct(Player, Year, Tm, Age, .keep_all = TRUE)

# Keep only TOT row for players who were traded mid-season
nba_withTOT <- nba %>%
  group_by(Year, Player) %>%
  mutate(count_tot = sum(Tm == "TOT")) %>%
  filter(count_tot == 0 | (count_tot > 0 & Tm == "TOT")) %>%
  select(-count_tot) %>%
  as.data.frame()


# -----------------------------------------------------------------------------
# 6. PERFORMANCE DATASET
# Filter: post-2000 seasons & above-average minutes per game
# -----------------------------------------------------------------------------

nba_performance <- nba_withTOT %>%
  filter(MpG > mean(MpG, na.rm = TRUE) & Year > 1999) %>%
  as.data.frame()

# NA summary
nba_performance %>% summarise_all(list(~ sum(is.na(.))))

# Impute missing 3P% and FT% with 0 (player did not attempt)
nba_performance$X3P. <- replace(nba_performance$X3P., is.na(nba_performance$X3P.), 0)
nba_performance$FT.  <- replace(nba_performance$FT.,  is.na(nba_performance$FT.),  0)

# Impute missing Ht / Wt with positional mean
nba_performance <- nba_performance %>%
  group_by(Pos) %>%
  mutate(
    MeanWt = mean(Wt, na.rm = TRUE),
    MeanHt = mean(Ht, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    Wt = ifelse(is.na(Wt), MeanWt, Wt),
    Ht = ifelse(is.na(Ht), MeanHt, Ht)
  ) %>%
  select(-MeanWt, -MeanHt) %>%
  as.data.frame()


# -----------------------------------------------------------------------------
# 7. PHYSICAL ANALYSIS
# -----------------------------------------------------------------------------

PosColorCode <- c("C" = "red", "PF" = "orange",
                  "SF" = "yellow", "SG" = "blue", "PG" = "green")

physique <- nba_withTOT %>%
  group_by(Year, Pos) %>%
  summarise(Height = mean(Ht, na.rm = TRUE),
            Weight = mean(Wt, na.rm = TRUE),
            .groups = "drop")

# -- Height vs Weight scatter by position
p1 <- ggplot(physique, aes(x = Weight, y = Height, color = Pos)) +
  geom_point(size = 4, alpha = 0.3) +
  scale_color_manual(values = PosColorCode, name = "") +
  labs(title = "Distribution of height and weight by position",
       x = "Weight (kg)", y = "Height (cm)") +
  theme_minimal()
p1
ggsave("output/figures/physique_scatter.png", p1, width = 8, height = 6, dpi = 150, bg = "white")

# -- Average height over time
avg <- nba_withTOT %>%
  group_by(Year) %>%
  summarise(Height = mean(Ht, na.rm = TRUE),
            Weight = mean(Wt, na.rm = TRUE),
            .groups = "drop")

p2 <- ggplot(avg, aes(x = Year, y = Height, linetype = "Trend line")) +
  geom_line() +
  geom_hline(aes(yintercept = mean(Height), linetype = "Avg line"), col = "red", alpha = 0.5) +
  scale_x_continuous(breaks = seq(1950, 2021, 10)) +
  scale_linetype_manual(name = "", values = c(2, 1), guide = guide_legend(reverse = TRUE)) +
  labs(x = "Year", y = "Height (cm)", title = "Evolution of average height") +
  theme_minimal() +
  theme(legend.position = "bottom")
p2

# -- Height evolution by position
p3 <- physique %>%
  ggplot(aes(x = Year, y = Height, group = Pos, color = Pos)) +
  geom_line() +
  scale_color_manual(values = PosColorCode, name = "") +
  scale_x_continuous(breaks = seq(1950, 2021, 10)) +
  labs(x = "Year", y = "Height (cm)", title = "Height evolution by position") +
  theme_minimal() +
  theme(legend.position = "bottom")
p3

# -- Average weight over time
p4 <- ggplot(avg, aes(x = Year, y = Weight, linetype = "Trend line")) +
  geom_line() +
  geom_hline(aes(yintercept = mean(Weight), linetype = "Avg line"), col = "red", alpha = 0.5) +
  scale_x_continuous(breaks = seq(1950, 2021, 10)) +
  scale_linetype_manual(name = "", values = c(2, 1), guide = guide_legend(reverse = TRUE)) +
  labs(x = "Year", y = "Weight (kg)", title = "Evolution of average weight") +
  theme_minimal() +
  theme(legend.position = "bottom")
p4

# -- Weight evolution by position
p5 <- physique %>%
  ggplot(aes(x = Year, y = Weight, group = Pos, color = Pos)) +
  geom_line() +
  scale_color_manual(values = PosColorCode, name = "") +
  scale_x_continuous(breaks = seq(1950, 2021, 10)) +
  labs(x = "Year", y = "Weight (kg)", title = "Weight evolution by position") +
  theme_minimal() +
  theme(legend.position = "bottom")
p5


# -----------------------------------------------------------------------------
# 8. FOUR FACTORS ANALYSIS BY POSITION
# Oliver's Four Factors: eFG%, TOV%, ORB%, FTr%
# -----------------------------------------------------------------------------

four_factors <- nba_performance %>%
  group_by(Pos) %>%
  summarise(
    "ORB%" = mean(ORB., na.rm = TRUE),
    "eFG%" = mean(eFG., na.rm = TRUE) * 100,
    "TOV%" = mean(TOV., na.rm = TRUE),
    "FTr%" = mean(FTr,  na.rm = TRUE) * 100,
    .groups = "drop"
  )

df_ff <- data.frame(
  stats    = rep(c("ORB%", "eFG%", "TOV%", "FTr%"), each = 5),
  position = rep(four_factors$Pos, times = 4),
  value    = c(four_factors$`ORB%`, four_factors$`eFG%`,
               four_factors$`TOV%`, four_factors$`FTr%`)
)

p6 <- ggplot(df_ff, aes(fill = position, y = value, x = stats)) +
  geom_bar(position = "dodge", stat = "identity", alpha = 0.8) +
  coord_flip() +
  scale_fill_manual(values = PosColorCode, name = "") +
  labs(x = "", y = "%", title = "Oliver's Four Factors by position") +
  theme_minimal() +
  theme(legend.position = "bottom")
p6
ggsave("output/figures/four_factors.png", p6, width = 8, height = 5, dpi = 150, bg = "white")


# -----------------------------------------------------------------------------
# 9. K-MEANS CLUSTERING — 2020/21 season
# Variables: TOV%, eFG%, ORB%, FTr%  (Oliver's Four Factors, per player)
# -----------------------------------------------------------------------------

players <- nba_performance %>%
  filter(Year == 2021) %>%
  mutate(
    "TOV%" = TOV.,
    "eFG%" = eFG. * 100,
    "ORB%" = ORB.,
    "FTr%" = FTr * 100
  ) %>%
  select(Player, Pos, `TOV%`, `eFG%`, `ORB%`, `FTr%`)

table(players$Pos)

players_data <- players[, 3:6]

# Correlation matrix of clustering variables
ggcorr(players_data, label = TRUE)

# Standardise
players_data_scale <- scale(players_data)
rownames(players_data_scale) <- players$Player

# -- Optimal number of clusters via Elbow method
p7 <- fviz_nbclust(players_data_scale, kmeans, method = "wss") +
  labs(subtitle = "Elbow method — optimal k")
p7
ggsave("output/figures/elbow_plot.png", p7, width = 7, height = 5, dpi = 150, bg = "white")

# -- Fit K-Means (k = 3)
set.seed(123)
km.out      <- kmeans(players_data_scale, centers = 3, iter.max = 100, nstart = 100)
km.clusters <- km.out$cluster

print(km.out)

# -- Cluster visualisation
p8 <- fviz_cluster(
  list(data = players_data_scale, cluster = km.clusters),
  main    = "K-Means Clustering — NBA 2020/21",
  ggtheme = theme_minimal()
)
p8
ggsave("output/figures/kmeans_cluster.png", p8, width = 9, height = 7, dpi = 150, bg = "white")

# -- Position distribution within clusters
table(km.clusters, players$Pos)

# -- Cluster profiles (mean of each variable per cluster)
cluster_profiles <- players %>%
  mutate(Cluster = km.clusters) %>%
  group_by(Cluster) %>%
  select(-Player, -Pos) %>%
  summarise_all(mean)

print(cluster_profiles)
