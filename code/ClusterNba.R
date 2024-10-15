library(tidyverse)
library(ggplot2)
library(measurements)
library(factoextra)

nba = read.csv("/Users/alfonsomarino/Desktop/Progetti/nba/seasons_stats.csv")
player_stat = read.csv("/Users/alfonsomarino/Desktop/Progetti/nba/player_data.csv")

#visualization structures
player_stat %>%
  glimpse()

nba %>%
  glimpse()

#removal NA  and eventually duplicates in player_stat
player_stat %>% 
  select(everything()) %>% 
  summarise_all(list(~sum(is.na(.))))

player_stat %>%
  filter(is.na(Wt) == T)

player_stat = player_stat %>%
  drop_na(Wt)

player_stat[duplicated(player_stat),]

player_stat = player_stat %>% 
  distinct(Player, .keep_all = T)

#conversion measurments in cm and kg
convertHt <- function(x) {
  x <- as.character(x)
  split <- strsplit(x, "-")
  feet <- as.numeric(split[[1]][1])
  inch <- as.numeric(split[[1]][2])
  round(conv_unit(feet, "ft", "cm") + conv_unit(inch, "inch", "cm"),0)
}

player_stat <- player_stat %>% 
  rowwise() %>% 
  mutate(Ht = convertHt(Ht), Wt = round(conv_unit(Wt, "lbs", "kg")))

player_stat %>%
  select(c(Wt, Ht)) %>%
  head(10)

#Removal of records for the year 2022 as the same as those for 2021
nba = nba %>% 
  filter(Year<2022) 


#addition column for hall of famer
#consequently removal of asterisk in the name of hall of famee player
nba %>%
  filter(str_detect(nba$Player, ".*\\*$")) %>%
  select(Player) %>%
  head(10)

nba = nba %>%
  mutate(HallOfFame = if_else(str_detect(Player, ".*\\*$"), "Yes", "No"))

nba$Player = gsub("\\*$", "", nba$Player)
player_stat$Player <- gsub("\\*$", "", player_stat$Player)

#addition statistics per-game and FTr (fourth Oliver's factor )
nba = nba %>% 
  mutate(MpG = round(MP/G,3), PpG = round(PTS/G,3), ApG = round(AST/G,3), 
         RpG = round(TRB/G,3), TOpG = round(TOV/G,3), BpG = round(BLK/G,3), 
         SpG = round(STL/G,3),FpG = round(PF/G, 3), .before = 9)

nba = nba %>% 
  mutate(FTr = round(FT/FGA,3), .before = 30)

#rename positions
#SF = Small Forward
#SG = Shooting Guard
#PF = Power Forward
#PG = Point Guard
#C = Centre
unique(nba$Pos)

nba <- nba %>%
  mutate(Pos = case_when(
    Pos == "PF-C" ~ "PF",
    Pos == "C-F" ~ "C",
    Pos == "SF-SG" ~ "SF",
    Pos == "C-PF" ~ "C",
    Pos == "SG-SF" ~ "SG",
    Pos == "PF-SF" ~ "PF",
    Pos == "SF-PF" ~ "SF",
    Pos == "SG-PG" ~ "SG",
    Pos == "SF-PG" ~ "SF",
    Pos == "C-SF" ~ "C",
    Pos == "PG-SG" ~ "PG",
    Pos == "PG-SF" ~ "PG",
    Pos == "SG-PF" ~ "SG",
    Pos == "SF-C" ~ "SF",
    Pos == "F-C" ~ "PF",
    Pos == "F-G" ~ "SF",
    Pos == "G-F" ~ "SF",
    Pos == "F" ~ "PF",
    Pos == "G" ~ "SG",
    TRUE ~ Pos
  ))

table(nba$Pos)

#merging of datasets

player_stat = player_stat %>% 
  select(c("Player","Ht", "Wt"))
player_stat = as.data.frame(player_stat)

nba = left_join(nba, player_stat, by = "Player", relationship = "many-to-many")

nba = nba %>% 
  select(Year:Tm, HallOfFame:Wt, everything()) 

nba = nba %>% 
  distinct(Player,Year, Tm, Age, .keep_all = T)

#adjusted dataset with TOT team
nba_withTOT = nba %>%
  group_by(Year, Player) %>%
  mutate(count_tot = sum(Tm == "TOT")) %>%
  filter(count_tot == 0 | (count_tot > 0 & Tm == "TOT")) %>%
  select(-count_tot)

nba_withTOT = as.data.frame(nba_withTOT)

#adjusted dataset for performance analysis
nba_performance = nba_withTOT %>% 
  filter(MpG > mean(MpG, na.rm = T) & Year > 1999)

nba_performance = as.data.frame(nba_performance)

#analysis about NA values for nba_performance dataset
#datasets contain NA values or missing information (" ") for different reasons
#e.g. because some statistics have been registered starting from a certain year or
#simply because that information misses for that player
nba_performance %>% 
  summarise_all(list(~sum(is.na(.))))

#The NA values related to the Ht, Wt columns are due to the fact 
#that those players are not present in the player_stat dataset

nba_performance$X3P. = replace(nba_performance$X3P., is.na(nba_performance$X3P.), 0)
nba_performance$FT. = replace(nba_performance$FT., is.na(nba_performance$FT.), 0)

nba_performance = nba_performance %>%
  group_by(Pos) %>%
  mutate(MeanWt = mean(Wt, na.rm = TRUE),
         MeanHt = mean(Ht, na.rm = TRUE)) %>%
  ungroup()

nba_performance$Wt = ifelse(is.na(nba_performance$Wt), nba_performance$MeanWt, nba_performance$Wt)
nba_performance$Ht = ifelse(is.na(nba_performance$Ht), nba_performance$MeanHt, nba_performance$Ht)

nba_performance <- nba_performance %>% select(-c(MeanWt, MeanHt))

nba_performance = as.data.frame(nba_performance)



PosColorCode <- c("C"="red", "PF"="orange", 
                  "SF"="yellow" ,"SG"="blue", "PG"="green")

physique <- nba_withTOT %>% 
  group_by(Year, Pos) %>% 
  summarise("Height" = mean(Ht, na.rm = T), "Weight"= mean(Wt, na.rm = T))
physique

ggplot(physique, aes(x=Weight, y=Height, color=Pos)) + 
  geom_point(size=4, alpha = 0.3)+
  ggtitle("Distribution of height and weight by position")+
  scale_color_manual(values = PosColorCode, name = "")

#physique evolution
avg <- nba_withTOT %>% 
  group_by(Year) %>% 
  summarise("Height"=mean(Ht, na.rm = T), "Weight"=mean(Wt, na.rm = T))

ggplot(avg, aes(x=Year, y=Height, linetype = "Trend line")) +
  geom_line()+
  labs(x="Year", y="Height (cm)", title = "Evolution of average height")+
  geom_hline(aes(yintercept = mean(Height), linetype = "Avg line"), col = "red", alpha = 0.5) +
  scale_x_continuous(breaks = seq(1950, 2021, 10)) +
  scale_linetype_manual(name = "", values = c(2, 1), guide = guide_legend(reverse = TRUE))+
  theme(legend.position = "bottom")

physique %>%
  ggplot( aes(x=Year, y=Height, group=Pos, color=Pos)) +
  geom_line()+
  labs(x="Year", y = "Heaight (cm)", title = "Height evoultion by position")+
  theme(legend.position = "bottom")+
  scale_color_manual(values = PosColorCode , name = "")+
  scale_x_continuous(breaks = seq(1950, 2021, 10))


ggplot(avg, aes(x=Year, y=Weight, linetype = "Trend line")) +
  geom_line()+
  labs(x="Anno", y="Weight (Kg)", title = "Evolution of average weight")+
  geom_hline(aes(yintercept = mean(Weight), linetype = "Avg line"), col = "red", alpha = 0.5) +
  scale_x_continuous(breaks = seq(1950, 2021, 10)) +
  scale_linetype_manual(name = "", values = c(2, 1), guide = guide_legend(reverse = TRUE))+
  theme(legend.position = "bottom")

physique %>%
  ggplot( aes(x=Year, y= Weight, group=Pos, color=Pos)) +
  geom_line()+
  labs(x="Year", y = "Weight (Kg)", title = "Weight evolution by position")+
  theme(legend.position = "bottom")+
  scale_color_manual(values = PosColorCode , name = "")+
  scale_x_continuous(breaks = seq(1950, 2021, 10))


x = nba_performance %>% 
  group_by(Pos) %>% 
  summarise("ORB%"= mean(ORB., na.rm = T), "eFG%"=mean(eFG., na.rm = T)*100,
            "TOV%"=mean(TOV., na.rm = T), "FTr%" = mean(FTr, na.rm = T)*100)

df <- data.frame(stats = rep(c("ORB%", "eFG%", "TOV%", "FTr%"), each = 5),
                 position = rep(x$Pos, times = 4),
                 value = c(x$`ORB%`,x$`eFG%`, x$`TOV%`, x$`FTr%`))

ggplot(df, aes(fill = position, y = value, x = stats))+
  geom_bar(position = "dodge", stat = "identity", alpha = 0.8)+
  labs(x="%", y="")+
  coord_flip()+
  theme(legend.position = "bottom")+
  scale_fill_manual(values = PosColorCode, name = "")

#CLUSTERING KMEANS
players <- nba_performance %>% 
  filter(Year == 2021) %>% 
  mutate("FpG" = round(PF/G,3)) %>% 
  select(Player, Pos, PpG/MpG, ApG/MpG, RpG/MpG, BpG/MpG, SpG/MpG, TOpG/MpG, FpG/MpG)

players <- nba_performance %>%
  filter(Year == 2021) %>%
  mutate(Pp36 = round(PpG * 36 / MpG, 3),
         Ap36 = round(ApG * 36 / MpG, 3),
         Rp36 = round(RpG * 36 / MpG, 3),
         Bp36 = round(BpG * 36 / MpG, 3),
         Sp36 = round(SpG * 36 / MpG, 3),
         TOp36 = round(TOpG * 36 / MpG, 3),
         Fp36 = round(round(PF/G,3)* 36 / MpG, 3)) %>%
  select(Player, Pos, Pp36, Ap36, Rp36, Bp36, Sp36, TOp36, Fp36)

players = nba_performance %>% 
  filter(Year == 2021) %>%
  mutate("TOV%"= TOV., "eFG%"=(eFG.*100),
         "ORB%"=ORB., "FTr%" = (FTr*100)) %>%
  select(Player, Pos, "TOV%", "eFG%", "ORB%", "FTr%")

table(players$Pos)
players_data <- players[3:6]
ggcorr(players_data)
plot(players_data)

players_data_scale <- scale(players_data)

kmeans(players_data_scale, centers = 3, iter.max = 100, nstart = 100)


fviz_nbclust(players_data_scale, kmeans, method = "wss")+
  labs(subtitle = "Elbow method")


#kmeans 
set.seed(123)
km.out <- kmeans(players_data_scale, centers = 3)
print(km.out)
plot(players_data, col = km.out$cluster)

km.clusters <- km.out$cluster

rownames(players_data_scale) <- players$Player

fviz_cluster(list(data = players_data_scale, cluster= km.clusters), 
             main = "", ggtheme = theme_minimal())
table(km.clusters, players$Pos)


players %>%
  mutate(Cluster = km.clusters) %>% 
  group_by(Cluster) %>%
  select(-Player, -Pos) %>% 
  summarise_all(mean)


