source("func.r")

# import data by season
s1617 <- readSeason(1617)
s1516<- readSeason(1516)
s1415<- readSeason(1415)
s1314<- readSeason(1314)

# aggregate seasons
allSeasons <- rbind(s1617, s1516, s1415, s1314) %>%
  arrange(desc(Season), desc(Date))

# add per match statistics
staticData <- allSeasons %>%
  mutate(GD = FTHG - FTAG, # goal difference
         SD = HS - AS, # shot difference
         STD = HST - AST, # shot on target difference
         GR = FTHG/(FTHG + FTAG), # goal ratio
         SR = HS/(HS + AS), # shot ratio
         STR = HST/(HST + AST)) # shot on target ratio

# statistics per team, separate tables for home and away
teamCols <- c("Season", "Date", "Team", "Venue", "GF", "GA", 
              "SF", "SA", "STF", "STA", "FF", "FA")
# home
homeStats <- allSeasons %>%
  mutate(Team = HomeTeam,
         GF = FTHG, # goals for
         GA = FTAG, # goals against
         SF = HS, # shots for
         SA = AS, # shots against
         STF = HST, # shots on target for
         STA = AST, # shots on target against
         FF = HF, # fouls for
         FA = AF, # fouls against
         Venue = "H" # mark cols as home
         )
homeStats <- homeStats[, teamCols]
# away
awayStats <- allSeasons %>%
  mutate(Team = AwayTeam,
         GF = FTAG,
         GA = FTHG,
         SF = AS,
         SA = HS,
         STF = AST,
         STA = HST,
         FF = AF,
         FA = HF,
         Venue = "A"
  )
awayStats <- awayStats[, teamCols]

# home + away
allStats <- rbind(homeStats, awayStats) %>%
  arrange(desc(Date))

# create list with history stats for each team
teamList <- unique(allSeasons$HomeTeam)
statList <- list()
for(i in 1:length(teamList)) {
  statList[[i]] <- teamStats(allStats, teamList[i])
}
names(statList) <- teamList

data <- matchStats(allSeasons, statList) %>%
  mutate(HomeTeam = factor(HomeTeam),
         AwayTeam = factor(AwayTeam, levels = levels(HomeTeam)),
         FTHG_lim = FTHG,
         FTAG_lim = FTAG
         ) %>%
           na.omit
# limit max full time goals to 4 
data[data$FTHG > 4, "FTHG_lim"] <- 4
data[data$FTAG > 4, "FTAG_lim"] <- 4
# Numeric representation of Results
data$FTR_num <- 1
data[data$FTR == "A", "FTR_num"] <- -1
data[data$FTR == "D", "FTR_num"] <- 0