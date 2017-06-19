library(dplyr)

# read and prepare csv table with data from on season
  # arguments: csv with match stats taken from 
  #            http://www.football-data.co.uk/germanym.php
  #            name convention for input: "_" + season years, e.g. "_1617"
  # value: ordered data.frame with relevant columns from input csv
readSeason <- function(season) {
  relCols <- c("Date", "HomeTeam", "AwayTeam", "FTHG", "FTAG", 
               "FTR", "HS", "AS", "HST", "AST", "HF", "AF")
  filename <- paste0("_", season, ".csv", sep = "")
  s <- read.csv(file = filename) %>%
    mutate(Season = season,
           Date = as.Date(as.character(Date), format = "%d/%m/%y"))
  s <- s[, c("Season", relCols)]
  s$HomeTeam <- gsub(" ", "_", s$HomeTeam)
  s$AwayTeam <- gsub(" ", "_", s$AwayTeam)
  print(paste0("Reading ", filename))
  print(paste0(nrow(s), " observations of ",
               ncol(s), " variables:"))
  print(names(s))
  s
}

# define weights for calculating a weighted mean of recent match stats 
  # arguments: extent <- length of output vector 
  #             (how many of recent matches are taken into account?)
  #            cutoff <- start of decrease 
  #             (from how many matchdays back are matches weighted less?)
  #            slope <- slope of decrease
  # value: vector with weigths for a weighted mean (sum() == 1). 
weight <- function(extent, cutoff, slope) {
  if(extent < cutoff) {
    cutoff <- extent #same weight for all if slope is beyond exent 
  }
  x <- c(1:extent) #cut input vector at extent
  y <- c()
  for (i in 1:length(x)) {
    if(i <= cutoff) {
      y <- c(y, 1) #before cutoff all values are set to 1
    } else {
      y <- c(y, 1-(i-cutoff)^2*slope) #after cutoff: exponential decrease
    }
  }
  y <- y[!is.na(y)] #limit extent to length of input vector (if input to short)
  if(y[length(y)] < 0) {
    y <- y + abs(y[length(y)]) #make all values >= 0
  }
  y <- y/sum(y) #set sum of output vector to 1
  y
}

# build a data frame with weighted mean stats for a single team
 # arguments: statframe <- data.frame with match&teamwise data
 #            team <- teamname (char)
 #            extent ... slope <- see weight()
 # value: a data frame for one team with additional columns for all weighted stats
teamStats <- function(statFrame, team, 
                        extent = 8, cutoff = 5, slope = 0.004) {
  teamFrame <- filter(statFrame, Team == team) #reduce data to the team's matches 
  for(i in nrow(teamFrame):1) { #loop for adding history stats by row/match
    statNames <- c("GF", "GA", "SF", "SA", "STF", "STA", "FF", "FA") #list of additional stats
    matchDate <- teamFrame[i, "Date"]
    matchVenue <- teamFrame[i, "Venue"]
    matchFrame <- filter(teamFrame, Date < matchDate, Venue == matchVenue) #reduce to matches before current row
    weightVals <- weight(extent, cutoff, slope)
    vecLength <- length(weightVals)
    if(nrow(matchFrame) < vecLength) { #cut result vector to remaining rows of df (if shorter) 
      vecLength <- nrow(matchFrame)
    }
    for (j in statNames) { #loop for adding all stats from vector "stats" to data frame
      teamFrame[i, paste0(j, "_")] <- sum(
        weightVals[1:vecLength]* #weights from function "weight"
          matchFrame[1:vecLength, j] #values from match stats
      )
    }
  }
  teamFrame
}

# add stat differences of opponents to match data table
matchStats <- function(matchTable, statList) {
  for(i in 1:nrow(matchTable)) {
    matchDate <- matchTable[i, "Date"]
    home <- matchTable[i, "HomeTeam"]
    away <- matchTable[i, "AwayTeam"]
    homeRow <- filter(statList[[home]], Date == matchDate)
    awayRow <- filter(statList[[away]], Date == matchDate)
    statNames <- c("GF", "GA", "SF", "SA", "STF", "STA", "FF", "FA")
    #columns with stat differences between teams
    matchTable[i, "GFH"] <- homeRow[1, "GF_"]
    matchTable[i, "GAH"] <- homeRow[1, "GA_"]
    matchTable[i, "SFH"] <- homeRow[1, "SF_"]
    matchTable[i, "SAH"] <- homeRow[1, "SA_"]
    matchTable[i, "STFH"] <- homeRow[1, "STF_"]
    matchTable[i, "STAH"] <- homeRow[1, "STA_"]
    matchTable[i, "FFH"] <- homeRow[1, "FF_"]
    matchTable[i, "FAH"] <- homeRow[1, "FA_"]
    matchTable[i, "GFA"] <- awayRow[1, "GF_"]
    matchTable[i, "GAA"] <- awayRow[1, "GA_"]
    matchTable[i, "SFA"] <- awayRow[1, "SF_"]
    matchTable[i, "SAA"] <- awayRow[1, "SA_"]
    matchTable[i, "STFA"] <- awayRow[1, "STF_"]
    matchTable[i, "STAA"] <- awayRow[1, "STA_"]
    matchTable[i, "FFA"] <- awayRow[1, "FF_"]
    matchTable[i, "FAA"] <- awayRow[1, "FA_"]
  }
  matchTable
}
