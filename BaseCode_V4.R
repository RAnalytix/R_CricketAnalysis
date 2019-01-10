# install.packages("cricketr")

library(cricketr)
library(dplyr)
library(tidyr)
library(sqldf)

setwd('C:/Training/R/CricketAnalysis/')

# Get listing of venue
VenueTable <- read.csv("VenueTable.csv")
venue_results <- list()


for (i in 1:nrow(VenueTable)) {
  
  venue_results[[i]] <- data.frame(VenueID = VenueTable$VenueID[i], stringsAsFactors = FALSE)
  
}

Venue <- data.table::rbindlist(venue_results, fill = TRUE)
Venue <- unique(Venue[,1])

# Players in database
PlayerTable <- read.csv("PlayerTable.csv")
player_results <- list()

for (i in 1:nrow(PlayerTable)) {
  
  player_results[[i]] <- data.frame(playerid = PlayerTable$PlayerID[i], stringsAsFactors = FALSE)

}

Player <- data.table::rbindlist(player_results, fill = TRUE)
Player <- unique(Player[,1])

# Result Type
ResultTable <- read.csv("ResultTable.csv")
result_results <- list()

for (i in 1:nrow(ResultTable)) {
  
  result_results[[i]] <- data.frame(ResultID = ResultTable$ResultID[i], stringsAsFactors = FALSE)
 
}

Result <- data.table::rbindlist(result_results, fill = TRUE)
Result <- unique(Result[,1])


rm(list= ls()[!(ls() %in% c('Result', 'Venue', 'Player', 'PlayerTable', 'VenueTable', 'ResultTable'))])



# Data Processing for ODI
extraction_list <- list()
m <- 0

for (i in 1:nrow(Player)) {
  
  for (j in 1:nrow(Venue)) {
    
    for (k in 1:nrow(Result)) {
      
      m <- m+1
      
      tryCatch ({
        
        post <- getPlayerDataOD(profile = Player$playerid[i], type = "batting", homeOrAway = Venue$VenueID[j], result = Result$ResultID[k]) 
        
        extraction_list[[m]] <- data.frame(Player = Player$playerid[i],
                                           Runs = post$Runs,
                                           Mins = post$Mins,
                                           BF = post$BF,
                                           Fours = post$`4s`,
                                           Sixes = post$`6s`,
                                           SR = post$SR,
                                           Pos = post$Pos,
                                           Dismissal = post$Dismissal,
                                           Inns = post$Inns,
                                           Opposition = post$Opposition,
                                           Ground = post$Ground,
                                           StartDate = post$`Start Date`,
                                           VenueType = Venue$VenueID[j], 
                                           ResultType = Result$ResultID[k], 
                                           MatchType = 'ODI', 
                                           stringsAsFactors = FALSE)
      }, error = function(e) e)
      
    }
    
  }
}


extraction1 <- data.table::rbindlist(extraction_list[!extraction_list=='NA'], fill = TRUE)
extraction1a <- left_join(extraction1, PlayerTable, by=c("Player" = "PlayerID"))
extraction1b <- left_join(extraction1a, VenueTable, by=c("VenueType" = "VenueID"))
extraction1c <- left_join(extraction1b, ResultTable, by=c("ResultType" = "ResultID"))

extraction1 <- subset(extraction1c, select = -c(VenueType, ResultType))

#Data clean up
## Removing rows where Player did not Bat
extraction1 <- extraction1[!(extraction1$Runs == 'DNB'),]
extraction1 <- extraction1[!(extraction1$Runs == 'TDNB'),]

## Convert Runs to Numeric
extraction1 <- extraction1[!(extraction1$Runs == 'absent'),]
extraction1 <- extraction1[!(extraction1$Runs == 'sub'),]
extraction1$Runs = as.numeric(gsub("\\*", "", extraction1$Runs))

# Convert character to numerc in R
extraction1$BF <- as.numeric(as.character(extraction1$BF))

extraction1$Fours[extraction1$Fours=="-"] <- 0
extraction1$Fours <- as.numeric(as.character(extraction1$Fours))

extraction1$Sixes[extraction1$Sixes=="-"] <-0
extraction1$Sixes <- as.numeric(as.character(extraction1$Sixes))

extraction1$Pos <- as.numeric(as.character(extraction1$Pos))
extraction1$Inns <- as.numeric(as.character(extraction1$Inns))

extraction1$SR[extraction1$SR=="-"] <- 0
options(digits=3)
extraction1$SR = as.double(as.character(extraction1$SR))

ODI <- extraction1

# Data Processing for T20
extraction_list <- list()
m <- 0

for (i in 1:nrow(Player)) {
  
  for (j in 1:nrow(Venue)) {
    
    for (k in 1:nrow(Result)) {
      
      m <- m+1
      
      tryCatch ({
        
        post <- getPlayerDataTT(profile = Player$playerid[i], type = "batting", homeOrAway = Venue$VenueID[j], result = Result$ResultID[k]) 
        
        extraction_list[[m]] <- data.frame(Player = Player$playerid[i],
                                           Runs = post$Runs,
                                           Mins = post$Mins,
                                           BF = post$BF,
                                           Fours = post$`4s`,
                                           Sixes = post$`6s`,
                                           SR = post$SR,
                                           Pos = post$Pos,
                                           Dismissal = post$Dismissal,
                                           Inns = post$Inns,
                                           Opposition = post$Opposition,
                                           Ground = post$Ground,
                                           StartDate = post$`Start Date`,
                                           VenueType = Venue$VenueID[j], 
                                           ResultType = Result$ResultID[k], 
                                           MatchType = 'T20', 
                                           stringsAsFactors = FALSE)
      }, error = function(e) e)
      
    }
    
  }
}

#extraction1 <- data.table::rbindlist(extraction_list[!extraction_list=='NULL'], fill = TRUE)
extraction1 <- data.table::rbindlist(extraction_list[!extraction_list=='NA'], fill = TRUE)
extraction1a <- left_join(extraction1, PlayerTable, by=c("Player" = "PlayerID"))
extraction1b <- left_join(extraction1a, VenueTable, by=c("VenueType" = "VenueID"))
extraction1c <- left_join(extraction1b, ResultTable, by=c("ResultType" = "ResultID"))

extraction1 <- subset(extraction1c, select = -c(VenueType, ResultType))

#Data clean up
## Removing rows where Player did not Bat
extraction1 <- extraction1[!(extraction1$Runs == 'DNB'),]
extraction1 <- extraction1[!(extraction1$Runs == 'TDNB'),]

## Convert Runs to Numeric
extraction1$Runs = as.numeric(gsub("\\*", "", extraction1$Runs))

# Convert character to numerc in R
extraction1$BF <- as.numeric(as.character(extraction1$BF))

extraction1$Fours[extraction1$Fours=="-"] <- 0
extraction1$Fours <- as.numeric(as.character(extraction1$Fours))

extraction1$Sixes[extraction1$Sixes=="-"] <-0
extraction1$Sixes <- as.numeric(as.character(extraction1$Sixes))

extraction1$Pos <- as.numeric(as.character(extraction1$Pos))
extraction1$Inns <- as.numeric(as.character(extraction1$Inns))

extraction1$SR[extraction1$SR=="-"] <- 0
options(digits=3)
extraction1$SR = as.double(as.character(extraction1$SR))

T20 <- extraction1

# Data Processing for Test Matches
extraction_list <- list()
m <- 0

for (i in 1:nrow(Player)) {
  
  for (j in 1:nrow(Venue)) {
    
    for (k in 1:nrow(Result)) {
      
      m <- m+1
      
      tryCatch ({
        
        post <- getPlayerData(profile = Player$playerid[i], type = "batting", homeOrAway = Venue$VenueID[j], result = Result$ResultID[k]) 
        
        extraction_list[[m]] <- data.frame(Player = Player$playerid[i],
                                           Runs = post$Runs,
                                           Mins = post$Mins,
                                           BF = post$BF,
                                           Fours = post$`4s`,
                                           Sixes = post$`6s`,
                                           SR = post$SR,
                                           Pos = post$Pos,
                                           Dismissal = post$Dismissal,
                                           Inns = post$Inns,
                                           Opposition = post$Opposition,
                                           Ground = post$Ground,
                                           StartDate = post$`Start Date`,
                                           VenueType = Venue$VenueID[j], 
                                           ResultType = Result$ResultID[k], 
                                           MatchType = 'Test', 
                                           stringsAsFactors = FALSE)
      }, error = function(e) e)
      
    }
    
  }
}

#extraction1 <- data.table::rbindlist(extraction_list[!extraction_list=='NULL'], fill = TRUE)
extraction1 <- data.table::rbindlist(extraction_list[!extraction_list=='NA'], fill = TRUE)
extraction1a <- left_join(extraction1, PlayerTable, by=c("Player" = "PlayerID"))
extraction1b <- left_join(extraction1a, VenueTable, by=c("VenueType" = "VenueID"))
extraction1c <- left_join(extraction1b, ResultTable, by=c("ResultType" = "ResultID"))

extraction1 <- subset(extraction1c, select = -c(VenueType, ResultType))

#Data clean up
## Removing rows where Player did not Bat
extraction1 <- extraction1[!(extraction1$Runs == 'DNB'),]
extraction1 <- extraction1[!(extraction1$Runs == 'TDNB'),]
extraction1 <- extraction1[!(extraction1$Runs == 'absent'),]

## Convert Runs to Numeric
extraction1$Runs = as.numeric(gsub("\\*", "", extraction1$Runs))

# Convert character to numerc in R
extraction1$BF[extraction1$BF=="-"] <- 0
extraction1$BF <- as.numeric(as.character(extraction1$BF))

extraction1$Fours[extraction1$Fours=="-"] <- 0
extraction1$Fours <- as.numeric(as.character(extraction1$Fours))

extraction1$Sixes[extraction1$Sixes=="-"] <-0
extraction1$Sixes <- as.numeric(as.character(extraction1$Sixes))

extraction1$Pos <- as.numeric(as.character(extraction1$Pos))
extraction1$Inns <- as.numeric(as.character(extraction1$Inns))

extraction1$SR[extraction1$SR=="-"] <- 0
options(digits=3)
extraction1$SR = as.double(as.character(extraction1$SR))

Test <- extraction1

write.csv(ODI, file="ODIData.csv")
write.csv(T20, file="T20.csv")
write.csv(Test, file="Test.csv")

rm(list= ls()[!(ls() %in% c('T20','ODI','Test'))])


