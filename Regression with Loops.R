library(lubridate)
library(GGally)
library(ggplot2)
library(scatterplot3d)
library(sqldf)


setwd('C:/Training/R/CricketAnalysis/')

df <- read.csv("ODIData.csv")
names(df)[names(df) == 'VenueType.y'] <- 'VenueType'


Player1 <- sqldf("Select Player, Runs, Mins, BF, Fours, Sixes, SR, Pos, Inns, 
                 case when VenueType='Away' then 0 
                         when VenueType='Home' then 1
                         else 0 end as VenueType,
                         case 
                         when Opposition = 'v South Africa' then 1
                         when Opposition = 'v Australia' then 1
                         when Opposition = 'v New Zealand' then 1
                         when Opposition = 'v England' then 1
                         when Opposition = 'v Pakistan' then 1
                         else 0 end as OppositionType,
                         case when Result = 'Won' then 1 
                         else 0 end as ResultType,
                         case
                         when Dismissal = 'not out' then 1
                         when Dismissal = 'retired notout' then 1
                         else 0 end as DismissalType
                         
                         from df")

Player1$Boundaries <- (Player1$Fours + Player1$Sixes)
Player1$BoundariesPct <- ifelse(!Player1$Boundaries, 0, Player1$Boundaries / Player1$Runs)
Player1$SR <- ifelse(!Player1$BF, 0, (Player1$Runs * 100)/(Player1$BF))


# Players in database
PlayerTable <- read.csv("PlayerTable.csv")
player_results <- list()

for (i in 1:nrow(PlayerTable)) {
  
  player_results[[i]] <- data.frame(playerid = PlayerTable$PlayerID[i], stringsAsFactors = FALSE)
  
}

Player <- data.table::rbindlist(player_results, fill = TRUE)
Player <- unique(Player[,1])

rm(list= ls()[!(ls() %in% c('Player','df','Player1' ))])


## Creating loop variables for Balls faced, Opposition Type & Venue Type
BF <- data.frame(rep(1:200, each=1))
names(BF) <- c("BF")

OppType <- data.frame(rep(0:1, each=1))
names(OppType) <- c("OppType")

VenueType <- data.frame(rep(0:1, each=1))
names(VenueType) <- c("VenueType")

extraction_list <- list()
m <- 0

for (h in 1:nrow(Player)) {
  
  for (i in 1:nrow(BF)) {
    
    for (j in 0:nrow(OppType)) {
      
      for (k in 0:nrow(VenueType)) {
        
  
  m <- m+1
  
  tryCatch ({
  
  ds <- Player1[ which(Player1$Player==Player$playerid[h]), ]
  
  fit <- lm(Runs ~ BF + OppositionType + VenueType,
            data=ds)
  
  post <- predict(fit, data.frame(Player = Player$playerid[h],
                                  BF = BF$BF[i], 
                                  OppositionType=OppType$OppType[j], 
                                  VenueType=VenueType$VenueType[k])
                  )
  
  extraction_list[[m]] <- data.frame(Player = Player$playerid[h],
                                     Runs = post, 
                                     BF = BF$BF[i],
                                     OppositionType = OppType$OppType[j],
                                     VenueType = VenueType$VenueType[k],
                                     stringsAsFactors = FALSE)
  }, error = function(e) e)
  
      }
    }
  }
}

PlayerForecast <- data.table::rbindlist(extraction_list[!extraction_list=='NA'], fill = TRUE)



PlayerDS <- sqldf("Select distinct Player, PlayerName from df")

finaldf <- merge(PlayerForecast, PlayerDS, by="Player", all.x = TRUE)

write.csv(finaldf, "PlayerForecast.csv")


