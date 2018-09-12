
#####################################################################################
### Project     : LoL Recommender System
### Script      : Preprocessing.R
### Description : Preprocessing crawled data 
#####################################################################################

#####################################################################################
### Setting up Environment
#####################################################################################

# Set working directory
setwd("C:/Users/Ro_Laptop/Dropbox/Public/github/LoL-Recommender")

# Load libraries
pkgs <- c("dplyr", "tidyr")
sapply(pkgs, require, character.only = T)


#####################################################################################
### Data load
#####################################################################################

play <- read.csv("Data/crawled_data_api/play.csv", stringsAsFactors = F)
champ <- read.csv("Data/crawled_data_api/champ.csv", stringsAsFactors = F)


#####################################################################################
### Preprocessing
#####################################################################################

temp <- play %>% left_join(champ %>% select(champId, champName), by = "champId") %>%
  select(gameId, champName, team, result)

gameId_list <- unique(temp$gameId)
game <- data.frame()
p <- progress_estimated(length(gameId_list))
for (game_id in gameId_list) {
  temp2 <- temp %>% filter(gameId == game_id) %>% arrange(team)
  temp3 <- data.frame(team100_1 = temp2$champName[1],
                      team100_2 = temp2$champName[2],
                      team100_3 = temp2$champName[3],
                      team100_4 = temp2$champName[4],
                      team100_5 = temp2$champName[5],
                      team100_6 = temp2$champName[6],
                      team100_7 = temp2$champName[7],
                      team100_8 = temp2$champName[8],
                      team100_9 = temp2$champName[9],
                      team100_10 = temp2$champName[10], 
                      result = ifelse(unique((temp2 %>% filter(team == 100))$result) == "Win", 1, 0))
  
  game <- game %>% bind_rows(temp3)
  p$tick()$print()
}

write.csv(game, "Data/game_result.csv", row.names = F)
