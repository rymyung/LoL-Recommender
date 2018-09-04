
#####################################################################################
### Project     : LoL Recommender System
### Script      : Preprocessing.R
### Description : Preprocessing crawled data 
#####################################################################################

#####################################################################################
### Setting up Environment
#####################################################################################

# Set working directory
setwd("C:/Users/rymyu/Dropbox/Public/github/LoL-Recommender")

# Load libraries
pkgs <- c("dplyr", "stringr", "tm", "qdap")
sapply(pkgs, require, character.only = T)


#####################################################################################
### Data load
#####################################################################################

play <- read.csv("Data/crawled_data_opgg/gameRecord.csv", stringsAsFactors = F)


#####################################################################################
### Preprocessing
#####################################################################################

play$index <- 1:nrow(play)

gameId_temp <- 1:nrow(play)
gameId_list <- sample(gameId_temp, nrow(play))

game <- data.frame()
p <- progress_estimated(nrow(play))
for (i in 1:nrow(play)) {
  temp <- play %>% filter(time == play$time[i]) %>% 
    filter(myTeams == play$myTeams[i] || enemyTeams == play$myTeams[i]) %>%
    mutate(team = ifelse(myTeams == play$myTeams[i], 100, 200),
           gameId = gameId_list[i])
  
  game <- game %>% bind_rows(temp)
  play <- play %>% filter(!index %in% temp$index)
  gameId_list <- gameId_list[2:length(gameId_list)]
  
  p$tick()$print()
}
