
#####################################################################################
### Project     : LoL Recommender System
### Script      : Search_Position.R
### Description : Search Positions of Users
#####################################################################################

#####################################################################################
### Setting up Environment
#####################################################################################

# Load libraries
pkgs <- c("dplyr", 'stringr')
sapply(pkgs, require, character.only = T)

# Load data
play <- read.csv('crawled_data/play_master.csv', stringsAsFactors = F)
champ <- read.csv('crawled_data/champ.csv', stringsAsFactors = F) %>%
  mutate(champId = as.character(champId))
match <- read.csv('crawled_data/match.csv', stringsAsFactors = F)
most <- read.csv('crawled_data/most7.csv', stringsAsFactors = F)
user <- read.csv('crawled_data/user.csv', stringsAsFactors = F) %>%
  mutate(accountId = as.character(accountId))


#####################################################################################
### Position Search
#####################################################################################

play_new <- play %>% filter(champId != "error")
play_new2 <- play_new %>% group_by(gameId) %>% summarize(n_obs = n())
  filter(n_obs == 10)
play_all <- play_new %>% filter(gameId %in% play_new2$gameId)

# Merge data
play_champ <- play_all %>% left_join(champ, by = 'champId') %>%
  left_join(match, by = 'gameId') %>%
  filter(queue == '420'); head(play_champ)

# Search position
temp_all <- data.frame()
position_list <- c("top", "jungle", "middle", "adc", "support")
p <- progress_estimated(nrow(play_champ)/5)
for (i in 1:(nrow(play_champ)/5)) {
  temp <- play_champ[(5*i-4):(5*i),] %>% mutate(lane = "")
  
  for (i in position_list) {
    check1 <- str_detect(temp$position1, i)
    
    if ((sum(check1) == 1) && (temp$lane[which(check1)] == "")) {
      temp$lane[which(check1)] <- i
    } else {
      check2 <- str_detect(temp$position2, i); check2
      if ((sum(check2) == 1) && (temp$lane[which(check2)] == "")) {
        temp$lane[which(check2)] <- i
      }
    }
    
  }
  check3 <- temp %>% filter(lane == "") %>% nrow()
  if (check3 == 1) {
    temp$lane[temp$lane == ""] <- position_list[!(position_list %in% temp$lane)]
  }
  temp_all <- temp_all %>% bind_rows(temp)
  p$tick()$print()
}

no_position <- temp_all %>% filter(lane == "") %>% select(gameId) %>% unique()
reduced <- temp_all %>% filter(!gameId %in% no_position$gameId) %>%
  select(gameId, accountId, champId, team, kill, death, assist, dealing, damaged, cs,
         result, lane)
      
write.csv(reduced, "play_removed2.csv", row.names = F)      


#####################################################################################
### Position Search - User
#####################################################################################

champ_most <- most %>% left_join(champ, by='champId')

user_most <- table(champ_most$accountId,champ_most$position1) %>% 
  data.frame() %>%
  spread(Var2, Freq) %>% rename(accountId = Var1) %>% 
  mutate(accountId = as.character(accountId))


position <- c()
for (i in 1:nrow(user_most)) {
  position <- c(position, names(user_most[,-1])[which.max(user_most[i,-1])])
}

user_most$position <- position

user_new <- user %>% left_join(user_most %>% select(accountId, position), by = 'accountId')

write.csv(user_new, 'user_position.csv', row.names = F)

