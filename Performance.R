
#####################################################################################
### Project     : LoL Recommender System
### Script      : Performance.R
### Description : Predict Game Result
#####################################################################################

#####################################################################################
### Setting up Environment
#####################################################################################

# Set working directory
setwd("C:/Users/Ro_Laptop/Dropbox/Public/공부/github/LoL-Recommender")

# Load libraries
pkgs <- c("dplyr", "tidyr", "ggplot2")
sapply(pkgs, require, character.only = T)

# Load data
user  <- read.csv("Data/crawled_data_api/user.csv", stringsAsFactors = F)
most  <- read.csv("Data/crawled_data_api/most7.csv", stringsAsFactors = F)
play  <- read.csv("Data/crawled_data_api/play.csv", stringsAsFactors = F)
champ <- read.csv("Data/crawled_data_api/champ.csv", stringsAsFactors = F)


#####################################################################################
### Preprocessing
#####################################################################################

# champ의 평균 승률
# champ의 평균 플레이 수
# champ를 플레이하는 user의 그 champ의 최근 2경기 승률
# champ를 플레이하는 user의 그 champ의 최근 10경기 승률
# champ를 플레이하는 user의 그 champ의 승률
# champ를 플레이하는 user의 그 champ의 플레이 수
# champ를 플레이하는 user의 그 champ의 평균 kda

head(user)
head(most)
head(play)
head(champ)

play %>% slice(1:10) %>% left_join(champ, by = "champId")

play %>% filter(accountId == 208788053) %>% 
  group_by(champId) %>% summarize(n_obs = n(),
                                  mean_kill = mean(kill),
                                  mean_death = mean(death),
                                  mean_assist = mean(assist),
                                  mean_dealing = mean(dealing),
                                  mean_damaged = mean(damaged)) %>%
  arrange(desc(n_obs)) %>% ungroup() %>% 
  left_join(champ %>% select(champId, champName), by = "champId")


play %>% filter(champId %in% c(69, 201)) %>% 
  ggplot() + geom_density(aes(kill, fill = "kill"), alpha = 0.3) +
  geom_density(aes(death, fill = "death"), alpha = 0.3) +
  geom_density(aes(assist, fill = "assist"), alpha = 0.3) +
  scale_fill_manual(name = "Type",
                    values = c("kill" = "darkorange",
                               "death" = "steelblue",
                               "assist" = "palegreen3")) +
  facet_grid(. ~ champId)


head(play)

play %>% left_join(champ, by = "champId") %>% 
  ggplot() + geom_density(aes(kill, fill = "kill"), alpha = 0.3) +
  geom_density(aes(death, fill = "death"), alpha = 0.3) +
  geom_density(aes(assist, fill = "assist"), alpha = 0.3) +
  scale_fill_manual(name = "Type",
                    values = c("kill" = "darkorange",
                               "death" = "steelblue",
                               "assist" = "palegreen3")) +
  facet_grid(. ~ position1)


play %>% left_join(champ, by = "champId") %>% 
  gather(key = "Type", value = "Value", kill, death, assist) %>%
  ggplot() + geom_density(aes(Value, fill = Type), alpha = 0.3) +
  facet_grid(Type ~ position1)
