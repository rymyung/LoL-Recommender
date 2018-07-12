
#####################################################################################
### Setting up Environment
#####################################################################################

# Set working directory
setwd("C:/Users/Ro_Laptop/Dropbox/Public/공부/Project/LoL/2018/LoL_new")

# Load libraries
pkgs <- c("dplyr", "tidyr")
sapply(pkgs, require, character.only = T)

# Load data
user <- read.csv("crawled_data/user_position.csv", stringsAsFactors = F)
play <- read.csv("crawled_data/play_removed.csv", stringsAsFactors = F)
champ <- read.csv("crawled_data/champ.csv", stringsAsFactors = F)


#####################################################################################
### Getting Edges for Users
#####################################################################################

# Make user - usr matrix for edges
df <- table(unique(user$accountId), unique(user$accountId)) %>% data.frame() %>%
  spread(Var2, Freq)
row.names(df) <- df$Var1
df <- df %>% select(-Var1)

# Calculate Edge Weight
sm <- df
ss <- df
os <- df
mw <- df
ml <- df
re <- df
p <- progress_estimated(nrow(df)*ncol(df))
for (account in rownames(df)) {
  for (account2 in names(df)) {
    
    temp <- play %>% filter(accountId %in% c(account, account2))
    
    # Get list of gameId where players play together
    gameId_list <- table(temp$gameId) %>% data.frame() %>% filter(Freq > 1) %>% select(Var1)
    
    if (nrow(gameId_list) == 0) {
      sm_count <- 0
      ss_count <- 0
      os_count <- 0
      mw_count <- 0
      ml_count <- 0
      re_count <- 0
    } else {
      # Same match
      same_match <- temp %>% filter(gameId %in% gameId_list$Var1); head(same_match)
      side <- same_match %>% group_by(gameId) %>% summarize(is_team = sum(team)); head(side)
      
      # Same team
      gameId_list2 <- side %>% filter(is_team %in% c(200, 400)) %>% select(gameId); head(gameId_list2)
      if (nrow(gameId_list2) != 0) {
        play_result <- same_match %>% 
          filter(gameId %in% gameId_list2$gameId) %>% 
          select(gameId, result) %>% unique(); play_result
        
        mw_count <- play_result %>% filter(result == "Win") %>% nrow()
        ml_count <- play_result %>% filter(result == "Lose") %>% nrow()
      } else {
        mw_count <- 0
        ml_count <- 0
      }
      
      # Different team
      gameId_list3 <- side %>% filter(is_team == 300) %>% select(gameId); nrow(gameId_list3)
      diff_team <- same_match %>% filter(gameId %in% gameId_list3$gameId); head(diff_team, 20)
      
      if (nrow(diff_team) != 0) {
        relative <- table(diff_team$accountId, diff_team$result) %>% data.frame() %>% 
          rename(accountId = Var1, result = Var2, count = Freq) %>% filter(accountId == account)
        
        re_count <- relative[relative$result == 'Win',]$count - relative[relative$result == 'Lose',]$count
      } else {
        re_count <- 0
      }
      
      sm_count <- (table(temp$gameId) > 1) %>% sum()
      ss_count <- side %>% filter(is_team %in% c(200, 400)) %>% nrow()
      os_count <- side %>% filter(is_team == 300) %>% nrow()
    }
    
    
    # Assign to data frame
    sm[account, account2] <- sm_count
    ss[account, account2] <- ss_count
    os[account, account2] <- os_count
    mw[account, account2] <- mw_count
    ml[account, account2] <- ml_count
    re[account, account2] <- re_count
    
    p$tick()$print()
  }
}
diag(sm) <- NA
diag(ss) <- NA
diag(os) <- NA
diag(mw) <- NA
diag(ml) <- NA
diag(re) <- NA

write.csv(sm, "user_simple_match.csv", row.names = rownames(df))
write.csv(ss, "user_same_side.csv", row.names = rownames(df))
write.csv(os, "user_opposite_side.csv", row.names = rownames(df))
write.csv(mw, "user_match_win.csv", row.names = rownames(df))
write.csv(ml, "user_match_lose.csv", row.names = rownames(df))
write.csv(re, "user_relative.csv", row.names = rownames(df))


#####################################################################################
### Getting Edges for Users by Positions
#####################################################################################

position_relative_user <- function(posit, pr = F) {
  # Make champ - champ matrix for edges by positions
  user_temp <- user %>% filter(position == posit)
  df <- table(unique(user_temp$accountId), unique(user_temp$accountId)) %>% data.frame() %>%
    spread(Var2, Freq)
  row.names(df) <- df$Var1
  df <- df %>% select(-Var1)
  
  # Calculate Edge Weight
  re <- df
  for (account in rownames(df)) {
    for (account2 in names(df)) {
      
      temp <- play %>% filter(accountId %in% c(account, account2))
      
      # Get list of gameId where champions play together
      gameId_list <- table(temp$gameId) %>% data.frame() %>% filter(Freq > 1) %>% select(Var1)
      
      if (nrow(gameId_list) == 0) {
        re_count <- 0
      }else {
        same_match <- temp %>% filter(gameId %in% gameId_list$Var1)
        side <- same_match %>% group_by(gameId) %>% summarize(is_team = sum(team))
        
        # Different team
        gameId_list3 <- side %>% filter(is_team == 300) %>% select(gameId)
        diff_team <- same_match %>% filter(gameId %in% gameId_list3$gameId)
        
        if (nrow(diff_team) != 0) {
          check <- table(diff_team$gameId, diff_team$lane) %>% 
            data.frame() %>% spread(Var2, Freq)
          if (posit %in% names(check)) {
            gameId_list4 <- check[c(which(check[,posit] == 2)), 'Var1'] %>% as.character()
            
            final_match <- same_match %>% filter(gameId %in% gameId_list4)
            if (nrow(final_match) != 0) {
              relative <- table(final_match$accountId, final_match$result) %>% data.frame() %>% 
                rename(accountId = Var1, result = Var2, count = Freq) %>% filter(accountId == account)
              
              win <- relative[relative$result == 'Win',]$count
              lose <- relative[relative$result == 'Lose',]$count
              
              if (pr == T) {
                re_count <- (win-lose)/(win+lose)
              } else {
                re_count <- win-lose
              }
              if (re_count <= 0) {
                re_count <- 0
              }
            } else {
              re_count <- 0
            }
          } else {
            re_count <- 0
          }
        } else {
          re_count <- 0
        }
      }
      re[account, account2] <- re_count
    }
  }
  diag(re) <- NA
  
  return(re)
}

# With probability
user_top_re_pr <- position_relative_user('top', pr = T)
user_jungle_re_pr <- position_relative_user('jungle', pr = T)
user_mid_re_pr <- position_relative_user('middle', pr = T)
user_adc_re_pr <- position_relative_user('adc', pr = T)
user_support_re_pr <- position_relative_user('support', pr = T)

write.csv(user_top_re_pr, "user_top_relative_pr.csv", row.names = rownames(user_top_re_pr))
write.csv(user_jungle_re_pr, "user_jungle_relative_pr.csv", row.names = rownames(user_jungle_re_pr))
write.csv(user_mid_re_pr, "user_middle_relative_pr.csv", row.names = rownames(user_mid_re_pr))
write.csv(user_adc_re_pr, "user_adc_relative_pr.csv", row.names = rownames(user_adc_re_pr))
write.csv(user_support_re_pr, "user_support_relative_pr.csv", row.names = rownames(user_support_re_pr))

# Without probability
user_top_re <- position_relative_user('top')
user_jungle_re <- position_relative_user('jungle')
user_mid_re <- position_relative_user('middle')
user_adc_re <- position_relative_user('adc')
user_support_re <- position_relative_user('support')

write.csv(user_top_re, "user_top_relative.csv", row.names = rownames(user_top_re))
write.csv(user_jungle_re, "user_jungle_relative.csv", row.names = rownames(user_jungle_re))
write.csv(user_mid_re, "user_middle_relative.csv", row.names = rownames(user_mid_re))
write.csv(user_adc_re, "user_adc_relative.csv", row.names = rownames(user_adc_re))
write.csv(user_support_re, "user_support_relative.csv", row.names = rownames(user_support_re))


#####################################################################################
### Getting Edges for Champions
#####################################################################################


# Make champ - champ matrix for edges
df <- table(unique(champ$champId), unique(champ$champId)) %>% data.frame() %>%
  spread(Var2, Freq)
row.names(df) <- df$Var1
df <- df %>% select(-Var1)

# Calculate Edge Weight
sm <- df
ss <- df
os <- df
mw <- df
ml <- df
re <- df
p <- progress_estimated(nrow(df)*ncol(df))
for (champ in rownames(df)) {
  for (champ2 in names(df)) {
    
    temp <- play %>% filter(champId %in% c(champ, champ2))
    
    # Get list of gameId where champions play together
    gameId_list <- table(temp$gameId) %>% data.frame() %>% filter(Freq > 1) %>% select(Var1)
    
    if (nrow(gameId_list) == 0) {
      sm_count <- 0
      ss_count <- 0
      os_count <- 0
      mw_count <- 0
      ml_count <- 0
      re_count <- 0
    } else {
      # Same match
      same_match <- temp %>% filter(gameId %in% gameId_list$Var1); head(same_match)
      side <- same_match %>% group_by(gameId) %>% summarize(is_team = sum(team))
      
      # Same team
      gameId_list2 <- side %>% filter(is_team %in% c(200, 400)) %>% select(gameId)
      if (nrow(gameId_list2) != 0) {
        play_result <- same_match %>% 
          filter(gameId %in% gameId_list2$gameId) %>% 
          select(gameId, result) %>% unique()
        
        mw_count <- play_result %>% filter(result == "Win") %>% nrow()
        ml_count <- play_result %>% filter(result == "Lose") %>% nrow()
      } else {
        mw_count <- 0
        ml_count <- 0
      }
      
      # Different team
      gameId_list3 <- side %>% filter(is_team == 300) %>% select(gameId)
      diff_team <- same_match %>% filter(gameId %in% gameId_list3$gameId)
      
      if (nrow(diff_team) != 0) {
        relative <- table(diff_team$champId, diff_team$result) %>% data.frame() %>% 
          rename(champId = Var1, result = Var2, count = Freq) %>% filter(champId == champ)
        
        re_count <- relative[relative$result == 'Win',]$count - relative[relative$result == 'Lose',]$count
      } else {
        re_count <- 0
      }
      
      sm_count <- (table(temp$gameId) > 1) %>% sum()
      ss_count <- side %>% filter(is_team %in% c(200, 400)) %>% nrow()
      os_count <- side %>% filter(is_team == 300) %>% nrow()
    }
    
    
    # Assign to data frame
    sm[champ, champ2] <- sm_count
    ss[champ, champ2] <- ss_count
    os[champ, champ2] <- os_count
    mw[champ, champ2] <- mw_count
    ml[champ, champ2] <- ml_count
    re[champ, champ2] <- re_count
    
    p$tick()$print()
  }
}
diag(sm) <- NA
diag(ss) <- NA
diag(os) <- NA
diag(mw) <- NA
diag(ml) <- NA
diag(re) <- NA

write.csv(sm, "champ_simple_match.csv", row.names = rownames(df))
write.csv(ss, "champ_same_side.csv", row.names = rownames(df))
write.csv(os, "champ_opposite_side.csv", row.names = rownames(df))
write.csv(mw, "champ_match_win.csv", row.names = rownames(df))
write.csv(ml, "champ_match_lose.csv", row.names = rownames(df))
write.csv(re, "champ_relative.csv", row.names = rownames(df))


#####################################################################################
### Getting Edges for Champions by Positions
#####################################################################################

position_relative_champ <- function(posit, pr = F) {
  # Make champ - champ matrix for edges by positions
  champ_temp <- champ %>% filter(position1 == posit | position2 == posit)
  df <- table(unique(champ_temp$champId), unique(champ_temp$champId)) %>% data.frame() %>%
    spread(Var2, Freq)
  row.names(df) <- df$Var1
  df <- df %>% select(-Var1)
  
  # Calculate Edge Weight
  re <- df
  for (champ1 in rownames(df)) {
    for (champ2 in names(df)) {
      
      temp <- play %>% filter(champId %in% c(champ1, champ2))
      
      # Get list of gameId where champions play together
      gameId_list <- table(temp$gameId) %>% data.frame() %>% filter(Freq > 1) %>% select(Var1)
      
      if (nrow(gameId_list) == 0) {
        re_count <- 0
      }else {
        same_match <- temp %>% filter(gameId %in% gameId_list$Var1)
        side <- same_match %>% group_by(gameId) %>% summarize(is_team = sum(team))
        
        # Different team
        gameId_list3 <- side %>% filter(is_team == 300) %>% select(gameId)
        diff_team <- same_match %>% filter(gameId %in% gameId_list3$gameId)
        
        if (nrow(diff_team) != 0) {
          check <- table(diff_team$gameId, diff_team$lane) %>% 
            data.frame() %>% spread(Var2, Freq)
          if (posit %in% names(check)) {
            gameId_list4 <- check[c(which(check[,posit] == 2)), 'Var1'] %>% as.character()
            
            final_match <- same_match %>% filter(gameId %in% gameId_list4)
            if (nrow(final_match) != 0) {
              relative <- table(final_match$champId, final_match$result) %>% data.frame() %>% 
                rename(champId = Var1, result = Var2, count = Freq) %>% filter(champId == champ1)
              
              win <- relative[relative$result == 'Win',]$count
              lose <- relative[relative$result == 'Lose',]$count
              if (pr == T) {
                re_count <- (win-lose)/(win+lose)
              } else {
                re_count <- win-lose
              }
              if (re_count <= 0) {
                re_count <- 0
              }
            } else {
              re_count <- 0
            }
          } else {
            re_count <- 0
          }
        } else {
          re_count <- 0
        }
      }
      re[champ1, champ2] <- re_count
    }
  }
  diag(re) <- NA
  
  return(re)
}

# With probability
champ_top_re_pr <- position_relative_champ('top', pr = T)
champ_jungle_re_pr <- position_relative_champ('jungle', pr = T)
champ_mid_re_pr <- position_relative_champ('middle', pr = T)
champ_adc_re_pr <- position_relative_champ('adc', pr = T)
champ_support_re_pr <- position_relative_champ('support', pr = T)

write.csv(champ_top_re_pr, "champ_top_relative_pr.csv", row.names = rownames(champ_top_re_pr))
write.csv(champ_jungle_re_pr, "champ_jungle_relative_pr.csv", row.names = rownames(champ_jungle_re_pr))
write.csv(champ_mid_re_pr, "champ_middle_relative_pr.csv", row.names = rownames(champ_mid_re_pr))
write.csv(champ_adc_re_pr, "champ_adc_relative_pr.csv", row.names = rownames(champ_adc_re_pr))
write.csv(champ_support_re_pr, "champ_support_relative_pr.csv", row.names = rownames(champ_support_re_pr))

# Without probability
champ_top_re <- position_relative_champ('top')
champ_jungle_re <- position_relative_champ('jungle')
champ_mid_re <- position_relative_champ('middle')
champ_adc_re <- position_relative_champ('adc')
champ_support_re <- position_relative_champ('support')

write.csv(champ_top_re, "champ_top_relative.csv", row.names = rownames(champ_top_re))
write.csv(champ_jungle_re, "champ_jungle_relative.csv", row.names = rownames(champ_jungle_re))
write.csv(champ_mid_re, "champ_middle_relative.csv", row.names = rownames(champ_mid_re))
write.csv(champ_adc_re, "champ_adc_relative.csv", row.names = rownames(champ_adc_re))
write.csv(champ_support_re, "champ_support_relative.csv", row.names = rownames(champ_support_re))

#####################################################################################
### Match win - match lose
#####################################################################################

user_mw <- read.csv("user_edges/user_match_win.csv", stringsAsFactors = F)
user_ml <- read.csv("user_edges/user_match_lose.csv", stringsAsFactors = F)

user_match <- user_mw - user_ml
user_match$X <- user_mw$X
names(user_match) <- c("X", user_match$X)
write.csv(user_match, "user_edges/user_match.csv", row.names = F)

champ_mw <- read.csv("champ_edges/champ_match_win.csv", stringsAsFactors = F)
champ_ml <- read.csv("champ_edges/champ_match_lose.csv", stringsAsFactors = F)

champ_match <- champ_mw - champ_ml
champ_match$X <- champ_mw$X
names(champ_match) <- c("X", champ_match$X)
write.csv(champ_match, "champ_edges/champ_match.csv", row.names = F)
