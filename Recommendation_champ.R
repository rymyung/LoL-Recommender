
#####################################################################################
### Setting up Environment
#####################################################################################

# Load libraries
pkgs <- c("dplyr", "tidyr", "stringr", "igraph", 'visNetwork', "colorspace")
sapply(pkgs, require, character.only = T)


#####################################################################################
### Data load
#####################################################################################

# Load user data
user <- read.csv("crawled_data/user_position.csv", stringsAsFactors = F)

# Load champ data
champ <- read.csv("crawled_data/champ.csv", stringsAsFactors = F) %>%
  mutate(champId = as.character(champId))

# Load user's most champ data
most <- read.csv("crawled_data/most7.csv", stringsAsFactors = F)

# Load champ same side synergy data
champ_mt <- read.csv("champ_edges/champ_match.csv", stringsAsFactors = F)

# Load champ opposite side synergy data
champ_re <- read.csv("champ_edges/champ_relative.csv", stringsAsFactors = F)

# Load champ position synergy data
champ_top <- read.csv('champ_position_edges/champ_top_relative.csv', stringsAsFactors = F)
champ_jungle <- read.csv('champ_position_edges/champ_jungle_relative.csv', stringsAsFactors = F)
champ_middle <- read.csv('champ_position_edges/champ_middle_relative.csv', stringsAsFactors = F)
champ_adc <- read.csv('champ_position_edges/champ_adc_relative.csv', stringsAsFactors = F)
champ_support <- read.csv('champ_position_edges/champ_support_relative.csv', stringsAsFactors = F)


#####################################################################################
### Setting my team & opposite team
#####################################################################################

# Select my team randomly => will be revised when team-matching function is ready
position <- c("top", "jungle", "middle", "adc", "support")
my_team <- sapply(position, function(x) {
  position_df <- user %>% filter(position == x);
  choose <- sample(1:nrow(position_df), 1);
  selected_df <- position_df %>% slice(choose)
}) %>% t() %>% unlist() %>% matrix(nrow=5, ncol=3) %>%
  data.frame(row.names = position, stringsAsFactors = F) %>% 
  rename(accountId = X1, 
         summonerName = X2,
         position = X3)
my_team <- my_team[sample(position, 5),]; my_team

# Select opposite team randomly
position <- c("top", "jungle", "middle", "adc", "support")
opposite_team <- sapply(position, function(x) {
  position_df <- user %>% filter(position == x);
  choose <- sample(1:nrow(position_df), 1);
  selected_df <- position_df %>% slice(choose)
}) %>% t() %>% unlist() %>% matrix(nrow=5, ncol=3) %>%
  data.frame(row.names = position, stringsAsFactors = F) %>% 
  rename(accountId = X1, 
         summonerName = X2,
         position = X3)
opposite_team <- opposite_team[sample(position, 5),]; opposite_team

# Allocate team location to my team
team_choose <- sample(1:2, 1); team_choose
if (team_choose == 1) {
  
} else {
  
}

# Create empty data frame for final picked champs
my_team_pick <- data.frame(champId = c("", "", "", "", ""), 
                           accountId = c("", "", "", "", ""), 
                           row.names = position, 
                           stringsAsFactors = F)

opposite_team_pick <- data.frame(champId = c("", "", "", "", ""), 
                                 accountId = c("", "", "", "", ""), 
                                 row.names = position, 
                                 stringsAsFactors = F)


#####################################################################################
### Predefined functions
#####################################################################################

# Make edges data frame
create_edges <- function(matrix) {
  edges <- matrix %>% gather(key = "source", value = "weight", -X) %>%
    rename(target = X) %>%
    mutate(source = str_remove(source, "X"),
           weight = ifelse(is.na(weight), 0, weight)) %>%
    filter(weight > 0) %>%
    select(source, target, weight)
  
  return(edges)
}

# Make nodes data frame
create_nodes <- function(matrix, type) {
  temp <- matrix %>% data.frame() %>% rename(name = X)
  if (type == 'user') {
    nodes <- temp %>% left_join(user, by = c('name' = 'accountId')) %>%
      select(name, summonerName, position) %>% rename(id = summonerName)
  } else if (type == 'champ') {
    nodes <- temp %>% left_join(champ %>% mutate(champId = as.integer(champId)), 
                                by = c('name' = 'champId')) %>%
      select(name, champName, position1) %>% 
      rename(id = champName, position = position1)
  } else {
    print('error')
    return()
  }
  
  return(nodes)
}

# my pick
my_pick <- function(my_num) {
  
  # My position to pick
  temp_position <- my_team$position[my_num]
  my_user <- my_team$accountId[my_num]
  
  if (my_num == 1) {
    # Filter most3 champ by probabilities(= win ratio + No.play)
    first <- most %>% filter(accountId == my_user) %>%
      mutate(point = winratio + play_num) %>% 
      arrange(desc(point)) %>% top_n(3, point) %>%
      mutate(pr = point/sum(point)) %>% 
      select(champId, pr, winratio, play_num)
    
    # Select one champ with the probabilities
    first_pick <- first$champId[sample(1:3, 1, prob = first$pr)]
    
    temp_df <- data.frame(champId = first_pick, 
                           accountId = my_user,
                           stringsAsFactors = F)
    
  } else {
    index <- which(rownames(opposite_team_pick) == temp_position) # Opposite pick where my position is
    
    if (opposite_team_pick$champId[index] == "") { # If there is no champ in temp position of opposite team
      # Enemy synergy
      enemy_synergy <- calculate_synergy(position = temp_position,
                                         type = "enemy",
                                         idx = index)
      
      # Ally synergy
      ally_synergy <- calculate_synergy(position = temp_position,
                                        type = "ally",
                                        idx = index)
      
      # Calculate final score
      synergy <- synergy_score(enemy = enemy_synergy, ally = ally_synergy)
      
    } else { # If there is a champ in temp position of opposite team
      # Line synergy
      line_synergy <- calculate_synergy(position = temp_position,
                                        type = "line",
                                        idx = index)
      
      # Enemy synergy
      enemy_synergy <- calculate_synergy(position = temp_position,
                                         type = "enemy",
                                         idx = index)
      
      # Ally synergy
      ally_synergy <- calculate_synergy(position = temp_position,
                                        type = "ally",
                                        idx = index)
      # Calculate final score
      synergy <- synergy_score(enemy = enemy_synergy, ally = ally_synergy, line = line_synergy)
    }
    
    # Choose champ by score
    temp_df <- most %>% filter(accountId == my_user) %>%
      mutate(normed_ratio = (winratio - min(winratio))/(max(winratio)-min(winratio)), 
             normed_play = (play_num - min(play_num))/(max(play_num)-min(play_num)),
             most_score = normed_ratio + normed_play) %>% 
      inner_join(synergy %>% mutate(name = as.integer(name)), by = c('champId' = 'name')) %>%
      mutate(most_score = (most_score - min(most_score))/(max(most_score)-min(most_score)),
             synergy_score = (synergy_score - min(synergy_score))/(max(synergy_score)-min(synergy_score)),
             score = most_score + synergy_score) %>%
      arrange(desc(score)) %>% slice(1) %>% select(champId, accountId)
  }
  rownames(temp_df) <- temp_position
  
  return(temp_df)
}

# Calculate shortest path
calculate_path <- function(graph) {
  dist <- data.frame()
  for (node in V(graph)$name) {
    path_result <- shortest_paths(graph, from = node, to = V(graph))
    path_list <- sapply(path_result$vpath, function(x) {len <- length(x); ifelse(len==0, 0, len-1)})
    dist <- dist %>% bind_rows(data.frame(temp = c(node, path_list)) %>% t() %>% data.frame())
  };
  names(dist) <- c('name',paste0("X", V(graph)$name))
  rownames(dist) <- dist$name
  dist <- dist[,-1]
  
  for (i in 1:ncol(dist)) {
    dist[,i] <- as.numeric(dist[,i])
  }
  
  return(dist)
}

# Function for calculating synergy by line, enemy or ally
calculate_synergy <- function(position, type, idx) {
  
  # Caculate line synergy
  if (type == "line") {
    
    # Select position network
    if (position == "top") {
      line_graph <- champ_top_graph
    } else if (position == "jungle") {
      line_graph <- champ_jungle_graph
    } else if (position == "middle") {
      line_graph <- champ_middle_graph
    } else if (position == "adc") {
      line_graph <- champ_adc_graph
    } else {
      line_graph <- champ_support_graph
    }
    
    # Select target
    target <- opposite_team_pick$champId[idx]
    
    # Calculate shortest path
    line_path <- calculate_path(line_graph)
    
    # Make data frame of candidate with path
    line_synergy <- data.frame(name = rownames(line_path), 
                               line = line_path[,paste0("X",target)], 
                               stringsAsFactors = F)
    
    return(line_synergy)
    
    # Caculate enemy synergy
  } else if (type == "enemy") {
    
    # Select target
    target <- (opposite_team_pick[-idx,] %>% filter(champId != ""))$champId
    
    # Calculate shortest path
    enemy_path <- calculate_path(champ_re_graph)
    
    # Make data frame of candidate with path
    enemy_synergy <- data.frame(name = rownames(enemy_path),
                                stringsAsFactors = F) 
    if (class(enemy_path[,paste0("X",target)])!="data.frame") {
      enemy_synergy$enemy <- enemy_path[,paste0("X",target)]
    } else {
      enemy_synergy$enemy <- enemy_path[,paste0("X",target)] %>% rowSums()
    }
    
    enemy_synergy <- enemy_synergy %>%
      filter(name %in% (champ %>% filter(position1 == position | 
                                           position2 == position))$champId)
    
    return(enemy_synergy)
    
    # Calculate ally synergy
  } else if (type == "ally") {
    
    # Select target
    target <- (my_team_pick %>% filter(champId != ""))$champId
    
    # Calculate shortest path
    ally_path <- calculate_path(champ_mt_graph)
    
    # Make data frame of candidate with path
    ally_synergy <- data.frame(name = rownames(ally_path),
                             stringsAsFactors = F) 
    if (class(ally_path[,paste0("X",target)])!="data.frame") {
      ally_synergy$ally <- ally_path[,paste0("X",target)]
    } else {
      ally_synergy$ally <- ally_path[,paste0("X",target)] %>% rowSums()
    }
    
    ally_synergy <- ally_synergy %>%
      filter(name %in% (champ %>% filter(position1 == position | 
                                           position2 == position))$champId)
    
    return(ally_synergy)
  } else {
    print("wrong type")
    return()
  }
}


# Function for final synergy
synergy_score <- function(enemy, ally, line = NULL) {
  if (!is.null(line)) {
    
    temp_df <- line %>% 
      left_join(enemy, by = 'name')  %>% 
      left_join(ally, by = 'name') %>%
      filter(name != opposite_team_pick$champId[index]) %>%
      mutate(line = ifelse(line == 0, 0, round(1/line, 2)),
             enemy = ifelse(enemy == 0, 0, round(1/enemy,2)),
             ally = ifelse(ally == 0, 0, round(1/ally,2)))
    
    synergy_df <- temp_df %>% 
      mutate(synergy_score = apply(temp_df[,-1], 1, 
                                   function(x) {(x*matrix(c(0.5, 0.25, 0.25), nrow=1)) %>% sum()})) %>%
      arrange(desc(synergy_score))
    
    
    return(synergy_df)
    
  } else if (is.null(line)) {
    
    temp_df <- enemy %>% 
      left_join(ally, by = 'name') %>%
      filter(name != opposite_team_pick$champId[index]) %>%
      mutate(enemy = ifelse(enemy == 0, 0, round(1/enemy,2)),
             ally = ifelse(ally == 0, 0, round(1/ally,2)))
    
    synergy_df <- temp_df %>% 
      mutate(synergy_score = apply(temp_df[,-1], 1, 
                                   function(x) {(x*matrix(c(0.5, 0.25, 0.25), nrow=1)) %>% sum()})) %>%
      arrange(desc(synergy_score))
    return(synergy_df)
    
  } else {
    print("wrong type")
    return()
  }
  
}

# Function for randomly pick to opposite team
opp_random_pick <- function(opp_num, picked_champ) {
  temp_position <- opposite_team$position[opp_num]
  position_df <- champ %>% filter((position1 == temp_position | position2 == temp_position) 
                                  & (!champId %in% picked_champ))
  choose <- sample(1:nrow(position_df), 1)
  selected_df <- position_df %>% slice(choose)
  
  randomly_picked <- data.frame(champId = selected_df$champId,
                                accountId = opposite_team$accountId[opp_num],
                                stringsAsFactors = F)
  
  rownames(randomly_picked) <- temp_position
  
  return(randomly_picked)
}

#####################################################################################
### Network Creation
#####################################################################################


# Champ all enemy synergy
champ_re_edges <- create_edges(champ_re); head(champ_re_edges, 20)
champ_re_nodes <- create_nodes(champ_re, 'champ'); head(champ_re_nodes, 20)

champ_re_graph <- graph_from_data_frame(champ_re_edges, directed = T, 
                                        vertices = champ_re_nodes$name)

# Champ all my synergy
champ_mt_edges <- create_edges(champ_mt); head(champ_mt_edges, 20)
champ_mt_nodes <- create_nodes(champ_mt, 'champ'); head(champ_mt_nodes, 20)

champ_mt_graph <- graph_from_data_frame(champ_mt_edges, directed = F,
                                        vertices = champ_mt_nodes$name)



# Champ top synergy
champ_top_edges <- create_edges(champ_top); head(champ_top_edges, 20)
champ_top_nodes <- create_nodes(champ_top, 'champ'); head(champ_top_nodes, 20)

champ_top_graph <- graph_from_data_frame(champ_top_edges, directed = T,
                                         vertices = champ_top_nodes$name)


# Champ jungle synergy
champ_jungle_edges <- create_edges(champ_jungle); head(champ_jungle_edges, 20)
champ_jungle_nodes <- create_nodes(champ_jungle, 'champ'); head(champ_jungle_nodes, 20)

champ_jungle_graph <- graph_from_data_frame(champ_jungle_edges, directed = T,
                                            vertices = champ_jungle_nodes$name)


# Champ middle synergy
champ_middle_edges <- create_edges(champ_middle); head(champ_middle_edges, 20)
champ_middle_nodes <- create_nodes(champ_middle, 'champ'); head(champ_middle_nodes, 20)

champ_middle_graph <- graph_from_data_frame(champ_middle_edges, directed = T,
                                            vertices = champ_middle_nodes$name)


# Champ adc synergy
champ_adc_edges <- create_edges(champ_adc); head(champ_adc_edges, 20)
champ_adc_nodes <- create_nodes(champ_adc, 'champ'); head(champ_adc_nodes, 20)

champ_adc_graph <- graph_from_data_frame(champ_adc_edges, directed = T,
                                         vertices = champ_adc_nodes$name)


# Champ support synergy
champ_support_edges <- create_edges(champ_support); head(champ_support_edges, 20)
champ_support_nodes <- create_nodes(champ_support, 'champ'); head(champ_support_nodes, 20)

champ_support_graph <- graph_from_data_frame(champ_support_edges, directed = T,
                                             vertices = champ_support_nodes$name)


#####################################################################################
### Case 1 : My team pick first
### First pick of my team
#####################################################################################

# Select my team randomly => will be revised when team-matching function is ready
position <- c("top", "jungle", "middle", "adc", "support")
my_team <- sapply(position, function(x) {
  position_df <- user %>% filter(position == x);
  choose <- sample(1:nrow(position_df), 1);
  selected_df <- position_df %>% slice(choose)
}) %>% t() %>% unlist() %>% matrix(nrow=5, ncol=3) %>%
  data.frame(row.names = position, stringsAsFactors = F) %>% 
  rename(accountId = X1, 
         summonerName = X2,
         position = X3)
my_team <- my_team[sample(position, 5),]; my_team

# Select opposite team randomly
opposite_team <- sapply(position, function(x) {
  position_df <- user %>% filter(position == x);
  choose <- sample(1:nrow(position_df), 1);
  selected_df <- position_df %>% slice(choose)
}) %>% t() %>% unlist() %>% matrix(nrow=5, ncol=3) %>%
  data.frame(row.names = position, stringsAsFactors = F) %>% 
  rename(accountId = X1, 
         summonerName = X2,
         position = X3)
opposite_team <- opposite_team[sample(position, 5),]; opposite_team


# Create empty data frame for final picked champs
my_team_pick <- data.frame(champId = c("", "", "", "", ""), 
                           accountId = c("", "", "", "", ""), 
                           row.names = position, 
                           stringsAsFactors = F)

opposite_team_pick <- data.frame(champId = c("", "", "", "", ""), 
                                 accountId = c("", "", "", "", ""), 
                                 row.names = position, 
                                 stringsAsFactors = F)
my_num <- 1
opp_num <- 1
picked_champ <- c()


# My first pick
temp_df <- my_pick(my_num); temp_df
my_team_pick[rownames(temp_df),] <- temp_df

picked_champ <- c(picked_champ, unique(temp_df$champId)); picked_champ
my_num <- my_num + 1; my_num

my_team_pick
opposite_team_pick

# Opposite first and second pick
for (i in 1:2) {
  temp_opp_df <- opp_random_pick(opp_num, picked_champ)
  opposite_team_pick[rownames(temp_opp_df),] <- temp_opp_df
  picked_champ <- c(picked_champ, unique(temp_opp_df$champId))
  opp_num <- opp_num + 1
}
my_team_pick
opposite_team_pick


# My second and third pick
for (i in 1:2) {
  temp_my_df <- my_pick(my_num); temp_my_df
  my_team_pick[rownames(temp_my_df),] <- temp_my_df
  picked_champ <- c(picked_champ, unique(temp_my_df$champId)); picked_champ
  my_num <- my_num + 1; my_num
}
my_team_pick
opposite_team_pick

# Opposite third and fourth pick
for (i in 1:2) {
  temp_opp_df <- opp_random_pick(opp_num, picked_champ)
  opposite_team_pick[rownames(temp_opp_df),] <- temp_opp_df
  picked_champ <- c(picked_champ, unique(temp_opp_df$champId))
  opp_num <- opp_num + 1
}
my_team_pick
opposite_team_pick

# My fourth and fifth pick
for (i in 1:2) {
  temp_my_df <- my_pick(my_num); temp_my_df
  my_team_pick[rownames(temp_my_df),] <- temp_my_df
  picked_champ <- c(picked_champ, unique(temp_my_df$champId)); picked_champ
  my_num <- my_num + 1; my_num
}
my_team_pick
opposite_team_pick

# Opposite fifth pick
temp_opp_df <- opp_random_pick(opp_num, picked_champ)
opposite_team_pick[rownames(temp_opp_df),] <- temp_opp_df
picked_champ <- c(picked_champ, unique(temp_opp_df$champId))
opp_num <- opp_num + 1

my_team_pick %>% left_join(champ %>% select(champId, champName), by = 'champId')
opposite_team_pick %>% left_join(champ %>% select(champId, champName), by = 'champId')


#####################################################################################
### TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST
#####################################################################################


temp_position <- my_team$position[my_num]
my_user <- my_team$accountId[my_num]

index <- which(rownames(opposite_team_pick) == temp_position)
opposite_team_pick$champId[index] == ""

# Enemy synergy
enemy_synergy <- calculate_synergy(position = temp_position,
                                   type = "enemy",
                                   idx = index); enemy_synergy

# Ally synergy
ally_synergy <- calculate_synergy(position = temp_position,
                                  type = "ally",
                                  idx = index); ally_synergy

temp_df <- enemy_synergy %>% 
  left_join(ally_synergy, by = 'name') %>%
  filter(name != opposite_team_pick$champId[index]) %>%
  mutate(enemy = ifelse(enemy == 0, 0, round(1/enemy,2)),
         ally = ifelse(ally == 0, 0, round(1/ally,2))); temp_df

synergy_df <- temp_df %>% 
  mutate(synergy_score = apply(temp_df[,-1], 1, 
                               function(x) {(x*matrix(c(0.5, 0.25, 0.25), nrow=1)) %>% sum()})) %>%
  arrange(desc(synergy_score))

synergy <- synergy_score(enemy = enemy_synergy, ally = ally_synergy)

# Choose champ by score
temp_df <- most %>% filter(accountId == my_user) %>%
  mutate(normed_ratio = (winratio - min(winratio))/(max(winratio)-min(winratio)), 
         normed_play = (play_num - min(play_num))/(max(play_num)-min(play_num)),
         most_score = normed_ratio + normed_play) %>% 
  inner_join(synergy %>% mutate(name = as.integer(name)), by = c('champId' = 'name')) %>%
  mutate(most_score = (most_score - min(most_score))/(max(most_score)-min(most_score)),
         synergy_score = (synergy_score - min(synergy_score))/(max(synergy_score)-min(synergy_score)),
         score = most_score + synergy_score) %>%
  arrange(desc(score)) %>% top_n(1, score) %>% select(champId, accountId)
