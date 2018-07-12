
#####################################################################################
### Setting up Environment
#####################################################################################

# Load libraries
pkgs <- c("dplyr", "tidyr", "stringr", "igraph", 'visNetwork', 
          "colorspace", "DescTools", "arules", "ggplot2", "RColorBrewer")
sapply(pkgs, require, character.only = T)

# Ignore warnings
options(warn = -1) 


#####################################################################################
### Data load
#####################################################################################

# Load user data
user <- read.csv("crawled_data/user_position.csv", stringsAsFactors = F)

# Load champ data
champ <- read.csv("crawled_data/champ.csv", stringsAsFactors = F)

# Load user's most champ data
most <- read.csv("crawled_data/most7.csv", stringsAsFactors = F)

# Load user same side synergy data
user_mt <- read.csv("user_edges/user_match.csv", stringsAsFactors = F)

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

# Load game record data
game  <- read.csv("crawled_data/win_data.csv", stringsAsFactors = F) ; head(game)


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

# Draw network : layout_with_mds, layout_in_circle
drawing_network <- function(node, edge, threshold, p = T, d = F, position) {
  
  # Filter by threshold
  adjusted_edges <- edge %>% filter(weight > threshold)
  adjusted_nodes <- node %>%
    filter(name %in% unique(c(unique(adjusted_edges$source), unique(adjusted_edges$target))))
  
  # Draw interactive network
  vis_edges <- adjusted_edges %>%
    rename(from = source,
           to = target)
  
  vis_nodes <- adjusted_nodes %>%
    rename(label = id,
           id = name)
  
  vis_nodes$color.border <- "black"
  vis_nodes$color.highlight.background <- "orange"
  vis_nodes$color.highlight.border <- "darkred"
  
  vis_edges$color <- "gray"
  vis_edges$width <- 1+vis_edges$weight/8 
  if (p & d) {
    vis_nodes$title <- vis_nodes$position
    vis_nodes$color.background <- rainbow_hcl(5)[factor(vis_nodes$position)]
    
    p <- visNetwork(vis_nodes, vis_edges, width='100%',height='950px', 
                    main=paste("network more than", threshold, "Games")) %>%
      visEdges(arrows ="to") %>%
      visOptions(highlightNearest = TRUE, 
                 selectedBy = "position") %>%
      visInteraction(navigationButtons = TRUE) %>%
      visLegend() %>%
      visIgraphLayout(layout = "layout_in_circle", physics = FALSE,
                      smooth = F, type = "square", randomSeed = NULL,
                      layoutMatrix = NULL)
  } else if (p & !d) {
    vis_nodes$title <- vis_nodes$position
    vis_nodes$color.background <- rainbow_hcl(5)[factor(vis_nodes$position)]
    
    p <- visNetwork(vis_nodes, vis_edges, width='100%',height='950px', 
                    main=paste("network more than", threshold, "Games")) %>%
      visOptions(highlightNearest = TRUE, 
                 selectedBy = "position") %>%
      visInteraction(navigationButtons = TRUE) %>%
      visLegend() %>%
      visIgraphLayout(layout = "layout_in_circle", physics = FALSE,
                      smooth = F, type = "square", randomSeed = NULL,
                      layoutMatrix = NULL)
  } else if (!p & d) {
    vis_nodes$title <- vis_nodes$label
    if (position == "adc") {
      vis_nodes$color.background <- rainbow_hcl(5)[1]
    } else if (position == "jungle") {
      vis_nodes$color.background <- rainbow_hcl(5)[2]
    } else if (position == "middle") {
      vis_nodes$color.background <- rainbow_hcl(5)[3]
    } else if (position == "support") {
      vis_nodes$color.background <- rainbow_hcl(5)[4]
    } else if (position == "top") {
      vis_nodes$color.background <- rainbow_hcl(5)[5]
    }
    p <- visNetwork(vis_nodes, vis_edges, width='100%',height='950px', 
                    main=paste("network more than", threshold, "Games")) %>%
      visEdges(arrows ="to") %>%
      visOptions(highlightNearest = TRUE) %>%
      visInteraction(navigationButtons = TRUE) %>%
      visLegend() %>%
      visIgraphLayout(layout = "layout_with_mds", physics = FALSE,
                      smooth = F, type = "square", randomSeed = NULL,
                      layoutMatrix = NULL)
  }
  else {
    vis_nodes$title <- vis_nodes$label
    p <- visNetwork(vis_nodes, vis_edges, width='100%',height='950px', 
                    main=paste("network more than", threshold, "Games")) %>%
      visOptions(highlightNearest = TRUE) %>%
      visInteraction(navigationButtons = TRUE) %>%
      visLegend() %>%
      visIgraphLayout(layout = "layout_with_mds", physics = FALSE,
                      smooth = F, type = "square", randomSeed = NULL,
                      layoutMatrix = NULL)
  }
  return(p) 
}


# Calculate network measures
network_measure <- function(graph, d = F) {
  measure_bet <- betweenness(graph, v = V(graph), directed = d, 
                             weights = NULL, nobigint = TRUE, normalized = TRUE)
  
  measure_close <- closeness(graph, vids = V(graph), mode = "all", 
                             weights = NULL, normalized = TRUE)
  
  measure_cc <- transitivity(graph, type = "global")
  
  measure_short <- calculate_path(graph);
  
  n <- 0
  for (i in 1:(length(V(graph))-1)) {
    n <- n + i
  }
  result <- data.frame(betweenness = mean(measure_bet), 
                       closeness = mean(measure_close), 
                       cluster_coef = measure_cc,
                       shortest = (sum(measure_short)/2)/n)
  
  return(result)
}


# Calculate shortest path in the graph
calculate_path <- function(graph) {
  dist <- data.frame()
  for (node in V(graph)$name) {
    path_result <- shortest_paths(graph, from = node, to = V(graph))
    path_list <- sapply(path_result$vpath, function(x) {len <- length(x); ifelse(len==0, 0, len-1)})
    dist <- dist %>% bind_rows(data.frame(node = c(node, path_list)) %>% t() %>% data.frame())
  };
  names(dist) <- c('name',paste0("X",V(graph)$name))
  rownames(dist) <- dist$name
  dist <- dist[,-1]
  
  for (i in 1:ncol(dist)) {
    dist[,i] <- as.numeric(dist[,i])
  }
  
  return(dist)
}

# Calculate user's most champs synergy
champ_synergy <- function(accountId1, accountId2) {
  user1_most <- (most %>% filter(accountId == accountId1))$champId
  user2_most <- (most %>% filter(accountId == accountId2))$champId
  if (nrow(most %>% filter(accountId == accountId2)) != 0) {
    temp <- champ_path %>% mutate(champId = rownames(champ_path)) %>%
      filter(champId %in% user1_most)
    rownames(temp) <- temp$champId
    result <- temp %>% select(paste0("X",user2_most))
    mean_path <- sum(result,na.rm=T)/(dim(result)[1]*dim(result)[2])
  } else {
    mean_path <- NA
  }
  
  return(mean_path)
}

# Recommend users
recommend_user <- function(target_user) {
  
  # Calculate user score
  target_path <- data.frame(accountId = rownames(user_path),
                            user_score = user_path[,paste0("X",target$name)], 
                            stringsAsFactors = F) %>%
    left_join(user %>% mutate(accountId = as.character(accountId)), by = "accountId") %>%
    filter(accountId != target$name & !is.na(position)) %>%
    arrange(user_score) %>% select(accountId, summonerName, position, user_score); head(target_path, 20)
  
  
  # Calculate champ score
  target_path$champ_score <- 0
  for (i in 1:nrow(target_path)) {
    target_path$champ_score[i] <- champ_synergy(target$name, target_path$accountId[i])
  }
  
  weight <- c(0.7, 0.3)
  # Calculate final score
  target_path <- target_path %>% 
    mutate(user_score = ifelse(user_score == 0, user_score, 1/user_score),
           champ_score = ifelse(champ_score == 0 | is.na(champ_score), champ_score, 1/champ_score)) %>%
    mutate(score = ifelse(is.na(champ_score), 
                          user_score, 
                          weight[1]*user_score + weight[2]*champ_score))
  
  # Select top 3 users in each position by score
  top_candidate <- target_path %>% filter(position == "top") %>% 
    arrange(score) %>% slice(1:3); top_candidate
  jungle_candidate <- target_path %>% filter(position == "jungle") %>% 
    arrange(score) %>% slice(1:3); jungle_candidate
  middle_candidate <- target_path %>% filter(position == "middle") %>% 
    arrange(score) %>% slice(1:3); middle_candidate
  adc_candidate <- target_path %>% filter(position == "adc") %>% 
    arrange(score) %>% slice(1:3); adc_candidate
  support_candidate <- target_path %>% filter(position == "support") %>% 
    arrange(score) %>% slice(1:3); support_candidate
  
  position_list <- c("top", "jungle", "middle", "adc", "support")
  final_candidate <- top_candidate %>% bind_rows(jungle_candidate) %>% 
    bind_rows(middle_candidate) %>% bind_rows(adc_candidate) %>% bind_rows(support_candidate) %>%
    filter(position %in% position_list[-which(position_list==target$position)]); final_candidate
  
  # Get possible combinations
  candidate_comb <- CombSet(final_candidate$accountId, 4, repl=FALSE, ord=FALSE) %>% data.frame()
  
  candidate_comb_reduced <- candidate_comb %>% 
    left_join(final_candidate %>% select(accountId, position, score), by = c("X1" = "accountId")) %>%
    rename(position1 = position, score1 = score) %>% 
    left_join(final_candidate %>% select(accountId, position, score), by = c("X2" = "accountId")) %>%
    rename(position2 = position, score2 = score) %>% 
    left_join(final_candidate %>% select(accountId, position, score), by = c("X3" = "accountId")) %>%
    rename(position3 = position, score3 = score) %>% 
    left_join(final_candidate %>% select(accountId, position, score), by = c("X4" = "accountId")) %>%
    rename(position4 = position, score4 = score)
  
  # filter posible combinations
  candidate_comb_reduced$count <- candidate_comb_reduced %>% 
    select(position1, position2, position3, position4) %>% 
    apply(1, function(x) {unique(x) %>% length()})
  
  final_comb <- candidate_comb_reduced %>% filter(count == 4) %>%
    mutate(comb_score = score1 + score2 + score3 + score4) %>%
    select(X1, X2, X3, X4, comb_score)  %>%
    #select(position1, X1, position2, X2, position3, X3, position4, X4, comb_score) %>% 
    arrange(comb_score) %>% slice(1:5); final_comb
  
  names(final_comb) <- c(position_list[-which(position_list==target$position)], "final_score")
  
  return(final_comb)
}

# my pick
my_pick <- function(my_num) {
  
  # My position to pick
  temp_position <- my_team$position[my_num]
  my_user <- my_team$accountId[my_num]
  
  if (my_num == 1) {
    # Filter most3 champ by probabilities(= win ratio + No.play)
    first <- most %>% filter(accountId == my_user) %>%
      mutate(normed_ratio = (winratio - min(winratio))/(max(winratio)-min(winratio)), 
             normed_play = (play_num - min(play_num))/(max(play_num)-min(play_num))) %>%
      mutate(point = normed_ratio + normed_play) %>% 
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
      synergy <- synergy_score(enemy = enemy_synergy, ally = ally_synergy, idx = index, position = temp_position)
      
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
      synergy <- synergy_score(enemy = enemy_synergy, ally = ally_synergy, idx = index, position = temp_position, line = line_synergy)
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

# Calculate synergy by line, enemy or ally
calculate_synergy <- function(position, type, idx) {
  
  # Caculate line synergy
  if (type == "line") {
    
    # Select position network
    if (position == "top") {
      line_path <- top_line_path
    } else if (position == "jungle") {
      line_path <- jungle_line_path
    } else if (position == "middle") {
      line_path <- middle_line_path
    } else if (position == "adc") {
      line_path <- adc_line_path
    } else {
      line_path <- support_line_path
    }
    
    # Select target
    target <- opposite_team_pick$champId[idx]
    
    # Make data frame of candidate with path
    line_synergy <- data.frame(name = rownames(line_path), 
                               line = line_path[,paste0("X",target)], 
                               stringsAsFactors = F)
    
    return(line_synergy)
    
    # Caculate enemy synergy
  } else if (type == "enemy") {
    
    # Select target
    target <- (opposite_team_pick[-idx,] %>% filter(champId != ""))$champId
    
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
    
    # Make data frame of candidate with path
    ally_synergy <- data.frame(name = rownames(champ_path),
                               stringsAsFactors = F) 
    if (class(champ_path[,paste0("X",target)])!="data.frame") {
      ally_synergy$ally <- champ_path[,paste0("X",target)]
    } else {
      ally_synergy$ally <- champ_path[,paste0("X",target)] %>% rowSums()
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

# Calculate synergy weights
calculate_weight <- function(position = NULL) {
  if (!is.null(position)) {
    # Select position network
    if (position == "top") {
      position_measure <- champ_top_measure
    } else if (position == "jungle") {
      position_measure <- champ_jungle_measure
    } else if (position == "middle") {
      position_measure <- champ_middle_measure
    } else if (position == "adc") {
      position_measure <- champ_adc_measure
    } else {
      position_measure <- champ_support_measure
    }
    
    weight_df <- champ_mt_measure %>% 
      bind_rows(champ_re_measure) %>%
      bind_rows(position_measure) %>%
      mutate(cc_weight = cluster_coef / sum(cluster_coef),
             sp_weight = shortest / sum(shortest),
             weight = cc_weight + sp_weight,
             weight = weight / sum(weight)) %>%
      select(weight)
    
  } else {
    weight_df <- champ_mt_measure %>% 
      bind_rows(champ_re_measure) %>%
      mutate(cc_weight = cluster_coef/sum(cluster_coef),
             sp_weight = shortest/sum(shortest),
             weight = cc_weight + sp_weight,
             weight = weight/sum(weight)) %>% 
      select(weight)
  }
  
  return(weight_df)
}

# Get final synergy score
synergy_score <- function(enemy, ally, idx, position, line = NULL) {
  if (!is.null(line)) { # When consider line synergy
    
    temp_df <- line %>% 
      left_join(enemy, by = 'name')  %>% 
      left_join(ally, by = 'name') %>%
      filter(name != opposite_team_pick$champId[idx]) %>%
      mutate(line = ifelse(line == 0, 0, round(1/line, 2)),
             enemy = ifelse(enemy == 0, 0, round(1/enemy,2)),
             ally = ifelse(ally == 0, 0, round(1/ally,2)))
    
    weight <- calculate_weight(position)
    
  } else if (is.null(line)) { # When not consider line synergy
    
    temp_df <- enemy %>% 
      left_join(ally, by = 'name') %>%
      filter(name != opposite_team_pick$champId[idx]) %>%
      mutate(enemy = ifelse(enemy == 0, 0, round(1/enemy,2)),
             ally = ifelse(ally == 0, 0, round(1/ally,2)))
    
    weight <- calculate_weight()
    
  } else {
    print("wrong type")
    return()
  }
  
  synergy_df <- temp_df %>% 
    mutate(synergy_score = apply(temp_df[,-1], 1, 
                                 function(x) {(x*(weight %>% as.matrix(nrow = 1))) %>% sum()})) %>%
    arrange(desc(synergy_score))
  
  return(synergy_df)
}

# Pick opposite team's champ randomly
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


# Remove duplicated rules
remove_rules <- function(rule){
  redundant.matrix <- is.subset(rule, rule)
  redundant.matrix[lower.tri(redundant.matrix, diag=T)] <- 0
  redundant <- colSums(redundant.matrix) >= 1
  game_pruned <- rule[!redundant]
  
  return(game_pruned)
}


# Check base performance
check_performance <- function() {
  
  threshold <- 0
  base_line <- data.frame()
  while (threshold < 100) {
    
    # Select picks randomly
    test= c()
    test = append(test,sample(champ$champName[champ$position1=='top'],1))
    test = append(test,sample(champ$champName[champ$position1=='middle'],1))
    test = append(test,sample(champ$champName[champ$position1=='jungle'],1))
    test = append(test,sample(champ$champName[champ$position1=='adc'],1))
    test = append(test,sample(champ$champName[champ$position1=='support'],1))
    
    win_confi_score <- 0 # Sum of confidence
    win_lift_score <- 0 # SUm of lift
    lose_confi_score <- 0 # Sum of confidence
    lose_lift_score <- 0 # SUm of lift
    win_n_rules <- 0 # No. of rules
    lose_n_rules <- 0 # No. of rules
    
    for (i in 1:length(Win_list)){
      a = length(Win_list[[i]])
      if (a == 3){
        if (is.element(Win_list[[i]][1] ,test) &
            is.element(Win_list[[i]][2] ,test) &
            is.element(Win_list[[i]][3] ,test)){
          win_confi_score = win_confi_score + Wrules$confidence[i]
          win_lift_score <- win_lift_score + Wrules$lift[i]
          win_n_rules <- win_n_rules + 1
        }}else{if (is.element(Win_list[[i]][1] ,test) &
                   is.element(Win_list[[i]][2] ,test)){
          win_confi_score = win_confi_score + Wrules$confidence[i]
          win_lift_score <- win_lift_score + Wrules$lift[i]
          win_n_rules <- win_n_rules + 1
        }}}
    
    for (i in 1:length(Lose_list)){
      a = length(Lose_list[[i]])
      if (a == 3){
        if (is.element(Lose_list[[i]][1] ,test) &
            is.element(Lose_list[[i]][2] ,test) &
            is.element(Lose_list[[i]][3] ,test)){
          lose_confi_score = lose_confi_score + Lrules$confidence[i]
          lose_lift_score <- lose_lift_score + Lrules$lift[i]
          lose_n_rules <- lose_n_rules + 1
        }}else{if (is.element(Lose_list[[i]][1] ,test) &
                   is.element(Lose_list[[i]][2] ,test)){
          lose_confi_score = lose_confi_score + Lrules$confidence[i]
          lose_lift_score <- lose_lift_score + Lrules$lift[i]
          lose_n_rules <- lose_n_rules + 1
        }}}
    
    if (win_n_rules >1) {
      win_final_confi_score <- win_confi_score / win_n_rules
      win_final_lift_score <- win_lift_score / win_n_rules
    } else {
      win_final_confi_score <- win_confi_score
      win_final_lift_score <- win_lift_score
    }
    if (lose_n_rules >1) {
      lose_final_confi_score <- lose_confi_score / lose_n_rules
      lose_final_lift_score <- lose_lift_score / lose_n_rules
    } else {
      lose_final_confi_score <- lose_confi_score
      lose_final_lift_score <- lose_lift_score
    }
    
    temp <- data.frame(champ1 = test[1],champ2 = test[2],champ3 = test[3],
                       champ4 = test[4],champ5 = test[5],
                       win_confidence = win_final_confi_score,
                       lose_confidence = lose_final_confi_score,
                       win_lift = win_final_lift_score,
                       lose_lift = lose_final_lift_score,
                       stringsAsFactors = F)
    base_line <- base_line %>% bind_rows(temp)
    threshold <- threshold + 1
  }
  return(base_line)
}


#####################################################################################
### Network Creation
#####################################################################################


# User same side synergy
user_mt_edges <- create_edges(user_mt)
user_mt_nodes <- create_nodes(user_mt, 'user')

user_mt_graph <- graph_from_data_frame(user_mt_edges, directed = F,
                                       vertices = user_mt_nodes$name)

# Champ same side synergy
champ_mt_edges <- create_edges(champ_mt)
champ_mt_nodes <- create_nodes(champ_mt, 'champ')

champ_mt_graph <- graph_from_data_frame(champ_mt_edges, directed = F,
                                        vertices = champ_mt_nodes$name)


# Champ opposite side synergy
champ_re_edges <- create_edges(champ_re)
champ_re_nodes <- create_nodes(champ_re, 'champ')

champ_re_graph <- graph_from_data_frame(champ_re_edges, directed = T, 
                                        vertices = champ_re_nodes$name)


# Champ top synergy
champ_top_edges <- create_edges(champ_top)
champ_top_nodes <- create_nodes(champ_top, 'champ')

champ_top_graph <- graph_from_data_frame(champ_top_edges, directed = T,
                                         vertices = champ_top_nodes$name)

# Champ jungle synergy
champ_jungle_edges <- create_edges(champ_jungle)
champ_jungle_nodes <- create_nodes(champ_jungle, 'champ')

champ_jungle_graph <- graph_from_data_frame(champ_jungle_edges, directed = T,
                                            vertices = champ_jungle_nodes$name)

# Champ middle synergy
champ_middle_edges <- create_edges(champ_middle)
champ_middle_nodes <- create_nodes(champ_middle, 'champ')

champ_middle_graph <- graph_from_data_frame(champ_middle_edges, directed = T,
                                            vertices = champ_middle_nodes$name)

# Champ adc synergy
champ_adc_edges <- create_edges(champ_adc)
champ_adc_nodes <- create_nodes(champ_adc, 'champ')

champ_adc_graph <- graph_from_data_frame(champ_adc_edges, directed = T,
                                         vertices = champ_adc_nodes$name)

# Champ support synergy
champ_support_edges <- create_edges(champ_support)
champ_support_nodes <- create_nodes(champ_support, 'champ')

champ_support_graph <- graph_from_data_frame(champ_support_edges, directed = T,
                                             vertices = champ_support_nodes$name)



#####################################################################################
### Drawing Network
#####################################################################################

# Degree Distribution
degree_value <- degree(champ_top_graph, v=V(champ_top_graph), mode = "all")
degree_df1 <- data.frame(degree = degree(champ_mt_graph, v=V(champ_mt_graph), mode = "all"),
                         graph = "champ_mt_graph")
degree_df2 <- data.frame(degree = degree(champ_re_graph, v=V(champ_re_graph), mode = "all"),
                         graph = "champ_re_graph")
degree_df3 <- data.frame(degree = degree(champ_top_graph, v=V(champ_top_graph), mode = "all"),
                         graph = "champ_top_graph")
degree_df_all <- rbind(degree_df1, degree_df2, degree_df3)

ggplot(degree_df_all, aes(degree, color = graph, fill = graph)) +
  geom_density(alpha = 0.2) + theme_minimal()


# User match win
drawing_network(user_mt_nodes %>% arrange(position), user_mt_edges, 3)

# Champ Same side synergy
drawing_network(champ_mt_nodes %>% arrange(position), champ_mt_edges, 15)

# Champ Relative
drawing_network(champ_re_nodes %>% arrange(position), champ_re_edges, 10, d = T)

# Top
drawing_network(champ_top_nodes, champ_top_edges, 3, p = F, d = T, position = "top")
drawing_network(champ_jungle_nodes, champ_jungle_edges, 3, p = F, d = T, position = "jungle")
drawing_network(champ_middle_nodes, champ_middle_edges, 3, p = F, d = T, position = "middle")
drawing_network(champ_adc_nodes, champ_adc_edges, 3, p = F, d = T, position = "adc")
drawing_network(champ_support_nodes, champ_support_edges, 3, p = F, d = T, position = "support")


#####################################################################################
### Network Measures
#####################################################################################

# Shortest path of user same side synergy
user_path <- calculate_path(user_mt_graph)

# Shortest path of most champ synergy
champ_path <- calculate_path(champ_mt_graph)

# Shortest path of champ opposite side synergy
enemy_path <- calculate_path(champ_re_graph)


# Shortest path of champ position synergy
top_line_path <- calculate_path(champ_top_graph)
jungle_line_path <- calculate_path(champ_jungle_graph)
middle_line_path <- calculate_path(champ_middle_graph)
adc_line_path <- calculate_path(champ_adc_graph)
support_line_path <- calculate_path(champ_support_graph)


# User same side synergy
user_mt_measure <- network_measure(user_mt_graph, d = F)

# Champ same side synergy
champ_mt_measure <-network_measure(champ_mt_graph, d = F)


# Champ opposite side synergy
champ_re_measure <-network_measure(champ_re_graph, d = T)


# Champ opposite side position synergy
champ_top_measure <-network_measure(champ_top_graph, d = T)
champ_jungle_measure <-network_measure(champ_jungle_graph, d = T)
champ_middle_measure <-network_measure(champ_middle_graph, d = T)
champ_adc_measure <-network_measure(champ_adc_graph, d = T)
champ_support_measure <-network_measure(champ_support_graph, d = T)


#####################################################################################
### User Recommendation
#####################################################################################

# Select user who get recommended randomly
target <- user_mt_nodes[sample(1:nrow(user_mt_nodes), 1),]; target


# Let's get it
recommended <- recommend_user(target); recommended


#####################################################################################
### Setting opposite team and else
#####################################################################################

# Select one of recommended users randomly
k <- sample(1:5, 1)

# Select my team 
my_team <- t(recommended[k,-5]) %>% data.frame() %>%
  rename(accountId = paste0("X", k)) %>% 
  mutate(accountId = as.character(accountId)) %>%
  bind_rows(data.frame(accountId = as.character(target$name), 
                       row.names = target$position, 
                       stringsAsFactors = F)) %>% 
  left_join(user %>% mutate(accountId = as.character(accountId)), by = "accountId"); my_team


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


# Create empty data frame for final picked champs
my_team_pick <- data.frame(champId = c("", "", "", "", ""), 
                           accountId = c("", "", "", "", ""), 
                           row.names = position, 
                           stringsAsFactors = F)

opposite_team_pick <- data.frame(champId = c("", "", "", "", ""), 
                                 accountId = c("", "", "", "", ""), 
                                 row.names = position, 
                                 stringsAsFactors = F)

my_num <- 1 # For my team pick
opp_num <- 1 # For opposite team pick
picked_champ <- c() # Prevent duplicated pick


#####################################################################################
### Case 1 : My team pick first
### First pick of my team
#####################################################################################

# My first pick
temp_df <- my_pick(my_num)
my_team_pick[rownames(temp_df),] <- temp_df; my_team_pick

picked_champ <- c(picked_champ, unique(temp_df$champId))
my_num <- my_num + 1

# Opposite first and second pick
for (i in 1:2) {
  temp_opp_df <- opp_random_pick(opp_num, picked_champ)
  opposite_team_pick[rownames(temp_opp_df),] <- temp_opp_df
  picked_champ <- c(picked_champ, unique(temp_opp_df$champId))
  opp_num <- opp_num + 1
}; opposite_team_pick

# My second and third pick
for (i in 1:2) {
  temp_my_df <- my_pick(my_num); temp_my_df
  my_team_pick[rownames(temp_my_df),] <- temp_my_df
  picked_champ <- c(picked_champ, unique(temp_my_df$champId)); picked_champ
  my_num <- my_num + 1
}; my_team_pick

# Opposite third and fourth pick
for (i in 1:2) {
  temp_opp_df <- opp_random_pick(opp_num, picked_champ)
  opposite_team_pick[rownames(temp_opp_df),] <- temp_opp_df
  picked_champ <- c(picked_champ, unique(temp_opp_df$champId))
  opp_num <- opp_num + 1
}; opposite_team_pick

# My fourth and fifth pick
for (i in 1:2) {
  temp_my_df <- my_pick(my_num); temp_my_df
  my_team_pick[rownames(temp_my_df),] <- temp_my_df
  picked_champ <- c(picked_champ, unique(temp_my_df$champId)); picked_champ
  my_num <- my_num + 1
}; my_team_pick

# Opposite fifth pick
temp_opp_df <- opp_random_pick(opp_num, picked_champ)
opposite_team_pick[rownames(temp_opp_df),] <- temp_opp_df; opposite_team_pick
picked_champ <- c(picked_champ, unique(temp_opp_df$champId))
opp_num <- opp_num + 1


# Recommender result
final_pick <- my_team_pick %>% 
  left_join(champ %>% mutate(champId = as.character(champId)) %>% 
              select(champId, champName), by = 'champId'); final_pick


#####################################################################################
### Recommender System Performance
#####################################################################################

# Make Transactions Data Set
game_list <- list()
for(i in 1:nrow(game)) {
  game_list[i] <- list(paste(game[i,1:6]))
}
game_trans <- as(game_list, "transactions"); game_trans

# Make Rules
set.seed(123)

Win_AR <- apriori(game_trans,
                  parameter  = list(minlen = 3, supp=0.003, conf=0),
                  appearance = list(rhs = c("Win"),
                                    default = "lhs")); summary(Win_AR)

Lose_AR <- apriori(game_trans,
                   parameter  = list(minlen = 3, supp=0.003, conf=0),
                   appearance = list(rhs = c("Lose"),
                                     default = "lhs")); summary(Lose_AR)

# Remove Redundancy
Win_rule <- remove_rules(Win_AR)
Lose_rule <- remove_rules(Lose_AR)

# Extract rules
Wrules <- inspect(sort(Win_rule, by = "confidence"))
Lrules <- inspect(sort(Lose_rule, by = "confidence"))

Win_list <- list()
Lose_list <- list()

for (i in 1:length(Wrules$lhs)){
  Win_list[i] <- as.character(Wrules$lhs[i]) %>% str_remove("\\{")  %>% str_remove("\\}") %>% str_split(",")
}
for (i in 1:length(Lrules$lhs)){
  Lose_list[i] <- as.character(Lrules$lhs[i]) %>% str_remove("\\{")  %>% str_remove("\\}") %>% str_split(",")
}


# Base Performance
base_performance_all <- data.frame()
p <- progress_estimated(100)
for (i in 1:100) {
  random_pick <- check_performance()
  base_performance <- random_pick[,6:9] %>% colMeans(na.rm = T)
  base_performance_all <- base_performance_all %>% bind_rows(base_performance)
  p$tick()$print()
}; colMeans(base_performance_all)


# Check performance
count <- 1
result_df <- data.frame()
while (T) {
  print(paste0("No. of try : ", count))
  
  # Select user who get recommended randomly
  target <- user_mt_nodes[sample(1:nrow(user_mt_nodes), 1),]; target
  
  # Let's get it
  recommended <- recommend_user(target); recommended
  
  # Select one of recommended users randomly
  k <- sample(1:5, 1)
  
  # Select my team 
  my_team <- t(recommended[k,-5]) %>% data.frame() %>%
    rename(accountId = paste0("X", k)) %>% 
    mutate(accountId = as.character(accountId)) %>%
    bind_rows(data.frame(accountId = as.character(target$name), 
                         row.names = target$position, 
                         stringsAsFactors = F)) %>% 
    left_join(user %>% mutate(accountId = as.character(accountId)), by = "accountId"); my_team
  
  
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
  
  
  # Create empty data frame for final picked champs
  my_team_pick <- data.frame(champId = c("", "", "", "", ""), 
                             accountId = c("", "", "", "", ""), 
                             row.names = position, 
                             stringsAsFactors = F)
  
  opposite_team_pick <- data.frame(champId = c("", "", "", "", ""), 
                                   accountId = c("", "", "", "", ""), 
                                   row.names = position, 
                                   stringsAsFactors = F)
  
  my_num <- 1 # For my team pick
  opp_num <- 1 # For opposite team pick
  picked_champ <- c() # Prevent duplicated pick
  
  # My first pick
  temp_df <- my_pick(my_num)
  my_team_pick[rownames(temp_df),] <- temp_df; my_team_pick
  
  picked_champ <- c(picked_champ, unique(temp_df$champId))
  my_num <- my_num + 1
  
  # Opposite first and second pick
  for (i in 1:2) {
    temp_opp_df <- opp_random_pick(opp_num, picked_champ)
    opposite_team_pick[rownames(temp_opp_df),] <- temp_opp_df
    picked_champ <- c(picked_champ, unique(temp_opp_df$champId))
    opp_num <- opp_num + 1
  }; opposite_team_pick
  
  # My second and third pick
  for (i in 1:2) {
    temp_my_df <- my_pick(my_num); temp_my_df
    my_team_pick[rownames(temp_my_df),] <- temp_my_df
    picked_champ <- c(picked_champ, unique(temp_my_df$champId)); picked_champ
    my_num <- my_num + 1
  }; my_team_pick
  
  # Opposite third and fourth pick
  for (i in 1:2) {
    temp_opp_df <- opp_random_pick(opp_num, picked_champ)
    opposite_team_pick[rownames(temp_opp_df),] <- temp_opp_df
    picked_champ <- c(picked_champ, unique(temp_opp_df$champId))
    opp_num <- opp_num + 1
  }; opposite_team_pick
  
  # My fourth and fifth pick
  for (i in 1:2) {
    temp_my_df <- my_pick(my_num); temp_my_df
    my_team_pick[rownames(temp_my_df),] <- temp_my_df
    picked_champ <- c(picked_champ, unique(temp_my_df$champId)); picked_champ
    my_num <- my_num + 1
  }; my_team_pick
  
  # Opposite fifth pick
  temp_opp_df <- opp_random_pick(opp_num, picked_champ)
  opposite_team_pick[rownames(temp_opp_df),] <- temp_opp_df; opposite_team_pick
  picked_champ <- c(picked_champ, unique(temp_opp_df$champId))
  opp_num <- opp_num + 1
  
  
  # Recommender result
  final_pick <- my_team_pick %>% 
    left_join(champ %>% mutate(champId = as.character(champId)) %>% 
                select(champId, champName), by = 'champId'); final_pick
  
  
  # Performance check
  win_confi_score <- 0 # Sum of confidence
  win_lift_score <- 0 # SUm of lift
  lose_confi_score <- 0 # Sum of confidence
  lose_lift_score <- 0 # SUm of lift
  win_n_rules <- 0 # No. of rules
  lose_n_rules <- 0 # No. of rules
  
  for (i in 1:length(Win_list)){
    len <- length(Win_list[[i]])
    check <- is.element(Win_list[[i]], final_pick$champName) %>% sum()
    if (check == len) {
      win_confi_score = win_confi_score + Wrules$confidence[i]
      win_lift_score <- win_lift_score + Wrules$lift[i]
      win_n_rules <- win_n_rules + 1
    }
  }
  for (i in 1:length(Lose_list)){
    len <- length(Lose_list[[i]])
    check <- is.element(Lose_list[[i]], final_pick$champName) %>% sum()
    if (check == len) {
      lose_confi_score = lose_confi_score + Lrules$confidence[i]
      lose_lift_score <- lose_lift_score + Lrules$lift[i]
      lose_n_rules <- lose_n_rules + 1
    }
  }
  
  if (win_n_rules >1) {
    win_final_confi_score <- win_confi_score / win_n_rules
    win_final_lift_score <- win_lift_score / win_n_rules
  } else {
    win_final_confi_score <- win_confi_score
    win_final_lift_score <- win_lift_score
  }
  if (lose_n_rules >1) {
    lose_final_confi_score <- lose_confi_score / lose_n_rules
    lose_final_lift_score <- lose_lift_score / lose_n_rules
  } else {
    lose_final_confi_score <- lose_confi_score
    lose_final_lift_score <- lose_lift_score
  }
  
  temp_df <- data.frame(champ1 = final_pick$champName[1],
                        champ2 = final_pick$champName[2],
                        champ3 = final_pick$champName[3],
                        champ4 = final_pick$champName[4],
                        champ5 = final_pick$champName[5],
                        win_confi = win_final_confi_score,
                        lose_confi = lose_final_confi_score,
                        win_lift = win_final_lift_score,
                        lose_lift = lose_final_lift_score,
                        stringsAsFactors = F)
  result_df <- result_df %>% bind_rows(temp_df)
  
  if (count > 100) {
    break
  }
  
  count <- count + 1
}; result_df
colMeans(result_df[,6:9])


# Performance comparison
performance_df <- data.frame(model = c("Base1",  "Recommender"),
                             performance = c(round(colMeans(base_performance_all)[1], 4), 
                                             round(colMeans(result_df[,6:9])[1], 4)))

df <- data.frame(base = base_performance_all$win_confidence,
                 recommender = result_df$win_confi[1:100])

# Remove 0
df_new <- df %>% filter(base != 0 & recommender != 0)

df_new %>% 
  ggplot(aes(base)) + geom_histogram(bins = 10, fill =col[4])

df_new %>% 
  ggplot(aes(recommender)) + geom_histogram(bins = 10, fill = "#91ACE1")


performance_df_new <- data.frame(model = c("Base", "Recommender"),
                                 performance = c(round(mean(df_new$base),4),
                                                 round(mean(df_new$recommender),4)))


base_df <- data.frame(model = rep("Base", length(df_new$base)),
                      performance = df_new$base)
recommender_df <- data.frame(model = rep("Recommender", length(df_new$recommender)),
                             performance = df_new$recommender)
all_df <- rbind(base_df, recommender_df)

ggplot(all_df, aes(performance, color = model, fill = model)) +
  geom_density(alpha = 0.2)+
  theme_minimal()


#####################################################################################
### TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST
#####################################################################################

