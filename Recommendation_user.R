
#####################################################################################
### Project     : LoL Recommender System
### Script      : Recommendation_user.R
### Description : Recommend Users
#####################################################################################

#####################################################################################
### Setting up Environment
#####################################################################################

# Set working directory
setwd("C:/Users/Ro_Laptop/Dropbox/Public/github/LoL-Recommender/Data")

# Load libraries
pkgs <- c("dplyr", "tidyr", "stringr", "igraph", 'visNetwork', "colorspace", "DescTools")
sapply(pkgs, require, character.only = T)

options(warn = -1) 

#####################################################################################
### Data load
#####################################################################################

# Load user data
user <- read.csv("crawled_data_api/user_position.csv", stringsAsFactors = F)

# Load champ data
champ <- read.csv("crawled_data_api/champ.csv", stringsAsFactors = F)

# Load user's most champ data
most <- read.csv("crawled_data_api/most7.csv", stringsAsFactors = F)

# Load ally data
user_mt <- read.csv("user_edges/user_match.csv", stringsAsFactors = F)

# Load champ same side synergy data
champ_mt <- read.csv("champ_edges/champ_match.csv", stringsAsFactors = F)


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

drawing_network <- function(node, edge, threshold, p = T, d = F) {
  
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
    champ_mt_graph <- graph_from_data_frame(vis_edges, directed = T,
                                            vertices = vis_nodes$id)
    vis_nodes$in_degree <- degree(champ_mt_graph, v=V(champ_mt_graph), mode = "in")
    vis_nodes$out_degree <- degree(champ_mt_graph, v=V(champ_mt_graph), mode = "out")
    
    vis_nodes$title <- paste0("User : ", vis_nodes$label, "<br>",
                              "Position : ", vis_nodes$position, "<br>",
                              "In-Degree : ", vis_nodes$in_degree, "<br>",
                              "Out-Degree : ", vis_nodes$out_degree)
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
    vis_nodes$title <- paste0("User : ", vis_nodes$id, "<br>",
                              "Position : ", vis_nodes$position, "<br>",
                              "In-Degree : ", vis_nodes$in_degree, "<br>",
                              "Out-Degree : ", vis_nodes$out_degree)
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
    p <- visNetwork(vis_nodes, vis_edges, width='100%',height='950px', 
                    main=paste("network more than", threshold, "Games")) %>%
      visEdges(arrows ="to") %>%
      visOptions(highlightNearest = TRUE) %>%
      visInteraction(navigationButtons = TRUE) %>%
      visLegend() %>%
      visIgraphLayout(layout = "layout_in_circle", physics = FALSE,
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
      visIgraphLayout(layout = "layout_in_circle", physics = FALSE,
                      smooth = F, type = "square", randomSeed = NULL,
                      layoutMatrix = NULL)
  }
  return(p) 
}

calculate_path <- function(graph) {
  dist <- data.frame()
  for (node in V(graph)$name) {
    path_result <- shortest_paths(graph, from = node, to = V(graph), weight = 1/E(graph)$weight)
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

# Calculate champs' synergy
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
  
  # Calculate final score
  target_path <- target_path %>% mutate(score = user_score + champ_score)
  
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
  candidate_comb <- CombSet(final_candidate$summonerName, 4, repl=FALSE, ord=FALSE) %>% data.frame()
  
  candidate_comb_reduced <- candidate_comb %>% 
    left_join(final_candidate %>% select(summonerName, position, score), by = c("X1" = "summonerName")) %>%
    rename(position1 = position, score1 = score) %>% 
    left_join(final_candidate %>% select(summonerName, position, score), by = c("X2" = "summonerName")) %>%
    rename(position2 = position, score2 = score) %>% 
    left_join(final_candidate %>% select(summonerName, position, score), by = c("X3" = "summonerName")) %>%
    rename(position3 = position, score3 = score) %>% 
    left_join(final_candidate %>% select(summonerName, position, score), by = c("X4" = "summonerName")) %>%
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


#####################################################################################
### Network Creation
#####################################################################################

# User all my synergy
user_mt_edges <- create_edges(user_mt); head(user_mt_edges, 20)
user_mt_nodes <- create_nodes(user_mt, 'user'); head(user_mt_nodes, 20)

user_mt_graph <- graph_from_data_frame(user_mt_edges, directed = F,
                                        vertices = user_mt_nodes$name)

#drawing_network(user_mt_nodes %>% arrange(position), user_mt_edges, 3, p = T, d = T)

# Champ all my synergy
champ_mt_edges <- create_edges(champ_mt); head(champ_mt_edges, 20)
champ_mt_nodes <- create_nodes(champ_mt, 'champ'); head(champ_mt_nodes, 20)

champ_mt_graph <- graph_from_data_frame(champ_mt_edges, directed = F,
                                        vertices = champ_mt_nodes$name)


#####################################################################################
### Shortest Path Calcaultion
#####################################################################################

# User synergy
user_path <- calculate_path(user_mt_graph)

# Most champ synergy
champ_path <- calculate_path(champ_mt_graph)


#####################################################################################
### User Recommendation
#####################################################################################

# Select user who get recommended randomly
target <- user_mt_nodes[sample(1:nrow(user_mt_nodes), 1),]; target

# Let's get it
recommended <- recommend_user(target); recommended


#####################################################################################
### TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST
#####################################################################################

test_edges <- data.frame(source = c('A','B','B','C','D','D','E','E','E','F','F','G','G','H'),
                         target = c('D','C','E','A','B','C','B','C','F','C','H','A','C','A'), 
                         weight = sample(1:5, 14, replace = T))
test_nodes <- data.frame(name = c('A','B','C','D','E','F','G','H'))

test_graph <- graph_from_data_frame(test_edges, directed = T,
                                       vertices = test_nodes$name)
test_result <- shortest_paths(test_graph, 
                              from = test_nodes$name[2], 
                              to = V(test_graph), 
                              algorithm = "dijkstra")

user1_most <- (most %>% filter(accountId == target$name))$champId
user2_most <- (most %>% filter(accountId == target_path$accountId[85]))$champId
temp <- champ_path %>% mutate(champId = rownames(champ_path)) %>%
  filter(champId %in% user1_most)
rownames(temp) <- temp$champId
result <- temp %>% select(paste0("X",user2_most))
mean_path <- sum(result,na.rm=T)/(dim(result)[1]*dim(result)[2])
