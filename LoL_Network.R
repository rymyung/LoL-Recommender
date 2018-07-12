
#####################################################################################
### Setting up Environment
#####################################################################################

# Load libraries
pkgs <- c("dplyr", "tidyr", 'stringr', 'igraph', 'visNetwork', "RColorBrewer", "colorspace")
sapply(pkgs, require, character.only = T)

# Load data
user <- read.csv("crawled_data/user_position.csv", stringsAsFactors = F)
champ <- read.csv('crawled_data/champ.csv', stringsAsFactors = F)

user_sm <- read.csv("user_edges/user_simple_match.csv", stringsAsFactors = F)
user_ss <- read.csv("user_edges/user_same_side.csv", stringsAsFactors = F)
user_os <- read.csv("user_edges/user_opposite_side.csv", stringsAsFactors = F)
user_mw <- read.csv("user_edges/user_match_win.csv", stringsAsFactors = F)
user_ml <- read.csv("user_edges/user_match_lose.csv", stringsAsFactors = F)
user_re <- read.csv("user_edges/user_relative.csv", stringsAsFactors = F)
user_mt <- read.csv("user_edges/user_match.csv", stringsAsFactors = F)

champ_sm <- read.csv("champ_edges/champ_simple_match.csv", stringsAsFactors = F)
champ_ss <- read.csv("champ_edges/champ_same_side.csv", stringsAsFactors = F)
champ_os <- read.csv("champ_edges/champ_opposite_side.csv", stringsAsFactors = F)
champ_mw <- read.csv("champ_edges/champ_match_win.csv", stringsAsFactors = F)
champ_ml <- read.csv("champ_edges/champ_match_lose.csv", stringsAsFactors = F)
champ_re <- read.csv("champ_edges/champ_relative.csv", stringsAsFactors = F)
champ_mt <- read.csv("champ_edges/champ_match.csv", stringsAsFactors = F)

champ_top <- read.csv('champ_position_edges/champ_top_relative.csv', stringsAsFactors = F)
champ_jungle <- read.csv('champ_position_edges/champ_jungle_relative.csv', stringsAsFactors = F)
champ_middle <- read.csv('champ_position_edges/champ_middle_relative.csv', stringsAsFactors = F)
champ_adc <- read.csv('champ_position_edges/champ_adc_relative.csv', stringsAsFactors = F)
champ_support <- read.csv('champ_position_edges/champ_support_relative.csv', stringsAsFactors = F)


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
    nodes <- temp %>% left_join(champ, by = c('name' = 'champId')) %>%
      select(name, champName, position1) %>% 
      rename(id = champName, position = position1)
  } else {
    print('error')
    return()
  }
  
  return(nodes)
}


# layout_with_mds
# layout_nicely
# layout_in_circle
# Draw network
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


#####################################################################################
### Manipulation data for network
#####################################################################################

# User Same Side
user_mw_edges <- create_edges(user_mw); head(user_mw_edges, 20)
user_mw_nodes <- create_nodes(user_mw, 'user'); head(user_mw_nodes, 20)


# User Match Win
user_mt_edges <- create_edges(user_mt); head(user_mt_edges, 20)
user_mt_nodes <- create_nodes(user_mt, 'user'); head(user_mt_nodes, 20)


# Champ Match Win
champ_mt_edges <- create_edges(champ_mt); head(champ_mt_edges, 20)
champ_mt_nodes <- create_nodes(champ_mt, 'champ'); head(champ_mt_nodes, 20)


# Champ Relative
champ_re_edges <- create_edges(champ_re); head(champ_re_edges, 20)
champ_re_nodes <- create_nodes(champ_re, 'champ'); head(champ_re_nodes, 20)


# Champ Top Relative
champ_top_edges <- create_edges(champ_top); head(champ_top_edges, 20)
champ_top_nodes <- create_nodes(champ_top, 'champ'); head(champ_top_nodes, 20)

# Champ jungle Relative
champ_jungle_edges <- create_edges(champ_jungle); head(champ_jungle_edges, 20)
champ_jungle_nodes <- create_nodes(champ_jungle, 'champ'); head(champ_jungle_nodes, 20)

# Champ middle Relative
champ_middle_edges <- create_edges(champ_middle); head(champ_middle_edges, 20)
champ_middle_nodes <- create_nodes(champ_middle, 'champ'); head(champ_middle_nodes, 20)

# Champ adc Relative
champ_adc_edges <- create_edges(champ_adc); head(champ_adc_edges, 20)
champ_adc_nodes <- create_nodes(champ_adc, 'champ'); head(champ_adc_nodes, 20)

# Champ support Relative
champ_support_edges <- create_edges(champ_support); head(champ_support_edges, 20)
champ_support_nodes <- create_nodes(champ_support, 'champ'); head(champ_support_nodes, 20)


#####################################################################################
### Drawing Network
#####################################################################################


# User match win
drawing_network(user_mt_nodes %>% arrange(position), user_mt_edges, 3)

# Champ Same side synergy
drawing_network(champ_mt_nodes %>% arrange(position), champ_mt_edges, 15)

# Champ Relative
drawing_network(champ_re_nodes %>% arrange(position), champ_re_edges, 10, d = T)

# Top
drawing_network(champ_top_nodes, champ_top_edges, 3, p = F, d = T)
drawing_network(champ_jungle_nodes, champ_jungle_edges, 3, p = F, d = T)
drawing_network(champ_middle_nodes, champ_middle_edges, 3, p = F, d = T)
drawing_network(champ_adc_nodes, champ_adc_edges, 3, p = F, d = T)
drawing_network(champ_support_nodes, champ_support_edges, 3, p = F, d = T)



#####################################################################################
### TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST
#####################################################################################
