champ <- read.csv("crawled_data/champ.csv", header = T, stringsAsFactors = F) ; head(champ)
game  <- read.csv("win_data.csv", header = T, stringsAsFactors = F) ; head(game)

# Make Transactions Data Set
game_list <- list()

for(i in 1:NROW(game)) {
  game_list[i] <- list(paste(game[i,1:6]))
}

game_trans <- as(game_list, "transactions")
game_trans

inspect(game_trans[1:5])
summary(game_trans)

sort(itemFrequency(game_trans), decreasing = T)[3:10]
itemFrequencyPlot(game_trans, topN = 20)
image(sample(game_trans, 100))

# Make Rules
set.seed(123)

Win_AR <- apriori(game_trans,
                  parameter  = list(minlen = 3, supp=0.0001, conf=0),
                  appearance = list(rhs = c("Win"),
                                    default = "lhs"))

summary(Win_AR)

Lose_AR <- apriori(game_trans,
                   parameter  = list(minlen = 3, supp=0.0001, conf=0),
                   appearance = list(rhs = c("Lose"),
                                     default = "lhs"))
summary(Lose_AR)

# Remove Redundancy

remove_re <- function(game_AR){
  redundant.matrix <- is.subset(game_AR, game_AR)
  redundant.matrix[lower.tri(redundant.matrix, diag=T)] <- 0
  redundant <- colSums(redundant.matrix) >= 1
  game_pruned <- game_AR[!redundant]
  
  return(game_pruned)
}

Win_rule = remove_re(Win_AR)
Lose_rule = remove_re(Lose_AR)


# Visualization

plot(game_pruned, method = "graph", interactive = TRUE, shading = NA)
plot(game_pruned)
plot(game_pruned, method = "grouped")
plot(game_pruned, method = "paracoord")


###############test

Wrules = inspect(sort(Lose_rule, by = "confidence"))
Lrules = inspect(sort(Win_rule, by = "confidence"))

Win_list = list()
Lose_list = list()

for (i in 1:length(Wrules$lhs)){
  Win_list[i] <- as.character(Wrules$lhs[i]) %>% str_remove("\\{")  %>% str_remove("\\}") %>% str_split(",")
}
for (i in 1:length(Lrules$lhs)){
  Lose_list[i] <- as.character(Lrules$lhs[i]) %>% str_remove("\\{")  %>% str_remove("\\}") %>% str_split(",")
}


###############
# Select picks randomly
test= c()
test = append(test,sample(champ$champName[champ$position1=='top'],1))
test = append(test,sample(champ$champName[champ$position1=='middle'],1))
test = append(test,sample(champ$champName[champ$position1=='jungle'],1))
test = append(test,sample(champ$champName[champ$position1=='adc'],1))
test = append(test,sample(champ$champName[champ$position1=='support'],1))

Win_base = check_performance(Win_list)
Lose_base = check_performance(Lose_list)

Win_base 
Lose_base

check_performance <- function(rule_list) {
  threshold <- 0
  base_line <- data.frame()
  while (threshold < 1) {
    
    
    confi_score <- 0 # Sum of confidence
    lift_score <- 0 # SUm of lift
    n_rules <- 0 # No. of rules
    
    for (i in 1:length(rule_list)){
      a = length(rule_list[[i]])
      if (a == 3){
        if (is.element(rule_list[[i]][1] ,test) &
            is.element(rule_list[[i]][2] ,test) &
            is.element(rule_list[[i]][3] ,test)){
          confi_score = confi_score + rules$confidence[i]
          lift_score <- lift_score + rules$lift[i]
          n_rules <- n_rules + 1
        }}else{if (is.element(rule_list[[i]][1] ,test) &
                   is.element(rule_list[[i]][2] ,test)){
          confi_score = confi_score + rules$confidence[i]
          lift_score <- lift_score + rules$lift[i]
          n_rules <- n_rules + 1
        }}}
    
    
    if (n_rules >1) {
      final_confi_score <- confi_score / n_rules
      final_lift_score <- lift_score / n_rules
    } else {
      final_confi_score <- confi_score / n_rules
      final_lift_score <- lift_score / n_rules
    }
    temp <- data.frame(champ1 = test[1],champ2 = test[2],champ3 = test[3],
                       champ4 = test[4],champ5 = test[5],confidence = final_confi_score,
                       lift = final_lift_score,
                       stringsAsFactors = F)
    base_line <- base_line %>% bind_rows(temp)
    threshold <- threshold + 1
  }
  return(base_line)
}
