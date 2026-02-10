# Helper functions

library(dplyr)
library(ggplot2)

# Functions used to help clean the data

find_refreshers <- function(condition_data){ # Finds number of people who refreshed the webpage
  refreshed <- condition_data %>%
    group_by(id) %>%
    summarise(num_groups = n_distinct(training_condition)) %>%
    filter(num_groups != 1)
  return(refreshed)
}

find_duplicates <- function(choices_data){ # Finds participants who repeated experiment more than once
  duplicates <- choices_data %>%
    filter(practice_trial == 0) %>%
    dplyr::select(id) %>%
    group_by(id) %>%
    tally() %>%
    filter(n > 24) %>%
    ungroup()
  if(nrow(duplicates) == 0){
    return("No Duplicates")
  }
  else{
    return(duplicates)
  }
}

find_failed_attention_check <- function(attention_check, attempts){ # Find participants who did not pass attention check within 'attempts'
  failed_attention_check <- attention_check %>%
    filter(attempt > attempts) %>%
    distinct(id) %>%
    dplyr::select(id)
  return(failed_attention_check)
}


prep_data <- function(responses){ # Clean data
  responses2<- responses %>%
    filter(practice_trial == 0) %>%
    dplyr::select(id, opponent, training_condition, receiver_offer, response, reaction_time) %>%
    mutate(fair = ifelse(receiver_offer >=  4, 1,0),
           opponent = as.factor(opponent),
           training_condition = as.factor(training_condition),
           accept = ifelse(response =='accept',1,0),
           training_condition = training_condition)
  return(responses2)
}

count_groups <- function(subinfo, refreshers){ # Count number of participants per training condition
  results <- subinfo %>%
    filter(!id %in% refreshers$id) %>%
    group_by(training_condition) %>%
    dplyr::select(id) %>%
    distinct() %>%
    tally()
  return(results)
}

## Plot graphs
plot_graphs <- function(df){
  df_num_prep <- df %>%
    group_by(training_condition) %>%
    summarise(num_participants = n_distinct(id)) 
  num_participants <- length(unique(df$id))
  
  num_groups <- n_distinct(df$training_condition)
  if(num_groups == 3){
    col_values = c('#7daea6','#cc92f8',"#61235d")
    col_labels = c('Control',"AI for others", 'AI for self')
  }  #ff7f84
  if(num_groups == 2){
    col_values = c('#7daea6',"#61235d")
    col_labels = c('Control',"AI")
  }
  
  # For training effect, just use standard error
  df_training_prep <- df %>%
    mutate(receiver_offer = as.factor(receiver_offer)) %>%
    group_by(opponent, training_condition, receiver_offer) %>%
    rename(offer = receiver_offer) %>%
    summarise(acceptance_rate = mean(accept),
              sd = sd(accept)) %>%
    ungroup()
  
  df_training <- left_join(df_training_prep, df_num_prep, by = "training_condition") %>% mutate(se = sd/sqrt(num_participants))
  
  opponent.labs = c("AI partner", "Human partner")
  names(opponent.labs) <- c("AI","human")
  
  plt_training <- ggplot(df_training, aes(x = offer, y = acceptance_rate, fill = training_condition))+
    geom_bar(stat = "identity",position=position_dodge())+
    geom_errorbar(aes(ymin = acceptance_rate - se, ymax = acceptance_rate + se), size = 1, width = 0.3, position=position_dodge(.9))+
    scale_fill_discrete(name = "AI Training")+
    labs(x = "Offer", y = "P(accepted)", fill = "Training condition")+
    scale_fill_manual(values=col_values, labels=col_labels)+
    facet_grid(. ~opponent, labeller = labeller(opponent = opponent.labs))+
    scale_y_continuous(breaks = seq(0, 1, by=0.25), expand = c(0,0), limits = c(0,1.05))+
    scale_x_discrete(labels = c("$1","$2","$3","$4","$5","$6"))+
    theme(legend.position="none")+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.text = element_text(size = 18), 
          legend.title = element_text(size = 18), strip.text = element_text(size = 18),
          panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.text=element_text(size=18),
          axis.title=element_text(size=18,face="bold"))
  print(plt_training)
  
  df_training_prep2 <- df %>%
    group_by(training_condition, fair) %>%
    rename(offer = receiver_offer) %>%
    summarise(acceptance_rate = mean(accept),
              sd = sd(accept))
  df_prep <- left_join(df_training_prep2, df_num_prep, by = "training_condition") %>% mutate(se = sd/sqrt(num_participants)) %>%
    mutate(fair = ifelse(fair == 1, "Fair", "Unfair"))
  
  pt <- ggplot(df_prep, aes(x = reorder(fair,+acceptance_rate), y = acceptance_rate, fill = training_condition))+
    geom_bar(stat = "identity",position=position_dodge())+
    geom_errorbar(aes(ymin = acceptance_rate - se, ymax = acceptance_rate + se), width = 0.2, position=position_dodge(.9))+
    scale_fill_discrete(name = "AI Training")+
    scale_y_continuous(expand = c(0,0), limits = c(0,1.05))+
    labs(x = "Offer", y = "P(accepted)", fill = "Training condition")+
    #scale_fill_manual(values=col_values, labels=col_labels)+
    scale_fill_manual(values=col_values)+
    theme(legend.position="none")+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.text = element_text(size = 18), 
          legend.title = element_text(size = 18),
          panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.text=element_text(size=18),
          axis.title=element_text(size=18,face="bold"))
  return(pt)
}

# Run logistic models
prep_me_data <- function(responses){ # Clean data so it's ready for mixed effects (me) model
  result <- responses %>% 
    mutate(opponent_AI = ifelse(opponent == "AI", 1, -1),
           accept = as.factor(accept),
           fair = ifelse(fair == 1, 1, -1),
           receiver_offer2 = receiver_offer - mean(receiver_offer),
           training_condition = as.factor(training_condition)) %>%
    dplyr::select(-receiver_offer) %>%
    rename(receiver_offer = receiver_offer2) %>%
    dplyr::select(id, accept, opponent_AI, training_condition, fair, receiver_offer)
  return(result)
}

run_one_mixed_effects_model <- function(responses){ # Mixed effects model for experiments 3A and 3B
  df_logit_model <- responses %>%
    mutate(opponent_AI = ifelse(opponent == "AI", 1, -1),
           fair = ifelse(fair == 1, 1, -1),
           ai_train_proposer = ifelse(training_condition == "training_receiver_self", 1, -1),
           receiver_offer2 = receiver_offer - mean(receiver_offer)) %>%
    dplyr::select(id, accept, opponent_AI, ai_train_proposer, fair, receiver_offer2) %>%
    rename(receiver_offer = receiver_offer2) 
  logit_model <- glmer(accept ~ opponent_AI * receiver_offer * ai_train_proposer + (1|id), data = df_logit_model, family = "binomial", control=glmerControl(optimizer="nlminbwrap",optCtrl=list(maxfun=2e5)))
  return(summary(logit_model))
}

run_mixed_effects_models <- function(df_logit_model){ # Mixed effects model for experiments 1, 2A, and 2B
  results <- summary(glmer(accept ~ opponent_AI*receiver_offer*training_condition + (1|id), data = df_logit_model,family = "binomial", control=glmerControl(optimizer="nlminbwrap",optCtrl=list(maxfun=2e5))))
  reference_training_others <- within(df_logit_model, training_condition <- relevel(training_condition, ref = "training_receiver_others"))
  results2 <- summary(glmer(accept ~ opponent_AI*receiver_offer*training_condition + (1|id), data = reference_training_others, family = "binomial", control=glmerControl(optimizer="nlminbwrap",optCtrl=list(maxfun=2e5))))
  print(results)
  return(results2)
}

run_mixed_effects_model_for_partner <- function(responses){ # Mixed effects model to investigate partner effects (for experiment 3b)
  df_logit_model <- responses %>%
    mutate(opponent_AI = ifelse(opponent == "AI", 1, -1),
           fair = ifelse(fair == 1, 1, -1),
           receiver_offer2 = receiver_offer - mean(receiver_offer)) %>%
    dplyr::select(id, accept, opponent_AI, fair, receiver_offer2) %>%
    rename(receiver_offer = receiver_offer2) 
  logit_model <- glmer(accept ~ opponent_AI * receiver_offer + (1|id), data = df_logit_model, family = "binomial", control=glmerControl(optimizer="nlminbwrap",optCtrl=list(maxfun=2e5)))
  return(summary(logit_model))
}

## T-test functions
ttest_opponent_condition_interaction <- function(responses)  { # Post hoc t-tests for Experiment 2a
  tt <- responses %>%
    group_by(id, opponent, training_condition) %>% 
    summarise(acceptance_rate = mean(accept)) 
  conditions <- unique(tt$training_condition)
  for (i in 1:length(conditions)){
    df2 <- tt %>% filter(training_condition == conditions[i]) %>%
      select(-training_condition) %>%
      pivot_wider(names_from = "opponent", values_from = "acceptance_rate")
    print(paste("For", conditions[i]))
    print(t.test(df2$AI, df2$human, paired = TRUE))
  }
}

# T-test comparison
ttest_ai_helper <- function(df, fairness){
  return(t.test(acceptance_rate ~ training_condition, data = df %>% filter(fair == fairness)))
}

ttest_fair_ai_interaction <- function(responses)  { # Use for Experiments 3A and 3B. (But not used in our analysis)
  tt <- responses %>%
    group_by(id, training_condition, fair) %>% 
    summarise(acceptance_rate = mean(accept)) 
  print("For fair offers:")
  print(ttest_ai_helper(tt,1))
  print("For Unfair Offers")
  print(ttest_ai_helper(tt,0))
}


options(scipen = 100, digits = 4)
ttest_six_pairwise <- function(responses){ # Use for Experiments 1, 2A, and 2B
  tt <- responses %>%
    group_by(id, training_condition, fair) %>% 
    summarise(acceptance_rate = mean(accept)) 
  
  groups <- as.character(unique(responses$training_condition))
  results <-  rbind(as.data.frame(t(combn(groups,2))), as.data.frame(t(combn(groups,2))))
  results$fairness <- rep(c(0,1), each = length(groups))
  results$t <- rep(0, each = dim(results)[1])
  results$df <- rep(0, each = dim(results)[1])
  results$p <- rep(0, each = dim(results)[1])
  for(i in 1:dim(results)[1]){
    df <- tt %>% filter(training_condition == results$V1[i] | training_condition == results$V2[i])
    ttest_results <- ttest_ai_helper(df, results$fairness[i])
    results$t[i] <- ttest_results$statistic
    results$df[i] <- ttest_results$parameter
    results$p[i] <- ttest_results$p.value
  }
  return(results)
}

