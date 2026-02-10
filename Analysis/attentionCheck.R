# Attention Check Information
library(xtable)
library(dplyr)
# Indicate the correct answers
## Questions about role, future role, and partner
correct_answers <- c("role"= "I will be responding to offers.",
                     "future_role"= "I will be proposing offers.",
                     "partner"="I will play with both AI and human participants.")
# Questions for AI training conditions
training_others_solution <- c("training"= "I will be training an AI to respond to offers.", "training_encounter"="No")
training_self_solution <- c("training"= "I will be training an AI to respond to offers.", "training_encounter"="Yes")
control_solution = "Question not asked"

# For experiment 5, have an additional questions of whether they respond others
others_encounter <- "No" 

calculate_attention_check_info <- function(attention_check, strategy = FALSE){
  
  # Update correct answers
  attention_check$correct_role <- correct_answers["role"]
  attention_check$correct_future_role <- correct_answers["future_role"]
  attention_check$correct_partner <- correct_answers["partner"]
  attention_check$correct_training <- control_solution
  attention_check$correct_training_encounter <- control_solution
  
  for (i in 1:length(attention_check$id)){
    if(attention_check$training_condition[i] == "training_receiver_others"){
      attention_check$correct_training[i] <- training_others_solution["training"]
      attention_check$correct_training_encounter[i] <- training_others_solution["training_encounter"]
    }
    if(attention_check$training_condition[i] == "training_receiver_self"){
      attention_check$correct_training[i] <- training_self_solution["training"]
      attention_check$correct_training_encounter[i] <- training_self_solution["training_encounter"]
    }
  }
  
  
  if(strategy == TRUE){
    attention_check_clean <- attention_check %>% 
      mutate(got_role = ifelse(correct_role == role,1,0),
             got_future_role = ifelse(correct_future_role == future_role,1,0),
             got_partner = ifelse(correct_partner == partner,1,0),
             got_training = ifelse(correct_training == training,1,0),
             got_training_encounter = ifelse(correct_training_encounter == training_encounter,1,0),
             got_strategy_accept = ifelse(grepl("accept", strategy_accept),1,0),
             got_strategy_reject = ifelse(grepl("reject", strategy_reject),1,0),
             got_strategy_accept = ifelse(strategy_accept == "Question not asked", 1 , got_strategy_accept),
             got_strategy_reject = ifelse(strategy_reject == "Question not asked", 1 , got_strategy_reject))
    
    missed_questions <- attention_check_clean %>%
      group_by(attempt) %>%
      summarise(participants_left = length(unique(id)),
                missed_role = participants_left - sum(got_role),
                missed_future_role = participants_left - sum(got_future_role),
                missed_partner = participants_left - sum(got_partner),
                missed_training = participants_left - sum(got_training),
                missed_training_encounter = participants_left - sum(got_training_encounter),
                missed_strategy_accept = participants_left - sum(got_strategy_accept),
                missed_strategy_reject = participants_left - sum(got_strategy_reject)) %>%
      select(-c(participants_left))    

  }
  else{
    attention_check_clean <- attention_check %>% 
      mutate(got_role = ifelse(correct_role == role,1,0),
             got_future_role = ifelse(correct_future_role == future_role,1,0),
             got_partner = ifelse(correct_partner == partner,1,0),
             got_training = ifelse(correct_training == training,1,0),
             got_training_encounter = ifelse(correct_training_encounter == training_encounter,1,0))
    
    missed_questions <- attention_check_clean %>%
      group_by(attempt) %>%
      summarise(participants_left = length(unique(id)),
                missed_role = participants_left - sum(got_role),
                missed_future_role = participants_left - sum(got_future_role),
                missed_partner = participants_left - sum(got_partner),
                missed_training = participants_left - sum(got_training),
                missed_training_encounter = participants_left - sum(got_training_encounter)) %>%
      select(-c(participants_left)) 
  }
  
  percentage_pass <- attention_check_clean %>%
    group_by(attempt) %>%
    summarise(passed = sum(all_correct)) %>%
    mutate(total_passed = cumsum(passed),
           percent_pass = cumsum(100*passed/sum(passed)))
  
  print(percentage_pass)
  return(missed_questions)
}

calculate_attention_check_info_exp3b <- function(attention_check){
  
  # Update correct answers
  attention_check$correct_role <- correct_answers["role"]
  attention_check$correct_future_role <- correct_answers["future_role"]
  attention_check$correct_partner <- correct_answers["partner"]
  attention_check$correct_training <- control_solution
  attention_check$correct_training_encounter <- control_solution
  attention_check$correct_others_encounter <- control_solution
  
  for (i in 1:length(attention_check$id)){
    if(attention_check$training_condition[i] == "training_receiver_self"){
      attention_check$correct_training[i] <- training_self_solution["training"]
      attention_check$correct_training_encounter[i] <- training_self_solution["training_encounter"]
      attention_check$correct_others_encounter[i] <- others_encounter
    }
  }
    attention_check_clean <- attention_check %>% 
      mutate(got_role = ifelse(correct_role == role,1,0),
             got_future_role = ifelse(correct_future_role == future_role,1,0),
             got_partner = ifelse(correct_partner == partner,1,0),
             got_training = ifelse(correct_training == training,1,0),
             got_training_encounter = ifelse(correct_training_encounter == training_encounter,1,0),
             got_others_encounter = ifelse(correct_others_encounter == others_encounter,1,0),
             got_strategy_accept = ifelse(grepl("accept", strategy_accept),1,0),
             got_strategy_reject = ifelse(grepl("reject", strategy_reject),1,0),
             got_strategy_accept = ifelse(strategy_accept == "Question not asked", 1 , got_strategy_accept),
             got_strategy_reject = ifelse(strategy_reject == "Question not asked", 1 , got_strategy_reject))
    
    missed_questions <- attention_check_clean %>%
      group_by(attempt) %>%
      summarise(participants_left = length(unique(id)),
                missed_role = participants_left - sum(got_role),
                missed_future_role = participants_left - sum(got_future_role),
                missed_partner = participants_left - sum(got_partner),
                missed_training = participants_left - sum(got_training),
                missed_training_encounter = participants_left - sum(got_training_encounter),
                missed_others_encounter = participants_left - sum(got_others_encounter),
                missed_strategy_accept = participants_left - sum(got_strategy_accept),
                missed_strategy_reject = participants_left - sum(got_strategy_reject)) %>%
      select(-c(participants_left))    

  percentage_pass <- attention_check_clean %>%
    group_by(attempt) %>%
    summarise(passed = sum(all_correct)) %>%
    mutate(total_passed = cumsum(passed),
           percent_pass = cumsum(100*passed/sum(passed)))
  
  print(percentage_pass)
  return(missed_questions)
}