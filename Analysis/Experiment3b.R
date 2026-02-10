setwd("./OSF")
source("./Analysis/helperFunctions.R")
source("./Analysis/attentionCheck.R")

# Need to download packages before running
library(ggplot2)
library(dplyr)
library(tidyverse)
library(lmerTest)

# (Note: Data has been cross-referenced with condition_exp1 when creating random ids)
responses_exp3b <- read.csv("./Data/Experiment3b/Experiment3b_session1_responses.csv") 
condition_exp3b <- read.csv("./Data/Experiment3b/Experiment3b_session1_condition.csv") %>% filter(id %in% responses_exp3b$id) # Cross-reference to make sure include participants with only saved data
subInfo_exp3b <- read.csv("./Data/Experiment3b/Experiment3b_session1_subinfo.csv")   %>% filter(id %in% responses_exp3b$id) # Cross-reference to make sure include participants with only saved data
attention_check_exp3b <- read.csv("./Data/Experiment3b/Experiment3b_attention_check.csv")  %>% filter(id %in% responses_exp3b$id & id %in% subInfo_exp3b$id) # Cross-reference to make sure include participants with only saved data

# Clean dataset
find_duplicates(responses_exp3b) 
refreshers <- find_refreshers(condition_exp3b) %>% filter(id %in% subInfo_exp3b$id)
failed_attention_check <- find_failed_attention_check(attention_check_exp3b, 3) 
cleaned_responses <-prep_data(responses_exp3b) %>% filter(!id %in% refreshers$id & id %in% subInfo_exp3b$id & id %in% attention_check_exp3b$id & !(id %in% failed_attention_check$id))  # Cross reference here as well

count_groups(subInfo_exp3b %>% filter(!id %in% failed_attention_check$id & id %in% attention_check_exp3b$id), refreshers) 


# Plot graphs
plot_graphs(cleaned_responses)

# Run mixed effects model
run_one_mixed_effects_model(cleaned_responses)

# Mixed effects model investigating partner effects
run_mixed_effects_model_for_partner(cleaned_responses %>% filter(training_condition == 'control'))
run_mixed_effects_model_for_partner(cleaned_responses %>% filter(training_condition == 'training_receiver_self'))

# Attention Check Data (found in Supplementary)
attention_check_clean <- attention_check_exp3b %>% filter(id %in% subInfo_exp3b$id & !(id %in% refreshers$id)) 
calculate_attention_check_info_exp3b(attention_check_clean)
