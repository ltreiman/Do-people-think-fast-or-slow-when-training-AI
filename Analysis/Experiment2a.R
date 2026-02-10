setwd("./OSF")
source("./Analysis/helperFunctions.R")
source("./Analysis/attentionCheck.R")

# Need to download packages before running
library(ggplot2)
library(dplyr)
library(tidyverse)
library(lmerTest)

# (Note: Data has been cross-referenced with condition_exp1 when creating random ids)
responses_exp2a <- read.csv("./Data/Experiment2a/Experiment2a_session1_responses.csv") 
condition_exp2a <- read.csv("./Data/Experiment2a/Experiment2a_session1_condition.csv") %>% filter(id %in% responses_exp2a$id) # Cross-reference to make sure include participants with only saved data
subInfo_exp2a <- read.csv("./Data/Experiment2a/Experiment2a_session1_subinfo.csv")   %>% filter(id %in% responses_exp2a$id) # Cross-reference to make sure include participants with only saved data
attention_check_exp2a <- read.csv("./Data/Experiment2a/Experiment2a_attention_check.csv")  %>% filter(id %in% responses_exp2a$id & id %in% subInfo_exp2a$id) # Cross-reference to make sure include participants with only saved data
session2a <- read.csv("./Data/Experiment2a/Experiment2a_session2_data.csv")

# Clean dataset
find_duplicates(responses_exp2a) # One duplicate but also refreshed the webpage. 
refreshers <- find_refreshers(condition_exp2a) %>% filter(id %in% subInfo_exp2a$id)
failed_attention_check <- find_failed_attention_check(attention_check_exp2a, 3)
cleaned_responses <-prep_data(responses_exp2a) %>% filter(!id %in% refreshers$id & id %in% subInfo_exp2a$id & id %in% attention_check_exp2a$id & !(id %in% failed_attention_check$id))  # Cross reference here as well

count_groups(subInfo_exp2a %>% filter(!id %in% failed_attention_check$id & id %in% attention_check_exp2a$id), refreshers) 

# Plot graphs
plot_graphs(cleaned_responses)

# Run mixed effects model
logit_data <- prep_me_data(cleaned_responses)
run_mixed_effects_models(logit_data)

# Posthoc t-tests comparing partner effects per training condition
ttest_opponent_condition_interaction(cleaned_responses)

# Attention Check Data (found in Appendix E)
attention_check_clean <- attention_check_exp2a %>% filter(id %in% subInfo_exp2a$id & !(id %in% refreshers$id)) 
calculate_attention_check_info(attention_check_clean, strategy = FALSE)
