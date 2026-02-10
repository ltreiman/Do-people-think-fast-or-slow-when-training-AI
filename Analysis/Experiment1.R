setwd("./OSF")
source("./Analysis/helperFunctions.R")

# Need to download packages before running
library(ggplot2)
library(dplyr)
library(tidyverse)
library(lmerTest)

# (Note: Data has been cross-referenced with condition_exp1 when creating random ids)
responses_exp1 <- read.csv("./Data/Experiment1/Experiment1_session1_responses.csv") 
condition_exp1 <- read.csv("./Data/Experiment1/Experiment1_session1_condition.csv") %>% filter(id %in% responses_exp1$id) # Cross-reference to make sure include participants with only saved data
subInfo_exp1 <- read.csv("./Data/Experiment1/Experiment1_session1_subinfo.csv") %>% filter(id %in% responses_exp1$id) # Cross-reference to make sure include participants with only saved data

# Clean dataset
find_duplicates(responses_exp1) # No participant completed this experiment more than once
refreshers <- find_refreshers(condition_exp1) %>% filter(id %in% subInfo_exp1$id)
cleaned_responses<- prep_data(responses_exp1) %>% filter(!id %in% refreshers$id) %>% filter(id %in% subInfo_exp1$id) 
count_groups(subInfo_exp1, refreshers) 

# Plot results 
plot_graphs(cleaned_responses)

# Run mixed effects model
logit_data <- prep_me_data(cleaned_responses)
run_mixed_effects_models(logit_data)

# T-test comparison between control and AI training for self
ttest_six_pairwise(cleaned_responses)