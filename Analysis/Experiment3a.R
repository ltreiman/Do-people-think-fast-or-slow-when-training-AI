setwd("./OSF")
source("./Analysis/helperFunctions.R")

# Need to download packages before running
library(ggplot2)
library(dplyr)
library(tidyverse)
library(lmerTest)

# (Note: Data has been cross-referenced with condition_exp1 when creating random ids)
responses_exp3a <- read.csv("./Data/Experiment3a/Experiment3a_session1_responses.csv") 
condition_exp3a <- read.csv("./Data/Experiment3a/Experiment3a_session1_condition.csv") %>% filter(id %in% responses_exp3a$id) # Cross-reference to make sure include participants with only saved data
subInfo_exp3a <- read.csv("./Data/Experiment3a/Experiment3a_session1_subinfo.csv") %>% filter(id %in% responses_exp3a$id) # Cross-reference to make sure include participants with only saved data

# Clean dataset
find_duplicates(responses_exp3a) # Same participant who duplicated the study refreshed the webpage
refreshers <- find_refreshers(condition_exp3a) %>% filter(id %in% subInfo_exp3a$id)
cleaned_responses<- prep_data(responses_exp3a) %>% filter(!id %in% refreshers$id) %>% filter(id %in% subInfo_exp3a$id) 
count_groups(subInfo_exp3a, refreshers) 

# Plot results 
plot_graphs(cleaned_responses)

# Run mixed effects model
run_one_mixed_effects_model(cleaned_responses)
