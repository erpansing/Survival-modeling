################################################################################
#                                                                              #
#                         Simulation of survival data                          #
#                                                                              #
# AUTHOR: Elizabeth Pansing                                                    #
#                                                                              #
# DESCRIPTION: .... The following code simulates data for paper assessing the  #
#                   strengths and weaknesses for different survival modeling   #
#                   approaches. 
################################################################################
rm(list = ls())

options(stringsAsFactors = FALSE)

#---------------------------------------|---------------------------------------
#                                Load libraries
#---------------------------------------|---------------------------------------

suppressMessages(library(tidyverse))

#---------------------------------------|---------------------------------------
#                             Establish directories
#---------------------------------------|---------------------------------------

proj_dir <- paste0('/Users/elizabethpansing/Box Sync/PhD/Code/Survival Modeling/Survival Modeling Active')
data_dir <- "Rda/"
code_dir <- "Code/"
write_dir <- "Output/"

################################################################################
#---------------------------------------|---------------------------------------
#                             Constant survival 
#---------------------------------------|---------------------------------------
################################################################################
Days <- 28
## TRUE DSR = 0.96
DSR <- 0.96


nests <- matrix(as.integer(rbinom(10000, 1, .10)), nrow = 100, byrow = T)


for(i in 1: Days){
  #list of cells with nests
  nestCell <- which(nests == 1)
  
  if(i == 1){
    nestID <- nestCell
    day    <- rep(i, length(nestCell))
  }else if(i != 1){
    nestID <- append(nestID, nestCell, after = length(nestID))
    day <- append(day, rep(i, length(nestCell), after = length(day)))
  }
  
  Surv <- rbinom(length(nestCell), size = 1, prob = DSR)
  
  
  sum(nests == 1)
  length(nestCell)
  sum(Surv == 0)
  length(nestCell) - sum(Surv == 0)
  
  nests[nestCell[Surv == 0]] <- 0
}

nestHist <- data.frame(nestID = nestID, day = day) %>% 
  mutate(., value = 1) %>% 
  spread(., key = day, value = value, fill = 0)






