closeAllConnections()
rm(list=ls())
cat("\014")
graphics.off()

library(tidyverse)
library(readxl)
library(readr)
library(dplyr)
library(plyr)
library(tidyr)
library(stringr)
library(caret)
library(ggplot2)
library(reshape2)




# With the following code you:
#   - import dataset formatted by kevin, 
#   - split dataset with respect to one of the two scenarios A | B
#   - compute link ratio on the train/test/all datasets previously splitted
# The analisys can be carried out for the incurred/Paid data, splitted on Motor or Liability


# Kevin's formatted dataset 
combined_data = read.csv( "C:\\Users\\lorenzo.invernizzi\\Desktop\\Astin\\Frank_formatted.csv")  # read csv file -> to change

# Motor | Liability
motor     = combined_data[combined_data$lob == 'Motor',]
liability = combined_data[combined_data$lob == 'Liability',]

# Incurred | Paid
motor_inc      = motor[motor$transaction_type == 'Incurred',]
motor_paid     = motor[motor$transaction_type == 'Paid',]
liability_inc  = liability[liability$transaction_type == 'Incurred',]
liability_paid = liability[liability$transaction_type == 'Paid',]


# Choose Cut Year
Cut_Year = 2000 # it is just an illustrative example -> cut_year is included in the test set

# Define Function to divide the db
divide_db <- function(db, Scenario, Cut_Year, type){
  
  if (Scenario == 'A'){
    # Note: In scenario A, the complete history of a single claim must be either in the train test or in the test set, 
    # hence i must reconstract the triangolar version
    
    db_sub <- subset(db, select = -c( calendar_year))
    db_tr  <- db_sub %>% spread(dev, value)
    db_tr[is.na(db_tr)] <- 0
    
    # Scenario A - Sampling withing AY of
    set.seed(1234)
    idx_train  = createDataPartition(db_tr$accident_year, p = 80/100, list = FALSE)
    train_tr   = db_tr[idx_train,]
    test_tr    = db_tr[-idx_train,]
    
    if (type == 'Motor'){
      train <- train_tr %>% gather(dev, value, `12`:`528`)  %>%   mutate(calendar_year = accident_year + as.integer(dev) / 12 - 1) %>% filter(value > 0)
      test  <- test_tr  %>% gather(dev, value, `12`:`528`)  %>%   mutate(calendar_year = accident_year + as.integer(dev) / 12 - 1) %>% filter(value > 0)
    } else if (type == 'Liability'){
      train <- train_tr %>% gather(dev, value, `12`:`516`)  %>%   mutate(calendar_year = accident_year + as.integer(dev) / 12 - 1) %>% filter(value > 0)
      test  <- test_tr  %>% gather(dev, value, `12`:`516`)  %>%   mutate(calendar_year = accident_year + as.integer(dev) / 12 - 1) %>% filter(value > 0)
    }
    
    # keep same order
    train <- train[, c(1,2,3,4,5,6,7,9,10,11,8)]
    test  <- test[, c(1,2,3,4,5,6,7,9,10,11,8)]
    
    # Return entire db, train and test
    return(list(db, train, test))
    
  } else if (Scenario == 'B'){
    
    # Scenario B - Cut wrt calendar Year
    train = db[db$calendar_year < Cut_Year,]
    test  = db[db$calendar_year >= Cut_Year,]
    
    # Return entire db, train and test
    return(list(db, train, test))
  }
}


### Motor Incurred ###

# scenario A
motor_inc_list_A <- divide_db(motor_inc, 'A', Cut_Year, 'Motor') # Cut_year is useless
sum(motor_inc_list_A[[1]][,9]) == sum(motor_inc_list_A[[2]][,9]) + sum(motor_inc_list_A[[3]][,9]) # check

# scenario B
motor_inc_list_B <- divide_db(motor_inc, 'B', Cut_Year) # Cut_year is useless
sum(motor_inc_list_B[[1]][,9]) == sum(motor_inc_list_B[[2]][,9]) + sum(motor_inc_list_B[[3]][,9]) # check


### Liability Incurred ###

# scenario A
liability_inc_list_A <- divide_db(liability_inc, 'A', Cut_Year, 'Liability')
sum(liability_inc_list_A[[1]][,9]) == sum(liability_inc_list_A[[2]][,9]) + sum(liability_inc_list_A[[3]][,9]) # check

# scenario B
liability_inc_list_B <- divide_db(liability_inc, 'B', Cut_Year)
sum(liability_inc_list_B[[1]][,9]) == sum(liability_inc_list_B[[2]][,9]) + sum(liability_inc_list_B[[3]][,9]) # check



# Defining function to compute and plot Link Ratio 
Compute_and_plot_ratio = function(db){
  
  # Create triangles
  db_sub <- subset(db, select = -c(calendar_year))
  db_tr  <- db_sub %>% spread(dev, value)
  db_tr[is.na(db_tr)] <- 0
  db_tr  <- subset(db_tr, select = -c(transaction_type, claim_number, occupancy, report_year, cover_type, status, lob))
    
  # sum wrt AY
  sum = db_tr %>% group_by(accident_year)  %>% summarise_all(funs(sum))
  
  # Computing Factors to plot
  #sum <- subset(sum, select = -c(ClaimNb, ReportYear))
  ratio = sum
  ratio[,-1] = 0
  # dim(ratio)
  for (i in seq(1,nrow(ratio))){
    for (j in seq(2,ncol(ratio)-1)){
      if (sum[i,j] == 0 || sum[i,j+1] == 0){ 
        ratio[i,j] = 0 
      } 
      else{
        ratio[i,j] = sum[i,j+1]/ sum[i,j]
      }
    }
  }
  
  # Plot
  to_plot = data.frame(DY = seq(1,ncol(ratio)-1))
  for (i in seq(min(ratio[,1]),max(ratio[,1]))){
    to_plot[,paste(i,"")] <- t(ratio[i-min(ratio[,1])+1,2:ncol(ratio)])
  }
  melted = melt(cbind(to_plot[1], as.matrix(to_plot[-1])), id = 'DY')
  # sum(to_plot[,-1]) == sum(melted[,3]) -> warning no prob
  plot <- ggplot(melted, aes(x = DY, y = value)) +
          geom_line(aes(group = variable, colour=variable)) +
          scale_y_continuous(limits = c(0.0001,10))
  # 0.0001 -  only in order to not display zeros
  # 10 - just for semplicity -> Be aware there are really big link ratios due to large claims 
  
  # return db, ratio table and plot
  return(list(db, ratio, plot))
}

### Motor Incurred ###

# scenario A
motor_inc_list_A_ratio       = Compute_and_plot_ratio(motor_inc_list_A[[1]])
motor_inc_list_A_ratio_train = Compute_and_plot_ratio(motor_inc_list_A[[2]])
motor_inc_list_A_ratio_test  = Compute_and_plot_ratio(motor_inc_list_A[[3]])

# scenario B
motor_inc_list_B_ratio       = Compute_and_plot_ratio(motor_inc_list_B[[1]])
motor_inc_list_B_ratio_train = Compute_and_plot_ratio(motor_inc_list_B[[2]])
motor_inc_list_B_ratio_test  = Compute_and_plot_ratio(motor_inc_list_B[[3]])


### Liability Incurred ###

# Scenario A
liability_inc_A_ratio       = Compute_and_plot_ratio(liability_inc_list_A[[1]])
liability_inc_A_ratio_train = Compute_and_plot_ratio(liability_inc_list_A[[2]])
liability_inc_A_ratio_test  = Compute_and_plot_ratio(liability_inc_list_A[[3]])

# Scenario B
liability_inc_B_ratio       = Compute_and_plot_ratio(liability_inc_list_B[[1]])
liability_inc_B_ratio_train = Compute_and_plot_ratio(liability_inc_list_B[[2]])
liability_inc_B_ratio_test  = Compute_and_plot_ratio(liability_inc_list_B[[3]])



### EXAMPLE ### 

# Ratio tables
ratio_tot   = liability_inc_B_ratio[[2]] # correctly separated into ratio_train anda ratio_test 
ratio_train = liability_inc_B_ratio_train[[2]]
ratio_test  = liability_inc_B_ratio_test[[2]] 

# Plots
liability_inc_B_ratio[[3]]
plot.new()
liability_inc_B_ratio_train[[3]]
plot.new()
liability_inc_B_ratio_test[[3]]









