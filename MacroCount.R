## MacroCount.R
## Version 1.2
## By: Dmitry Horodetsky
##
## To Do:
## 1) Add a Weekly Summary (plot + Info)
##    Done!
##
## V 1.1 - addCheat(), open foodDB when using predictByID(), fixed bugs
##
## V 1.2 - getMonthlyAvg(); generates Average Macros and Calories and plots them

if (!require(plyr, quietly=TRUE)) {
  install.packages("plyr")
  library(plyr)
}

if (!require(ggplot2, quietly=TRUE)) {
  install.packages("ggplot2")
  library(ggplot2)
}

if (!require(cowplot, quietly=TRUE)) {
  install.packages("cowplot")
  library(cowplot)
}

########################################
#HELPER FUNCTIONS
########################################

#######################
#Initialization Script
#######################
.initializeModule <- function(){
  if (file.exists("MacroCount.RData")){
    load("MacroCount.RData", envir = .GlobalEnv)
  } else {
    
    ##
    #foodLog
    ##
    
    foodLog <<- data.frame(matrix(ncol = 6, nrow = 0), stringsAsFactors = FALSE)
    names(foodLog) <<- c("Date", "Proteins", "Carbs", "Fats", "Calories", "Current Weight")
    
    ##
    #macroGoals
    ##
    
    macroGoals <<- data.frame(matrix(ncol = 2, nrow = 4), stringsAsFactors = FALSE)
    names(macroGoals) <<- c("Macros", "Percentages")
    rownames(macroGoals)[1] <- "Protein (g)"
    rownames(macroGoals)[2] <- "Carbs (g)"
    rownames(macroGoals)[3] <- "Fats (g)"
    rownames(macroGoals)[4] <- "Calories"
    
    macroGoals <<- macroGoals
    
    
    ##
    #foodDB
    ##
    foodDB <<- data.frame(matrix(ncol = 6, nrow = 0), stringsAsFactors = FALSE)
    names(foodDB) <<- c("Name", "Quantity", "Proteins", "Carbs", "Fats", "Calories")
    
    ##
    #dailySummary
    ##
    dailySummary <<- data.frame(matrix(ncol = 2, nrow = 4), stringsAsFactors = FALSE)
    names(dailySummary) <- c("Current", "Goal")
    rownames(dailySummary)[1] <- "Protein (g)"
    rownames(dailySummary)[2] <- "Carbs (g)"
    rownames(dailySummary)[3] <- "Fats (g)"
    rownames(dailySummary)[4] <- "Calories"
    
    
    dailySummary <<- dailySummary
    
    
    ##
    #Weight
    ##
    
    currentWeight <<- NA
    
  }
}

.initializeModule()

#######################
#Summarize Every Change
#######################

.Summarize <- function(){
  
  todays_indices <- which(foodLog$Date==as.character(Sys.Date()))
  foodLog <- foodLog[todays_indices,]
  
  dailySummary[1,1] <<- sum(as.numeric(foodLog$Proteins))
  dailySummary[2,1] <<- sum(as.numeric(foodLog$Carbs))
  dailySummary[3,1] <<- sum(as.numeric(foodLog$Fats))
  dailySummary[4,1] <<- sum(as.numeric(foodLog$Calories))
  
  dailySummary[1,2] <<- macroGoals[1,1]
  dailySummary[2,2] <<- macroGoals[2,1]
  dailySummary[3,2] <<- macroGoals[3,1]
  dailySummary[4,2] <<- macroGoals[4,1]
  
  
}

.Summarize()

##########
#AutoSave
##########

.AutoSave <- function(){
  save(foodDB, foodLog, macroGoals, currentWeight, dailySummary, file = "MacroCount.RData")
}

##################################################
##################################################

###########################
#Module Functions
###########################

################
#Add Food
################

addFood <- function(){
  
  package_portion <- as.numeric(readline(prompt="Enter the portion size found on the Calorie Label (mass or quantity): "))
  package_protein <- as.numeric(readline(prompt="Enter the Protein (grams) found on the Calorie Label: "))
  package_carbs <- as.numeric(readline(prompt="Enter the Carbs (grams) found on the Calorie Label: "))
  package_fats <- as.numeric(readline(prompt="Enter the Total Fats (grams) found on the Calorie Label: "))
  package_calories <- round(1.2*(package_protein*4 + package_carbs*4 + package_fats*9), digits =2)
  portion_size <- as.numeric(readline(prompt="Enter your Portion Size: "))
  
  portion_multiplier <- portion_size / package_portion 
  
  portion_protein <- round(portion_multiplier*package_protein, digits = 2)
  portion_carbs <- round(portion_multiplier*package_carbs,digits = 2)
  portion_fats <- round(portion_multiplier*package_fats,digits = 2)
  
  total_cals <- round(1.1*(4*portion_protein + 4*portion_carbs + 9*portion_fats),digits =2)
  
  todays_date <- as.character(Sys.Date())
  
  entry_summary <- c(todays_date, portion_protein, portion_carbs, portion_fats, total_cals,currentWeight)
  
  foodLog[nrow(foodLog)+1,] <<- entry_summary
  
  message ("Added to Log!")
  
  add_to_db <- (readline(prompt="type 'yes' if you want to add this food to your database: "))
  
  if (add_to_db == "yes"){
    package_name <- (readline(prompt="What is the name of your food? "))
    db_summary <- c(package_name, package_portion, package_protein, package_carbs, package_fats, package_calories)
    foodDB[nrow(foodDB)+1,] <<- db_summary
    message("Added to Food Database!")
  }
  .AutoSave()
  .Summarize()
}



#######################
#Add Food From Database
#######################
addFromDB <- function(){
  food_id <- as.numeric(readline(prompt="What is the Food ID? (row in foodDB): "))
  portion_size <- as.numeric(readline(prompt="Enter your Portion Size: "))
  db_portion <-as.numeric(foodDB[food_id,2])
  
  pm <- portion_size / db_portion
  
  db_protein <-round(as.numeric(foodDB[food_id,3]) * pm, digits = 2)
  db_carbs <-round(as.numeric(foodDB[food_id,4]) * pm, digits = 2)
  db_fats <-round(as.numeric(foodDB[food_id,5]) * pm,digits = 2)
  db_cals <- round(1.1*(db_protein*4 + db_carbs*4 +db_fats*9) ,digits =2)
  
  todays_date <- as.character(Sys.Date())
  
  entry_summary <- c(todays_date,db_protein,db_carbs,db_fats,db_cals,currentWeight)
  
  foodLog[nrow(foodLog)+1,] <<- entry_summary
  
  .AutoSave()
  .Summarize()
  
  message ("Added to Log!")
  
}



###########################
#Add a Food to Your Database
###########################
addToDatabase <- function(){
  food_name <-(readline(prompt="What is the Food Name? "))
  package_portion <- as.numeric(readline(prompt="Enter the portion size found on the Calorie Label (mass or quantity): "))
  package_protein <- as.numeric(readline(prompt="Enter the Protein (grams) found on the Calorie Label: "))
  package_carbs <- as.numeric(readline(prompt="Enter the Carbs (grams) found on the Calorie Label: "))
  package_fats <- as.numeric(readline(prompt="Enter the Total Fats (grams) found on the Calorie Label: "))
  package_calories <- round(1.2*(package_protein*4 + package_carbs*4 + package_fats*9), digits =2)
  
  entry_summary <- c(food_name, package_portion, package_protein, package_carbs, package_fats,package_calories)
  
  foodDB[nrow(foodDB)+1,] <<- entry_summary
  
  .AutoSave()
  
  message("Added to Database!")
  
}

##########################
#predictByID (Predict intake, using a foodDB entry)
##########################
predictByID <- function(){
  #open up the foodDB in case you forgot to do so
  View(foodDB)
  
  .Summarize()
  
  food_id <- as.numeric(readline(prompt="What is the Food ID? (row in foodDB): "))
  repeat_loop = TRUE
  while(repeat_loop){
    portion_size <- as.numeric(readline(prompt="Enter your Portion Size: "))
    db_portion <-as.numeric(foodDB[food_id,2])
    
    pm <- portion_size / db_portion
    
    db_protein <-round(as.numeric(foodDB[food_id,3]) * pm, digits = 2)
    db_carbs <-round(as.numeric(foodDB[food_id,4]) * pm, digits = 2)
    db_fats <-round(as.numeric(foodDB[food_id,5]) * pm,digits = 2)
    db_cals <- round(1.1*(db_protein*4 + db_carbs*4 +db_fats*9) ,digits =2)
    
    prediction_db <- dailySummary
    
    prediction_db[1,1]<- as.numeric(prediction_db[1,1]+db_protein)
    prediction_db[2,1]<- as.numeric(prediction_db[2,1]+db_carbs)
    prediction_db[3,1]<- as.numeric(prediction_db[3,1]+db_fats)
    prediction_db[4,1]<- as.numeric(prediction_db[4,1]+db_cals)
    
    print(prediction_db)
    message (paste("Your Portion:",portion_size))
    message (paste("Default Portion",db_portion))
    
    choice <- as.numeric(readline(prompt="Use (1) to change portion size. Use (2) to add entry to foodLog. Use (3) to exit. Choice: "))
    
    if (choice == 2){
      db_cals <- round(1.1*(db_protein*4 + db_carbs*4 +db_fats*9) ,digits =2)
      todays_date <- as.character(Sys.Date())
      entry_summary <- c(todays_date,db_protein,db_carbs,db_fats,db_cals,currentWeight)
      foodLog[nrow(foodLog)+1,] <<- entry_summary
      message("Added to Food Log!")
      
      .AutoSave()
      .Summarize()  & break
      
    }
    
    if (choice == 3){
      break
    }
    
  }
  
}

##########################
#Set Weight
##########################
setWeight <- function(){
  currentWeight <<- as.numeric(readline(prompt="Enter your Weight (number only): "))
  .AutoSave()
}


###########################
#Set Your Macro Goals
###########################
setGoals <- function(){
  protein_goal <- as.numeric(readline(prompt="Enter the Protein Goal (grams): "))
  carb_goal <- as.numeric(readline(prompt="Enter the Carbs Goal (grams): "))
  fat_goal <- as.numeric(readline(prompt="Enter the Total Fats Goal (grams): "))
  
  macro_sum <- protein_goal + carb_goal + fat_goal
  
  calories_goal <- round((protein_goal*4+carb_goal*4+fat_goal*9), digits = 2)
  
  protein_percentage <- round((protein_goal / macro_sum)*100, digits =0)
  carb_percentage <- round((carb_goal / macro_sum)*100, digits =0)
  fat_percentage <- round((fat_goal / macro_sum)*100, digits =0)
  
  macroGoals[1,1] <<- protein_goal
  macroGoals[2,1] <<- carb_goal
  macroGoals[3,1] <<- fat_goal
  macroGoals[4,1] <<- calories_goal
  
  macroGoals[1,2] <<- protein_percentage
  macroGoals[2,2] <<- carb_percentage
  macroGoals[3,2] <<- fat_percentage
  macroGoals[4,2] <<- NA
  
  message("Done!")
  
  .AutoSave()
  
}

##################
#Add a Cheat Day
##################

addCheat <- function(){
  
  #Set your Cheat Day Parameters
  #I recommend not counting your macros on this day and instead
  #just logging a huge value for each macro
  #The default numbers in parentheses set 6490 kcal
  
  set_protein <-as.numeric("400")
  set_carbs <- as.numeric("400")
  set_fat <-as.numeric ("300")
  
  calc_cals <- round(1.1*(4*set_protein+4*set_carbs+9*set_fat),digits = 2)
  
  todays_date <- as.character(Sys.Date())
  
  entry_summary <- c(todays_date,set_protein,set_carbs,set_fat,calc_cals,currentWeight)
  foodLog[nrow(foodLog)+1,] <<- entry_summary
  message("Cheat Day Added!")
  
}

#####################
#Get Monthly Average
#####################

getMonthlyAvg <- function(){
  
  #Get Current Month, Year
  date <- Sys.Date()
  date_formatted <- format(date, "%b, %y")
  
  #Subset the Log
  log_subset <- apply(foodLog[1],2, function(x) format(as.Date(x), "%b, %y"))
  filtered_indixes <- which(log_subset %in% date_formatted)
  monthly_log <- (foodLog[filtered_indixes,])
  
  #Get the Weights early on
  start_weight <- monthly_log[1,6]
  end_weight <- monthly_log[nrow(monthly_log),6]
  
  #Group by Days
  monthly_log[1] <-apply(monthly_log[1],2, function(x) (as.numeric(as.Date(x))))
  
  monthly_log[2] <- apply(monthly_log[2],2, function(x) (as.numeric((x))))
  monthly_log[3] <- apply(monthly_log[3],2, function(x) (as.numeric((x))))
  monthly_log[4] <- apply(monthly_log[4],2, function(x) (as.numeric((x))))
  monthly_log[5] <- apply(monthly_log[5],2, function(x) (as.numeric((x))))
  monthly_log[6] <- apply(monthly_log[6],2, function(x) (as.numeric((x))))
  
  monthly_log <-ddply(monthly_log,"Date",numcolwise(sum))
  
  
  #Get the Goals
  goal_protein <- macroGoals[1,1] 
  goal_carbs <- macroGoals[2,1] 
  goal_fats <- macroGoals[3,1] 
  goal_cals <- macroGoals[4,1] 
  
  #Get Current averages
  avg_protein <- round(mean(as.numeric(monthly_log[,2])),digits =2)
  avg_carbs <- round(mean(as.numeric(monthly_log[,3])),digits =2)
  avg_fats <-round(mean(as.numeric(monthly_log[,4])),digits =2)
  avg_cals <-round(mean(as.numeric(monthly_log[,5])),digits =2)
  
  message("===========")
  message("Your Weight:")
  message(paste("At the Start:",start_weight))
  message(paste("At the End:",end_weight))
  message("===========")
  message("Proteins:")
  message(paste("Actual:",avg_protein))
  message(paste("Goal:",goal_protein))
  message("===========")
  message("Carbs:")
  message(paste("Actual:",avg_carbs))
  message(paste("Goal:",goal_carbs))
  message("===========")
  message("Fats:")
  message(paste("Actual:",avg_fats))
  message(paste("Goal:",goal_fats))
  message("===========")
  message("Calories:")
  message(paste("Actual:",avg_cals))
  message(paste("Goal:",goal_cals))
  message("===========")
  
  #Macros Plot
  gram_df <- data.frame(matrix(nrow = 3))
  gram_df$macros <- c("protein", "carbs","fats")
  gram_df[1]<-NULL
  gram_df$grams <- c(avg_protein, avg_carbs,avg_fats)
  gram_df$hline <- c(goal_protein,goal_carbs,goal_fats)
  
  bp <- ggplot(gram_df, aes(x=macros, y=grams)) +
    geom_bar(position=position_dodge(), stat="identity")
  bp <- bp + geom_errorbar(width=0.7, aes(y=hline, ymax=hline, ymin=hline), colour="#AA0000")
  bp <- bp+ geom_text(aes(x=macros, y=grams, label=grams,vjust=-0.5))
  
  #Calorie Plot
  cal_df <-data.frame(matrix(nrow = 1))
  cal_df$calories <- c("average")
  cal_df[1]<-NULL
  cal_df$kcal <- c(avg_cals)
  cal_df$hline <- c(goal_cals)
  
  bp2 <- ggplot(cal_df, aes(x=calories, y=kcal)) +
    geom_bar(position=position_dodge(), stat="identity", width =.5)
  bp2 <- bp2 + geom_errorbar(width=0.3, aes(y=hline, ymax=hline, ymin=hline), colour="#AA0000")
  bp2 <- bp2 + geom_text(aes(x=calories, y=kcal, label=kcal,vjust=-0.5))
  
  theme_set(theme_gray())
  plot_grid(bp, bp2, labels = c("A", "B"))
  
}

