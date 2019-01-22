#____________Libraries________
	library(tidyverse)
	library(stringr)
	library(xlsx)
	library(arules)
	library(dplyr)
	library(readxl)
#______________________________

#_________Import My Functions_____________
source("MyLibrary.R") 

# Set working directory
# setwd("C:/Users/User/Desktop/omea2018/AssociationRules_Scripts")
# _________________________________________

# ==============================
#       Import Data 
# ==============================
oldData <- read.csv(file ="../data/269-dad.csv",header = TRUE,
                    fileEncoding = "UTF-8-BOM",sep = ";")
newData <- read_xls("../data/pithia2018.xls", sheet = "propt")

# Clean Datasets 
refineNew <- cleanData(newData)
refineOld <- cleanData(oldData)

# Merge Datasets
rawPithia_all <- rbind.data.frame(refineOld, refineNew)
rm(refineNew, refineOld, newData, oldData)

# Remove Dublicates
rawPithia_all <- select(rawPithia_all, -id_PS_mathimatos)
rawPithia_all <- unique(rawPithia_all)

# Import P5 Course -> Course_id
p5Ids <-read.xlsx(file = "../data/P5_courses_id.xlsx",sheetName = 1,encoding = "UTF-8")

# Import all courses_ids equal to p5
equalTo_df <- read.xlsx(file = "../data/all_courses_matches_toP5_for_import_to_R.xlsx", sheetIndex = 1, as.data.frame = TRUE,encoding = "UTF-8",header = TRUE)

#___________________Insert equal_to_P5 variable____________
unifiedPithia <- rawPithia_all
rm(rawPithia_all)

# create an empty colum equal to p5 to testData 
unifiedPithia <- mutate(unifiedPithia,
                   "equal_to"= NA)

# fill "equal_to" with data now
for(i in 1: nrow(equalTo_df)){
  rowIndex <- which(unifiedPithia$course_id == as.character(equalTo_df[i, 1]))
  unifiedPithia$equal_to[rowIndex] <- as.character(equalTo_df[i, 2])
}

# Store data With Routes
route_data <- filter(unifiedPithia, trexousa_kateuthinsi_foititi == 'Μηχανικοί Δικτύων' | trexousa_kateuthinsi_foititi == 'Μηχανικοί Λογισμικού'| trexousa_kateuthinsi_foititi == 'Μηχανικοί Η/Υ')

#     _____                _                                   
#    / ____|              | |                                  
#   | |     _ __ ___  __ _| |_ ___                             
#   | |    | '__/ _ \/ _` | __/ _ \                            
#   | |____| | |  __/ (_| | ||  __/                            
#    \_____|_|  \___|\__,_|\__\___|                      
#    _______                             _   _
#   |__   __|                           | | (_)                
#      | |_ __ __ _ _ __  ___  __ _  ___| |_ _  ___  _ __  ___ 
#      | | '__/ _` | '_ \/ __|/ _` |/ __| __| |/ _ \| '_ \/ __|
#      | | | | (_| | | | \__ \ (_| | (__| |_| | (_) | | | \__ \
#      |_|_|  \__,_|_| |_|___/\__,_|\___|\__|_|\___/|_| |_|___/
#                                                              

# Create Transactions  with Routes
# source("Routes.R",encoding = 'UTF-8')

# Create Transactions  with NO Routes
source("NoRoutes.R", encoding = 'UTF-8')
#                        _            _ 
#       /\              (_)          (_)
#      /  \   _ __  _ __ _  ___  _ __ _ 
#     / /\ \ | '_ \| '__| |/ _ \| '__| |
#    / ____ \| |_) | |  | | (_) | |  | |
#   /_/    \_\ .__/|_|  |_|\___/|_|  |_|
#            | |                        
#            |_|                        

# Set Apriori parametres
params = list(supp = 0.005, conf= 0.55, maxlen = 5, maxtime=5, ext = TRUE)

# Find  rules  related to given courses  
appearnc = list (default="lhs", rhs="Γλώσσες και Τεχνολογίες Ιστού ->καλά")

# Run apriori
rules <- apriori(transactions, parameter = params, appearance = NULL, control = list (verbose=F))


#_________________Targeting Performance_____________________

rules_good  <- subset(rules, subset = rhs %pin% "καλά")
rules_good <- rules_good[!is.redundant(rules_good, measure = "lift")]
rules_good <-DATAFRAME(rules_good,separate = TRUE)
rules_good <- arrange(rules_good, desc(lift), desc(count), desc(confidence))
rules_good <- filter(rules_good,
                     count >= 10,
                     lift >= 4)

rules_great <- subset(rules, subset = rhs %pin% "πολύ καλά")
rules_great <- rules_great[!is.redundant(rules_great, measure = "lift")]
rules_great <-DATAFRAME(rules_great,separate = TRUE)
rules_great <- arrange(rules_great, desc(lift), desc(count), desc(confidence))
rules_great <- filter(rules_great,
                     count >= 10,
                     lift >= 4)

rules_best <- subset(rules, subset = rhs %pin% "άριστα")
rules_best <- rules_best[!is.redundant(rules_best,measure = "lift")]
rules_best <-DATAFRAME(rules_best,separate = TRUE)
rules_best <- arrange(rules_best, desc(lift), desc(count), desc(confidence))
rules_best <- filter(rules_best,
                     count >= 10,
                     lift >= 4)
#_____________________________________________________________

# Mine Frequent Items
# summary(transactions) 
# 
# #  Scatter Plot Rules
# library('arulesViz')
# plot(rules_good, measure = c("support", "confidence"), shading = "lift")
# plot(rules_great, measure = c("support", "confidence"), shading = "lift")
# plot(rules_best, measure = c("support", "confidence"), shading = "lift")
# 
# # Plots number of items contained in the rule
# library('arulesViz')
# plot(rules_good, method = "two-key plot")
# plot(rules_great, method = "two-key plot")
# plot(rules_best, method = "two-key plot")

#_______________Export__Rules_________ 
write.xlsx(rules_good, col.names = TRUE, file = "../output/Finalrules.xlsx",append = FALSE,sheetName 
="καλά")

write.xlsx(rules_great, col.names = TRUE, file = "../output/Finalrules.xlsx",append = TRUE,sheetName 
="πολύ καλά")

write.xlsx(rules_best, col.names = TRUE, file = "../output/Finalrules.xlsx",append = TRUE,sheetName  
="άριστα")
