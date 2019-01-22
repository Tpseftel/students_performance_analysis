# SCRIPT PURPOSE ####
# Compute and store passed_failed students into dataframes per year that they first tested

# Install packages if  are  not exist####
# install.packages("plyr")
# install.packages("dplyr")  
# install.packages("readxl")
# install.packages("stringr")

# Import packages
library(readxl)
library(dplyr)
library(stringr)

# Import functions
source("myfunctions.R")

# ================================
# Data Initialization
# ================================
start_year  <- 2016
string_date <- "2016_2017"
filename <- "..\\figures\\under_grad\\passedFailed\\2016\\"

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
pithia_edit <- rbind.data.frame(refineOld, refineNew)
rm(refineNew, refineOld,newData, oldData)

# Remove Dublicates
pithia_edit <- select(pithia_edit, -id_PS_mathimatos)
pithia_edit <- unique(pithia_edit)
# ====================================
# ====================================

# Get only p5 observations
p5 <- subset(pithia_edit, pithia_edit$exam_year >= 2015)

# ==============================
#       Compute Statistics
# ==============================
#store p5 per course
p5_course_lst <- split(p5, p5$course_title)
p5_course_lst <- p5_course_lst[sapply(p5_course_lst, function(x) dim(x)[1]) > 0]

stud_passed2016 <- data.frame()
stud_failed2016 <- data.frame()

library(plyr)

# ----For Every Course---- 
# Compute percentages of students 
for (i in names(p5_course_lst))
{
  
  tot2016 <- 0
  first   <- 0
  second  <- 0
  third   <- 0
  fourth  <- 0
  fifth   <- 0
  sixth   <- 0
  seventh <- 0
  eighth  <- 0
  
  #failed student count
  first_f   <- 0
  second_f  <- 0
  third_f   <- 0
  fourth_f  <- 0
  fifth_f   <- 0
  sixth_f   <- 0
  seventh_f <- 0
  eighth_f  <- 0
  
  st <- count(p5_course_lst[[i]]$studid)
  
  # Store students id 
  studid <- st$x
  
  # For every student----
  for (j in studid)
  {
    # i course observations
    c <- p5_course_lst[[i]] 
    
    # store  all  grades student j at course i
    gr <- c$grade[c$studid == j]
    
    # Create flag2016 #### 
    
    #store dates that j student tested
    year <- c$exam_year[c$studid == j]
    
    #check if the j student tested first time to spesific academic year
    flag2016 <- min(year) == start_year
    
    if (flag2016)
    {
      tot2016 <- tot2016 + 1
      
      # check if j have passed the course
      if (max(gr) >= 5)
      {
        #find out with wich try he passed
        try <- length(gr)
        switch(
          try,
          first <- first + 1,
          second <- second + 1,
          third <- third + 1,
          fourth <- fourth + 1 ,
          fifth <- fifth + 1,
          sixth <- sixth + 1,
          seventh <- seventh + 1,
          eighth <- eighth + 1
          )
        } else
      {
        # Count students that havent passed the course yet
        
        # Failed students
        try <- length(gr)
        switch(
          try,
          first_f <- first_f + 1,
          second_f <- second_f + 1,
          third_f <- third_f + 1,
          fourth_f <- fourth_f + 1 ,
          fifth_f <- fifth_f + 1,
          sixth_f <- sixth_f + 1,
          seventh_f <- seventh_f + 1,
          eighth_f <- eighth_f + 1
        )
      }
    }
  }
  
  # Store computed data to dataframes #### 
  total_stud <- length(studid)
  
  # Students passed2016 percents
  df2016 <- data.frame(course=i, total = total_stud,
                      total2016 = tot2016, 
                      first = round(100 * first / tot2016, digits = 1),
                      second = round(100 * second / tot2016, digits = 1),
                      third = round(100 * third / tot2016, digits = 1),
                      fourth = round(100 * fourth / tot2016, digits = 1),
                      fifth = round(100 * fifth / tot2016, digits = 1),
                      sixth = round(100 * sixth / tot2016, digits = 1),
                      seventh = round(100 * seventh / tot2016, digits = 1),
                      eighth = round(100 * eighth / tot2016, digits = 1))
  
  
  stud_passed2016<-rbind(stud_passed2016, df2016)
  
  # Students failed2016 percs
  df_failed2016 <- data.frame(course = i, total = total_stud,
                              total2016 = tot2016, 
                              first = round(100 * first_f / tot2016, digits = 1),
                              second = round(100 * second_f / tot2016, digits = 1),
                              third = round(100 * third_f / tot2016, digits = 1),
                              fourth = round(100 * fourth_f / tot2016, digits = 1),
                              fifth = round(100 * fifth_f / tot2016, digits = 1),
                              sixth = round(100 * sixth_f / tot2016, digits = 1),
                              seventh = round(100 * seventh_f / tot2016, digits = 1),
                              eighth = round(100 * eighth_f / tot2016, digits = 1))
  
  stud_failed2016 <- rbind(stud_failed2016, df_failed2016)
}
#Delete Useless variables ####
rm(p5_course_lst,c,df_failed2016,df2016,st,eight,eighth_f,i,j,total_stud,tot2016,try,year,pithia,p4)

#     _____                           _          _____                 _         
#    / ____|                         | |        / ____|               | |        
#   | |  __  ___ _ __   ___ _ __ __ _| |_ ___  | |  __ _ __ __ _ _ __ | |__  ___ 
#   | | |_ |/ _ \ '_ \ / _ \ '__/ _` | __/ _ \ | | |_ | '__/ _` | '_ \| '_ \/ __|
#   | |__| |  __/ | | |  __/ | | (_| | ||  __/ | |__| | | | (_| | |_) | | | \__ \
#    \_____|\___|_| |_|\___|_|  \__,_|\__\___|  \_____|_|  \__,_| .__/|_| |_|___/
#                                                               | |              
#                                                               |_|              
# Load packages
library(ggplot2)
library(scales)
library(extrafont)
library(dplyr)

# For each row(course) Create barlot ####
for(i in 1:73) #number of courses
{
  # Primary preparation of data ####
  
  # Merge df into one ----
  Editdf <- transf_df(pass = stud_passed2016[i,],fail= stud_failed2016[i,])
  
  # Transform Editdf so i can use it for create the barplot
  Editdf <- ddply(Editdf, .(try),
                  transform, pos = cumsum(percent) - (0.5 * percent))
  
  # Create barplot from  Editdf dataframe ---- 
  
  # Set filename and Path up ####
  trimname <- paste0(string_date, gsub("[/:]",x=stud_passed2016[i,1], replacement = ""))
  filename <- paste0(filename,trimname)
  filename <- paste0(filename,".png")
  
  # Save barplot as png image
  png(filename = filename)
  
  # set color of bars(passed,failed)
  fill <- c("#E11724", "#197519")
  
  # Set barplot up
  barpl<-ggplot()+geom_bar(data=Editdf,aes(x=try,fill=status,y=percent),stat = "identity")
  
  barpl <- barpl + geom_text(data=Editdf ,aes(x = try, y = pos,
                                              label = paste0(percent,"%")), size=3,color="white")
  barpl <- barpl +theme(legend.position="bottom", legend.direction="horizontal",
                        legend.title = element_blank())
  
  barpl <- barpl + labs(x = "Try", y = "Percentage") +
    scale_y_continuous(labels = dollar_format(suffix = "%", prefix = "")) +
    ggtitle(paste0(stud_passed2016[i,1], string_date)) +
    scale_fill_manual(values=fill) +
    theme(plot.title = element_text(size=11,face="bold")) +
    
    theme(axis.line = element_line(size=1, colour = "black"),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.border = element_blank(), panel.background = element_blank()) 
  print(barpl)
  dev.off()
}
