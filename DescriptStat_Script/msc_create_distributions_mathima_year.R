# SCRIPT PURPOSE ####

# Create a stat_density plot (distribution ) of  grades for entire academic year per course

# Install packages if  are  not exist####

# Install packages if  are  not exist####
install.packages("ggplot2")   
install.packages("dplyr")
install.packages("scales")
install.packages("extrafont")
install.packages("readxl")
install.packages("stringr")

# Libraries ####
library(dplyr)
library(ggplot2)
library(scales)
library(extrafont)
library(readxl)
library(stringr)

# Load my functions
source("myfunctions.R")

# ================================
# Data Initialization
# ================================
string_date <- "2015_2016"  
acad_year   <- 2015 # For academic year 2017-2018 acad_year will be 2017
filepath    <- "..\\figures\\msc\\dist_year\\"


# =====================================================================
#                               Import Data 
# =====================================================================
msc_old <- read.csv(file ="../data/MSC.csv",header = FALSE,
                     fileEncoding = "UTF-8-BOM",sep=";")
msc_new <- read_xls("../data/pithia2018.xls", sheet = "pms")

# set variable names
colnames(msc_old) <- c("studid",
                        "in_year",
                        "semid",
                        "course_id",
                        "course_title",
                        "exam",
                        "exam_year",
                        "grade")

msc_new <- cleanData(msc_new)

msc_new <- select(msc_new, c("studid", "reg_year","semester","course_id","course_title","exam","exam_year","grade")) 
names(msc_new) <- c("studid","in_year","semid","course_id","course_title","exam","exam_year","grade")

# Merge old with new Data
msc_edit <-  rbind.data.frame(msc_old, msc_new)

# Remove dublicates if exist 
msc_edit <- unique(msc_edit)

# =====================================================================

# Remove "ΠΕΡΙΟΔΟΣ" from exam column
msc_edit$exam<- gsub("ΠΕΡΙΟΔΟΣ",x=msc_edit$exam, replacement = "")
rm(msc_new, msc_old)

#=======================================================================  
#           Start Analysis
#=======================================================================

# store observations for specific academic year only
m2017 <- filter( msc_edit, exam_year == acad_year)


#store  computed statistics 
courseStats <- data.frame()

# Group by course_title
m2017_by_course <- group_by(m2017, course_title)

# COMPUTE STATISTICS  ####

# compute requested statistics by course 
courseStats <- summarise(m2017_by_course,
                         total = round(length(grade)),
                         zero_one = paste(round(findPerc(grade, 0, 1), 1), " %"),
                         passed = paste(round(percPassed(grade), 1), "%"),
                         mean = round(mean(grade[grade > 1]), 1))

# Delete variables
rm(m2017,m2017_by_course)

# Generate PLOTS ####
# Choose academic year with grades > 1

m2017_edit1 <- filter(msc_edit, exam_year == acad_year, grade > 1)

# split msc2017_edit1 to a list per course
m2017_course_lst1 <- split(m2017_edit1, m2017_edit1$course_title)

# delete variable with no use
rm(m2017_edit1)

# Remove null objects
m2017_course_lst1 <- m2017_course_lst1[sapply(m2017_course_lst1,function(x) dim(x)[1])>0]

# create and save distibution each course to  different plot
for(i in 1:nrow(courseStats))
{
  # set Filename and Path up
  trimname <- paste0(string_date, gsub("[/:]", x = courseStats$course_title[i], replacement = ""))
  
  filename <- paste0(filepath, trimname) %>%
              paste0(".png")
  
  # save plot as png image
  png(filename = filename)
  
  # create  plot title
  t <- as.character(courseStats$course_title[i]) %>%
      paste0("(") %>%  
      paste0(string_date) %>%
      paste0(")")
  
  title     <- fix_titledf(courseStats, i)
  title     <- paste(t,title,sep="\n")
  
  bp <- ggplot(m2017_course_lst1[[i]],
               aes(x=grade,color= "red"))+
        labs(y="%") +
        ggtitle(title) +
        stat_density(geom="line",
                     position="identity",
                     adjust = 0.5) +
        scale_y_continuous(labels=scales::percent,
                           limits = c(0,.5),
                           breaks = seq(0,0.5,0.05)) +
        scale_x_continuous("grade",
                           limits = c(0,10),
                           breaks = seq(0,10,1)) +
        theme(plot.title = element_text(size=10),
              legend.position = "bottom")
  print(bp) 
  dev.off()
}
#Delete variables
rm(filename,i,t,title,trimname, filepath)
