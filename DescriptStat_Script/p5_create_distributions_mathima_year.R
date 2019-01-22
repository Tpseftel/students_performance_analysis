# SCRIPT PURPOSE ####

# Create a stat_density plot (distribution ) of  grades for entire academic year per course

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
filepath    <- "..\\figures\\under_grad\\dist_year\\"

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
rm(refineNew, refineOld, newData, oldData)

# Remove Dublicates
pithia_edit <- select(pithia_edit, -id_PS_mathimatos)
pithia_edit <- unique(pithia_edit)

#=======================================================================	
# 					Start Analysis
#======================================================================= 

# Store observations for specific academic year only
p2017 <- filter(pithia_edit, exam_year == acad_year)

# Group data by course_title
p2017_by_course <- group_by(p2017, course_title) 

# store  computed statistics 
courseStats <- data.frame()

# compute requested statistics by course 
courseStats <- summarise(p2017_by_course,
                         total = round(length(grade)),
                         zero_one = paste(round(findPerc(grade, 0, 1), 1), " %"),
                         passed = paste(round(percPassed(grade), 1), "%"),
                         mean = round(mean(grade[grade > 1]), 1))
# CREATE PLOTS #

# Choose year and with grades > 1
tempVar2017 <- filter(pithia_edit, exam_year == acad_year, grade > 1 )

# store obs per course data frame to list
acadYear2017CourseList <- split(tempVar2017, tempVar2017$course_title)

# delete variable with no use from now on
rm(tempVar2017)

# remove null objects
acadYear2017CourseList <- acadYear2017CourseList[sapply(acadYear2017CourseList, function(x) dim(x)[1]) > 0]

# create and save distibution each course to  different plot
for(i in 1:nrow(courseStats))
{
  # Set Filename and Path up
  trimname <- paste0("Π", string_date) %>%
                    paste0(gsub("[/:]", x = courseStats$course_title[i], replacement = ""))
  
  # Create file name of figure
  filename <- filepath   %>%
              paste0(trimname) %>%
              paste0(".png")
  
  # Save plot as png image
  png(filename = filename)
  
  # Create plot title
  t <- courseStats$course_title[i] %>%
    paste0("(") %>%  
    paste0(string_date) %>%
    paste0(")")
  
  title <-  fix_titledf(courseStats,i)
  title <-  paste(t, title, sep = "\n")
  
  #==============================================================
  #				 Generate Graphs
  #==============================================================
      bp <- ggplot(acadYear2017CourseList[[i]], aes(x = grade, color= "red")) +	 labs(y = "%") +
  	  ggtitle(title) +
   	  stat_density(geom = "line", position = "identity", adjust = 0.5) +
      scale_y_continuous(labels = scales::percent, limits = c(0, .5),
                       breaks = seq(0,0.5,0.05)) +
      scale_x_continuous("grade", limits = c(0,10),breaks = seq(0,10,1)) +
      theme(plot.title = element_text(size = 10), 
          legend.position = "bottom")
      print(bp)
  
  dev.off()
}
  