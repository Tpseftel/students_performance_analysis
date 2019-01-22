# SCRIPT PURPOSE ####
# MSC
# Create a stat_density plot (distribution ) of grades per Exam per course
# install.packages("ggplot2")   
# install.packages("dplyr")
# install.packages("scales")
# install.packages("extrafont")
# install.packages("readxl")
# install.packages("stringr")

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
filepath <- "..\\figures\\msc\\dist_exam\\"


# =====================================================================
#                               Import Data 
# =====================================================================
msc_old <- read.csv(file ="../data/MSC.csv",header = FALSE,
                     fileEncoding = "UTF-8-BOM",sep=";")
msc_new <- read_xls("../data/pithia2018.xls", sheet = "pms")

# set variable names
colnames(msc_old) <- c( "studid",
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
msc_edit$exam<- gsub("ΠΕΡΙΟΔΟΣ", x=msc_edit$exam, replacement = "")
rm(msc_new, msc_old)

#===================================================================== 
#           Start Analysis
#=====================================================================

# store observations for specific academic year only
msc_2017 <- filter(msc_edit, exam_year == acad_year)

# split p2015_edit by course
m2017_course_lst <- split(msc_2017, msc_2017$course_title)

# Delete useless objects from now on
rm(msc_2017)

#remove null objects
m2017_course_lst <- m2017_course_lst[sapply(m2017_course_lst,function(x) dim(x)[1])>0]

m2017_res <- list()

# COMPUTE STATISTICS  ####

# compute statistics for each course
for(i in names(m2017_course_lst))
{
  # compute statistics per exam and return a table with results
  m2017_r <- aggregate(m2017_course_lst[[i]]$grade, by = list(m2017_course_lst[[i]]$exam),
                      FUN = function(x) c(synolika = round(length(x)),
                                          percentage0_1 = paste(round(findPerc(x , 0, 1), 1), " %"),
                                          passedPercent = paste(round(percPassed(x), 1), " %"),
                                          mesos = round(mean(x[x > 1]), 1)))
  
  # store computed statistics as datafram
  m2017_res[[i]]  <- do.call(data.frame, m2017_r)
  names(m2017_res[[i]]) <-c("Exam", "TotalPapers", "Zero_One%", "Passed%", "Average")  
}

# Delete useless Objects from now on
rm(m2017_r, i, m2017_course_lst)

# CREATE PLOTS ####

# store obs for specific academic year  and with grades > 1
m2017_edit1 <- filter(msc_edit,exam_year ==  acad_year & msc_edit$grade > 1)

# split msc2017_edit1 to a list per course
m2017_course_lst1 <- split(m2017_edit1,m2017_edit1$course_title)

# Delete useless Object from now on
rm(m2017_edit1)

# Remove null objects
m2017_course_lst1 <- m2017_course_lst1[sapply(m2017_course_lst1,function(x) dim(x)[1])>0]

for(i in names(m2017_course_lst1))
{
  # Set Filename and Path up
  trimname <- paste0("MSC_", acad_year) %>%
  			  paste0(gsub("[/:]", x = i, replacement = ""))
  
  filename <- filepath %>%
    paste0(trimname) %>%
    paste0(".png")
  
  # save plot as png file
  png(filename = filename)

  # create plot title
  t <- fix_title(m2017_res[[i]])
  
  # The title of Course
  courseTitle <- names(m2017_res[i])
  title <- paste0(courseTitle, "(") %>%
           paste0(string_date) %>%
           paste0(")") %>%
           paste0(t)

   # create distribution plot
  bp <- ggplot(m2017_course_lst1[[i]], 
               aes(x = grade, colour=exam)) +
        labs(y = "%") +
        ggtitle(title) +
        stat_density(geom = "line", position = "identity") +
        scale_y_continuous(labels = scales::percent,
                           limits = c(0, .5),
                           breaks = seq(0, 0.5, 0.05)) +
        scale_x_continuous("grade", limits = c(0, 10),
                           breaks = seq(0, 10, 1)) +
        theme(plot.title = element_text(size = 10),
              legend.position = "bottom")
  
  print(bp)
  dev.off()
}


#Delete variables ####
rm(t,title,trimname,filename,bp)

