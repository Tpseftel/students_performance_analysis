# SCRIPT PURPOSE ####
# Undergraduate
# Create a stat_density plot (distribution ) of grades per Exam per course

# Install packages if they are  not exist####
# install.packages("ggplot2")   
# install.packages("plyr")   
# install.packages("dplyr")  
# install.packages("scales")
# install.packages("extrafont")
# install.packages("stringr")
# install.packages("readxl") 

# Load packages #
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
filepath    <- "..\\figures\\under_grad\\dist_exam\\"

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

# =======================================================================  
#           Start Analysis
# ======================================================================= 
  
# Choose year
# =========================================================
  p2017_edit <- filter(pithia_edit,exam_year == acad_year)
 # =========================================================
  # split p2017_edit by course
  p2017_course_lst <- split(p2017_edit,p2017_edit$course_title) # p2017_course_list is a list object   

  # delete useles object from now on
  rm(p2017_edit)
  
  # remove null objects (courses)
  p2017_course_lst <- p2017_course_lst[sapply(p2017_course_lst,function(x) dim(x)[1])>0]

  p2017_res_edit <- list()
  
# COMPUTE STATISTICS  ####
  
# compute statistics for each course 
for(i in names(p2017_course_lst))
{
  p2017_res <- aggregate(p2017_course_lst[[i]]$grade, by = list(p2017_course_lst[[i]]$exam),
                     FUN = function(x) c(synolika = round(length(x)),
                                         percentage0_1 = paste(round(findPerc(x, 0, 1), 1), " %"),
                                         passedPercent = paste(round(percPassed(x),1)," %"),
                                         mesos = round(mean(x[x > 1]), 1)))
  # store computed statistics as dataframe
  p2017_res_edit[[i]]  <- do.call(data.frame, p2017_res)
  names(p2017_res_edit[[i]]) <- c("Exam", "TotalPapers", "Zero_One%", "Passed%", "Average")
}

# Delete variables with no use from now on  
rm(p2017_res, p2017_course_lst)

# =============================
#      Generate Figures 
# =============================

# Choose year  and with grades > 1
# ===============================================================
 p2017_edit1 <- filter(pithia_edit, exam_year == acad_year, grade > 1)
# ===============================================================

# Split p5 to a list per course
p2017_course_lst1 <- split(p2017_edit1,p2017_edit1$course_title)

# delete useles object from now on
rm(p2017_edit1)

# remove null objects
p2017_course_lst1 <- p2017_course_lst1[sapply(p2017_course_lst1, function(x) dim(x)[1]) > 0]

for(i in names(p2017_course_lst1))
{
  # Set Filename and Path up
  trimname <- paste0("Π", string_date) %>%
                    paste0(gsub("[/:]", x = i, replacement = ""))
  
  # Path tha figure will be saved
  filename <- filepath %>%
              paste0( trimname) %>%
              paste0(".png")
  
  # Save plot as png file
  png(filename = filename)
  
  # Create plot title
  t <- fix_title(p2017_res_edit[[i]])
  
  # The title of Course
  courseTitle <- names(p2017_course_lst1[i])
  
    title <- paste0(courseTitle, "(") %>%
             paste0(string_date) %>%
             paste0(")") %>%
             paste0(t)
  
  # create distribution plot
  bp <- ggplot(p2017_course_lst1[[i]], aes(x=grade, colour=exam))+labs(y="%") +
    ggtitle(title) +
    stat_density(geom="line", position="identity")+
    scale_y_continuous(labels=scales::percent, limits = c(0, .5),breaks = seq(0, 0.5, 0.05)) +
    scale_x_continuous("grade", limits = c(0, 10), breaks = seq(0, 10, 1)) +
    theme(plot.title = element_text(size=10),
          legend.position = "bottom")
  print(bp)
  
  dev.off()
}

# Delete variables ####
rm( p2017_course_lst1, bp, filename, i,filepath, t, title, trimname, p2017_edit,p2017_edit1)
