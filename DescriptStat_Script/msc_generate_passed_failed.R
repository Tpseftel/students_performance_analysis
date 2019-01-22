# SCRIPT PURPOSE ####
# Compute and store passed_failed students into dataframes per year that they first tested

# Install packages if  are  not exist####
# install.packages("plyr")
# install.packages("dplyr")  
# install.packages("stringr") 

# call packages
library(readxl)
library(stringr)
library(dplyr)

source("myfunctions.R")

# ================================
# Data Initialization
# ================================
start_year  <- 2016
string_date <- "2016_2017"
filepath <- "..\\figures\\msc\\passedFailed\\2017\\"

# =====================================================================
#                               Import Data 
# =====================================================================
msc_old <- read.csv(file ="../data/MSC.csv",header = FALSE,
                     fileEncoding = "UTF-8-BOM", sep=";")
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

# Remove "ΠΕΡΙΟΔΟΣ" from exam column
msc_edit$exam<- gsub("ΠΕΡΙΟΔΟΣ", x=msc_edit$exam, replacement = "")
rm(msc_new, msc_old)

# ===================================================================
#                         Compute Statistics
# ===================================================================
msc_course_lst <- split(msc_edit, msc_edit$course_title)

stud_passed2017 <- data.frame()
stud_failed2017 <- data.frame()

library(plyr)

#----For Every course---- 
# Compute percentages of student
for (i in names(msc_course_lst))
{
  tot2017  <- 0
  first    <- 0
  second   <- 0
  third    <- 0
  fourth   <- 0
  fifth    <- 0
  sixth    <- 0
  seventh  <- 0
  eighth   <- 0
  
  # Failed student count
  first_f    <- 0
  second_f   <- 0
  third_f    <- 0
  fourth_f   <- 0
  fifth_f    <- 0
  sixth_f    <- 0
  seventh_f  <- 0
  eighth_f   <- 0
  
  st <- count(msc_course_lst[[i]]$studid)
  studid <- st$x
  
  # For every student----
  for (j in studid)
  {
    # i course obs
    c <- msc_course_lst[[i]]
    
    # Store  all  grades student j at course i
    gr <- c$grade[c$studid == j]
    
    # Store dates that j student tested
    year <- c$exam_year[c$studid == j]
   
    # Flag2017----
     flag2017 <- min(year) == start_year
    
       if (flag2017)
    {
      # Counts total student for the specific year
      tot2017 <- tot2017 + 1
      
      # Check if j student have passed the course
      if (max(gr) >= 5)
      {
        # Find out with wich try they passed
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
          first_f   <- first_f + 1,
          second_f  <- second_f + 1,
          third_f   <- third_f + 1,
          fourth_f  <- fourth_f + 1 ,
          fifth_f   <- fifth_f + 1,
          sixth_f   <- sixth_f + 1,
          seventh_f <- seventh_f + 1,
          eighth_f  <- eighth_f + 1
        )
      }
    }
  }
  
  # total students----
  total_stud <- length(studid)
  
  # stud passed2017
  df2017 <- data.frame(course=i,total=total_stud,
                       total2017 = tot2017, 
                       first=round(100*first/tot2017,digits = 1),
                       second=round(100*second/tot2017,digits = 1),
                       third=round(100*third/tot2017,digits = 1),
                       fourth=round(100*fourth/tot2017,digits = 1),
                       fifth=round(100*fifth/tot2017,digits = 1),
                       sixth=round(100*sixth/tot2017,digits = 1),
                       seventh=round(100*seventh/tot2017,digits = 1),
                       eighth=round(100*eighth/tot2017,digits = 1))
 
  # add computed statistics to stud_passed2017
  stud_passed2017 <- rbind(stud_passed2017,df2017)
 
  #stud failed2017
  df_failed2017 <- data.frame(course=i,total=total_stud,
                              total2017 = tot2017, 
                              first=round(100*first_f/tot2017,digits = 1),
                              second=round(100*second_f/tot2017,digits = 1),
                              third=round(100*third_f/tot2017,digits = 1),
                              fourth=round(100*fourth_f/tot2017,digits = 1),
                              fifth=round(100*fifth_f/tot2017,digits = 1),
                              sixth=round(100*sixth_f/tot2017,digits = 1),
                              seventh=round(100*seventh_f/tot2017,digits = 1),
                              eighth=round(100*eighth_f/tot2017,digits = 1))
  
   # add computed statistics to stud_passed2017
   stud_failed2017 <- rbind(stud_failed2017, df_failed2017)
}

# Delete Useless variables ####
rm(c,df_failed2017,df2017,st,eighth_f,i,j,total_stud,tot2017,try,year,msc_course_lst)


#     _____                           _          _____                 _         
#    / ____|                         | |        / ____|               | |        
#   | |  __  ___ _ __   ___ _ __ __ _| |_ ___  | |  __ _ __ __ _ _ __ | |__  ___ 
#   | | |_ |/ _ \ '_ \ / _ \ '__/ _` | __/ _ \ | | |_ | '__/ _` | '_ \| '_ \/ __|
#   | |__| |  __/ | | |  __/ | | (_| | ||  __/ | |__| | | | (_| | |_) | | | \__ \
#    \_____|\___|_| |_|\___|_|  \__,_|\__\___|  \_____|_|  \__,_| .__/|_| |_|___/
#                                                               | |              
#                                                               |_|              
library(dplyr)
library(ggplot2)
library(scales)
library(extrafont)

# For each row(course) Create barlot ####
for(i in 1:10)
{
  # Primary preparation of data ####
  # Merge df into one ----
  Editdf <- transf_df(pass = stud_passed2017[i,],fail= stud_failed2017[i,])
  
  # transform Editdf so i can use it for create the barplot
  Editdf <- ddply(Editdf, .(try),
                  transform, pos = cumsum(percent) - (0.5 * percent))
  
  # Set filename and Path up ####
  trimname <- paste0(string_date, gsub("[/:]", x=stud_passed2017[i, 1], replacement = ""))
  
  filename <- paste0(filepath, trimname) %>%
              paste0(".png")
  
  # Save barplot as png image
  png(filename = filename)
  
  # Set color of bars(passed,failed)
  fill <- c("#E11424", "#197519")
  # ================================
  #         Generate Figures
  # ================================
  barpl<-ggplot()+geom_bar(data=Editdf,aes(x=try,fill=status,y=percent),stat = "identity")
  
  barpl <- barpl + geom_text(data=Editdf ,aes(x = try, 
                                              y = pos,
                                              label = paste0(percent,"%")), 
                             size=3,color="white")
  barpl <- barpl +theme(legend.position="bottom",
                        legend.direction="horizontal",
                        legend.title = element_blank())
  
  barpl <- barpl + labs(x = "Try",
                        y = "Percentage") +
    scale_y_continuous(labels = dollar_format(suffix = "%",
                                              prefix = "")) +
    ggtitle(paste0(stud_passed2017[i,1],"(2017-18)"))+
    scale_fill_manual(values=fill) +
    theme(plot.title = element_text(size=10,
                                    face="bold"))+
    
    theme(axis.line = element_line(size=1, colour = "black"),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.border = element_blank(), panel.background = element_blank()) 
  
  print(barpl)
  dev.off()
}
