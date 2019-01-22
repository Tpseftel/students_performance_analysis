# MY FUNCTIONS----

zero_one <- function(x){
  # Computes  vector number of values between[0, 1].  
  # 
  # Args: 
  #   x: vector whose values between [0,1] is to be calculated.
  # 
  # Returns:
  #   the number of values between [0,1] of x vector.
  # 

  grapta <- length(x[x >= 0 & x <= 1])
  return(grapta)
}



biger_five <- function(x){
  # Computes vector's number of values which are equal or biger than 5.
  # 
  # Args:
  #   x: vector whose number of values that is equal or bigger than five is to be calculated.
  #   
  # Returns:
  #   the number of values which are > 5.
  


  grapta <- sum(x >= 5)
  return (grapta)
}



findPerc <- function(x, min, max){
  # Computes the percent of values between an maximun and lower limit of a given vector.
  # 
  #   Args:
  #       x: vector whose values between min max is to be computed.
  #     min: maximum limit. 
  #     max: lower limit.
  # 
  # Returns: x's percange of values between[min,max]
  # 

  part_num   <- length(x[x >= min & x <= max])
  tot_num    <- length(x)
  percentage <- (part_num / tot_num) * 100
  
  return(percentage)
  
}



percPassed <- function(x){
  # Computes the percent of values biger >= 5 without taking into account values [0,1].
  # 
  # Args:
  #   x: vector whose percent of values that is >= 5 is to be calculated.
  # 
  # Returns:
  #   Percentage of values of x that is >= 5
  

  tot_num    <- length(x) - zero_one(x)
  part_num   <- biger_five(x)
  percentage <- (part_num / tot_num) * 100
  
  return(percentage)
}



exp_df_list <- function(lst, path){
  # Output contents of a list to csv format at path location.
  #
  # Args:
  #    lst: list whose contents are to be outputed.
  #   path: Location that list's contents are to be outouted.
  #  
  # Warning:  
  #   list contents have to be data frames.
  
  for (i in names(lst)){
  
    filename <- path
    trimname <- gsub("[/:]", x = i, replacement = "")
    filename <- paste0(filename, trimname)
    filename <- paste0(filename, ".csv")
    
    write.csv(x = lst[[i]], file = filename)
    
  }
}



fix_title <- function(x)
{
  title <- "\n"
  
  for (i in 1:(nrow(x)))
  {
    title <- paste0(title, x[i, 1])
    for (j in 2:ncol(x))
    {
      title <- paste(title, x[i, j], sep = ",  ")
    }
    title <- paste0(title, "\n")
  }
  
  
  
  return(title)
}




fix_titledf <- function(x, j){
  # set a string up  with values of a dataframe which is to be used as plot title.
  #
  # Args:
  #   x: dataframe which is to be used to create title.
  #   j: index of dataframe's row.
  #
  # Returns:
  #   String which is will be used for plot title.

  title <- x[j, 2]
  for (i in 3:ncol(x)){
    
    title <- paste(title, (x[j, i]), sep = ",  ")
    
  }
  return(title)
}

transf_df <- function(pass, fail){
  # Merges to dataframes into one,so it can be used to create barplot.
  # 
  # Args:
  #   pass: data frame with percentages of passed students.
  #   fail: data frame with percentages of failed students.
  # 
  # Returns:
  #  The new merged dataframe.

  #set up new df
  df<- data.frame(status=character(10),try=character(10),percent=numeric(10), stringsAsFactors=FALSE)
  df[1:5,1]<- "passed"
  df[6:10,1]<- "failed"
  df$try[1:5] <-1:5 
  df$try[6:10] <-1:5 
  
  i <- 1
  q <- 6
  
  for(j in 4:8){
  
    #passed
    df[i,3] <-pass[1,j]
    i<-i+1
  }
  
  for(j in 4:8){
  
    #failed
    df[q,3]<- fail[1,j]
    q <- q+1
    
  }
    return(df)
}


cleanData <- function(pithia_edit){
    # Fix column names
    names(pithia_edit)[2]  <- "school"
    names(pithia_edit)[3]  <- "reg_year"
    names(pithia_edit)[4]  <- "semester"
    names(pithia_edit)[5]  <- "class_group"
    names(pithia_edit)[6]  <- "course_id"
    names(pithia_edit)[7]  <- "course_title"
    names(pithia_edit)[8]  <- "course_type"
    names(pithia_edit)[9]  <- "exam"
    names(pithia_edit)[10] <- "exam_year"
    names(pithia_edit)[11] <- "grade"
    names(pithia_edit)[16] <- "graduation_year"
    names(pithia_edit)[17] <- "degree_grade"

    # fix grade values
    pithia_edit$grade <- as.character(pithia_edit$grade)

    temp <- str_replace(pithia_edit$grade, pattern = ",", replacement = ".")
    #fix minus values
    pithia_edit$grade <- abs(as.double(temp))
     
    #remove uselless variables
    rm(temp,fixedHeader,splitHead)

    # remove reduntant levels
    pithia_edit$exam <- factor(pithia_edit$exam)

    # Fix variable's type
    pithia_edit$trexon_id_PS_foititi <- as.factor(pithia_edit$trexon_id_PS_foititi)

    # fix variables type
    pithia_edit$trexon_id_kateuthinsi_foititi <- as.factor(pithia_edit$trexon_id_kateuthinsi_foititi) 
    return(pithia_edit)
}