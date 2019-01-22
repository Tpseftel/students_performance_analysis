# delete mixed courses than arent passed both theory and lab
# parametre df = dataframe
delMixed <- function(df){
  
  for(i in 1:nrow(df)){
    code <- df$equal_to[i]
    if(is.na(code))
      return ('yes')
    # return(str_length(code)== 9)
    # check if course is mixed
    if(str_length(code)== 9){
      # check if is theory
      if(str_sub(code, 9, 9) == 1 ){
        # search for lab (ends with2)
        lab <- code
        substr(lab, 9, 9) <- "2"
        # lab dont exist
        if(!(sum(grepl(lab, df$equal_to)) >= 1)){
          #mark this row with zero
          df$equal_to[i] = 0
        }
      }
      # check if is lab
      if(str_sub(code, 9, 9) == 2 ){
        # search for theory(ends with 1)
        theory <- code
        substr(theory, 9, 9) <- "1"
        # thery dont exist
        if(!(sum(grepl(theory, df$equal_to)) >= 1)){
          # mark this row with zero
          df$equal_to[i] = 0
        }
      }
    }
  }
  i = 0
  # delete marked  rows  
  df <- df[!df$equal_to == 0, ]
  
  return(df)
}

# computes mixes course grade,60% theory grade + 40% lab grade 
computeMixedCourseGrade <- function(df)
{ 
  for(i in 1:nrow(df)){
    
    
    code <- df$equal_to[i]
    # isMixed
    if(str_length(code)== 9){
      
      # isTheory ->index[i]
      if(str_sub(code, 9, 9) == 1 ){
        # find lab
        lab <- code
        substr(lab, 9, 9) <- "2"
        lab_index <- grep(lab, df$equal_to)
        lab_grade <- df$max_grade[lab_index]
        
        # store lab times tested
        lab_times <- df$times[lab_index]
        
        
        theory_grade <- df$max_grade[i]
        
        #store theory times tested
        theory_times <- df$times[i]
        
        
        # compute new_grade 
        new_grade <- ((theory_grade * 60) / 100) + ((lab_grade * 40) / 100)
        
        # assign new grade to theory
        df$max_grade[i] <- new_grade
        
        # more times lab or theory 
        if(theory_times >= lab_times){
          df$times[i] <- theory_times
        }else{
          df$times[i] <- lab_times
        }
        # make lab null
        df$max_grade[lab_index] <- 0
        
      } 
     }
    
  }	
  #rm rows with null(imply labs)
  df <- df[!df$max_grade == 0, ]
  
  return(df)
}

deleteOldlabs <- function(df)
{
  for (i in 1:nrow(df)) {
    code <- df$equal_to[i]
   if(str_length(code) == 8){
      course_length <-str_length(df$course_title[i])
      last_char <- substr(df$course_title[i], course_length, course_length) 
      
      if(last_char == 'Ε' || last_char == 'E'){  # English and greek 'E'
        df$max_grade[i] <- 0 
      }
    }
  }
  df <- df[!df$max_grade == 0, ]
  return(df)
}

applyPenalty <- function(df)
  {
  
  # create final_grade column
  df <- mutate(df,
               final_grade = max_grade)
  
  for (i in 1:nrow(df)) {
    # compute new grade	
    if(df$times[i] > 2){
      df$final_grade[i] <- df$max_grade[i] - (df$times[i] * 0.3)
    }
    
  }
  return(df)	
}


transToBasketFormat <- function(df)
{
  df<-select(df, c(studid, performance))
  df$equal_to <-NULL
  y <- ddply(df,'studid', function(df1)paste(df1$performance, 
                                             collapse = ","))
  return(y)
}

prepareForTransactions <- function(df)
{
  df <- select(df, c('studid','performance'))
  df$equal_to <-NULL
  df$performance <- as.factor(df$performance)
  df$studid <- as.numeric(df$studid)
  names(df)[1] <- 'TID'
  names(df)[2] <- 'items'
  df <- as.data.frame(df)
  
  return(df) 
} 
replaceTitleToP5Compound <-function(df, p5)
{
	for (i in 1:nrow(df)) {
		# find row where course _id matches
		rowIndex <- which(p5$course_id == df$equal_to[i]) 
		df$course_title[i] <- p5$course_title[rowIndex]
	}
	return(df)
}

# add kateuthinsi foititi to items column
addRoute <-function(df)
{
  kat <- df[1,3]
  df$trexousa_kateuthinsi_foititi <- NULL
  
  de<-data.frame(df[1,1],kat)
  names(de)<-c("TID","items")
  
  df <- rbind(df, de)
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