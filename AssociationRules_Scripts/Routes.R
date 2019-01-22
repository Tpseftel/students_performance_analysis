#    _____                                 _____        _           __                                 _            _ 
#   |  __ \                               |  __ \      | |         / _|               /\              (_)          (_)
#   | |__) | __ ___ _ __   __ _ _ __ ___  | |  | | __ _| |_ __ _  | |_ ___  _ __     /  \   _ __  _ __ _  ___  _ __ _ 
#   |  ___/ '__/ _ \ '_ \ / _` | '__/ _ \ | |  | |/ _` | __/ _` | |  _/ _ \| '__|   / /\ \ | '_ \| '__| |/ _ \| '__| |
#   | |   | | |  __/ |_) | (_| | | |  __/ | |__| | (_| | || (_| | | || (_) | |     / ____ \| |_) | |  | | (_) | |  | |
#   |_|   |_|  \___| .__/ \__,_|_|  \___| |_____/ \__,_|\__\__,_| |_| \___/|_|    /_/    \_\ .__/|_|  |_|\___/|_|  |_|
#                  | |                                                                     | |                        
#    _____             _            
#   |  __ \           | |           
#   | |__) |___  _   _| |_ ___  ___ 
#   |  _  // _ \| | | | __/ _ \/ __|
#   | | \ \ (_) | |_| | ||  __/\__ \
#   |_|  \_\___/ \__,_|\__\___||___/
#                                   
#                                   
student_courses_data <- group_by(route_data,
                                   studid,
                                   equal_to,
                                   course_title,
                                  trexousa_kateuthinsi_foititi) %>%
        summarise(times = n(),
                  max_grade = max(grade))

# Remove NA records

student_courses_data <- na.omit(student_courses_data) 

# Remove records with grade < 5
student_courses_data1 <- filter(student_courses_data, max_grade >= 5)
rm(student_courses_data)


#Arrange the above result by student and then by course
student_courses_data2 <- arrange(student_courses_data1, studid, equal_to)
rm(student_courses_data1)


# Split df per student
df_per_student <- split(student_courses_data2, student_courses_data2$studid)
rm(student_courses_data2)


# Delete mixed courses that student havent pass both lab and theory
x <- lapply(df_per_student, delMixed )
rm(student_courses_data2)
# Remove null dataframes from list
x<-x[sapply(x, function(x) dim(as.data.frame(x))[1]) > 0]
x<-x[sapply(x, function(x) {
  if(length(dim(x)) > 0)
    return(TRUE)
  else
    return(FALSE)
    })]


# Compute grade for mixed courses (60% - %40)
computedMixedCourses <- lapply(x, computeMixedCourseGrade)
rm(df_per_student,x)

# keep only theory from mixed courses than lab dont exist now
refinedData <- lapply(computedMixedCourses, deleteOldlabs)
rm(computedMixedCourses)

#remove null dataframes
refinedData<-refinedData[sapply(refinedData, function(x) dim(as.data.frame(x))[1]) > 0]
refinedData<-refinedData[sapply(refinedData, function(x) {
  if(length(dim(x)) > 0)
    return(TRUE)
  else
    return(FALSE)
    })]

# Apply penalty to grades
withPenaltData <-lapply(refinedData, applyPenalty) 
rm(refinedData)


# Classify grades
classified <- lapply(withPenaltData, function(df){
  df<- mutate(df,
              category = cut(final_grade, breaks = c(0, 6.5, 7.5, 8.5, 10), labels = c("μέτρια", "καλά", "πολύ καλά", "άριστα")))
  return(df)
})


#____________course_title Standardization(to P5 titles)________

# Import P5 Course -> Course_id
p5Ids <-read.xlsx(file = "../data/P5_courses_id.xlsx",sheetName = 1, encoding = "UTF-8")


onlyP5titles <- lapply(classified, replaceTitleToP5Compound, p5Ids )


# "-Θ " from course tittle
onlyP5titles_refined <- lapply(onlyP5titles,function(df){
  df$course_title <- gsub("- Θ","",df$course_title, ignore.case = TRUE)
  return(df)
})


#_____OPTIONAL____Remove certain course from data____________ 
onlyP5titles_refined <- lapply(onlyP5titles_refined, function(df){
  row <-which(df$course_title == "Γραφικά Υπολογιστών")
  df <- df[-row, ]
  return(df)
})

# #______Optional remove records by Performance
# onlyP5titles_refined2 <- lapply(onlyP5titles_refined, function(df){
#   row <-which(df$category == "μέτρια")
#   df <- df[-row, ]
#   return(df)
# })

# Generate studen's Performance
studensPerformance <- lapply(onlyP5titles_refined, function(df) {
 df <-  unite(df, col = "performance", sep = "->",  c(course_title, category),remove = FALSE)
 return(df)
  })

rm(onlyP5titles,onlyP5titles_refined)

# Transform Data to transactions(Basket Format)
studensPerformance <- lapply(studensPerformance, ungroup)
x <- lapply(studensPerformance, function(df){
  df <- select(df, c('studid','performance','trexousa_kateuthinsi_foititi'))
  df$equal_to <-NULL
  df$performance <- as.factor(df$performance)
  df$studid <- as.numeric(df$studid)
  names(df)[1] <- 'TID'
  names(df)[2] <- 'items'
  df <- as.data.frame(df)
  
  return(df)

  })
rm(studensPerformance)

# Add student Route to transaction
route_trans <- lapply(x, addRoute)

# Unified df per student(list) to one df
x <- bind_rows(route_trans)


# #__________ Implement__Arules___________________________
transactions <- x
rm(x)
# Cast df transactions class
transactions <- as(split(transactions[,"items"], transactions[,"TID"]), "transactions")
