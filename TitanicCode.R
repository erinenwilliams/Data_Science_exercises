#load the data in RStudio
titanic_original <- read_csv("~/Documents/titanic_original.csv")

#1 port of embarkation - replace missing values with S
titanic_original$embarked[is.na(titanic_original$embarked)] <- "S"

#2 age - calculate mean
mean_age <- mean(titanic_original$age, na.rm = TRUE) 

#2 age - populate missing values with mean
titanic_original$age[is.na(titanic_original$age)] <- 29.88

#3 lifeboat - empty == NA
titanic_original$boat[is.na(titanic_original$boat)] <- "None"

#4 cabin - new binary column 'has_cabin_number'
titanic_original$has_cabin_number <- as.numeric(!(is.na(titanic_original$cabin)))

#5 submit code
write_csv(titanic_original, "titanic_clean.csv")