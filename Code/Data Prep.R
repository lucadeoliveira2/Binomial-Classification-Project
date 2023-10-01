### Getting Data and Libraries ###

salary <- read.csv("Data and Visualisations/salary.csv")
install.packages('dplyr')
install.packages('tidyr')
install.packages('Amelia')
library(dplyr)
library(tidyr)
library(Amelia)

head(salary)
dim(salary)
unique(salary$workclass)
summary(salary$education)
str(salary)

### Data Prep ###

salary <- salary %>%
  select(-capital.gain, -capital.loss, -relationship, -fnlwgt)%>%
  mutate(across(where(is.character), trimws))%>%
  filter(occupation != '?') # Removes 1823 rows (5.6%)

Marital_Status <- function(x){
  if(x == 'Widowed'){
  }else if(x == 'Divorced' | x == 'Seperated' | x == 'Never-Married'){
    x <- 'Not Married'
  }else{
    x <- 'Married'
  }
  return(x)
}
salary$marital.status <- factor(sapply(salary$marital.status, Marital_Status))

Education <- function(x){
  if(x == 'Bachelors' | x == 'Some-college' | grepl('Assoc',x)){
    x <- 'Degree'
  }else if(x == 'Doctorate' | x == 'Prof-school'){
    x <- 'PostGrad'
  }else if(x == 'Masters' | x == 'HS-grad'){
    
  }else {
    x <- 'Below HS'
  }
  return(x)
}
salary$education <- factor(sapply(salary$education, Education), 
       levels = c('Below HS', 'HS-grad', 'Degree', 'Masters', 'PostGrad'))

subset(salary, occupation == 'Adm-clerical')

Workclass <- function(x, y){
  if(x == 'Private'){
    
  }else if(grepl('gov', x)){
    x <- 'Government Job'
  }else if(grepl('Self', x)){
    x <- 'Self Employed'
  }else{
    same_job <- subset(salary, occupation == y)
    x <- names(sort(summary(factor(same_job$workclass)), decreasing = T))[1]
    if(x == 'Private'){
      
    }else if(grepl('gov', x)){
      x <- 'Government Job'
    }else if(grepl('Self', x)){
      x <- 'Self Employed'
    }
  }
  return(x)
}
salary$workclass <- factor(sapply(salary$workclass, Workclass, y = salary$occupation))

Native_Region <- function(x){
  Europe <- c('England', 'France', 'Germany', 'Greece', 'Holand-Netherlands', 'Hungary', 'Ireland',
              'Italy', 'Poland', 'Portugal', 'Scotland', 'Yugoslavia')
  Asia <- c('Cambodia', 'China', 'Hong', 'India', 'Iran', 'Japan', 'Laos', 'Philippines', 'Taiwan',
            'Thailand', 'Vietnam')
  North_America <- c('United-States', 'Canada')
  South_America <- c('Columbia', 'Ecuador', 'Peru', 'South')
  if(x == '?'){
    x <- 'Unknown'
  }else if(x %in% Europe){
    x <- 'Europe'
  }else if(x %in% Asia){
    x <- 'Asia'
  }else if(x %in% North_America){
    x <- 'North America'
  }else if(x %in% South_America){
    x <- 'South America'
  }else{
    x <- 'Central America'
  }
  return(x)
}
salary$native.country <- factor(sapply(salary$native.country, Native_Region))

str(salary)

salary <- salary %>%
  mutate(across(where(is.character), as.factor))
