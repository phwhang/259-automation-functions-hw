#PSYC 259 Homework 4 - Writing functions
#Optional, for extra credit if all questions are answered

#List names of students collaborating with: (just me) Priscilla Whang

# clear console, environment
cat('\014')
rm(list = ls())

### SETUP: RUN THIS BEFORE STARTING ----------
library(tidyverse)
set.seed(1)
id <- rep("name", 30)
x <- runif(30, 0, 10)
y <- runif(30, 0, 10)
z <- runif(30, 0, 10)
ds <- tibble(id, x, y, z)

### Question 1 ---------- 

#Vectors x, y, and z contain random numbers between 1 and 10. 
#Write a function called "limit_replace" that will replace values less than 2 or greater than 8 with NA
#Then, run the function on x and save the results to a new vector "x_replace" to show it worked

x
sum(is.na(x)) # 0
sum(x < 2)    # 4
sum(x > 8)    # 6

limit_replace <- function(vec) {
  vec <- ifelse(vec < 2 | vec > 8, 
                NA, 
                vec)
}

x_replace <- limit_replace(x)

x_replace
sum(is.na(x_replace)) # 10; 4 + 6

### Question 2 ---------- 

#Make a new version of limit_replace that asks for arguments for a lower boundary and an upper boundary
  #so that they can be customized (instead of being hard coded as 2 and 8)
#Run the function on vector y with boundaries 4 and 6, saving the results to a new vector "y_replace"

y
sum(is.na(y)) # 0
sum(y < 4)    # 7
sum(y > 6)    # 12

limit_replace <- function(vec, lower, upper) {
  vec <- ifelse(vec < lower | vec > upper,
                NA,
                vec)
}

y_replace <- limit_replace(y, 4, 6)

y_replace
sum(is.na(y_replace)) # 19; 7 + 12

### Question 3 ----------

#Write a function called "plus_minus_SD" that can take one of the vectors (x/y/z) as input
  #and "num_of_SDs" as an input and returns the boundaries +/- num_of_SDs around the mean. 
#plus_minus_SD(x, 1) would give +/- 1SD around the mean of x, plus_minus_SD(y, 2) would give +/- 2SDs around the mean 
#Make num_of_SDs default to 1
#run the new function on x, y, and z with 1 SD

plus_minus_SD <- function(vec, num_of_SDs = 1) {
  vec_mean <- mean(vec)
  vec_sd <- sd(vec)
  boundary <- num_of_SDs * vec_sd
  
  lower <- vec_mean - boundary
  upper <- vec_mean + boundary
  
  return(c(lower = lower,
           upper = upper))
}

plus_minus_SD(x, 1)
plus_minus_SD(y, 1)
plus_minus_SD(z, 1)

### Question 4 ----------

#Write an another new version of limit_replace
#This time, make the upper and lower boundaries optional arguments
#If they are not given, use +/- 1 SD as the boundaries (from your plus_minus_SD function)
#Apply the function to each column in ds, and save the results to a new tibble called "ds_replace"

limit_replace <- function(vec, 
                          lower = NULL, 
                          upper = NULL) {
  
  if (is.null(lower) | is.null(upper)) {
    boundaries <- plus_minus_SD(vec, 1)
    lower <- boundaries['lower']
    upper <- boundaries['upper']
  }
  
  vec <- ifelse(vec < lower | vec > upper,
                NA,
                vec)
  
  return(vec)
}

names(ds) # "id" "x"  "y"  "z"

ds_replace <- ds %>% 
  select(-id) %>% 
  mutate(across(everything(), limit_replace))

ds_replace

### Question 5 ----------

#Add a "stopifnot" command to your limit_replace function to make sure it only runs on numeric variables
#Try running it on a non-numeric input (like "id") to make sure it gives you an error

?stopifnot

limit_replace <- function(vec, 
                          lower = NULL, 
                          upper = NULL) {
  
  stopifnot(is.numeric(vec))
  
  if (is.null(lower) | is.null(upper)) {
    boundaries <- plus_minus_SD(vec, 1)
    lower <- boundaries['lower']
    upper <- boundaries['upper']
  }
  
  vec <- ifelse(vec < lower | vec > upper,
                NA,
                vec)
  
  return(vec)
}

ds_replace <- ds %>% 
  mutate(id = limit_replace(id)) # is.numeric(vec) not TRUE

### Question 6 ----------

#What other requirements on the input do you need to make the function work correctly?
#Add another stopifnot to enforce one more requirement

# what if vec was empty?
w <- numeric(0)
limit_replace(w) # logical(0)

limit_replace <- function(vec, 
                          lower = NULL, 
                          upper = NULL) {
  
  stopifnot(is.numeric(vec))
  stopifnot(length(vec) > 0)
  
  if (is.null(lower) | is.null(upper)) {
    boundaries <- plus_minus_SD(vec, 1)
    lower <- boundaries['lower']
    upper <- boundaries['upper']
  }
  
  vec <- ifelse(vec < lower | vec > upper,
                NA,
                vec)
  
  return(vec)
}

limit_replace(w) # proper error msg

### Question 7 ----------

#Clear out your workspace and load the built-in diamonds dataset by running the lines below
#RUN THIS CODE
rm(list = ls())
library(tidyverse)
ds_diamonds <- diamonds

#Save your two functions to an external file (or files) 
#Then, load your functions from the external files(s)
#Next, run your limit_replace function on all of the numeric columns in the new data set
#and drop any rows with NA, saving it to a new tibble named "ds_trimmed"

?source
?writeLines

writeLines(
  c('plus_minus_SD <- function(vec, num_of_SDs = 1) {',
    '  vec_mean <- mean(vec)',
    '  vec_sd <- sd(vec)',
    '  boundary <- num_of_SDs * vec_sd',
    '',
    '  lower <- vec_mean - boundary',
    '  upper <- vec_mean + boundary',
    '',
    '  return(c(lower = lower, upper = upper))',
    '}',
    '',
    
    'limit_replace <- function(vec, lower = NULL, upper = NULL) {',
    '  stopifnot(is.numeric(vec))',
    '  stopifnot(length(vec) > 0)',
    '',
    '  if (is.null(lower) | is.null(upper)) {',
    '    boundaries <- plus_minus_SD(vec, 1)',
    '    lower <- boundaries["lower"]',
    '    upper <- boundaries["upper"]',
    '  }',
    '',
    '  vec <- ifelse(vec < lower | vec > upper, NA, vec)',
    '',
    '  return(vec)',
    '}',
    sep = '\n'
  ),
  'functions.R'
)

# checked that they're in my folder

source('functions.R')

# check
names(ds_diamonds)
diamonds %>% head(20)

ds_trimmed <- ds_diamonds %>% 
  mutate(across(where(is.numeric),
                ~ limit_replace(.)))

ds_trimmed

### Question 8 ----------

#The code below makes graphs of diamond price grouped by different variables
#Refactor it to make it more efficient using functions and/or iteration
#Don't worry about the order of the plots, just find a more efficient way to make all 6 plots
#Each cut (Premium/Ideal/Good) should have a plot with trimmed and untrimmed data
#The title of each plot should indicate which cut and whether it's all vs. trimmed data

ds_diamonds %>% filter(cut == "Premium") %>% 
  ggplot(aes(x = clarity, y = price)) + 
  geom_boxplot() + 
  ggtitle("Premium, all") + 
  theme_minimal()

ds_diamonds %>% filter(cut == "Ideal") %>% 
  ggplot(aes(x = clarity, y = price)) + 
  geom_boxplot() + 
  ggtitle("Ideal, all") +
  theme_minimal()

ds_diamonds %>% filter(cut == "Good") %>% 
  ggplot(aes(x = clarity, y = price)) + 
  geom_boxplot() + 
  ggtitle("Good, all") +
  theme_minimal()

ds_trimmed %>% filter(cut == "Premium") %>% 
  ggplot(aes(x = clarity, y = price)) + 
  geom_boxplot() + 
  ggtitle("Premium, trimmed") + 
  theme_minimal()

ds_trimmed %>% filter(cut == "Ideal") %>% 
  ggplot(aes(x = clarity, y = price)) + 
  geom_boxplot() + 
  ggtitle("Ideal, trimmed") +
  theme_minimal()

ds_trimmed %>% filter(cut == "Good") %>% 
  ggplot(aes(x = clarity, y = price)) + 
  geom_boxplot() + 
  ggtitle("Good, trimmed") +
  theme_minimal()

plotter <- function(data, cut_type, trim_type) {
  data %>% 
    filter(cut == cut_type) %>% 
    ggplot(aes(x = clarity, y = price)) +
    geom_boxplot() + 
    ggtitle(paste(cut_type, trim_type)) + 
    theme_minimal()
}

cut_types <- c('Premium', 'Ideal', 'Good')

lapply(cut_types, 
       function(cut) plotter(ds_diamonds, cut, 'all'))

lapply(cut_types, 
       function(cut) plotter(ds_trimmed, cut, 'trimmed'))
