###############
### Cleaning Data
###############
## Name:Noon Pokaratsiri Goldstein, Pauline Sander, Axel Allen
## Matriculation number:2581469, 2581310, 2579024

# Please do the "Cleaning Data with R" exercise that was assigned in DataCamp.
# We recommend that you take notes during the DataCamp tutorial, so you're able to use the commands 
# you learned there in the exercises below.
# This week, the exercise will be about getting data into a good enough shape to start analysing. 
# Next week, there will be a tutorial on how to further work with this data.
## You need to provide a serious attempt to each exercise in order to have
## the assignment graded as complete. 

# 1. Download the data file "digsym.csv" from the moodle and save it in your working directory. 


# 2. Read in the data into a variable called "dat".
dat <- read.csv(file="digsym.csv", header=TRUE, sep=",")

# 3. Load the libraries languageR, stringr, dplyr and tidyr.
library(languageR)
library(stringr)
library(dplyr)
library(tidyr)

# 4. How many rows, how many columns does that data have?
str(dat)
#Observations: 3,700
#Variables: 11
# in 3700 rows and 11 columns

# 5. Take a look at the structure of the data frame using "glimpse".
glimpse(dat)

# 6. View the first 20 rows, view the last 20 rows.
head(dat, 20)
tail(dat, 20)

# 7. Is there any missing data in any of the columns?
#many missing values. e.g. rows 1-10, the values for StimulsDS1.CRESP and StimulusDS1.RESP are missing,
#the StimulDS1.RT also have NA values.

# 8. Get rid of the row number column.
dat <- select(dat, -c('X'))

# 9. Put the Sub_Age column second.
dat <- dat[c(1, 10, 2:9)]
head(dat)
# 10. Replace the values of the "ExperimentName" column with something shorter, more legible.
dat$ExperimentName <- "DSK"

# 11. Keep only experimental trials (encoded as "Trial:2" in List), get rid of practice trials 
# (encoded as "Trial:1"). When you do this, assign the subset of the data to a variable "data2", 
# then assign data2 to dat and finally remove data2.
data2 <- filter(dat, List == "Trial:2")
dat <- data2
remove(data2)

# 12. Separate Sub_Age column to two columns, "Subject" and "Age", using the function "separate".
data2 <- separate(dat, Sub_Age, c("Subject", "Age"), sep = " _ ")
dat <- data2
remove(data2)

# 13. Make subject a factor.
dat$Subject <- factor(dat$Subject)

# 14. Extract experimental condition ("right" vs. "wrong") from the "File" column:
# i.e. we want to get rid of digit underscore before and the digit after the "right" and "wrong".
dat$File <- str_replace(dat$File, "[:digit:]_right[:digit:]?", "right")
dat$File <- str_replace(dat$File, "[:digit:]_wrong[:digit:]?", "wrong")

# 15. Using str_pad to make values in the File column 8 chars long, by putting 0 at the end  (i.e., 
# same number of characters, such that "1_right" should be replaced by "1_right0" etc).
dat$File <- str_pad(dat$File, 8, "right", "0")

# 16. Remove the column "List".
dat <- subset(dat, select = -List)

# 17. Change the data type of "Age" to integer.
dat$Age <- as.integer(dat$Age)

# 18. Missing values, outliers:
# Do we have any NAs in the data, and if so, how many and where are they?
sum(is.na(dat))
# There are no NAs.

# 19. Create an "accuracy" column using ifelse-statement.
# If actual response (StimulDS1.RESP) is the same as the correct response (StimulDS1.CRESP), put 
# in value 1, otherwise put 0.
dat$accuracy <- ifelse(dat$StimulDS1.RESP == dat$StimulDS1.CRESP, 1, 0)

# 20. How many wrong answers do we have in total?
wansw <- nrow(dat) - sum(dat$accuracy)
wansw
# 185 answers are wrong.

# 21. What's the percentage of wrong responses?
wansw/nrow(dat)
# About 5.5 percent of the responses are wrong.

# 22. Create a subset "correctResponses" that only contains those data points where subjects 
# responded correctly. 
correctResponses <- filter(dat, accuracy == "1")


# 23. Create a boxplot of StimulDS1.RT - any outliers?


# 24. Create a histogram of StimulDS1.RT with bins set to 50.


# 25. Describe the two plots - any tails? any suspiciously large values?


# 26. View summary of correct_RT.


# 27. There is a single very far outlier. Remove it and save the result in a new dataframe named 
# "cleaned".


## EXTRA Exercises:
##You can stop here for your submission of this week's assignment,
##but you are encouraged to try it now. 
##All these exercises will be discussed and solved in the tutorial!

# 28. Dealing with the tail of the distribution: outlier removal
# Now we want to define a cutoff value for the StimulDS1.RT variable in the correctResp dataset.
# Values should not differ more than 2.5 standard deviations from the grand mean of this variable.
# This condition should be applied in a new variable called "correct_RT_2.5sd", which prints NA 
# if an RT value is below/above the cutoff. 


# 29. Take a look at the outlier observations.
# Any subjects who performed especially poorly?


# 30. How many RT outliers are there in total?


# 31. Plot a histogram and boxplot of the correct_RT_2.5sd column again - nice and clean eh?


# 32. Next, we'd like to take a look at the average accuracy per subject.
# Using the "cast" function from the library "reshape", create a new data.frame which shows the 
# average accuracy per subject. Rename column which lists the average accuracy as "avrg_accuracy".


# 33. Sort in ascending order or plot of the average accuracies per subject.


# 34. Would you exclude any subjects, based on their avrg_accuracy performance?


# 35. Congrats! Your data are now ready for analysis. Please save the data frame you created 
# into a new file called "digsym_clean.csv".