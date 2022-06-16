# initialize the required packages for the EDA
library(tidyverse)
library(janitor)
library(Hmisc)
library(funModeling)
library(corrplot)

# import the dataset for this EDA
red_wine_all  <- read_csv(file = "data/winequality-red.csv")

# General overview of the dataset --------

# output the first 6 observations to the console
head(red_wine_all)

# output the last 6 observations to the console
tail(red_wine_all)

# view the whole dataset in a separate window
View(red_wine_all)

# get a summary on the whole dataset
summary(red_wine_all)

# get a glimpse on the dataset
glimpse(red_wine_all)

# get the status of the dataset
status(red_wine_all)

# "citric acid" has quite few 0s, and the names have spaces, so they must be fixed
# clean the data for any undesired forms
red_wine_cleaned <- clean_names(dat = red_wine_all)

describe(red_wine_cleaned)

# Plotting variables ---------

ggplot(red_wine_cleaned) +
    geom_bar(aes(quality))

# we see that most of the wines are of quality 5 & 6

ggplot(red_wine_cleaned) + 
    geom_bar(aes(residual_sugar))

# we can notice that the graph is skewed to the right

ggplot(red_wine_cleaned) +
    geom_bar(aes(alcohol))

# we can notice that the graph is skewed to the right

ggplot(red_wine_cleaned) +
    geom_point(aes(quality, residual_sugar))

# we see that the wines of quality 5 & 6 tend to be more sugary

ggplot(red_wine_cleaned) +
    geom_point(aes(quality, alcohol))

# some what moderate level of alcohol in quality 5 & 6
# high alcohol levels tend to be present in higher quality wine

ggplot(red_wine_cleaned) + 
    geom_point(aes(p_h, fixed_acidity))

# we see that as the ph level increases, the fixed acidity decreases

ggplot(red_wine_cleaned) +
    geom_point(aes(alcohol, density))
    # density of the wine decreases as alcohol levels increase
    
# Conculsion ---------

# Based on the above visualization made, we can clearly
# see that there is a strong correlation between
# density and alcohol
# So lets calculate the correlation alongside the covariance 

cov(red_wine_cleaned)

# now lets tidy it up for density
correlation_table(red_wine_cleaned, target = c("density"))

# now for alcohol
correlation_table(red_wine_cleaned, target = "alcohol")


# we can see that both variables correlate to each other with a -0.5

# we also see that there is a correlation between ph levels
# and fixed acidity, so lets check them out

# lets see it from the ph view
correlation_table(red_wine_cleaned, target = "p_h")

# now from the fixed acidity view
correlation_table(red_wine_cleaned, target = "fixed_acidity")

# we can see that both variables correlate to each other with a -0.68

# finally, we plot the correlation plot to verify our findings
corrplot(cor(red_wine_cleaned))


