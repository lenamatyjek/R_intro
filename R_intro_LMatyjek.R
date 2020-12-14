################################################################################
#                                                                              #
#                           Introduction to R                                  #
#                       Lena Matyjek, 14/12/2020                               #
#                                                                              #
################################################################################

# BASICS:
# script ares, console/terminal/etc, environment/history/etc, Files/Plots/etc.
# R as calculator (numbers and operators: +,-,/,*,>,<,==,!=, &, |)
# objects: 1 is meaningful, but a is not; vector, matrix, table, funciton, model, etc.
# objects and values (= or <-)
# removing objects, e.g. rm(a)
# "" and ''
# packages
# arrow up, tab, ctrl/command+enter

################################################################################

# Settings
rm(list=ls()) # This cleans the working directory

############################### WORKING DIRECTORY ############################## 

# Set up your working directory
#setwd("xxx") # replace XXX with your directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # This will set the working directory to the folder in which this script is.

# Check your working directory
getwd()

################################# LOADING FILE #################################

# Load from CSV:
read.csv2('./iris.csv', header = T, sep = ',')

# Load from CSV and ascribe to object "dane":
dane <- read.csv2('./iris.csv', header = T, sep = ',')

# # You can also take these data from existing R datasets:
# library(datasets) # this loads a package. It's in the R base, no need to install it.
# data(iris)

################################# VIEWING FILE #################################

# See the data:
colnames(dane) # see the names of the columns
rownames(dane) # see row names; often automatic in reading CSV in R; can be ignored for now
head(dane,5) # see the first 5 rows; replace 5 with any other number
tail(dane,5) # see the last 5 rows
View(dane) # see a table in a new tab; also done by simply clicking on the object in the global environment

############################ MANIPULATING DATA FRAME ###########################

# $ operator refers to a column;
# data frames are organised with rows and columns, like that: df[r,c]
# All rows and columns are indexed, so we can refer to them by numbers:

# See column no 1:
dane[,1]

# See row no 1:
dane[1,]

# See the last column in the data frame:
dane[,length(dane)] # note: length of data frame is the number of columns!

# See the last row in the data frame:
dane[length(dane$X),] # note: length of a column in a data frame is the number of rows!

# See one column based on its name
dane$Sepal.Length

# See all data for one species
unique(dane$Species) # shows unique values of Species (regardless of no of occurences)
dane[dane$Species == 'setosa',] # show all data for setosa
dane$Sepal.Length[dane$Species == 'setosa'] # show sepal length for all setosas

# Remove a column
colnames(dane)
dane$X <- NULL
colnames(dane)


#################################### CLASSES  ##################################

# The most commonly used classes in data frames are: numeric/integer, character, factor
# But of course there are more classes of objects. Way more.

# See a class of an object
class(1)
a = 1
class(a)
a = "ach, poniedziałki"
class(a)
class(dane)
class(dane$Sepal.Length)

# See all classes in the data frame at once
sapply(dane,class)

# Change the classes so that the data make sense!
head(dane) # see the beggining data; Sepal length probably should be a number.
dane$Sepal.Length <- as.numeric(dane$Sepal.Length)
class(dane$Sepal.Length) # check is it's numeric now

    # TASK: change also: sepal width, petal legth, petal width
    # (...)
    
    # EXTRA: a faster way to do so is:
    # dane[,c(1:4)] <- lapply(dane[,c(1:4)],as.numeric)

# See Species as characters, numeric, and factors
as.character(dane$Species)
as.numeric(dane$Species) # notice the warning!
as.factor(dane$Species) # notice "Levels"!

as.factor(as.character(dane$Sepal.Length))

# Convert Species of factors and view it
dane$Species <- as.factor(dane$Species)

dane$Species # notice the levels and their order

########################## DESCRIPTIVE STATS  ##################################

# Mean of the petal length
mean(dane$Sepal.Length)

    # Round the number
    round(mean(dane$Sepal.Length),2)
    
    # See what happens when there are missing data. Save one to an object, remove it in the data frame (replace with NA), and find the mean
    missing_value <- dane$Sepal.Length[1]
    missing_value
    dane$Sepal.Length[1] <- NA
    dane$Sepal.Length[1]
    
    mean(dane$Sepal.Length) # the mean is NA! That's because NA isn't a number that can be added or divided.
    
    mean(dane$Sepal.Length, na.rm = T) # that's a solution. We ask R to ignore the NAs in the column
    
    dane$Sepal.Length[1] <- missing_value # assign the value back
    rm(missing_value) # remove the object

# Median
median(dane$Sepal.Length)

# Min / max
min(dane$Sepal.Length)
max(dane$Sepal.Length)

# Standard deviation & error
sd(dane$Sepal.Length)
sd(dane$Sepal.Length)/sqrt(length(dane$Sepal.Length)) # standard error is just the standard deviation divided by the square root of the sample size
    
    # EXTRA
    # We can create a custom function to get se! For that we need to create an object (se), which is a function, and which does what we already did
    # se <- function(x) sd(x)/sqrt(length(x))

# We can see different descriptive stats in one function, but we need a package which provides a funciton for it:
#install.packages("psych")
library(psych) # load the package
describe(dane$Sepal.Length)

# Different stats across groups
library(dplyr)
dane %>%
  group_by(Species) %>%
  summarise(sl_mean <- mean(Sepal.Length),
            sw_mean <- mean(Sepal.Width))


    # TASK
    # Find means, sd, and se for all 4 variables across the species!

############################## DATA VISUALISATION  #############################

# Histogram
hist(dane$Sepal.Length)

# Scatter plot
plot(dane$Sepal.Length)

# Scatter plot of two variables
plot(dane$Sepal.Length, dane$Sepal.Width)

# Boxplot
boxplot(dane$Sepal.Length)

# The real deal for plotting is ggplot. Let's (install and) load the package
library(ggplot2)

ggplot(data = dane, aes(x = Sepal.Length, y = Sepal.Width)) # this only has the data in, but R doesn't know what to do with this yet

ggplot(data = dane, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_point() # now we ask R to show us the raw data points

ggplot(data = dane, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_line() # now we ask R to connect the points with a line; not informative here

ggplot(data = dane, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_smooth() # here R chooses a smoothing function to show the relationship between x and y

# Let's customise a bit

ggplot(data = dane, aes(x = Sepal.Length, y = Sepal.Width, colour = Species)) +
  geom_smooth()

ggplot(data = dane, aes(x = Sepal.Length, y = Sepal.Width, colour = Species)) +
  geom_smooth() +
  theme_minimal()

ggplot(data = dane, aes(x = Sepal.Length, y = Sepal.Width, colour = Species)) +
  geom_smooth() +
  theme_minimal() +
  scale_color_brewer(palette = 1)

ggplot(data = dane, aes(x = Sepal.Length, y = Sepal.Width, colour = Species)) +
  geom_smooth() +
  theme_minimal() +
  scale_color_manual(values = c("black","red","blue")) # or no of colours in HEX

ggplot(data = dane, aes(x = Sepal.Length, y = Sepal.Width, colour = Species)) +
  geom_smooth() +
  theme_minimal() +
  scale_color_manual(values = c("black","red","blue")) + # or no of colours in HEX
  xlab("Length of the sepal") +
  ylab("Width of the sepal")

################################## LOOPS  ######################################

# Let's say we want to add to the data frame info about the colour of the flower.
# Let's say that it happens so that all flowers with Sepal length less than 6 are red and all others - blue.
# Let's add this information using a for and if loop.
# Then, let's add that all setosas are from lab1, all versicolor and virginica from lab2.

# For loop
for (i in 1:length(dane$Sepal.Length)) {
  if (dane$Sepal.Length[i] <= 6) {
    dane$colour[i] <- "red" # see the comment below
  } else {
    dane$colour[i] <- "blue" 
  }
}
  # additional comment: note that R can ascribe a value to a column that doesn't yet exist!
  # This may be sometimes problematic, so you may want to first create an empty column.
  # As an example, we will do it below before the while loop.


# While loop
dane <- plyr::ddply(dane,c("Species")) # Let's say we have a sorted data frame by the column "Species"

dane['lab'] <- as.character()

i = 1 # set a counter
while (i <= length(dane$Sepal.Length)) {
  if (dane$Species[i] == 'setosa') {
    dane$lab[i] <- 'lab1'
  } else if (dane$Species[i] == 'versicolor' | dane$Species[i] == 'virginica') {
    dane$lab[i] <- 'lab2'
  }
  i = i+1 # don't forget to increase the counter!
}

# NOTE
# Often, loops can be replaced by indexing. Indexing is faster and more elegant in the code.
# Let's see how the work for both the previous loops.

t1 <- Sys.time() # get time of the system
for (i in 1:length(dane$Sepal.Length)) {
  if (dane$Sepal.Length[i] <= 6) {
    dane$colour[i] <- "red" # see the comment below
  } else {
    dane$colour[i] <- "blue" 
  }
}
t2 <- Sys.time() # get time of the system again

text <- "This loop took:" # create a message to beprited in the consol
text2 <- "secs."
info_whole <- paste(text,round(t2-t1,3),text2)
print(info_whole) # print it

t1 <- Sys.time()
dane$colour[dane$Sepal.Length <= 6] <- "red"
dane$colour[dane$Sepal.Length > 6] <- "blue"
t2 <- Sys.time()

text <- "Indexing took:" # create a message to beprited in the consol
text2 <- "secs."
info_whole <- paste(text,round(t2-t1,3),text2)
print(info_whole) # print it

# So indeed, it's faster with indexing and it takes less code to achieve the same.
# Take-home message: use indexing if you can! Leave loops for when it's necessary.
# Let's check the while loop too:
t1 <- Sys.time()
i = 1 # set a counter
while (i <= length(dane$Sepal.Length)) {
  if (dane$Species[i] == 'setosa') {
    dane$lab[i] <- 'lab1'
  } else if (dane$Species[i] == 'versicolor' | dane$Species[i] == 'virginica') {
    dane$lab[i] <- 'lab2'
  }
  i = i+1 # don't forget to increase the counter!
}
t2 <- Sys.time()
print(paste("This loop took:",round(t2-t1,3),"secs."))

t1 <- Sys.time()
dane$lab[dane$Species == "setosa"] <- 'lab1'
dane$lab[dane$Species != "setosa"] <- 'lab2'
t2 <- Sys.time()
print(paste("Indexing took:",round(t2-t1,3),"secs."))

    # TASK
    # add a dummy variable (0,1) to the data frame so that only cases of flowers
    # with sepals longer than the mean of the whole sample AND petals wider than
    # the median of the sample get value 1. You can use indexes or a loop.

################################# SIMPLE STATS  ################################
    # EXTRA
    # Always check classes before running stats!

# t test - difference between petal length in setosa and in virginica
t.test(dane$Petal.Length[dane$Species == 'setosa'],
       dane$Petal.Length[dane$Species == 'virginica'])

# anova
aov(formula = Petal.Length ~ Species, data = dane)

my_anova <- aov(Petal.Length ~ Species, data = dane)
summary(my_anova)

options(scipen=999) # remove scientific notation
summary(my_anova)

# linear regression
my_model <- lm(Petal.Length ~ Species, data = dane)
summary(my_model)

# correlation
library(Hmisc)
rcorr(as.matrix(dane[,c(1:4)]),type="pearson")

