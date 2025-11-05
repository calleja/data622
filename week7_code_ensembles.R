#Earthquake database - CSV - https://earthquake.usgs.gov/earthquakes/feed/v1.0/csv.php

#indicators of geographic faults----------scatterplot-------------------------------------

library(dplyr) 
library(tidyverse)
library(mapview)

#all_hour.csv
#all_day.csv
#all_week.csv
#all_month.csv

z<-read.csv(url("https://earthquake.usgs.gov/earthquakes/feed/v1.0/summary/all_day.csv"))
#print(head(z))

count(z['latitude'])

dim(z)
summary(z)

plot(x=z$latitude, y=z$longitude, xlab="LAT", ylab="LON")

# Plotting the model will illustrate the error rate as we average across more trees 
# and shows that our error rate stabilizes with around 150 trees.

# count observations by group
z %>% count(place, sort=T)

mapview(z, xcol = "longitude", ycol = "latitude", crs = 4269, grid = FALSE) #set the map projection 2 to a common projection standard such as WGS84 via the argument crs = 4326.)

#----------------------- Decision Tree----------------------------------------------

library(party)

# Create the input data frame.
input.dat <- z[c(1:100),]

# Give the chart file a name.
png(file = "decision_tree.png")

# Create the tree. Invert mag & depth
output.tree <- ctree(depth ~ latitude + longitude + mag, data = input.dat) 

# Plot the tree.
plot(output.tree)

# Save the file.
dev.off()

#-----------------Random Forest Regression--------------------

# Create random forest for regression

library(randomForest)
mag.rf <- randomForest(depth~ ., data = z, mtry = 3, importance = TRUE, na.action = na.omit)

#Print regression model
print(mag.rf)

# Output to be present as PNG file
png(file = "randomForestRegression.png")

# Plot the error vs the number of trees graph
plot(mag.rf)

# Saving the file
dev.off()


#------------Random Forest Classification-----------------------

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(z$mag, SplitRatio = 0.8)
training_set = subset(z, split == TRUE)
test_set = subset(z, split == FALSE)

# Feature Scaling
training_set[3] = scale(training_set[3])
test_set[3] = scale(test_set[3])

# Fitting Random Forest Classification to the Training set
# install.packages('randomForest')
library(randomForest)
set.seed(12345)
classifier = randomForest(x = training_set[3],
                          y = training_set$mag,
                          ntree = 500)

# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[3])

# Making the Confusion Matrix
cm = table(test_set[, 3], y_pred)

summary(training_set)
count(training_set)

summary(test_set)
count(test_set)

# Visualize Training set results
set = training_set
X1 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
X2 = seq(min(set[, 3]) - 1, max(set[, 3]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('depth', 'mag')
##y_grid = predict(classifier, grid_set)
plot(set[, 3],
     main = 'Random Forest Classification (Training set)',
     xlab = 'depth', ylab = 'mag',
     xlim = range(X1), ylim = range(X2))


# Visualize Test set results
set = test_set
X1 = seq(min(set[, 2]) - 2, max(set[, 2]) + 1, by = 0.01)
X2 = seq(min(set[, 3]) - 3, max(set[, 3]) + 1, by = 0.01)
#grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('depth', 'mag')
y_grid = predict(classifier, grid_set)
plot(set[, 3], main = 'Random Forest Classification (Test set)',
     xlab = 'depth', ylab = 'mag',
     xlim = range(X1), ylim = range(X2))

# Choosing the number of trees
plot(classifier)

#----------------------------------------------------------