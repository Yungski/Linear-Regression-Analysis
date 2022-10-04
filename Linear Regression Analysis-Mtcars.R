##Loading on the dataframe to R

#Then, we attach the dataframe in order to use it for our analysis
library(tinytex)
data(mtcars)
attach(mtcars)
?"mtcars"

###Exploring the dataset to understand its properties
##The structure of the data
View(mtcars)
library(psych)
describe(mtcars)
summary(mtcars)

#As it is numeric variables we are able to get summary statistics of all the relevant columns

##Creating plots of the predictor variables against our response variable mpg

plot(mpg,cyl,type= "p", col="red",main = "MPG against CYL", xlab = "mpg" , ylab = "cyl")

plot(mpg,disp,type= "p",col="blue",main = "MPG against DISP",xlab = "mpg" , ylab = "disp")

plot(mpg,hp,type= "p",col="orange",main = "MPG against HP", xlab = "mpg" , ylab = "hp")

plot(mpg,drat,type= "p",col="green", main = "MPG against DRAT", xlab = "mpg" , ylab = "drat")

plot(mpg,wt,type= "p",col="violet",main = "MPG against WT", xlab = "mpg" , ylab = "wt")

plot(mpg,qsec,type= "p",col="brown",main = "MPG against QSEC", xlab = "mpg" , ylab = "qsec")

plot(mpg,vs,type= "p",col="pink",main = "MPG against VS", xlab = "mpg" , ylab = "vs")

plot(mpg,am,type= "p",col="black",main = "MPG against AM", xlab = "mpg" , ylab = "am")

plot(mpg,gear,type= "p",col="purple",main = "MPG against GEAR", xlab = "mpg" , ylab = "gear")

plot(mpg,carb,type= "p",main = "MPG against CARB", xlab = "mpg" , ylab = "carb")


##Histogram of our variable of focus mpg


hist(mtcars$mpg,xlab = "MPG",main = "Histogram of MPG")


##Conducting a correlation analysis on the variables
mtcars[1:11]
cor(mtcars[c("mpg", "cyl", "disp", "hp","drat","wt","qsec","vs","am","gear","carb")])
#This correlation matrix gives information on the correlation of the various variables

##Vizualizing the relationship between the variables using a scatterplot matrix
pairs(mtcars[c("mpg", "cyl", "disp", "hp","drat","wt","qsec","vs","am","gear","carb")])

##The improved scatterplot matrix
pairs.panels(mtcars)


##Training the Model with mpg as the response variable and the rest as the predictor variables
cars <- lm(mpg ~ cyl + disp + hp + drat + wt + qsec + vs + am + gear + carb, data = mtcars)
summary(cars)


##The results of this regression indicate:

# A unit change in the number of cylinders in the vehicles is associated with a 0.1114 decrease in the miles per gallon in the vehicles.
#A unit change in the displacement in the vehicles is associated with a 0.01334 increase in the miles per gallon in the vehicles.
#A unit change in the gross horsepower of the vehicles is associated with a 0.02148 decrease in the miles per gallon in the vehicles.
#A unit change in the rear axle ratio of the vehicles is associated with a 0.78711 increase in the miles per gallon in the vehicles.
#A unit change in the weight of the vehicles is associated with a 3.71530 decrease in the miles per gallon in the vehicles.
#A unit change in the quarter mile time of the vehicles is associated with a 0.82104 increase in the miles per gallon in the vehicles.
#A unit change in the engine cylinder configuration of the vehicles is associated with a 0.31776 increase in the miles per gallon in the vehicles.
#A unit change in the transmission type in the vehicles is associated with a 2.52023 increase in the miles per gallon in the vehicles.
#A unit change in the number of number of forward gears in the vehicles is associated with a 0.65541 increase in the miles per gallon in the vehicles.
#A unit change in the number of carburetors in the vehicles is associated with a 0.19942 decrease in the miles per gallon in the vehicles.


##Evaluating the Cars model performance
cars_model <- lm(mpg ~ ., data = mtcars)
cars_model
summary(cars_model)

#The model has an R- squared of 86.9% - Indicating that it explains that percentage of variation in the data on 21 degrees of freedom.

#The adjusted R-squared of 80.66% indicates the model does well in explaining the variation in the dataset.


##Adding non- linear relationship to the gear and carb variables
mtcars$gear3 <- mtcars$gear^3

mtcars$carb2 <- mtcars$carb^2

View(mtcars)


##Adding a binary relationship to the variable- number of cylinders

#This is because the number of cylinders are either 4, 6 or 8
mtcars$cyl <- ifelse(mtcars$cyl <= 6, 1, 0)
View(mtcars)

#If the number of cylinders are 4/6 the value assigned is 1 and if the number of cylinders is 8 the value assigned is zero.

##Creating the improved linear model
#The interaction effect is between the weight and the acceleration- to try assess the effect that weight directly has on the acceleration of the vehicles in question.
cars2 <- lm(mpg ~ cyl + disp + hp + drat + wt*qsec + vs + am + gear + carb + carb2 + gear3, data=mtcars)
summary(cars2)


#This model has an R- squared of 91.76% - Indicating that it explains that percentage of variation in the data on 21 degrees of freedom.
#The adjusted R-squared of 85.8% indicates the model does better in explaining the variation in the dataset.





