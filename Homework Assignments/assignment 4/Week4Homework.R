### Homework 4 r questions


###########################
# question 1 part b: Graph the demand for Klein's product given I = 5000, A = 20, and Z = 1000
###########################
  # formula = Q = -104 -2.1P +3.2I + 1.5A + 1.6z

  # for the problem: I = 5,000, A = 20, Z = 1000
  
  # calculate the derived formula:

new_coef <- -104 + (3.2*5000) + (1.5* 20) + (1.6*1000)

# the new formula is : Q = 17,526 -2.1P


prices <- 1:10000

vals <- new_coef - 2.1 * prices

plot(vals, xlab = 'price of competitor product', ylab = 'quantity demanded')


###########################
# question 1 part c: Input 500 for price
###########################

  # if P equals 500 and the previous conditions hold:

new_coef - (2.1 * 500)


###########################
# question 2 : Graph the demand for Klein's product given I = 5000, A = 20, and Z = 1000
###########################


library("AER")
data("CPS1985")
#Write data to excel

# dangit...didn't make this an R-project...

setwd("C:/SMU Data science/econometrics/Homework Assignments/assignment 4")

library(xlsx) #load the package
write.csv(x = CPS1985, file = "CPS1985_DataDownload.csv") # used this because I didn't want to set the JAVA_HOME variable
# for the xlsx package on my computer...long story...


# R model:
wage_lm <- lm(wage ~ education, data = CPS1985)
           
summary(wage_lm)
anova(wage_lm)
coef(wage_lm)
confint(wage_lm, level = 0.95)



