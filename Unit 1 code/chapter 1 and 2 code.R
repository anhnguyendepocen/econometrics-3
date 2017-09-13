# chapter 1: applied econometrics


install.packages("AER")


library(AER)


 data("Journals", package = "AER")

df<- Journals

dim(df)
str(df)
summary(df)

names(df)

# plot the per citation price versus the references:

plot(subs~ price/citations, data = df)

# that doesnt look good, let's take the log:

plot(log(subs)~ log(price/citations), data = df)


# see that the number of subs declines as price increases



# create a linear model

clf <- lm(log(subs)~ log(price/citations), data = df)
abline(clf)

# summarize the model:

summary(clf)

# remember that the coefficient on the log(price/citation) is the elacticity of this variable:


###################
#### Example 2 ####
###################

install.packages("quantreg")

library(quantreg)



data("CPS1985", package = "AER")
df2<- CPS1985


# create a simple linear model whic hwil lhave the same exact syntax as a simple linear regression plus a Tau term:



cps_lm <- lm(log(wage) ~ experience + I( experience^2)+ education , data = df2)

# quantile regression: for data with nonconstant variance: estimates the quantile impact of the regressio model
# http://data.library.virginia.edu/getting-started-with-quantile-regression/

cps_rq <- rq(log(wage) ~ experience + I( experience^2) + education, data = df2, tau = seq(0.2, 0.8, by = 0.15))



# create the dataframe as sepearate piece:


cps2<- data.frame(education = mean(df2$education), experience = min(df2$experience): max(df2$experience))

cps2 <- cbind(cps2, predict(cps_lm, newdata = cps2, interval = 'prediction'))

cps2<- cbind(cps2, predict(cps_rq, newdata = cps2, type = ""))


plot (log(wage)~ experience, data = df2)


for (i in 6:10) lines(cps2[,i]~experience, data = cps2, col = "red")
plot(summary(cps_rq))

# some features of hte plots may be overshadowed, so we will use the kernel densities in order to show:

library(KernSmooth)

cps_bkde<- bkde2D(cbind(df2$experience, log(df2$wage)), bandwidth = c(3.5, 5.0), gridsize = c(200, 200))

image(cps_bkde$x1, cps_bkde$x2, cps_bkde$fhat, col = rev(gray.colors(10, gamma = 1)), xlab = "experience", ylab = "log(wage)" )
box() # draws a box around a plot

lines(fit ~ experience, data = cps2) # 
lines(lwr~ experience, data = cps2, lty = 2) # lty is line type
lines(upr~ experience, data = cps2, lty = 2)


####################
### Chapter 2 ######
####################


# we cane se vectors to create expressions:


# vector arithmetic:


x <- c(1.8, 3.14, 4, 88.169, 13)

2*x+3

5:1 * x +1:5
log(x)


# subsetting vectors:
x[c(1,4)]

x[-c(1,4)]


# patterned vectors:


ones <- rep(1, 10)
print(ones)

even <- seq(from =2, to = 20, by = 2)
print(even)

trend <- 1981:2005
print(trend)

 # can conatenate vectors:

c(ones, even)


#2.2 - matrix operations

a<- matrix(1:6, nrow = 2)

print(a)

t(a)
dim(a)
nrow(a)
ncol(a)

# subsetting matrix:

a1<- a[1:2, c(1,3)]
print(a1)


# compute the determinant:

det(a1) # -4 = nonsingular



solve(a1) 

# check that this is the inverse:
a1%*% solve(a1)
# can also get the kronecker()

# petterned matrices:

diag(4)


# combining matrices:

cbind(1, a1)

print(a1)

rbind(a1, diag(4,2))
diag(4,2)




# in R lists are vectors where each element can have any data type:


