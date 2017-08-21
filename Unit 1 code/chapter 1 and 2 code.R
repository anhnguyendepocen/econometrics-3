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
cps_rq <- rq(log(wage) ~ experience + I( experience^2) + education, data = df2, tau = seq(0.2, 0.8, by = 0.15))



# create the dataframe as sepearate piece:


cps2<- data.frame(education = mean(df2$education), experience = min(df2$experience): max(df2$experience))

cps2 <- cbind(cps2, predict(cps_lm, newdata = cps2, interval = 'prediction'))

cps2<- cbind(cps2, predict(cps_rq, newdata = cps2, type = ""))









