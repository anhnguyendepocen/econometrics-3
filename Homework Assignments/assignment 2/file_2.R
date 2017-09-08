#************
# MSDS Week 2 Class Practical Learning
#************

#as seen in Chapter 3 of textbook
#Code sourced from textbook - 
#Kleiber, Christian; Zeileis, Achim. Applied Econometrics with R. Springer New York. Kindle Edition. 
#*
#*****************
# Simple Linear Regression
#*****************
install.packages("AER")
library("AER")


rm(list = ls())
gc()
par(mfrow=c(1,1))

data("Journals")
journals <- Journals[, c("subs", "price")]
journals$citeprice <- Journals$price/Journals$citations
summary(journals)


plot(log(subs) ~ log(citeprice), data = journals)
jour_lm <- lm(log(subs) ~ log(citeprice), data = journals)
abline(jour_lm)

class(jour_lm)

names(jour_lm)

summary (jour_lm)

jour_slm <- summary(jour_lm)
class(jour_slm)
names(jour_slm)

jour_slm$coefficients


#*****************
# Analysis of Variance
#*****************

anova(jour_lm)
coef(jour_lm)
confint(jour_lm)
confint(jour_lm, level = 0.99)

#*****************
# Prediction
#*****************

predict(jour_lm, newdata = data.frame(citeprice = 2.11), interval = "confidence")

predict(jour_lm, newdata = data.frame(citeprice = 2.11), interval = "prediction")

lciteprice <- seq(from = -6, to = 4, by = 0.25) 
jour_pred <- predict(jour_lm, interval = "prediction", newdata = data.frame(citeprice = exp(lciteprice))) 
plot(log(subs) ~ log(citeprice), data = journals) 
lines(jour_pred[, 1] ~ lciteprice, col = 1) 
lines(jour_pred[, 2] ~ lciteprice, col = 1, lty = 2) 
lines(jour_pred[, 3] ~ lciteprice, col = 1, lty = 2)

par(mfrow = c(2, 2)) 
plot(jour_lm) 
par(mfrow = c(1, 1))

#*****************
# Testing Linear Hypothesis
#*****************

linearHypothesis(jour_lm, "log(citeprice) = -0.5")


#***************************
#  End of Week 2
#***************************
