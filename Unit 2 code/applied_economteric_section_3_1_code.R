#################################################
## Applied econometrics in R  - section 3.1 code
################################################

library(AER)

data("Journals")
df <- Journals
df <- df[,c("subs","price")]


df$citeprice <- Journals$price / Journals$citations
summary(df)

# when we have alot of skewnes, it makes sense to transform the variables : log in this case

# creates elasticities anyhow

plot(log(subs) ~ log(citeprice), data = df)
jour_lm <- lm(log(subs)~ log(citeprice), data = df)
abline(jour_lm)


# can inspect the regression model:
class(jour_lm)

# see the components of hte model:
names(jour_lm)
# p. 59 of the text shows a good description for each of these

# more detailed view: - this is a bit much potentially
str(jour_lm)

# look at hte regression results summary:
summary(jour_lm)
#remember that hte F-stat is the squared value of the t-stat in this instance

# look at the summary oject for the journal:
jour_slm <- summary(jour_lm)
class(jour_slm)
names(jour_slm)
# these are the outputs of the journal model

# look at the coeffs:

jour_slm$coefficients

# more overview of information:
str(jour_slm)

###############
# ANOVA table
###############

anova(jour_lm)

# notice that the mSE reduction from the logciteprice was 125.9 on a total of 225.99

# remember that hte ratio of MSEs test the hypotheses if H0: B2 = 0

# this can also be used to compare several nested models

###############
# Point and interval estimates
###############


coef(jour_lm)
# easy way to print the coeffs again

# confidence intervals:

confint(jour_lm)
# default 95%

###############
# prediction intervals
###############

# remember confidence versus prediction intervals 


predict(jour_lm, newdata = data.frame (citeprice = 2.11), interval = 'confidence')
predict(jour_lm, newdata = data.frame (citeprice = 2.11), interval = 'prediction')


# we can also visualize the prediction and confidence intervals:

lciteprice<- seq(from = -6, to = 4, by = 0.25)
jour_pred <- predict(jour_lm, interval = 'prediction', newdata = data.frame(citeprice = exp(lciteprice)))
jour_pred_2 <- predict(jour_lm, interval = 'confidence', newdata = data.frame(citeprice = exp(lciteprice)))

plot(log(subs) ~ log(citeprice), data = df)
lines(jour_pred[,1] ~ lciteprice, col = 1)
lines(jour_pred[,2] ~ lciteprice, col = 1, lty = 2)
lines(jour_pred[,3] ~ lciteprice, col = 1, lty = 2)

# plot confidence interval as well:
lines(jour_pred_2[,2] ~ lciteprice, col = 2, lty = 2)
lines(jour_pred_2[,3] ~ lciteprice, col = 2, lty = 2)

# can also plot the regression diagnostics:

par(mfrow = c(2,2))
plot(jour_lm)
par(mfrow = c (1,1))

# can also just plot one at a time:
plot(jour_lm, which = 2) # for QQPlot


###############
# Testing a linear hypothesis
###############

# this tests the more general hypothesis that we are interested in, rather than the hypothesis of each regressor

# requires a linear model to be fitted and a linear hypothesis:

linearHypothesis(jour_lm, "log(citeprice) = -0.5")
# the evidence suggests that the elasticity is not significantly different from 0.5


