###############
### Econometrics unit 3.2 - 3.7
###############

###############
### 3.2 - multiple regression
###############


library(AER)

data("CPS1988")

summary(CPS1988)

# experience computed as : age - education - 6

cps_lm <- lm(log(wage) ~ experience + I(experience^2) +education + ethnicity, data = CPS1988)

# must specify the I to mean that the variable is considered in isolation and not as interaction in the equation

summary(cps_lm)


# coefficients all have expected sign and corresponding variables are significant

# return on education is 8.57% each year

### Dummy variables and contrast coding

# generally want to create dummies for all but the reference variable in 

# the function I

# had to use because the functions: * , / , and ^ all work for interaction effects in the model

# must isolate them within a function in order to work correctly

# for non transforms, the I() function is what works



#### Using ANOVA to compare models

# can leave out certain variables to compare the models screated with the anova function

# fit one model without ethnicity:

cps_no_eth <- lm(log(wage) ~ experience + I(experience^2) +education, data = CPS1988)

anova(cps_no_eth, cps_lm)
# we can see that the variable ethnicity is in fact significant

# anova reads models in the order that they are entered

# if only a single model is passed, it sequentially adds terms in the order specified by the formula

anova(cps_lm)
# the second to last line in this model is equivalent to the anova of the comparison between eth and no eth


# can do the same thing with waldtest:

waldtest(cps_lm, . ~ . -ethnicity)

# just doesnt report the residual sum of squares


###############
### 3.3 - partial linear models
###############


# quadratic terms are common, but may be better to model in more flexible way

# these new models take the form parametric externsion :
# log(wage) = b1 + g(experience) + b2 education + b3thnicity + e

# g is known as a spline - automatically models non-linearities and interaction between variables
# https://en.wikipedia.org/wiki/Multivariate_adaptive_regression_splines

cps_plm <- lm(log(wage) ~ bs(experience, df = 5) + education + ethnicity, data = CPS1988)
# the coefficients on the spline are not easily interpretted - look back at BC function

#  2 approaches: specify the piececewise polynomial (default 3)and the knots by hand
#        Or supply df parameter

# chose 5 based on below code test:

cps_bs <- lapply(3:10, function(i) lm(log(wage) ~ bs(experience, df = i)+ education + ethnicity, data = CPS1988))
# create a list of bs fitted models

structure (sapply(cps_bs, AIC, k = log(nrow(CPS1988))), .Names = 3:10) # pick the lowest value here = 5

# now plot the models with quadratic term and the partially linear model:

cps <- data.frame(experience = -2:60, education = with(CPS1988, mean(education[ethnicity=='cauc'])), 
                  ethnicity = 'cauc')

cps$yhat1 <-predict(cps_lm, newdata = cps)

cps$yhat2 <- predict(cps_plm, newdata = cps)

plot(log(wage)~ jitter(experience, factor = 3), pch = 19, col = rgb(0.5, 0.5, 0.5, alpha = 0.02), data = CPS1988)
lines(yhat1 ~ experience, data = cps, lty = 2)
lines(yhat2 ~ experience, data = cps)
legend('topleft',c('quadratic','spline'), lty = c(2, 1), bty = 'n')
# the spline approach appears to better capture the feature below the 7 year mark in the data, this is a very important
# observation in the ability of hte model to fit the data correctly

# alerternate approach to spline model is the kernel model in the NP package

# In order to prevent overfitting: we added Jitter, and set the color to semitransparent gray in order to better see 
# the density of the distribution- get this from adding RGB all the same in the function for RGB
# alpha blending is not avaialble in all plotting devices for windows - windows(), apple - quartz()

###############
### 3.4 - factors, interactions, and weights
###############

# many times ecnomic studies include adding in a dummy variable of some sort

# interested in an impirical interaction between ethnicity and other variables

# typical model: y - mx + b - no interaction
# interaction model : y = a*x ; y = a + x +a:x - a:x gives the reference in slopes compared to the refrence category
# model with interaction and nested coding: y ~a/x; y ~a + x%in%a - explicit slope coeff estimated for each category in A
# model all 2-way interactions: y~(a + b + c) ^2; y~a*b*c - a:b:c

### Interactions

# right now, the model does not specifically allow ethnicity to impact anything but the intercept drawn; want it to impact slope of variable

cps_int<- lm(log(wage) ~ experience + I(experience^2) + education * ethnicity, data = CPS1988)
coeftest(cps_int) # only used for compactness summary() works too

# see that it is statistically significant, but with only 30k records, hard to hang hat on


# we see in the above output that twe get an intercept, experience coef, i experience the different in intercepts,
# and the difference in slopes

# the call below eliminates the inclusion of ethnicity and education
cps_int<- lm(log(wage) ~ experience + I(experience^2) + education : ethnicity, data = CPS1988)

coeftest(cps_int)

### separate regressions for each level

# could calculate 2 different regressions for caucasions and af americans

# could compute 2 different LM statements or using the argument to nest within each level:

cps_sep <- lm(log(wage) ~ ethnicity / (experience + I(experience^2) + education) -1, data = CPS1988)
cps_sep_cf<- matrix(coef(cps_sep), nrow = 2)
rownames(cps_sep_cf) <- levels(CPS1988$ethnicity)
colnames(cps_sep_cf) <- names(coef(cps_lm))[1:4]
cps_sep_cf

# we see that the impact of education is similar for both of the groups, but other coeffs smaller for afam

# compare the new model versus the old model:
anova(cps_sep, cps_lm)
# we can see that the interaction model fits much better than the model with no interaction term involved.

### Change of the reference category

# by default, R uses the first value in the an unordered factor as the reference category

# can simply be done with a resorting of the data, or, can be done via constrasts()

CPS1988$region <- relevel(CPS1988$region, ref = "south")
cps_region <- lm(log(wage) ~ ethnicity + education + experience + I(experience^2) + region, data = CPS1988)
coef(cps_region)


### weighted least squares

# cross section regressions are often plagued by heteroskedasticity
# weighted least squares can be one solution : can see in the top left graph of plot function in R
# g is a skedastic function = g =(zTlambda)

# r-code:

data("Journals")



Journals$citeprice <- Journals$price / Journals$citations
jour_lm <- lm(log(subs)~ log(citeprice), data = Journals) # original regression

jour_wls1 <- lm(log(subs)~ log(citeprice), data = Journals, weights = 1/citeprice^2)

jour_wls2 <- lm(log(subs)~ log(citeprice), data = Journals, weights = 1/citeprice) # not the squared version

plot(log(subs) ~ log(citeprice), data = Journals)
abline(jour_lm)
abline(jour_wls1, lwd = 2, lty = 2)
abline(jour_wls2, lwd = 2, lty = 3)
legend("bottomleft", c ("OLS","WLS1","WLS2"), lty = 1:3, lwd = 2, bty = "n")

# many times, we do not know which type of model to use, this leads to feasible least squares:


auxreg<- lm(log(residuals(jour_lm)^2) ~log(citeprice), data = Journals)
jour_fgls1 <- lm(log(subs) ~ log(citeprice), weights = 1 / exp(fitted(auxreg)), data = Journals)
# possible to iterate further to yield a second variant of FGLS:
 

gamma2i <- coef(auxreg)[2]
gamma2 <- 0

while (abs((gamma2i - gamma2)/gamma2) > 1e-7) {
  
  gamma2 <- gamma2i
  fglsi <- lm(log(subs) ~log(citeprice), data = Journals, weights = 1/citeprice^gamma2)
  gamma2i <- coef(lm(log(residuals(fglsi)^2) ~ log(citeprice), data = Journals))[2]
}

jour_fgls2<- lm(log(subs) ~ log(citeprice), data = Journals, weights = 1/citeprice^gamma2)

# loop specifies that as long as the relative change in the slope of the coeff exceed a value of 10-7 then continue iterating
coef(jour_fgls2)


###############
### 3.5 - linear regression with time series data
###############

data( "USMacroG")
plot(USMacroG[,c("dpi","consumption")], lty = c (3, 1), plot.type = "single", ylab = " ")


# models fitted with lags using the dynlm() function as follows:

install.packages("dynlm")
library(dynlm)
cons_lm1<- dynlm(consumption ~ dpi + L(dpi), data = USMacroG)
cons_lm2<- dynlm(consumption ~ dpi + L(consumption), data = USMacroG)
summary(cons_lm1)
# can see coeffs and formula
summary(cons_lm2)
summary()

# there were additional notes:
# skipped to the next section : encompassing test p.83

###############
### 3.6 - linear regression with panel data
###############

# the plm package covers panel data:

install.packages("plm")
library(plm)

### static linear models

data("Grunfeld", package = "AER")

gr <- subset(Grunfeld, firm %in% c ("General Electric", "General Motos", "IBM"))

pgr <- plm.data(gr, index = c("firm","year"))


gr_pool<- plm(invest ~ value + capital, data = pgr, model = "pooling")

# fixed effects model obtained via a within-transformed model

gr_fe <- plm(invest ~ value + capital, data = pgr, model = "within")
summary(gr_fe)
# balanced panel give observations per firm, listed here so we can see that the dataset is balanced

# can also try a 2 way mode

gr_pool <- plm(invest ~ value + capital, data = pgr, model = "within", effect = "twoways")
summary(gr_pool)

# need to check if fixed effects are needed:

pFtest(gr_fe, gr_pool)
