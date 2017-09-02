#################################################
## Applied econometrics in R  - section 2.8 code
################################################

library(AER)


data("CPS1985")

df <- CPS1985

# can provide a summary with : 

str(df)
summary(df)
sapply(df,class)

head(df)
tail(df)


# can change the levels of a factor variable:

levels(df$occupation)

levels(df$occupation)[c(2,6)] <- c("techn","mgmt")


# we can attach, but I wont, because it is not a good idea:

summary(df$wage) # 5 number summary


mean(df$wage) # can also get each element separately

median(df$wage)

# also:

fivenum(df$wage)
# not as good:


# graphical summaries:

par(mfrow = c(1,2))

hist(df$wage, freq = FALSE ) # freq shows the densities)
hist(df$wage, freq = TRUE)

par(mfrow = c(1,1))

hist(log(df$wage), freq = FALSE)
lines ( density(log(df$wage)), col = 4) # the density function will plot the line of the density distribution


#############
## One categorical variable
#############

summary(df$occupation)
# provides counts of each level
# alternative:

table(df$occupation)

# for relative frequencies:
tab <- table(df$occupation)
prop.table( tab)

# best to visualize with barplots:

barplot(tab)
pie(tab)

#############
## Two categorical variables
#############

# create cross tabs with the stabs function

xtabs(~gender + occupation, data = df)

# mosaic plots can provide nice breakdowns of the variables:

plot(gender ~ occupation, data = df)
# width tells relative size of the group, and then breakdown shows the proportion

#############
## Two numeric variables
#############

# can use correltaion, remember that pearson correlation corefficient not necessarily good for positively or heavily skewed variables

cor(log(df$wage), df$education)
cor(log(df$wage), df$education, method = "spearman")
# basically the same and no 

# loot at a plot:

plot(log(df$wage) ~ df$education)

#############
## One numeric and one categorical variable
#############


# can look at by group means

tapply(log(df$wage), df$gender, mean)
# can replace with different calculations
tapply(log(df$wage), df$gender, summary)


# will auto do a box and whicker plot when you have this blend of variables:

plot(log(df$wage) ~ df$gender)


# good to look at a qqplot as well:

mwage <- subset(df, gender  == 'male')$wage
fwage <- subset(df, gender  == 'female')$wage
qqplot(mwage, fwage, xlim = range(df$wage), ylim = range(df$wage), xaxs = 'i', yaxs = 'i', 
       xlab = 'male',ylab = 'female')

abline(0, 1)
# what this shows is that the male wages at most quantiles is higher than the female wage at these quantiles
