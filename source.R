# Set up the current working directory. Vary based on your computer.
setwd("Desktop/R Workshop/panda/intro-to-pandas-and-matplotlib-master")


# Read table containing information about tips
data = read.csv("tips.csv")
# Single Boxplot
boxplot(data$totbill, main="How Much People Spent For a Meal", ylab = "Amount of money spent in US dollars")
# Exercise: Boxplot for the tips people given for a meal
boxplot(data$tip, main="How Much People Spent As Tips", ylab="Amount of tips given in US dollars")

# Multiple Boxplot (class-conditional)
boxplot(totbill~day, data = data, main = "How Much People Spent For a Meal - Across Days", xlab = "Days", ylab = "Amount of money spent in US dollars")
# Exercise: Class-conditional Boxplot based on day or night
boxplot(totbill~time, data = data, main = "How much People Sprnt For a Meal - Across Time", xlab = "time", ylab = "Amount of Money Spent")

# Single data manipulation
# Exercise: Class-conditional Boxplot based on table size
boxplot(totbill~size, data = data, main = "How Much People Spent For a Meal - Table Size", xlab = "Number of people for one table", ylab = "Amount of money spent in US dollars")
data$billPerPerson = data$totbill / data$size
boxplot(billPerPerson~size, data = data, main = "How Much People Spent For a Meal - Table Size", xlab = "Number of people for one table", ylab = "Amount of money spent per person")


# Single Histogram
hist(data$totbill)

# Setting Up Breakpoints
breakpoints <- seq(0, 55, 2)
hist(data$totbill, breaks = breakpoints, main = "How Much People Spent For a Meal", xlab = "Amount of Money Spent in US Dollars", ylab = "Number of Transactions")
# Setting Up the limit on x/y-axis
hist(data$totbill, breaks = breakpoints, main = "How Much People Spent For a Meal", xlab = "Amount of Money Spent in US Dollars", ylab = "Number of Transactions", xlim = c(0, 20))
hist(data$totbill, breaks = breakpoints, main = "How Much People Spent For a Meal", xlab = "Amount of Money Spent in US Dollars", ylab = "Number of Transactions", ylim = c(0, 40))
# Exercise: Histogram for tips people given for a meal

# Conditional Hisrogram
library(lattice)
histogram(~totbill | day, data, main = "How Much People Spent For a Meal - Across Days", xlab = "Amount of Money Spent", ylab = "Percent of Total")
# Exercise: Class-conditional Histogram based on day or night
histogram(~data$totbill | data$time, main = "How Much People Spent For a Meal - Across Time", xlab = "Amount of Money Spent", ylab = "Percent of Total")



data = read.csv("Stat100_2013fall_survey01.csv")
# Scatter plot
plot(data$height, data$weight, xlab = "Height", ylab = "Weight", main = "Height-Weight Relationship for Students in Stat 100")
# Regression Line
# Notice that plot(x, y) and abline(lm(y~x))
abline(lm(data$weight~data$height))
lm(data$weight~data$height)

# Label for Scatter Plots
data$genderChar = substr(data$gender, 1, 1)
data$genderChar = toupper(data$genderChar)
plot(data$height, data$weight, xlab = "Height", ylab = "Weight", main = "Height-Weight Relationship for Students in Stat 100 - Gender", type="n")
text(data$height, data$weight, labels=as.character(data$genderChar))

# Bonus: Color Different Categories in Scatter Plot
data$genderChar = replace(data$genderChar, data$genderChar == 'M', 1)
data$genderChar = replace(data$genderChar, data$genderChar == 'F', 2)
class(data$genderChar) <- "numeric"
plot(data$height, data$weight, xlab = "Height", ylab = "Weight", main = "Height-Weight Relationship for Students in Stat 100", col = c("blue", "red")[data$genderChar])
legend("topright", legend = c("M", "F"), col=c("blue", "red"), pch=1)

plot(data$ageMother, data$ageFather, main = "Age for Mother and Father", xlab="Age of Mother", ylab = "Age of Father", col = c("blue", "red")[data$genderChar])
legend("bottomright", legend = c("M", "F"), col=c("blue", "red"), pch=1)
