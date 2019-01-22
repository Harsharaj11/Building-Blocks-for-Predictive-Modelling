url <- "https://bgreenwell.github.io/uc-bana7052/data/alumni.csv"
alumni <- read.csv(url)
DT::datatable(alumni)  # requires DT package

attach(alumni)
plot(alumni_giving_rate~student_faculty_ratio)
mod <- lm(alumni_giving_rate~student_faculty_ratio)
summary(mod)
abline(mod)
par(mfrow = c(2,2))
plot(mod)

# Above code creates 4 plots

# 1. Residuals Vs fitted
# use this plot to check 2 assumptions
# a. Homoscsdasticity : Whether variance in residuals is 
# constant across fitted values
# b. Linearity : Whether mean of residuals is zero across 
# fitted values i.e. whether there is dependent variable has
# linear relationship with independent variables

# 2. QQ Plot
# Use this plot to check
# whether residuals are normally distributed
