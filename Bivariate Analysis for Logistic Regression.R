# defining function for hypothesis testing for continuos variables -------------
num_var_hypo_test <- function(varx,vary,xname){
  
  # treating response variable var y
  test_data <- data.frame(varxd = varx,varyd = as.numeric(as.numeric(vary) - 1))
  attach(test_data)
  
  # building single predictor logistic regression model
  model <- glm(varyd~varxd, family = binomial(link = "logit"))
  
  # predicting response variables's probability with model
  xv <- seq(min(varxd),max(varxd),length.out = 10000)
  yv <- predict(model,list(varxd = xv),type = "response")
  pred_data <- data.frame(xv,yv)
  
  # fetching coefficient and p-value for the predictor variable
  title <- paste("response Vs"
               ,xname
               ,"| beta ="
               ,round(if (nrow(coef(summary(model))) == 1) {0} else 
                 {coef(summary(model))[2,1]},digits = 2)
               ,"| p ="
               ,round(if (nrow(coef(summary(model))) == 1) {0} else 
                 {coef(summary(model))[2,4]},digits = 2)
  )
  
  # update file location for storing bivariate results
  fname <- paste("D:/Mays Competitions/Hypothesis Testing/",xname,".png",sep = "")
  
  # plotting actual and predicted response variable
  library(ggplot2)
  ggplot(test_data, aes(x = varxd, y = varyd)) + 
    geom_point() + 
    geom_line(data = pred_data, aes(x = xv, y = yv)) +
    ggtitle(title) +
    ylab("Response") +
    xlab(xname)
  
  # saving plot at specified location
  ggsave(filename = fname, dpi = 600, width = 8, height = 6, units = "in")
  
  # function returning coefficient & p-value for registering purpose
  p_value <- if (nrow(coef(summary(model))) == 1) {0} else 
    {coef(summary(model))[2,4]}
  beta <- if (nrow(coef(summary(model))) == 1) {0} else 
    {coef(summary(model))[2,1]}
  
  
  detach(test_data)
  return(c(beta,p_value))
}


# Testing hypothesis for continuos variables ------------------------------

# loading analytical dataset
load("D:/Mays Competitions/TAMU_FINAL_DATASET_2018/raw_clean_saved.Rda")

# selecting only continuos & response variables for hypothesis testing
raw_clean_num <- data.frame(raw_clean[unlist(lapply(raw_clean
                                                    , is.numeric))]
                            ,raw_clean$AMI_FLAG)

# testing hypothesis for continuos variable using for loop & defined function

for (i in 2:(ncol(raw_clean_num) - 1))
{
  # collecting coefficient & p-value for continuos variable
  out <-  num_var_hypo_test(raw_clean_num[,i]
                    ,raw_clean_num[,ncol(raw_clean_num)]
                    ,colnames(raw_clean_num)[i])
  row_content <- paste(colnames(raw_clean_num)[i],out[1],out[2],sep = ",")
  
  # writing coefficient & p-value for continuos variable in csv file
  
  # update location and name of the csv file
  cat(row_content
      ,file = "D:/Mays Competitions/Hypothesis Testing/hypothesis_testing.csv"
      ,sep = "\n"
      , append = TRUE)
}


# defining function for hypothesis testing for categorical variables --------

cat_var_hypo_test <- function(varx,vary,xname){

# treacting response variable var y  
test_data <- data.frame(varxd = varx,varyd = as.numeric(as.numeric(vary)-1))
attach(test_data)

# building a logistic model with single categorical variable
model <- glm(varyd~varxd, family = binomial(link = "logit"))

# calculating event rate across different categories of the variable
bar_data <- data.frame(barxd = unique(test_data$varxd)
                       ,baryd = as.vector(table(test_data)[,1]
                                          / table(test_data$varxd)))
# fetching p-value for the predictor
title <- paste("Response Vs"
             ,xname
             ,"| p ="
             ,round(min(coef(summary(model))[-1,4]),digits = 2)
)

# plotting bar plot for event rate across category
ggplot(data = bar_data, aes(x = barxd, y = baryd)) + 
  geom_bar(stat = "identity") +
  ggtitle(title) +
  xlab(xname) +
  ylab("Response")

# update location
fname <- paste("D:/Mays Competitions/Hypothesis Testing/",xname,".png",sep = "")

# saving plot in the file
ggsave(filename = fname, dpi = 600, width = 8, height = 6, units = "in")

# returning p-value for registration
p_value <- min(coef(summary(model))[-1,4])

detach(test_data)
return(p_value)
}



# Testing hypothesis for categorical variables ----------------------------

# selecting only categorical variables
raw_clean_cat <- data.frame(raw_clean[unlist(lapply(raw_clean, is.factor))]
                            ,raw_clean$AMI_FLAG)

# testing hypothesis for categorical variable using for loop & defined function
for (i in 1:(ncol(raw_clean_cat) - 1))
{
  # collecting p-value for the categorical variable
  out <-  cat_var_hypo_test(raw_clean_cat[,i]
                            ,raw_clean_cat[,ncol(raw_clean_cat)]
                            ,colnames(raw_clean_cat)[i])
  row_content <- paste(colnames(raw_clean_cat)[i]," ",out,sep = ",")
  
  # registering p-values for variables in csv file
  cat(row_content
      ,file = "D:/Mays Competitions/Hypothesis Testing/hypothesis_testing.csv"
      ,sep = "\n"
      ,append = TRUE)
}

