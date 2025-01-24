# In this R script, I will create the panelled data, merge it with the church appointment 
# data and run the analysis. 
# The first part is original work, the analysis part borrows from the code we used
# in class, and has been commented to mention this.

#### Packages ####
library(haven)
library(dplyr)
library(data.table)
library(tidyverse)
library(plm)
library(foreign)
library(readr)
library(randomForest)
library(stringr)
library(foreign)
library(vtable)
library(lfe)
library(huxtable)
library(glmnet)
library(jtools)

#### Creating the panels in the population data ####
population_data <- read_dta("/Users/BRAJENDRA2/Courses/Data_Analytics/Submission/MB_modified/Data/Population_year.dta")

pop_data <- population_data %>% mutate(year_panel = case_when(
  (year >= 1525) & (year < 1550) ~ 1525,
  (year >= 1550) & (year < 1575) ~ 1550,
  (year >= 1575) & (year < 1600) ~ 1575,
  (year >= 1600) & (year < 1625) ~ 1600,
  (year >= 1625) & (year < 1650) ~ 1625,
  (year >= 1650) & (year < 1675) ~ 1650,
  (year >= 1675) & (year < 1700) ~ 1675,
  (year >= 1700) & (year < 1725) ~ 1700,
  (year >= 1725) & (year < 1750) ~ 1725,
  (year >= 1750) & (year < 1775) ~ 1750,
  (year >= 1775) & (year < 1800) ~ 1775,
  (year >= 1800) & (year < 1825) ~ 1800,
  (year >= 1825) & (year < 1850) ~ 1825,
  (year >= 1850) & (year < 1875) ~ 1850,
  TRUE ~ 0)
) %>% select(-lat_MB, -lon_MB) %>% filter(year_panel != 0) %>% select(cced_id, C_ID, Population, year, year_panel, latitude, longitude)

# Modifying church_data before merging
church_data <- read.csv("/Users/BRAJENDRA2/Courses/Data_Analytics/Submission/MB_modified/Data/final2.csv")

# Which type of appointments matter?
church_data <- church_data %>% mutate(Type =  sub("Appt.*", "App", Type)) #makes all Appointments the same
church_data <- church_data %>% mutate(Type =  sub("Vac.*", "Vac", Type)) #makes all Vacancies the same
church_data <- church_data %>% mutate(Type =  sub("Disp.*", "Dis", Type)) #makes all Dispensations the same
church_data <- church_data %>% mutate(Type =  sub("Subsc.*", "Sub", Type)) #makes all Subscriptions the same
church_data <- church_data %>% mutate(Type =  sub("Ord.*", "Ord", Type)) #makes all Ordinations the same
church_data <- church_data %>% rename("year" = "Year")
unique(church_data$Type)  # checking that the mutations worked and all appointment types are classified
church_data %>% group_by(Type) %>% summarise(n = n()) # to check the distribution of types

# Choosing which appointment types to keep - Based on the details in 
# https://theclergydatabase.org.uk/how-to-use-the-database/record-window/
# I decided to keep Appointments, Subscriptions, Ordinations and Dispensations
# Given that Libc or Liber cleri and vacancies formed around 30% of all observations,
# this filtering led to a significant drop in number of observations
church_data1 <- church_data %>% filter(Type == "App" | Type == "Dis" | Type == "Sub"| Type == "Ord")

# Create panels in church data:
church_data1 <- church_data1 %>% mutate(year_panel = case_when(
  (year >= 1525) & (year < 1550) ~ 1525,
  (year >= 1550) & (year < 1575) ~ 1550,
  (year >= 1575) & (year < 1600) ~ 1575,
  (year >= 1600) & (year < 1625) ~ 1600,
  (year >= 1625) & (year < 1650) ~ 1625,
  (year >= 1650) & (year < 1675) ~ 1650,
  (year >= 1675) & (year < 1700) ~ 1675,
  (year >= 1700) & (year < 1725) ~ 1700,
  (year >= 1725) & (year < 1750) ~ 1725,
  (year >= 1750) & (year < 1775) ~ 1750,
  (year >= 1775) & (year < 1800) ~ 1775,
  (year >= 1800) & (year < 1825) ~ 1800,
  (year >= 1825) & (year < 1850) ~ 1825,
  (year >= 1850) & (year < 1875) ~ 1850,
  TRUE ~ 0)
) %>% filter(year_panel != 0) %>% filter(cced_id != 1) # realised later that id 1
# is for church appointments with "Other" locations and not linked to a specific C_ID,
# another step which reduces observations significantly (more than 50% are dropped)
# church_data %>% group_by(cced_id) %>% summarise(n = n()) shows that cced_id alone 
# had 203418 observations.


#### Balancing and merging data ####

# Create a balanced dataset - balanced means for each unit of time, 
# (here year_panel) we have the same number of units of observation
# (here town C_ID)

# Merging the data into a dataframe called outcome. I use full_join because I 
# wanted to keep all observations in both datasets, which should lead to better 
# balancing. I also created year_panel in church_data1 for this reason.
outcome <- full_join(church_data1, pop_data, by = c("cced_id", "year_panel"))

# Number of time periods = number of year_panels = 12
# Number of units of observation = number of unique C_IDs
unique(outcome$C_ID)
# which is 1012

# I used the make.pbalanced function from the plm package to balance the dataset
# By default, this takes the union of available time periods over all individuals 
# (w/o NA values). Missing time periods for an individual are identified and 
# corresponding rows are inserted and filled with NA for the non--index variables 
# This means, only time periods present for at least one individual are inserted, if missing.
balanced <- make.pbalanced(outcome, index = c('C_ID', 'year_panel'))

# Counting the number of appointments in each C_ID and panel with a function

app_counter <- function(df) {
  v = unique(na.omit(df$C_ID))
  c <- data.frame()
  for (i in v) {
    r <- df %>% filter(C_ID == i) %>% mutate(app_count = sum(!is.na(Type)))
    c <- bind_rows(c,r)
  }
  c
}

data1 <- app_counter(balanced)

data1 <- data1 %>% select(year_panel,C_ID,latitude,longitude,app_count,
                          Population,PersonID)
data1 %>% group_by(PersonID) %>% summarise(n = n()) # to check that PersonIDs are unique

# Removing double counting of PersonIDs in data
data2 <- unique(data1)

# Plot relationship between Population and Appointments
data2 %>% ggplot(aes(x = year_panel, y = Population)) + geom_point() + theme_minimal() 
# shows the relationship well
data2 %>% ggplot(aes(x = year_panel, y = app_count)) + geom_point()

# Creating summary statistics for data2 

summarystat_data2 <- describe(data2, fast = T)
summarystat_data2
write_excel_csv(summarystat_data2, "summarystat_data2.csv")

#### Report point estimates: #####

# First clearing out the unnecessary objects in the environment
rm(list=setdiff(ls(), "data2"))

# The code after this is based heavily on what we did in class. Any modifications
# are commented.
# saves only the rows that are complete (without NAs or 0s) to the new dataframe, total
total <-  data2[complete.cases(data2),] 
# Around 20k observations are lost, this line

is.pbalanced(total) # This is false, implying that the dataset we work with is not
# actually balanced.

## Create test and training datasets 

smp_size <- floor(0.75*nrow(total)) # assign the largest integer not greater than 75%
# of the total dataset. For example, 75% of the number of rows of total is 67142.25 for me, 
# and floor makes that 67142.

# Then we create a vector which contains a random sample of index numbers from total 
# and then create the test and train data by subsetting. We do not use simple sampling
# because the test and train datasets cannot overlap i.e. contain the same observations
train_ind <-sample(seq_len(nrow(total)),size=smp_size) 
train <- total[train_ind,]
test <- total[-train_ind,]

# Now we use the training data to get OLS estimates of the population data,
# and we will compare this to OLS estimates from the population data (total).
# We will also compare it to a fixed effects (FE) model

## Population = Appointments + C_ID fixed effects 

# Getting OLS estimates from the regression of Population on Appointments  
# with the controls latitude, longitude, time (year_panel), town (C_ID), using 
# the training data and saving it to OLS_1, We use the lm function.
OLS_1 <- lm(formula = Population ~ year_panel + C_ID + 
              latitude + longitude + app_count, 
            data = train)
summary(OLS_1) # summarise the main results in a table. 
# Contrary to what I expected, I get a negative coefficient on app_count, implying
# that appointments reduce populations. This result (and all other coefficients)
# is significant


# Now, we get the OLS estimates from the regression of Population on Appointments  
# with the controls latitude, longitude, time (year_panel), town (C_ID), using 
# the total data and save it to OLS-2. This is to compare the coefficients across
# these two datasets.

OLS_2 <- lm(formula = Population ~ year_panel + C_ID + 
              latitude + longitude + app_count, 
            data = total)
summary(OLS_2)
# As beforeI get a negative coefficient on app_count, implying
# that appointments reduce populations. This result (and all other coefficients)
# is significant The magnitude of the coefficient is also similar (-0.27) to that from 
# the training data.


# Now we do a fixed effects regression, using town(C_ID) and time(year_panel) 
# fixed effects to estimate the causal impact of appointments on population
FE <- felm(Population ~ app_count | C_ID + year_panel, data = train)


# huxreg is a function which is used to build a table of regressions from a list
# of models, to compactly and beautifully display the results of multiple 
# regressions together.
huxresult <- huxreg("Train data" = OLS_1, "Total data" = OLS_2 , "FE" = FE,
       coefs = "app_count", statistics = c("N"="nobs", R2 = "r.squared"))
huxresult

# The coefficient on the fixed effects model is very large and negative, although
# it is insignificant. This points to a systematic problem with the data I have,
# and it would be interesting to see the results with data from others.

#### test OLS_1 
# Now we use the training data's estimates to predict the population (outcome or 
# Y variable) for the test data. We save this as a new column in the test dataset
test$Pop_ols <- predict(OLS_1, newdata = test)

# evaluate:
# calculate the standard deviation in the population from the test dataset
sd_real <- sd(test$Population)  
# Calculate the mean squared errors for population as the square of the difference
# between predicted and actual values of population divided by the standard 
# deviation of the values of population in the test dataset.
test$msqe_ols <- (test$Pop_ols-test$Population)^2 / sd_real
test$msqe_ols_sqrt <- sqrt((test$Pop_ols-test$Population)^2)

# We also create a summary table which shows the results of how many observations
# of population in the test data are different from the predicted values by a 
# factor of 100. 250 and 500. The first three lines create logical vectors
# which test these conditions and the sumtable function creates a summary of the
# results.
test$Pred_100_ols = ifelse(abs(test$Pop_ols-test$Population)<100,1,0)
test$Pred_250_ols = ifelse(abs(test$Pop_ols-test$Population)<250,1,0)
test$Pred_500_ols = ifelse(abs(test$Pop_ols-test$Population)<500,1,0)

sumtable(test, vars=c('Population','Pop_ols','msqe_ols','msqe_ols_sqrt',
                      'Pred_100_ols','Pred_250_ols','Pred_500_ols'), add.median=TRUE)

# this table shows that for most of the observations, the difference between
# actual and predicted values is larger than a factor of 500. Therefore the OLS 
# regressions are rather weak at predicting the correct values.

# We now plot the predicted and actual values from the test data using ggplot

# First, we create a ggplot object, ggp
ggp <- ggplot(data = test, aes(x = year_panel))

# Then we add layers for the two data (predicted and actual values) we want to plot
# With the poly function, we plot a regression curve that is polynomial of degree 4 
# 
ggp + stat_smooth(method = "lm",
              aes(y = Population),
              formula = y ~ poly(x, degree = 4),
              se = FALSE) +
  stat_smooth(method = "lm",
              aes(y = Pop_ols),
              formula = y ~ poly(x, degree = 4),
              se = FALSE,
              color = "black", linetype = "twodash")

##########################

#Log(Pop) = log(App) + C_ID fixed effects

# We do a similar analysis as before, the only difference being that we use 
# log variables. The code is the one from above but modified, so the one 
# discussed in class but was not part of the class script. 

# Getting OLS estimates from the regression of log(Population) on log(Appointments)  
# with the controls latitude, longitude, time (year_panel), town (C_ID), using 
# the training data and saving it to OLS_1, Note that I have added a constant (1) 
# to the variables that I am log transforming, because otherwise the observations of 
# zero cause problems (Error in lm.fit(x, y, offset = offset, singular.ok = 
# singular.ok, ...) : NA/NaN/Inf in 'y')
OLS_3 <- lm(formula = log(Population + 1) ~ year_panel + C_ID + 
              latitude + longitude + log(app_count + 1), 
            data = train)
summary(OLS_3) # summarise the main results in a table. 
# There is now a positive but small coefficient on app_count, implying
# that appointments increase populations. This result (and all other coefficients)
# is insignificant


# Now, we get the OLS estimates from the regression of log(Population) on
# log(Appointments) with the controls latitude, longitude, time (year_panel),
# town (C_ID), using the total data and save it to OLS-2. This is to compare the
# coefficients across these two datasets.

OLS_4 <- lm(formula = log(Population + 1) ~ year_panel + C_ID + 
              latitude + longitude + log(app_count + 1), 
            data = total)
summary(OLS_4)
# As before I get a positive coefficient on app_count, implying that
# appointments increase populations. This result (and all other coefficients) is
# insignificant The magnitude of the coefficient is also similar (0.01037) to that
# from the training data.


# Now we do a fixed effects regression, using town(C_ID) and time(year_panel) 
# fixed effects to estimate the causal impact of appointments on population
FE_log <- felm(log(Population + 1) ~ log(app_count + 1) | C_ID + year_panel, data = train)


# huxreg is a function which is used to build a table of regressions from a list
# of models, to compactly and beautifully display the results of multiple 
# regressions together.
huxresult2 <- huxreg("Train data" = OLS_3, "Total data" = OLS_4 , "FE" = FE_log,
                    coefs = "log(app_count + 1)", statistics = c("N"="nobs", R2 = "r.squared"))
huxresult2

#### test OLS_1 
# Now we use the training data's estimates to predict the population (outcome or 
# Y variable) for the test data. We save this as a new column in the test dataset
test$Pop_ols_log <- predict(OLS_3, newdata = test)
test$Pop_ols_log_pred <- exp(test$Pop_ols_log)

# evaluate:
# calculate the standard deviation in the population from the test dataset
sd_real <- sd(test$Population)  
# Calculate the mean squared errors for population as the square of the difference
# between predicted and actual values of population divided by the standard 
# deviation of the values of population in the test dataset.
test$msqe_ols_log <- (test$Pop_ols_log_pred-test$Population)^2 / sd_real
test$msqe_ols_log_sqrt <- sqrt((test$Pop_ols_log_pred-test$Population)^2)

# We also create a summary table which shows the results of how many observations
# of population in the test data are different from the predicted values by a 
# factor of 100, 250 and 500. The first three lines create logical vectors
# which test these conditions and the sumtable function creates a summary of the
# results.
test$Pred_100_ols_log = ifelse(abs(test$Pop_ols_log_pred-test$Population)<100,1,0)
test$Pred_250_ols_log = ifelse(abs(test$Pop_ols_log_pred-test-test$Population)<250,1,0)
test$Pred_500_ols_log = ifelse(abs(test$Pop_ols_log_pred-test-test$Population)<500,1,0)

sumtable(test, vars=c('Population','Pop_ols_log_pred','msqe_ols_log','msqe_ols_log_sqrt',
                      'Pred_100_ols_log','Pred_250_ols_log','Pred_500_ols_log'), add.median=TRUE)

# We now plot the predicted and actual values from the test data using ggplot

# First, we create a ggplot object, ggp
ggp <- ggplot(data = test, aes(x = year_panel))

# Then we add layers for the two data (predicted and actual values) we want to
# plot With the poly function, we plot a regression curve that is polynomial of
# degree 4
ggp + stat_smooth(method = "lm",
                  aes(y = Population),
                  formula = y ~ poly(x, degree = 4),
                  se = FALSE) +
  stat_smooth(method = "lm",
              aes(y = Pop_ols_log_pred),
              formula = y ~ poly(x, degree = 4),
              se = FALSE,
              color = "black", linetype = "twodash")

# Although the graph shows a significant difference between real and predicted
# values the shape of the curves is more similar

#####Predict population in a test dataset and compare OLS to a random forest ###
#prediction algorithm

# Create a dataset containing just population
outcome_data <- train[,c("Population")] 

# Create the training dataset by removing Population from it ( and "lnPop","lnApp" in the original version, but I did not define those variables)
train_data <- train[,!names(train) %in%
                      c("Population", "PersonID")]

rf_fit <- randomForest(outcome_data ~ . , # We regress outcome_data on all the variables in the dataset defined by the 'data' parameter (here train_data)
                       data = train_data,
                       ntree = 1000, # number of trees
                       na.action = na.omit)

# Some code that I think should be equivalent to what we do above
rf_fit2 <- randomForest(Population ~ year_panel + C_ID + latitude + longitude + app_count,
                        data = train,
                        ntree = 1000,
                        na.action = na.omit)

# Calculating variable importance for random forest objects produced by train
varImpPlot(rf_fit)
# shows that app_count is the most important predictor, followed by latitude, year_panel,
# longitude and then C_ID

# Adding a new variable to test which is based on predictions by the random forest model
test$Pop_rf <- predict(rf_fit, newdata = test)

# Evaluating the predictions of this model
# calculate the standard deviation in the population from the test dataset
sd_real <- sd(test$Population)
# Calculate the mean squared errors for population as the square of the difference
# between predicted and actual values of population divided by the standard 
# deviation of the values of population in the test dataset.
test$msqe_rf <- (test$Pop_rf-test$Population)^2 / sd_real
test$msqe_rf_sqrt <- sqrt((test$Pop_rf-test$Population)^2)

# We also create a summary table which shows the results of how many observations
# of population in the test data are different from the predicted values by a 
# factor of 100, 250 and 500. The first three lines create logical vectors
# which test these conditions and the sumtable function creates a summary of the
# results.
test $Pred_100_rf = ifelse(abs(test$Pop_rf-test$Population)<100,1,0)
test $Pred_250_rf = ifelse(abs(test$Pop_rf-test$Population)<250,1,0)
test $Pred_500_rf = ifelse(abs(test$Pop_rf-test$Population)<500,1,0)

sumtable(test, 
         vars=c('Population','Pop_ols', 'Pop_rf',
                'msqe_ols','msqe_rf','msqe_ols_sqrt','msqe_rf_sqrt',
                'Pred_100_ols','Pred_100_rf',
                'Pred_250_ols','Pred_250_rf',
                'Pred_500_ols','Pred_500_rf'), add.median=TRUE)

# We again create a ggplot object which defines the basic plot values
# Then we add layers for the three data (predicted from OLS, predicted from
# Random Forest and actual values) we want to plot With the poly function, we
# plot a regression curve that is polynomial of degree 4

ggp <- ggplot(data=test, aes(x = year_panel))
ggp + 
  stat_smooth(method="lm",
              aes(y = Population),
              formula = y ~ poly(x,4),
              se = FALSE) +
  stat_smooth(method="lm",
              aes(y = Pop_ols),
              formula = y ~ poly(x,4),
              se = FALSE,
              color = "black", linetype="twodash") +
  stat_smooth(method="lm",
              aes(y = Pop_rf),
              formula = y ~ poly(x,4),
              se = FALSE,
              color = "black")


# Here the blue line is the test data, the black line is the random forest 
# prediction and the black dashed line is the prediction by OLS. It is clear 
# that a random forest prediction is a much better predictor for the data.


##### Using lasso and ridge prediction algorithms #### 
# (The remaining part follows the same pattern as above, but the code is not from the class)

# Lasso and ridge regressions are methods we can use to fit a regression model when 
# multicollinearity is present in the data. In lasso regression, we select a 
# value for λ that produces the lowest possible test MSE (mean squared error).

#define response variable
y <- train$Population

#define matrix of predictor variables
x <- data.matrix(train[, c('app_count', 'year_panel', 'C_ID', 'latitude',
                           'longitude')])

# The difference between lasso and ridge is basically whether we set alpha = 0 or 1
# in the glmnet function. Setting alpha = 1 means using lasso and alpha = 0 means 
# we are using ridge prediction algorithm. 

# Lasso prediction algorithm

#perform k-fold cross-validation to find optimal lambda value
lasso_fit <- cv.glmnet(x, y, alpha = 1, na.action = na.omit()) 
# cv.glmnet() automatically performs k-fold cross validation using k = 10 folds.

#find optimal lambda value that minimizes test MSE
best_lambda <- lasso_fit$lambda.min
best_lambda

#produce plot of test MSE by lambda value
plot(lasso_fit) 

#find coefficients of best model
best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)
coef(best_model)

# Predicting values with the lasso algorithm on the test data
test_data <- test %>% select(app_count, year_panel, C_ID, latitude, longitude)
test$Pop_lasso <- predict(best_model, s = best_lambda, newx = makeX(test_data))

# Evaluating
# calculate the standard deviation in the population from the test dataset
sd_real <- sd(test$Population)
# Calculate the mean squared errors for population as the square of the difference
# between predicted and actual values of population divided by the standard 
# deviation of the values of population in the test dataset.
test$msqe_lasso <- (test$Pop_lasso-test$Population)^2 / sd_real
test$msqe_lasso_sqrt <- sqrt((test$Pop_lasso-test$Population)^2)

# We also create a summary table which shows the results of how many observations
# of population in the test data are different from the predicted values by a 
# factor of 100, 250 and 500. The first three lines create logical vectors
# which test these conditions and the sumtable function creates a summary of the
# results.
test $Pred_100_lasso = ifelse(abs(test$Pop_lasso-test$Population)<100,1,0)
test $Pred_250_lasso = ifelse(abs(test$Pop_lasso-test$Population)<250,1,0)
test $Pred_500_lasso = ifelse(abs(test$Pop_lasso-test$Population)<500,1,0)

sumtable(test, 
         vars=c('Population','Pop_ols', 'Pop_lasso',
                'msqe_ols','msqe_lasso','msqe_ols_sqrt','msqe_lasso_sqrt',
                'Pred_100_ols','Pred_100_lasso',
                'Pred_250_ols','Pred_250_lasso',
                'Pred_500_ols','Pred_500_lasso'), add.median=TRUE)

# We again create a ggplot object which defines the basic plot values
# Then we add layers for the three data (predicted from OLS, predicted from
# Random Forest and actual values) we want to plot With the poly function, we
# plot a regression curve that is polynomial of degree 4

ggp <- ggplot(data = test, aes(x = year_panel))
ggp + 
  stat_smooth(method="lm",
              aes(y = Population),
              formula = y ~ poly(x,4),
              se = FALSE) +
  stat_smooth(method="lm",
              aes(y = Pop_ols),
              formula = y ~ poly(x,4),
              se = FALSE,
              color = "black", linetype = "twodash") +
  stat_smooth(method="lm",
              aes(y = Pop_lasso),
              formula = y ~ poly(x,4),
              se = FALSE,
              color = "green")

# Here the blue line is the test data, the green line is the random forest 
# prediction and the black dashed line is the prediction by OLS. It is not clear 
# that a lasso prediction is a better predictor for the data.

# Ridge prediction algorithm 

#fit ridge regression model 
ridge_fit <- cv.glmnet(x, y, alpha = 0)

#find optimal lambda value that minimizes test MSE
best_lambda <- ridge_fit$lambda.min
best_lambda

#produce plot of test MSE by lambda value
plot(cv_model)

# find coefficient of best model
best_model <- glmnet(x, y, alpha = 0, lambda = best_lambda)
coef(best_model)

#use fitted best model to make predictions
test$Pop_ridge <- predict(model, s = best_lambda, newx = makeX(test_data))

# Evaluating
# calculate the standard deviation in the population from the test dataset
sd_real <- sd(test$Population)
# Calculate the mean squared errors for population as the square of the difference
# between predicted and actual values of population divided by the standard 
# deviation of the values of population in the test dataset.
test$msqe_ridge <- (test$Pop_ridge-test$Population)^2 / sd_real
test$msqe_ridge_sqrt <- sqrt((test$Pop_ridge-test$Population)^2)

# We also create a summary table which shows the results of how many observations
# of population in the test data are different from the predicted values by a 
# factor of 100, 250 and 500. The first three lines create logical vectors
# which test these conditions and the sumtable function creates a summary of the
# results.
test $Pred_100_ridge = ifelse(abs(test$Pop_ridge-test$Population)<100,1,0)
test $Pred_250_ridge = ifelse(abs(test$Pop_ridge-test$Population)<250,1,0)
test $Pred_500_ridge = ifelse(abs(test$Pop_ridge-test$Population)<500,1,0)

sumtable(test, 
         vars=c('Population','Pop_ols', 'Pop_ridge',
                'msqe_ols','msqe_ridge','msqe_ols_sqrt','msqe_ridge_sqrt',
                'Pred_100_ols','Pred_100_ridge',
                'Pred_250_ols','Pred_250_ridge',
                'Pred_500_ols','Pred_500_ridge'), add.median=TRUE)

# We again create a ggplot object which defines the basic plot values
# Then we add layers for the three data (predicted from OLS, predicted from
# Random Forest and actual values) we want to plot With the poly function, we
# plot a regression curve that is polynomial of degree 4

ggp <- ggplot(data = test, aes(x = year_panel))
ggp + 
  stat_smooth(method="lm",
              aes(y = Population),
              formula = y ~ poly(x,4),
              se = FALSE) +
  stat_smooth(method="lm",
              aes(y = Pop_ols),
              formula = y ~ poly(x,4),
              se = FALSE,
              color = "black", linetype = "twodash") +
  stat_smooth(method="lm",
              aes(y = Pop_ridge),
              formula = y ~ poly(x,4),
              se = FALSE,
              color = "red")


# Here the blue line is the test data, the red line is the random forest 
# prediction and the black dashed line is the prediction by OLS. It is not clear 
# that a ridge prediction is a better predictor for the data.









