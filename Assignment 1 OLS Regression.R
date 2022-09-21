# Telecom Work Measurement Study
# 
# The data originates from study for a telecom company (we are only using an excerpt). 
# According to the website, “the purpose of the study was to model the total hours worked 
# in a section of Telecom in terms of the counts of various tasks. It was hoped that such 
# a model could be used to predict hours worked and hence staffing requirements in changing 
# circumstances. The number of hours worked by employees in a fault reporting centre were 
# recorded, together with the number of faults of each type which were recorded. Employees 
# often work on a flexitime system which allows them to build up time and to leave early 
# every second Friday.”


tel_data <- read.csv('tel.csv')
head(tel_data)
summary(tel_data)

# Run a regression of Hours on all of the variables, and report your findings.
first_reg <- lm(Hours ~., data = tel_data) 
summary(first_reg)
confint(first_reg)

# From our linear model, we can observe that Hotline (Hot), service order count of type A (SOA)
# and fault variable type (RWT) have a statistically significant relationship with number of 
# hours worked (Hours). This suggests that they might be good predictors for our model. Based on
# the coefficient estimates, we can infer that there is a positive relationship between 'Hours' and 
# the 3 features that were found to be significant.

# Include an additional variable that indicated whether the Day is Friday. Rerun your regression. 
# Adding a new variable 'Friday' that sets 5 equal to 1 and all other days equal to 0.
tel_data$Friday = as.integer(ifelse(tel_data$Day==5,1,0)) 
second_reg <- lm(Hours ~., data = tel_data)
summary(second_reg)
confint(second_reg)

# After re-running the regression model with an additional variable 'Friday', we see that 'Friday'
# is now the most significant predictor of 'Hours' based on its p-value. It is expected that 'Friday' 
# has a strong negative relationship with 'Hours' since most people would want to leave work early on 
# Fridays leading to weekends. We also find that 'RWT' no longer has a significant relationship with 
# 'Hours'.

# We will see if our 'full' model provides a reasonable fit by checking the residuals
preds <- predict(first_reg)
eps <- tel_data$Hours - preds
par(mfrow=c(1,2))
hist(eps)
qqnorm(eps)
qqline(eps)

# After checking for residuals, we see that the points do not fall tightly on a straight line in our 
# QQPlot, suggesting that our sample data is slightly skewed and did not come from a good normal 
# distribution.

# Based on the previous analysis, we will try to build a better model than the “full” model. We will 
# run a backward selection to find the best model.

# We will use the step function to run a backward stepwise for our original full model 'first_reg' 
# which does not include the new variable 'Friday'
backward <- step(first_reg, direction = 'backward')

# The lower the AIC value means better fit for our regression model. In finding our best model, the 
# step function looks at the AIC value of our model after the corresponding variable is dropped.
# For example, the AIC value for our 'full' model is 159.52, if we remove the variable 'SOB', we 
# can lower the AIC value to 157.58 suggesting that 'SOB' is not a good predictor for our model. The
# step function checks the AIC value again and again until the model can no longer produce a reduction
# in AIC value. 

backward$coefficients

# In our backward stepwise regression, we find that 'ByDa', 'RWT', 'SOA', 'Hot' and 'Day' are the 
# best predictors in building our model.

# We will see if our backward selection model provides a better fit by checking the residuals
preds_2 <- predict(backward)
eps_2 <- tel_data$Hours - preds_2
par(mfrow=c(1,2))
hist(eps_2)
qqnorm(eps_2)
qqline(eps_2)

# In contrast to our 'full' model, the points in our backward selection model fall tighter on the 
# straight line suggesting better normal distribution and better model fit for predicting 'Hours'
