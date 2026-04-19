# Percentage changes in emissions 
#----------------------------------
co2att <- -0.06119
no2att <- -0.11091
ghgatt <- -0.06156

#co2
(exp(co2att)-1)*100

#no2
(exp(no2att)-1)*100

#no2
(exp(ghgatt)-1)*100

# Percentage changes in Traffic counts
#------------------------------------
# No covariates
weekend_nocov <- -0.07074
weekdays_nocov <- -0.12013
allday_nocov <- -0.07641

#weekend
(exp(weekend_nocov)-1)*100

#weekdays
(exp(weekdays_nocov)-1)*100

#alldays
(exp(allday_nocov)-1)*100

#With covariates
weekend_withcov  <- -0.04553
weekdays_withcov <- -0.07131
allday_withcov   <- -0.04579


#weekend
(exp(weekend_withcov)-1)*100

#weekdays
(exp(weekdays_withcov)-1)*100

#alldays
(exp(allday_withcov)-1)*100

#Percentage changes in air quality
#---------------------------------
# All
air_all_nocov <- -0.14064 
air_all_withcov <- -0.10097

#Air qual all no cov
(exp(air_all_nocov )-1)*100

#Air qual all with cov
(exp(air_all_withcov)-1)*100


# Urban
air_urban_nocov <- -0.14631 
air_urban_withcov <- -0.10710

#Air qual urban no cov
(exp(air_urban_nocov )-1)*100

#Air qual urban with cov
(exp(air_urban_withcov)-1)*100
