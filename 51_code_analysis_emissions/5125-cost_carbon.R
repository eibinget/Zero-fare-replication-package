

# *************************************************************************
#   Creator: Tobias Eibinger and Sachintha Fernando
#   Project: Zero Fare
#   Estimate Cost of Carbon (CO2)
#   Date: 2026-04-05
# *************************************************************************

#post-treatment * ATT
library(haven)
library(dplyr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

data <- read_dta("../30_data_analysis/NUTS2-Final-Panel.dta")
head(data)

co2 <- data %>% 
  dplyr::select(year, co2, NUTS2code) %>%
  dplyr::filter(year>2019 & NUTS2code=="LU00")
co2

co2lu <- (co2[2,2]+co2[3,2])/2
co2lu

att <- 0.059 # in %

cntf <- co2lu / (1-att)

abt <- cntf-co2lu
abt

c <- 41000000

c/abt

