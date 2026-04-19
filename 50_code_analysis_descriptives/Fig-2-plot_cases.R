

# *************************************************************************
#   Creator: Tobias Eibinger and Sachintha Fernando
#   Project: Zero Fare
#   CREATE PLOTS Covid Cases (Fig 2)
#   Date: 2026-04-05
# *************************************************************************

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(haven)

# Read the updated data
rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


data <- read_dta("../30_data_analysis/covid-cases-final.dta")
glimpse(data)


colnames(data)[5] <- "cases_pop_nuts2"

# Filter for the year 2020
data_2020 <- data[data$year == 2020,]
data_2021 <- data[data$year == 2021,]

# Aggregate data to find total cases for each country and NUTS2 region in 2020
country_level <- data_2020 %>%
  group_by(ccode) %>%
  summarize(total_country_cases = mean(cases_pop_nuts2, na.rm = TRUE))

region_level <- data_2020 %>%
  group_by(ccode, NUTS2code) %>%
  summarize(total_region_cases = mean(cases_pop_nuts2, na.rm = TRUE))

# Find minimum and maximum cases for each country
min_max_region <- region_level %>%
  group_by(ccode) %>%
  summarize(min_cases = min(total_region_cases, na.rm=T), 
            max_cases = max(total_region_cases, na.rm=T))

# Merge with country-level data
final_data <- merge(country_level, min_max_region, by = "ccode")

# Arrange the dataframe alphabetically by country
final_data <- final_data %>% arrange(ccode)

#restrict to final sample
final_data <- final_data %>%
  filter(ccode%in%c("BE", "CZ", "DK", "FI", "FR", "EL", 
                    "HU", "IT", "LU", "NL", "PL", "PT",
                    "SK", "ES")
  )

# Set the factor levels of the country column to match the descending order
final_data$ccode <- factor(final_data$ccode, levels = rev(unique(final_data$ccode)))

# Plotting
ggplot(final_data, aes(y = ccode)) +
  geom_point(aes(x = min_cases), color = "black", shape=6) +
  geom_point(aes(x = max_cases), color = "black", shape=2) +
  geom_point(aes(x = total_country_cases), color = "black", shape=19, size=2.5) +
  geom_segment(aes(x = min_cases, xend = max_cases, yend = ccode), color = "black", lwd=0.25) +
  labs(title = "",
       x = "",
       y = "") +
  theme_minimal() +
  theme(legend.position = "none")


############################

# Load necessary libraries
library(ggplot2)
library(dplyr)

# Filter for the years 2020 and 2021
data_2020 <- data[data$year == 2020,]
data_2021 <- data[data$year == 2021,]

# Function to process data for a given year
process_data <- function(data_year) {
  country_level <- data_year %>%
    group_by(ccode) %>%
    summarize(total_country_cases = mean(cases_pop_nuts2, na.rm = TRUE))
  
  region_level <- data_year %>%
    group_by(ccode, NUTS2code) %>%
    summarize(total_region_cases = mean(cases_pop_nuts2, na.rm = TRUE))
  
  min_max_region <- region_level %>%
    group_by(ccode) %>%
    summarize(min_cases = min(total_region_cases), 
              max_cases = max(total_region_cases))
  
  final_data <- merge(country_level, min_max_region, by = "ccode")
  return(final_data)
}

# Process data for both years
final_data_2020 <- process_data(data_2020)
final_data_2021 <- process_data(data_2021)


# Combine 2020 and 2021 data
final_data <- rbind(mutate(final_data_2020, year = 2020), mutate(final_data_2021, year = 2021))

# Arrange the dataframe alphabetically by country
final_data <- final_data %>% arrange(ccode, year)

# Set the factor levels of the country column to match the descending order
final_data$ccode <- factor(final_data$ccode, levels = rev(unique(final_data$ccode)))

#restrict to final sample
final_data <- final_data %>%
  filter(ccode%in%c("BE", "CZ", "DK", "FI", "FR", "EL", 
                    "HU", "IT", "LU", "NL", "PL", "PT",
                    "SK", "ES")
  )


# Offset for distinguishing between years
year_offset <- 0.2

# Plotting
ggplot(final_data, aes(y = ccode)) +
  geom_point(aes(x = min_cases - year_offset, color = as.factor(year)), shape = 6) +
  geom_point(aes(x = max_cases + year_offset, color = as.factor(year)), shape = 2) +
  geom_point(aes(x = total_country_cases, color = as.factor(year)), shape = 19, size = 2.5) +
  geom_segment(aes(x = min_cases - year_offset, xend = max_cases + year_offset, yend = ccode, color = as.factor(year))) +
  scale_color_manual(values = c("black", "gray60")) +
  labs(title = NULL, 
       x = NULL, 
       y = NULL, 
       color = "") +
  theme_minimal() +
  theme(legend.position = "bottom",  # Move legend to bottom
        plot.margin = margin(5.5, 5.5, 5.5, 5.5), 
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        plot.title = element_blank()) +  
  scale_x_continuous(breaks = seq(0, 2050, by = 500), 
                     limits = c(0, 2050)) 






library(ggplot2)

# Ensure year is treated as a factor
final_data$year <- as.factor(final_data$year)

# Plot
ggplot(final_data) +
  # Points for minimum cases
  geom_point(
    aes(
      x = min_cases - year_offset,
      y = ccode,
      color = year
    ),
    shape = 6,
    position = position_nudge(y = ifelse(final_data$year == "2020", 0.2, -0.2))
  ) +
  
  # Points for maximum cases
  geom_point(
    aes(
      x = max_cases + year_offset,
      y = ccode,
      color = year
    ),
    shape = 2,
    position = position_nudge(y = ifelse(final_data$year == "2020", 0.2, -0.2)) 
  ) +
  
  # Points for total_country_cases
  geom_point(
    aes(
      x = total_country_cases,
      y = ccode,
      color = year
    ),
    shape = 19, size = 2.5,
    position = position_nudge(y = ifelse(final_data$year == "2020", 0.2, -0.2)) 
  ) +
  
  # Segments between min_cases and max_cases
  geom_segment(
    aes(
      x = min_cases - year_offset,
      xend = max_cases + year_offset,
      y = ccode,
      yend = ccode,
      color = year
    ),
    position = position_nudge(y = ifelse(final_data$year == "2020", 0.2, -0.2)) 
  ) +
  
  # Custom color scale
  scale_color_manual(values = c("2020" = "black", "2021" = "gray60")) +
  
  # Minimal theme
  labs(
    title = NULL,
    x = NULL,
    y = NULL,
    color = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",  
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank()) +
  
  # X-axis customization
  scale_x_continuous(
    breaks = seq(0, 2050, by = 500), 
    limits = c(0, 2050)              
  )


ggsave(file="../60_results/Fig-2-cases_variation_fs.pdf",
       scale=1,
       width = 8, 
       height = 6, 
       units = "in"
)




