# *************************************************************************
#   Creator: Tobias Eibinger and Sachintha Fernando
#   Project: Zero Fare
#   CREATE FUEL PRICE PLOTS (Fig E.4, Table E.1)
#   Date: 2026-04-05
# *************************************************************************

rm(list=ls())

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)

# Read the data
fuel_diesel <- read.csv("../30_data_analysis/fuel_diesel_cts.csv")

# Reshape the data to wide format
data_wide <- fuel_diesel %>%
  dplyr::select(year, country, diesel) %>%
  spread(key = country, value = diesel)

# Calculate relative prices of Luxembourg relative to other countries
relative_prices <- data_wide %>%
  mutate(relative_price_BE = LU / BE,
         relative_price_FR = LU / FR,
         relative_price_DE = LU / DE) %>%
  dplyr::select(year, relative_price_BE, relative_price_FR, relative_price_DE)

# Reshape for ggplot2
relative_prices_melted <- relative_prices %>%
  gather(key = "country", value = "relative_price", -year)


#petrol
# Read the data
fuel_petrol <- read.csv("../30_data_analysis/fuel_petrol_cts.csv")

# Reshape the data to wide format
data2_wide <- fuel_petrol %>%
  dplyr::select(year, country, petrol) %>%
  spread(key = country, value = petrol)

# Calculate relative prices of Luxembourg relative to other countries
relative_prices2 <- data2_wide %>%
  mutate(relative_price_BE = LU / BE,
         relative_price_FR = LU / FR,
         relative_price_DE = LU / DE) %>%
  dplyr::select(year, relative_price_BE, relative_price_FR, relative_price_DE)

# Reshape for ggplot2
relative_prices_melted2 <- relative_prices2 %>%
  gather(key = "country", value = "relative_price", -year)


#diesel
# Plot the relative prices as time series
library(RColorBrewer)
colors <- brewer.pal(5, "Set2")

d1 <- ggplot(fuel_diesel, aes(x = as.factor(year), group = country)) +
  geom_bar(aes(y = diesel, fill = country), stat = "identity", position = position_dodge(width = 0.8), alpha=0.8) +
  geom_line(aes(y = diesel, color = country), size = 0.5, position = position_dodge(width = 0.8)) +
  geom_point(aes(y = diesel, color = country), position = position_dodge(width = 0.8), size = 3, shape = 21, fill = "black") +
  scale_fill_manual(values = colors) + # Color palette for bars
  scale_color_manual(values = colors) + # Color palette for lines and points
  theme_minimal() +
  labs(title = "",
       x = "",
       y = "Diesel Price in Eurocents")  +
  theme(legend.position = "bottom") 

d2 <- ggplot(relative_prices_melted, aes(x = year, y = relative_price, color = country)) +
  geom_line(size=1) + 
  scale_color_manual(values = colors) + 
  labs(title = "",
       x = "",
       y = "Relative Diesel Price Index") +
  theme_minimal() +
  ylim(0.7, 1)   +
  theme(legend.position = "none") 
library(patchwork)

d2/d1

library(ggplot2)
library(ggpattern)

# Define the corresponding point shapes
point_shapes <- c(15, 16, 17)  # Square, Circle, Triangle Up

# Adjust the labels for the legend
legend_labels <- c("LU/BE", "LU/DE", "LU/FR")

# Calculate the scaling factor for aligning y-axes
scaling_factor <- max(fuel_diesel$diesel) / 0.5 

# Define colors for each country (using original, softer colors)
bar_colors <- c("lightcoral", "lightgreen", "lightblue", "lightgoldenrodyellow", 
                "lightpink", "lightcyan", "lightgray")  
bar_colors <- c("grey95", "grey95", "grey95", "grey95", 
                "grey95", "grey95", "grey95")  

# Define patterns for countries
bar_patterns <- c("stripe", "crosshatch", "circle", "none", "stripe", "crosshatch", "circle")

# Create the plot
combined_plot <- ggplot(fuel_diesel, aes(x = as.factor(year), y = diesel)) +
  # Bar plot for Diesel Prices with colors and patterns
  geom_bar_pattern(aes(fill = country, pattern = country), stat = "identity", 
                   position = position_dodge(width = 0.9),
                   alpha = 0.8, pattern_density = 0.3, 
                   pattern_spacing = 0.02, pattern_size = 0.3, pattern_alpha = 0.4, color = "black", size = 0.3) +
  scale_fill_manual(values = bar_colors) + 
  scale_pattern_manual(values = bar_patterns) + 
  
  # Line plot for Relative Diesel Prices aligned with bars
  geom_line(data = subset(relative_prices_melted, country != "LU"), 
            aes(x = as.numeric(as.factor(year)) + 
                  (as.numeric(as.factor(country)) - 1) * 0.9 / length(unique(fuel_diesel$country)) - 0.45 + 0.9 / (2 * length(unique(fuel_diesel$country))),
                y = (relative_price - 0.5) * scaling_factor, 
                group = country), color = "black", size = 1, show.legend = FALSE) +
  
  geom_point(data = subset(relative_prices_melted, country != "LU"), 
             aes(x = as.numeric(as.factor(year)) + 
                   (as.numeric(as.factor(country)) - 1) * 0.9 / length(unique(fuel_diesel$country)) - 0.45 + 0.9 / (2 * length(unique(fuel_diesel$country))),
                 y = (relative_price - 0.5) * scaling_factor, 
                 shape = country), color = "black", size = 2.5) + 
  
  scale_shape_manual(values = point_shapes, labels = legend_labels, guide = guide_legend(override.aes = list(color = "black"))) + 
  
  # Primary y-axis for Diesel Prices with adjusted limits and breaks
  scale_y_continuous(name = "Diesel Price in Eurocents", limits = c(0, 1750), breaks = seq(0, 1750, by = 500),
                     sec.axis = sec_axis(~ . / scaling_factor + 0.5, 
                                         name = "Relative Diesel Price", 
                                         breaks = seq(0.5, 1, by = 0.1))) +

  labs(x = "") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    axis.title.y.left = element_text(color = "black", size = 14),
    axis.title.y.right = element_text(color = "black", size = 14),
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 12),
    axis.text.y = element_text(size = 12),
    legend.title = element_blank(),
    panel.grid.major.y = element_line(color = "grey90", size = 0.5, linetype = "solid"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )

# Print the combined plot
combined_plot

ggsave(file="../60_results/Fig-F.1-fuel-diesel.pdf", 
       plot=combined_plot,
       scale=1,
       width = 6,
       height = 4,
       units = "in"
)

# time-weighted pre-treatment average
t16 = .4939565
t17 = .063485
t18 = .4425585

fd <- fuel_diesel %>%
  mutate(w = ifelse(year==2016, t16, 0)) %>%
  mutate(w = ifelse(year==2017, t17, w)) %>%
  mutate(w = ifelse(year==2018, t18, w)) %>%
  mutate(tw = diesel*w)

twpa <- fd %>%
  group_by(country) %>%
  summarise(twpa = sum(tw))
twpa

poa <- fd %>%
  group_by(country) %>%
  dplyr::filter(year>2019) %>%
  summarise(poa = mean(diesel))
poa

pra <- fd %>%
  group_by(country) %>%
  dplyr::filter(year<=2019) %>%
  summarise(pra = mean(diesel))
pra

d19 <- fd %>%
  group_by(country) %>%
  dplyr::filter(year==2019)
d19

d21 <- fd %>%
  group_by(country) %>%
  dplyr::filter(year==2021)
d21

d22 <- fd %>%
  group_by(country) %>%
  dplyr::filter(year==2022)
d22

dp <- merge(twpa, d21, by="country")
dp <- merge(dp, d19, by="country")
dp <- merge(dp, d22, by="country")
dp <- merge(dp, poa, by="country")
dp <- merge(dp, pra, by="country")
dp <- dp %>% dplyr::select(country, pra, twpa, diesel.y, diesel.x, diesel, poa)
colnames(dp) <- c("country", "pre-avg", "tw-pre-avg", "2019", "2021", "2022", "post-avg")
dp



# relative prices twpre
rpre_be <-  dp[4,3]/dp[1,3] #BE
rpre_de <-  dp[4,3]/dp[2,3] #DE
rpre_fr <-  dp[4,3]/dp[3,3] #FR

# relative prices post-avg
rpst_be <- dp[4,7]/dp[1,7] #BE
rpst_de <- dp[4,7]/dp[2,7] #DE
rpst_fr <- dp[4,7]/dp[3,7] #FR


data.frame(
  type = c(rep("diesel", 3)),
  country = c("BE", "DE", "FR"),
  relpre = c(rpre_be, rpre_de, rpre_fr),
  relpst = c(rpst_be, rpst_de, rpst_fr),
  r19 = c(relative_prices$relative_price_BE[4], relative_prices$relative_price_DE[4], 
          relative_prices$relative_price_FR[4]),
  r21 = c(relative_prices$relative_price_BE[5], relative_prices$relative_price_DE[5], 
          relative_prices$relative_price_FR[5]),
  r22 = c(relative_prices$relative_price_BE[6], relative_prices$relative_price_DE[6], 
          relative_prices$relative_price_FR[6])
)

#######################
#petrol
#######################

# Plot the relative prices as time series
library(RColorBrewer)
colors <- brewer.pal(5, "Set2")


p1 <- ggplot(fuel_petrol, aes(x = as.factor(year), group = country)) +
  geom_bar(aes(y = petrol, fill = country), stat = "identity", position = position_dodge(width = 0.8), alpha=0.8) +
  geom_line(aes(y = petrol, color = country), size = 0.5, position = position_dodge(width = 0.8)) +
  geom_point(aes(y = petrol, color = country), position = position_dodge(width = 0.8), size = 3, shape = 21, fill = "black") +
  scale_fill_manual(values = colors) + # Color palette for bars
  scale_color_manual(values = colors) + # Color palette for lines and points
  theme_minimal() +
  labs(title = "",
       x = "",
       y = "Petrol Price in Eurocents")  +
  theme(legend.position = "bottom") 

p2 <- ggplot(relative_prices_melted2, aes(x = year, y = relative_price, color = country)) +
  geom_line(size=1) +  # Specify the size of the line for better visibility
  scale_color_manual(values = colors) +  # Apply the same color palette as before
  labs(title = "",
       x = "",
       y = "Relative Petrol Price Index") +
  theme_minimal() +
  ylim(0.7, 1)+
  theme(legend.position = "none") 

library(patchwork)
p2/p1

library(ggplot2)
library(ggpattern)

# Define the corresponding point shapes
point_shapes <- c(15, 16, 17)  # Square, Circle, Triangle Up

# Adjust the labels for the legend
legend_labels <- c("LU/BE", "LU/DE", "LU/FR")

# Calculate the scaling factor for aligning y-axes
scaling_factor <- max(fuel_petrol$petrol) / 0.5  # Align relative prices to diesel prices

# Define colors for each country (using original, softer colors)
bar_colors <- c("lightcoral", "lightgreen", "lightblue", "lightgoldenrodyellow", 
                "lightpink", "lightcyan", "lightgray")  
bar_colors <- c("grey95", "grey95", "grey95", "grey95", 
                "grey95", "grey95", "grey95")  

# Define patterns that match the number of countries
bar_patterns <- c("stripe", "crosshatch", "circle", "none", "stripe", "crosshatch", "circle")

# Create the plot
combined_plot <- ggplot(fuel_petrol, aes(x = as.factor(year), y = petrol)) +
  # Bar plot for Diesel Prices with colors and patterns
  geom_bar_pattern(aes(fill = country, pattern = country), stat = "identity", 
                   position = position_dodge(width = 0.9),  # Ensure bars are side by side
                   alpha = 0.8, pattern_density = 0.3, 
                   pattern_spacing = 0.02, pattern_size = 0.3, pattern_alpha = 0.4, color = "black", size = 0.3) +
  scale_fill_manual(values = bar_colors) +  # Apply the original color scheme
  scale_pattern_manual(values = bar_patterns) +  # Apply the correct patterns to match the bars
  
  # Line plot for Relative Diesel Prices aligned with bars
  geom_line(data = subset(relative_prices_melted2, country != "LU"), 
            aes(x = as.numeric(as.factor(year)) + 
                  (as.numeric(as.factor(country)) - 1) * 0.9 / length(unique(fuel_petrol$country)) - 0.45 + 0.9 / (2 * length(unique(fuel_petrol$country))),
                y = (relative_price - 0.5) * scaling_factor, 
                group = country), color = "black", size = 1, show.legend = FALSE) +  # Line color set to black and legend hidden for line
  
  geom_point(data = subset(relative_prices_melted2, country != "LU"), 
             aes(x = as.numeric(as.factor(year)) + 
                   (as.numeric(as.factor(country)) - 1) * 0.9 / length(unique(fuel_petrol$country)) - 0.45 + 0.9 / (2 * length(unique(fuel_petrol$country))),
                 y = (relative_price - 0.5) * scaling_factor, 
                 shape = country), color = "black", size = 2.5) +  # Point color set to black
  
  scale_shape_manual(values = point_shapes, labels = legend_labels, guide = guide_legend(override.aes = list(color = "black"))) +  # Apply the corresponding point shapes with custom labels and ensure color override
  
  # Primary y-axis for Diesel Prices with adjusted limits and breaks
  scale_y_continuous(name = "Petrol Price in Eurocents", limits = c(0, 1750), breaks = seq(0, 1750, by = 500),
                     sec.axis = sec_axis(~ . / scaling_factor + 0.5, 
                                         name = "Relative Petrol Price", 
                                         breaks = seq(0.5, 1, by = 0.1))) +
  
  # Common x-axis and theme settings
  labs(x = "") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    axis.title.y.left = element_text(color = "black", size = 14),
    axis.title.y.right = element_text(color = "black", size = 14),
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 12),
    axis.text.y = element_text(size = 12),
    legend.title = element_blank(),
    panel.grid.major.y = element_line(color = "grey90", size = 0.5, linetype = "solid"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )

# Print the combined plot
combined_plot

ggsave(file="../60_results/Fig-F.1-fuel-petrol.pdf", 
       plot=combined_plot,
       scale=1,
       width = 6,  # Specify width in inches
       height = 4,  # Specify height in inches
       units = "in" # Specify units (options: "in", "cm", "mm")
)

fp <- fuel_petrol %>%
  mutate(w = ifelse(year==2016, t16, 0)) %>%
  mutate(w = ifelse(year==2017, t17, w)) %>%
  mutate(w = ifelse(year==2018, t18, w)) %>%
  mutate(tw = petrol*w)

twpap <- fp %>%
  group_by(country) %>%
  summarise(twpap = sum(tw))
twpap

poap <- fp %>%
  group_by(country) %>%
  dplyr::filter(year>2019) %>%
  summarise(poap = mean(petrol))
poap

prap <- fp %>%
  group_by(country) %>%
  dplyr::filter(year<=2019) %>%
  summarise(prap = mean(petrol))
prap

d19p <- fp %>%
  group_by(country) %>%
  dplyr::filter(year==2019)
d19p

d21p <- fp %>%
  group_by(country) %>%
  dplyr::filter(year==2021)
d21p

d22p <- fp %>%
  group_by(country) %>%
  dplyr::filter(year==2022)
d22p

pdp <- merge(twpap, d21p, by="country")
pdp <- merge(pdp, d19p, by="country")
pdp <- merge(pdp, d22p, by="country")
pdp <- merge(pdp, poap, by="country")
pdp <- merge(pdp, prap, by="country")
pdp <- pdp %>% dplyr::select(country, prap, twpap, petrol.y, petrol.x, petrol, poap)
colnames(pdp) <- c("country", "pre-avg", "tw-pre-avg", "2019", "2020", "2021", "post-avg")
pdp

# relative prices twpre
prpre_be <-  pdp[4,3]/pdp[1,3] #BE
prpre_de <-  pdp[4,3]/pdp[2,3] #DE
prpre_fr <-  pdp[4,3]/pdp[3,3] #FR

# relative prices post-avg
prpst_be <- pdp[4,7]/pdp[1,7] #BE
prpst_de <- pdp[4,7]/pdp[2,7] #DE
prpst_fr <- pdp[4,7]/pdp[3,7] #FR



data.frame(
  type = c(rep("petrol",3)),
  country = c("BE", "DE", "FR"),
  relpre = c(prpre_be, prpre_de, prpre_fr),
  relpst = c(prpst_be, prpst_de, prpst_fr),
  r19 = c(relative_prices2$relative_price_BE[4], relative_prices2$relative_price_DE[4], 
          relative_prices2$relative_price_FR[4]),
  r21 = c(relative_prices2$relative_price_BE[5], relative_prices2$relative_price_DE[5], 
          relative_prices2$relative_price_FR[5]),
  r22 = c(relative_prices2$relative_price_BE[6], relative_prices2$relative_price_DE[6], 
          relative_prices2$relative_price_FR[6])
)
