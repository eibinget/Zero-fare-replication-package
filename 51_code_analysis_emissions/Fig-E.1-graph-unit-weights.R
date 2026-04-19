
# *************************************************************************
#   Creator: Tobias Eibinger and Sachintha Fernando
#   Project: Zero Fare
#   Creating the unit weights bubble graph (Fig E.1)
#   Date: 2026-04-05
# *************************************************************************

# Load necessary libraries
library(ggplot2)
library(tidyverse)
library(haven)
library(scales) 
library(RColorBrewer)


# Load your data
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

input_dir <- "../30_data_analysis"
output_dir <- "../60_results"

data <- read_dta(paste0(input_dir, "/unit_weights_co2.dta"))

# Create a new column for country based on the first two letters of NUTS2code
data$country <- substr(data$NUTS2code, 1, 2)

# Remove rows with missing values in omega1
data <- na.omit(data)


# Plot with vertical lines btw. countries

# Ensure the palette has enough colors for the number of countries
num_colors_needed <- length(unique(data$country))
palette_colors <- hcl.colors(num_colors_needed, palette = "Set3", rev = TRUE)

# Add a numeric index for each region to help position the vertical lines
data <- data %>%
  arrange(country, NUTS2code) %>%
  mutate(NUTS2code_num = as.numeric(as.factor(NUTS2code)))

# Create a dataset for vertical line positions
vline_positions <- data %>%
  group_by(country) %>%
  summarize(vline_x = max(NUTS2code_num) + 0.5, .groups = "drop")

# Plotting the data
ggplot(data, aes(x = NUTS2code_num, y = omega1, size = omega1, color = country)) +
  geom_point(shape = 16, na.rm = TRUE) + 
  geom_vline(data = vline_positions, aes(xintercept = vline_x), color = "grey60", linetype = "dashed", size = 0.5) +
  theme_minimal() +
  labs(title = "", x = "", y = "",
       color = "") +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.position = "bottom", 
    legend.title.align = 0.5,
    plot.margin = unit(c(0.5, 0.5, 0.5, 0), "lines"),
    legend.margin = margin(t = -24, unit = "pt") 
  ) +
  scale_x_continuous(
    breaks = data$NUTS2code_num,
    labels = data$NUTS2code,
    expand = c(0.005,0.005)
  ) +
  scale_color_manual(values = palette_colors) + 
  scale_size_continuous(range = c(1, 10), guide = FALSE) +
  guides(color = guide_legend(nrow = 1, byrow = TRUE, title.position = "top",
                              override.aes = list(size = 6))) +
  ylim(0, max(data$omega1))



ggsave(
  file=paste0(output_dir,"/Fig-E.1-unit_weights.pdf"),
  scale = 1,
  width = 12,
  height = 8,
  units = "in"
)

