

# *************************************************************************
#   Creator: Tobias Eibinger and Sachintha Fernando
#   Project: Zero Fare
#   Creates pre-trends graph (Fig E.2)
#   Date: 2026-04-05
# *************************************************************************


library(ggplot2)
library(dplyr)
library(tidyr)
library(haven)


input_dir <- "../30_data_analysis"
output_dir <- "../60_results"


data <- read_dta(paste0(input_dir,"/yadj_timetrends.dta"))


# Pivot the data
data_long <- pivot_longer(data, cols = c(con_avg, w_con_avg, yadj_weighted, lu_yadj), names_to = "variables", values_to = "value")


# Only the pre-treatment trends 
data_long_pre <- data_long %>% filter(year <=2019)

# Plot the normalized time series
line_types <- c("con_avg" = "dotdash", 
                "w_con_avg" = "dashed", 
                "yadj_weighted" = "dotted",
                "lu_yadj" = "solid")

line_labels <- c("con_avg" = "Simple avg all units", 
                 "w_con_avg" = "Simple avg positively weighted units", 
                 "yadj_weighted" = "Weighted average", 
                 "lu_yadj" = "Luxembourg")


# Normalized graph 
#----------------------------------------------

# Normalize the data by subtracting the mean and dividing by the standard deviation for each variable


data_long_normalized_pre <- data_long_pre %>%
  group_by(variables) %>%
  mutate(value_normalized = (value - mean(value))) %>%
  ungroup()


# Combining the two graphs 
library(patchwork)


# 1. Define the color mapping
line_colors <- c("con_avg" = "gray80",
                 "w_con_avg" = "gray60",
                 "yadj_weighted" = "gray40", 
                 "lu_yadj" = "black") 

line_colors <- c(
  "con_avg"       = "gray40",
  "w_con_avg"     = "#1F77B9",
  "yadj_weighted" = "#FD6467",
  "lu_yadj"       = "black"
)


p1 <- ggplot(data_long_pre, aes(x = year, y = value, linetype = variables, color = variables)) +
  geom_line(size = 3) +
  labs(
    title = "(a) Absolute outcome",
    x = " ", y = " ",
    linetype = NULL, color = NULL
  ) +
  theme_minimal() + 
  ylim(3.5, 5) +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 20, face = "bold"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text = element_text(size = 20)
  ) +
  scale_linetype_manual(values = line_types, labels = line_labels) +
  scale_color_manual(values = line_colors, labels = line_labels) 


p2 <- ggplot(data_long_normalized_pre, aes(x = year, y = value_normalized, linetype = variables, color = variables)) +
  geom_line(size = 3) +
  labs(
    title = "(b) Normalized outcome",
    x = " ", y = " ",
    linetype = NULL, color = NULL
  ) +
  theme_minimal() +
  ylim(-0.08,0.08)+
  theme(
    legend.position = "none",
    plot.title = element_text(size = 20, face = "bold"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text = element_text(size = 20)
  ) +
  scale_linetype_manual(values = line_types, labels = line_labels) +
  scale_color_manual(values = line_colors, labels = line_labels) 

library(patchwork)

(p1 / plot_spacer() / p2) + 
  plot_layout(ncol = 1, guides = 'collect', heights = c(1.2, 0.5, 1.2)) & 
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 20),
    legend.key.size = unit(1.5, "cm")
  )

ggsave(file=paste0(output_dir,"/Fig-E.2-outcome-preperiod-combined.pdf"), 
scale = 2,
width = 10,
height = 7,
units = "in"
)


