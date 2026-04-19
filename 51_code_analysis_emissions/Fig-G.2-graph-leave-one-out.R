

# *************************************************************************
#   Creator: Tobias Eibinger and Sachintha Fernando
#   Project: Zero Fare
#   Creates graph for the leave one out analysis (Fig G.2)
#   Date: 2026-04-05
# *************************************************************************


library(haven)
library(ggplot2)
library(dplyr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

input_dir <- "../30_data_analysis"
output_dir <- "../60_results"

# Leaving all regions out
data <- read_dta(paste0(input_dir,"/leave-one-out-att.dta"))
data <- data.frame(data)

hist(data$att)

data %>%
  distinct(id2, NUTS2code) %>%
  right_join(
    tibble(
      stat = c("min", "max"),
      att = c(min(data$att, na.rm = TRUE), max(data$att, na.rm = TRUE)),
      placebo_unit = c(data$placebo_unit[which.min(data$att)],
                       data$placebo_unit[which.max(data$att)])
    ),
    by = c("id2" = "placebo_unit")
  ) %>%
  dplyr::select(stat, att, placebo_unit = id2, NUTS2code)



ggplot(data, aes(x = att)) +
  geom_density(
    aes(y = ..density../sum(..density..)), 
    fill = "#BDE2C3", 
    alpha = 0.7
  ) + 
  geom_vline(
    xintercept = -0.062, 
    color = "black", 
    linetype = "dashed", 
    size = 0.8
  ) + 
  labs(
    title = " ",
    x = " ",
    y = " "
  ) + 
  scale_y_continuous(
    expand = c(0, 0)
  ) + 
  xlim(-0.08, -0.040) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      hjust = 0.5, 
      size = 30, 
      face = "bold"
    ), 
    axis.title = element_text(size = 40),
    axis.text = element_text(size = 40),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray", size = 0.2),
    panel.grid.minor.y = element_blank()
  )


ggsave(
  file=paste0(output_dir,"/Fig-G.2-leave-one-out.pdf"),
  scale = 2,
  width = 10,
  height = 7,
  units = "in"
)


# Leaving countries out
#------------------------------
rm(list=ls())

input_dir <- "../30_data_analysis"
output_dir <- "../60_results"


data <- read_dta(paste0(input_dir,"/leave-one-out-country.dta"))
data <- data.frame(data)

data %>%
  distinct(id2, ccode) %>%
  right_join(
    tibble(
      stat = c("min", "max"),
      att = c(min(data$att, na.rm = TRUE), max(data$att, na.rm = TRUE)),
      placebo_unit = c(data$placebo_unit[which.min(data$att)],
                       data$placebo_unit[which.max(data$att)])
    ),
    by = c("id2" = "placebo_unit")
  ) %>%
  dplyr::select(stat, att, placebo_unit = id2, ccode)


hist(data$att)

ggplot(data, aes(x = att)) +
  geom_density(
    aes(y = ..density../sum(..density..)), 
    fill = "#CBD5E8", 
    alpha = 0.7
  ) + 
  geom_vline(
    xintercept = -0.062, 
    color = "black", 
    linetype = "dashed", 
    size = 0.8
  ) + 
  labs(
    title = "",
    x = "",
    y = ""
  ) + 
  scale_y_continuous(
    expand = c(0,0)
  ) + 
  scale_x_continuous(
    expand = c(0, 0)
  ) + xlim(-0.1,-0.01)+ 
  theme_minimal() + 
  theme(
    plot.title = element_text(
      hjust = 0.5, 
      size = 30, 
      face = "bold"
    ), 
    axis.title = element_text(size = 40),
    axis.text = element_text(size = 40),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray", size = 0.2),
    panel.grid.minor.y = element_blank()
  )




ggsave(
  file=paste0(output_dir,"//Fig-G.2-leave-one-out-country.pdf"),
  scale = 2,
  width = 10,
  height = 7,
  units = "in"
)

