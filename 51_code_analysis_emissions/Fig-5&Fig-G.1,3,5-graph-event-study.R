
# *************************************************************************
#   Creator: Tobias Eibinger and Sachintha Fernando
#   Project: Zero Fare
#   Creates the event study graphs (Fig 6, G.1, G.3, G.5)
#   Date: 2026-04-05
# *************************************************************************


library(haven)
library(dplyr)
library(ggplot2)

# Urban sample
#---------------------------------------------------------------------------------
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

input_dir <- "../30_data_analysis"
output_dir <- "../60_results"

data <- read_dta(paste0(input_dir,"/att_and_CI_placebo_urban.dta"))
data <- data.frame(data)

df <- data.frame(est = c(data$d1[1:6], data$d2[1:6],data$d3[1:6]),
                 upper = c(data$UCI_1[1:6],data$UCI_2[1:6], data$UCI_3[1:6]),
                 lower = c(data$LCI_1[1:6],data$LCI_2[1:6], data$LCI_3[1:6]),
                 model = rep(c("CO2", "NOX", "GHG"), each = 6),
                 year = rep(c(2016,2017,2018,2019,2021,2022),3))  


# Models
df$model <- factor(df$model, levels = c("CO2", "NOX", "GHG"))



line_types <- c("CO2" = "solid", 
                "NOX" = "dotdash", 
                "GHG" = "longdash")

cols <- c("CO2" = "black", "NOX" = "grey30", "GHG" = "grey60")
shapes <- c("CO2" = 16, "NOX" = 17, "GHG" = 15)

pd <- position_dodge(0.8)

df %>%
  ggplot(aes(
    x = factor(year), y = est,
    group = model,
    linetype = model, shape = model, color = model
  )) +
  geom_line(position = pd, linewidth = 1.5) +
  geom_point(position = pd, size = 9) +
  geom_errorbar(
    aes(ymin = lower, ymax = upper),
    position = pd, width = 0.25, linewidth = 1.5
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 1) +
  geom_vline(xintercept = 4, linetype = "dashed", color = "black", linewidth = 1) +
  coord_cartesian(ylim = c(-0.35, 0.15)) +
  scale_linetype_manual(values = line_types) +
  scale_shape_manual(values = shapes) +
  scale_color_manual(values = cols) +
  guides(
    color = guide_legend(
      override.aes = list(
        linetype  = line_types[levels(df$model)],
        shape     = shapes[levels(df$model)],
        linewidth = 1.5,
        size      = 9
      )
    ),
    linetype = "none",
    shape = "none"
  ) +
  labs(x = NULL, y = NULL, color = NULL) +
  theme_bw() +
  theme(
    axis.title.y = element_blank(),
    axis.title.x = element_text(size = 30),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.text = element_text(size = 30),
    axis.text.x = element_text(size = 30),
    axis.text.y = element_text(size = 30),
    legend.key.size = unit(3, "cm"),
    #panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )


ggsave(file=paste0(output_dir,"/Fig-5-es-emissions-urban.pdf"),
       scale = 2,
       width = 12,
       height = 6,
       units = "in"
)

# Rural sample
#---------------------------------------------------------------------------------
rm(list=ls())
input_dir <- "../30_data_analysis"
output_dir <- "../60_results"

data <- read_dta(paste0(input_dir,"/att_and_CI_placebo_rural.dta"))
data <- data.frame(data)

df <- data.frame(est = c(data$d1[1:6], data$d2[1:6],data$d3[1:6]),
                 upper = c(data$UCI_1[1:6],data$UCI_2[1:6], data$UCI_3[1:6]),
                 lower = c(data$LCI_1[1:6],data$LCI_2[1:6], data$LCI_3[1:6]),
                 model = rep(c("CO2", "NOX", "GHG"), each = 6),
                 year = rep(c(2016,2017,2018,2019,2021,2022),3))  


# Models
df$model <- factor(df$model, levels = c("CO2", "NOX", "GHG"))



line_types <- c("CO2" = "solid", 
                "NOX" = "dotdash", 
                "GHG" = "longdash")

cols <- c("CO2" = "black", "NOX" = "grey30", "GHG" = "grey60")
shapes <- c("CO2" = 16, "NOX" = 17, "GHG" = 15)

pd <- position_dodge(0.8)

df %>%
  ggplot(aes(
    x = factor(year), y = est,
    group = model,
    linetype = model, shape = model, color = model
  )) +
  geom_line(position = pd, linewidth = 1.5) +
  geom_point(position = pd, size = 9) +
  geom_errorbar(
    aes(ymin = lower, ymax = upper),
    position = pd, width = .25, linewidth = 1.5
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 1) +
  geom_vline(xintercept = 4, linetype = "dashed", color = "black", linewidth = 1) +
  coord_cartesian(ylim = c(-0.4, 0.15)) +
  scale_y_continuous(breaks = seq(-0.4, 0.1, by = 0.1)) +
  scale_linetype_manual(values = line_types) +
  scale_shape_manual(values = shapes) +
  scale_color_manual(values = cols) +
  guides(
    color = guide_legend(
      override.aes = list(
        linetype  = line_types[levels(df$model)],
        shape     = shapes[levels(df$model)],
        linewidth = 1.5,
        size      = 9
      )
    ),
    linetype = "none",
    shape = "none"
  ) +
  labs(x = NULL, y = NULL, color = NULL) +
  theme_bw() +
  theme(
    axis.title.y = element_blank(),
    axis.title.x = element_text(size = 30),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.text = element_text(size = 30),
    axis.text.x = element_text(size = 30),
    axis.text.y = element_text(size = 30),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.key.size = unit(3, "cm")
  )


ggsave(file=paste0(output_dir,"/Fig-G.3-es-emissions-rural.pdf"),
       scale = 2,
       width = 12,
       height = 6,
       units = "in"
)




# Event study-SDID In time placebo
#--------------------------------
rm(list=ls())
input_dir <- "../30_data_analysis"
output_dir <- "../60_results"

data <- read_dta(paste0(input_dir,"/intimeplacebo_es_att_and_CI.dta"))
data <- data.frame(data)

df <- data.frame(est = c(data$d1[1:4], data$d2[1:4],data$d3[1:4]),
                 upper = c(data$UCI_1[1:4],data$UCI_2[1:4], data$UCI_3[1:4]),
                 lower = c(data$LCI_1[1:4],data$LCI_2[1:4], data$LCI_3[1:4]),
                 model = rep(c("CO2", "NOX", "GHG"), each = 4),
                 year = rep(c(2016,2017,2018,2019),3))  


# Models
df$model <- factor(df$model, levels = c("CO2", "NOX", "GHG"))



line_types <- c("CO2" = "solid", 
                "NOX" = "dotdash", 
                "GHG" = "longdash")

cols <- c("CO2" = "black", "NOX" = "grey30", "GHG" = "grey60")
shapes <- c("CO2" = 16, "NOX" = 17, "GHG" = 15)

pd <- position_dodge(0.8)

df %>%
  ggplot(aes(
    x = factor(year), y = est,
    group = model,
    linetype = model, shape = model, color = model
  )) +
  geom_line(position = pd, linewidth = 2) +
  geom_point(position = pd, size = 9) +
  geom_errorbar(
    aes(ymin = lower, ymax = upper),
    position = pd, width = 1, linewidth = 2
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 1) +
  geom_vline(xintercept = 3.5, linetype = "dashed", color = "black", linewidth = 1) +
  coord_cartesian(ylim = c(-0.1, 0.1)) +
  scale_linetype_manual(values = line_types) +
  scale_shape_manual(values = shapes) +
  scale_color_manual(values = cols) +
  guides(
    color = guide_legend(
      override.aes = list(
        linetype  = line_types[levels(df$model)],
        shape     = shapes[levels(df$model)],
        linewidth = 2,
        size      = 9
      )
    ),
    linetype = "none",
    shape = "none"
  ) +
  labs(x = NULL, y = NULL, color = NULL) +
  theme_bw() +
  theme(
    axis.title.y = element_blank(),
    axis.title.x = element_text(size = 30),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.text = element_text(size = 30),
    axis.text.x = element_text(size = 30),
    axis.text.y = element_text(size = 30),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.key.size = unit(3, "cm")
  )



ggsave(file=paste0(output_dir,"/Fig-G.1-es-intime-placebo.pdf"),
       scale = 2,
       width = 12,
       height = 6,
       units = "in"
)




#------------------------------------------------------------
# Event study-SDID Other sectors
#--------------------------------
rm(list=ls())


input_dir <- "../30_data_analysis"
output_dir <- "../60_results"

data <- read_dta(paste0(input_dir,"/othersectors_es_att_and_CI.dta"))
data <- data.frame(data)

df <- data.frame(est = c(data$d1[1:7], data$d2[1:7],data$d3[1:7]),
                 upper = c(data$UCI_1[1:7], data$UCI_2[1:7],data$UCI_3[1:7]),
                 lower = c(data$LCI_1[1:7],  data$LCI_2[1:7], data$LCI_3[1:7]),
                 model = rep(c( "Building", "All sectors","All sectors No Road"), each = 7),
                 year = rep(c(2016,2017,2018,2019,2020,2021,2022),3))  


# Models
df$model <- factor(df$model, levels = c( "Building",  "All sectors", "All sectors No Road"))

line_types <- c(
  "Building" = "solid",
  "All sectors" = "dotdash",
  "All sectors No Road" = "twodash"
)

cols <- c(
  "Building" = "grey40",
  "All sectors" = "grey10",
  "All sectors No Road" = "grey70"
)

shapes <- c(
  "Building" = 16,
  "All sectors" = 18,
  "All sectors No Road" = 15
)


pd <- position_dodge(0.8)

df %>%
  ggplot(aes(
    x = factor(year), y = est,
    group = model,
    linetype = model, shape = model, color = model
  )) +
  geom_line(position = pd, linewidth = 2) +
  geom_point(position = pd, size = 9) +
  geom_errorbar(
    aes(ymin = lower, ymax = upper),
    position = pd, width = 1, linewidth = 2
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 1) +
  geom_vline(xintercept = 3.5, linetype = "dashed", color = "black", linewidth = 1) +
  coord_cartesian(ylim = c(-0.45, 0.15)) +
  scale_linetype_manual(values = line_types) +
  scale_shape_manual(values = shapes) +
  scale_color_manual(values = cols) +
  guides(
    color = guide_legend(
      override.aes = list(
        linetype  = line_types[levels(df$model)],
        shape     = shapes[levels(df$model)],
        linewidth = 2,
        size      = 9
      )
    ),
    linetype = "none",
    shape = "none"
  ) +
  labs(x = NULL, y = NULL, color = NULL) +
  theme_bw() +
  theme(
    axis.title.y = element_blank(),
    axis.title.x = element_text(size = 30),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.text = element_text(size = 30),
    axis.text.x = element_text(size = 30),
    axis.text.y = element_text(size = 30),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.key.size = unit(3, "cm")
  )

ggsave(file=paste0(output_dir,"/Fig-G.4-es-other-sectors.pdf"),
       scale = 2,
       width = 12,
       height = 6,
       units = "in"
)







