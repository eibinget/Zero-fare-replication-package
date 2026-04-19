
# *************************************************************************
#   Creator: Tobias Eibinger and Sachintha Fernando
#   Project: Zero Fare
#   Creates the ATT emission graphs (Fig 5)
#   Date: 2026-04-05
# *************************************************************************



library(dplyr)
library(ggplot2)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

input_dir <- "../30_data_analysis"
output_dir <- "../60_results"


library(ggplot2)
library(dplyr)
#----------------------
# SDID ATT - main specification
#----------------------
#1. CO2
att1 <- -0.06119
lower1 <-  -0.12297 
upper1 <-   0.00059

#2. Nox
att2 <-     -0.11088 
lower2 <-  -0.20107
upper2 <-   -0.02069
  
#3. GHG
att3 <-   -0.06155
lower3 <-    -0.12315  
upper3 <-   0.00005


#-----------------------------------------
att <- c(att3, att2, att1)
lower <- c(lower3, lower2, lower1)
upper <- c(upper3, upper2, upper1)
specification <- c( "CO2",  "NOX","GHG")


df <- data.frame( att, lower, upper, specification)

# Specify the order of levels
df$specification <- factor(df$specification, levels = c("CO2","NOX","GHG"))


line_types <- c("CO2" = "solid", 
                "NOX" = "dotdash", 
                "GHG" = "longdash")

cols <- c("CO2" = "black", "NOX" = "grey30", "GHG" = "grey60")
shapes <- c("CO2" = 16, "NOX" = 17, "GHG" = 15)

cap_width <- 0.1

df %>%
  ggplot(aes(
    x = specification,
    y = att,
    color = specification,
    shape = specification,
    linetype = specification
  )) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 1.5) +
  
  # vertical CI line
  geom_segment(
    aes(xend = specification, y = lower, yend = upper),
    linewidth = 2
  ) +
  
  # bottom cap
  geom_segment(
    aes(
      x    = as.numeric(specification) - cap_width,
      xend = as.numeric(specification) + cap_width,
      y    = lower,
      yend = lower
    ),
    linewidth = 2
  ) +
  
  # top cap
  geom_segment(
    aes(
      x    = as.numeric(specification) - cap_width,
      xend = as.numeric(specification) + cap_width,
      y    = upper,
      yend = upper
    ),
    linewidth = 2
  ) +
  
  geom_point(size = 10) +
  coord_cartesian(ylim = c(-0.25, 0.01)) +
  scale_color_manual(values = cols) +
  scale_shape_manual(values = shapes) +
  scale_linetype_manual(values = line_types) +
  labs(x = NULL, y = NULL, color = NULL, shape = NULL, linetype = NULL) +
  theme_bw() +
  theme(
    legend.position = "none",
    axis.text = element_text(size = 30),
    axis.text.y = element_text(size = 30),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank()
  )


ggsave(file=paste0(output_dir,"/Fig-4-percap-att-es-main.pdf"), 
       scale=2,
       width = 10,
       height = 7,
       units = "in"
)

