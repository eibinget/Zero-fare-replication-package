

# *************************************************************************
#   Creator: Tobias Eibinger and Sachintha Fernando
#   Project: Zero Fare
#   CREATE ATT PLOTS (Fig H.2 and H.3)
#   Date: 2026-04-05
# *************************************************************************

# Set wd to store plot exports
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(ggplot2)
library(dplyr)

#----------------------
# SDID ATT
#----------------------

# ATTs (rounded)
# run the script either with or without covariates

#----------------------
# no covariates
#----------------------

#1. Mo-Su
att1 <-  -0.07641
lower1 <-  -0.17329
upper1 <-  0.02047

#2. Mo-Fr
att2 <-  -0.07074
lower2 <-  -0.18624
upper2 <-   0.04476
  
#3. Sa-Su
att3 <-  -0.12013
lower3 <-  -0.17347
upper3 <- -0.06678

#-----------------------------------------
att <- c(att1, att2, att3)
lower <- c(lower1, lower2, lower3)
upper <- c(upper1, upper2, upper3)
specification <- c( "Mo-Su",  "Mo-Fr","Sa-Su")


df <- data.frame( att, lower, upper, specification)

# Specify the order of levels
df$specification <- factor(df$specification, levels = c( "Mo-Su",  "Mo-Fr","Sa-Su"))

cols = c("black", "grey80", "grey40")

line_types <- setNames(c("solid", "longdash", "dotdash"), levels(df$specification))

df %>%
  ggplot(aes(x = specification, y = att, col = specification, linetype = specification)) +
  geom_point(position = position_dodge(1), size = 10) +
  geom_errorbar(aes(ymin = lower, ymax = upper), position = position_dodge(1), width = 0.5, lwd = 3) +
  ylim(-0.25, 0.1) +
  xlab("") + ylab("") +
  labs(title = "") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 1) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none",
        axis.text = element_text(size = 50),
        axis.text.y = element_text(size = 50),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) +
  scale_color_manual(values = cols) +
  scale_linetype_manual(values = line_types)



ggsave(file="../60_results/Fig-H.2-att-traffic_counts.pdf", 
       scale=2,
       width = 10,  
       height = 7,  
       units = "in" 
)
#--------------------------------------------------


#----------------------
# with covariates
#----------------------

#1. Mo-Su
att1 <-  -0.04579
lower1 <-  -0.16079
upper1 <-  0.06922

#2. Mo-Fr
att2 <-  -0.04553
lower2 <-  -0.17781
upper2 <-   0.08676

#3. Sa-Su
att3 <-  -0.07131
lower3 <-  -0.14190
upper3 <- -0.00072

#-----------------------------------------
att <- c(att1, att2, att3)
lower <- c(lower1, lower2, lower3)
upper <- c(upper1, upper2, upper3)
specification <- c( "Mo-Su",  "Mo-Fr","Sa-Su")


df <- data.frame( att, lower, upper, specification)

# Specify the order of levels
df$specification <- factor(df$specification, levels = c( "Mo-Su",  "Mo-Fr","Sa-Su"))

cols = c("black", "grey80", "grey40")
#shape = c("1", "2", "3")

line_types <- setNames(c("solid", "longdash", "dotdash"), levels(df$specification))

df %>%
  ggplot(aes(x = specification, y = att, col = specification, linetype = specification)) +
  geom_point(position = position_dodge(1), size = 10) +
  geom_errorbar(aes(ymin = lower, ymax = upper), position = position_dodge(1), width = 0.5, lwd = 3) +
  ylim(-0.25, 0.1) +
  xlab("") + ylab("") +
  labs(title = "") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 1) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none",
        axis.text = element_text(size = 50),
        axis.text.y = element_text(size = 50),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) +
  scale_color_manual(values = cols) +
  scale_linetype_manual(values = line_types)



ggsave(file="../60_results/Fig-H.3-att-traffic_counts-covs.pdf", 
       scale=2,
       width = 10,  
       height = 7,  
       units = "in" 
)
#--------------------------------------------------
