# Event study-SDID (Main analysis)
#--------------------------------

library(haven)
library(ggplot2)
library(dplyr)
library(zoo)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

data <- read_dta("../30_data_analysis/NO2_att_and_CI_placebo.dta")
data <- data.frame(data)

data <- data |>
  mutate(time_ym = as.yearmon(as.numeric(time) / 12 + 1960))


# Data notes
# 1. d1 - All no covariates
# 2. d2 - All with covariates 
# 3. d3 - Urban no covariates 
# 4. d4 - Urban with covariates



#-------------------------------------------------------------------
# Plot 1: No covariates (all stations + urban station)
#------------------------------------------------------------------

df_noprec <- bind_rows(
  data |> transmute(time_ym, est = d1, lower = LCI_1, upper = UCI_1,
                    model = "All stations"),
  data |> transmute(time_ym, est = d3, lower = LCI_3, upper = UCI_3,
                    model = "Urban stations")
)

df_noprec <- df_noprec |>
  arrange(time_ym) |>
  mutate(t_index = as.numeric(factor(time_ym, ordered = TRUE)))

vline_idx <- df_noprec |>
  filter(time_ym == as.yearmon("2020-02")) |>
  distinct(t_index) |>
  pull(t_index)

p1 <- ggplot(df_noprec,
             aes(x = t_index, y = est,
                 color = model,
                 linetype = model,
                 shape = model,
                 group = model)) +
  geom_vline(xintercept = vline_idx,
             color = "black", linetype="dashed") +
  geom_line(position = position_dodge(width = 0.5),
            linewidth = 0.7) +
  geom_point(position = position_dodge(width = 0.5),
             size = 2) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                position = position_dodge(width = 0.5),
                width = 0.01) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_x_continuous(
    breaks = df_noprec$t_index,
    labels = format(df_noprec$time_ym, "%Y-%m")
  ) +
  scale_linetype_manual(values = c(
    "All stations" = "solid",
    "Urban stations" = "longdash"
  )) +
  scale_shape_manual(values = c(
    "All stations" = 16,
    "Urban stations" = 17
  )) +
  scale_color_manual(values = c(
    "All stations" = "black",
    "Urban stations" = "grey60"
  )) +
  theme_bw() +
  theme(
    axis.title = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, size=13),
    axis.text.y = element_text(size=16),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.key.size=unit(2, "cm"),
    legend.text = element_text(size = 14), 
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )
p1

ggsave(
  file =("../60_results/Fig-8-airqual-no-covariates.pdf"),
  scale=1,
  width = 14, 
  height = 7,
  units = "in"
)




#----------------------------------------------------------------------------------
# Plot 2: With covariates (all stations + urban stations)
#------------------------------------------------------------------

df_noprec <- bind_rows(
  data |> transmute(time_ym, est = d2, lower = LCI_2, upper = UCI_2,
                    model = "All stations"),
  data |> transmute(time_ym, est = d4, lower = LCI_4, upper = UCI_4,
                    model = "Urban stations")
)

df_noprec <- df_noprec |>
  arrange(time_ym) |>
  mutate(t_index = as.numeric(factor(time_ym, ordered = TRUE)))

vline_idx <- df_noprec |>
  filter(time_ym == as.yearmon("2020-02")) |>
  distinct(t_index) |>
  pull(t_index)

p2 <- ggplot(df_noprec,
             aes(x = t_index, y = est,
                 color = model,
                 linetype = model,
                 shape = model,
                 group = model)) +
  geom_vline(xintercept = vline_idx,
             color = "black", linetype="dashed") +
  geom_line(position = position_dodge(width = 0.5),
            linewidth = 0.7) +
  geom_point(position = position_dodge(width = 0.5),
             size = 2) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                position = position_dodge(width = 0.5),
                width = 0.01) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_x_continuous(
    breaks = df_noprec$t_index,
    labels = format(df_noprec$time_ym, "%Y-%m")
  ) +
  scale_linetype_manual(values = c(
    "All stations" = "solid",
    "Urban stations" = "longdash"
  )) +
  scale_shape_manual(values = c(
    "All stations" = 16,
    "Urban stations" = 17
  )) +
  scale_color_manual(values = c(
    "All stations" = "black",
    "Urban stations" = "grey60"
  )) +
  theme_bw() +
  theme(
    axis.title = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, size=13),
    axis.text.y = element_text(size=16),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.key.size=unit(2, "cm"),
    legend.text = element_text(size = 14),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )
p2

ggsave(
  file =("../60_results/Fig-9-airqual-with-covariates.pdf"),
  scale=1,
  width = 14, 
  height = 7,
  units = "in"
)











