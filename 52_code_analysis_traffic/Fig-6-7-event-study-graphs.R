

# *************************************************************************
#   Creator: Tobias Eibinger and Sachintha Fernando
#   Project: Zero Fare
#   CREATE EVENT STUDY PLOTS (Fig. 7, 8)
#   Date: 2026-04-05
# *************************************************************************


#--------------------------------
# Event study-SDID (Main analysis)
#--------------------------------

library(haven)
library(ggplot2)
library(dplyr)
library(zoo)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
data <- read_dta("../30_data_analysis/traffic_att_and_CI_placebo.dta")
data <- data.frame(data)

data <- data |>
  mutate(time_ym = as.yearmon(as.numeric(time) / 12 + 1960))

# -------------------------------
# Plot 1: no covariates
# -------------------------------
df_noprec <- bind_rows(
  data |> transmute(time_ym, est = d1, lower = LCI_1, upper = UCI_1,
                    model = "Mo-Su"),
  data |> transmute(time_ym, est = d2, lower = LCI_2, upper = UCI_2,
                    model = "Mo-Fr"),
  data |> transmute(time_ym, est = d3, lower = LCI_3, upper = UCI_3,
                    model = "Sa-Su")
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
            linewidth = 1.5) +
  geom_point(position = position_dodge(width = 0.5),
             size = 4) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                position = position_dodge(width = 0.5),
                width = .01, linewidth=1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  
  # show ALL months (breaks) and labels
  scale_x_continuous(
    breaks = sort(unique(df_noprec$t_index)),
    labels = format(df_noprec$time_ym[match(sort(unique(df_noprec$t_index)),
                                            df_noprec$t_index)], "%Y-%m")
  ) +
  
  scale_y_continuous(
    limits = c(-0.4, 0.2),
    breaks = seq(-0.4, 0.2, by = 0.2)
  ) +
  
  scale_linetype_manual(values = c(
    "Mo-Su" = "solid",
    "Mo-Fr" = "longdash",
    "Sa-Su" = "dotdash" 
  )) +
  scale_shape_manual(values = c(
    "Mo-Su" = 16,
    "Mo-Fr" = 17,
    "Sa-Su" = 15
  )) +
  scale_color_manual(values = c(
    "Mo-Su" = "black",
    "Mo-Fr" = "grey80",
    "Sa-Su" = "grey40"
  )) +
  theme_bw() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.title = element_text(size = 30),  
    axis.text = element_text(size = 30),  
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 30),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )
p1

ggsave(
  file = "../60_results/Fig-6-es-traffic_counts.pdf",
  scale = 2,
  width = 12,  
  height = 7, 
  units = "in"
)




# -------------------------------
# Plot 1: with covariates
# -------------------------------

library(haven)
library(ggplot2)
library(dplyr)
library(zoo)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
data <- read_dta("../30_data_analysis/traffic_att_and_CI_placebo.dta")
data <- data.frame(data)

data <- data |>
  mutate(time_ym = as.yearmon(as.numeric(time) / 12 + 1960))


# -------------------------------
# Plot 1: WITH covariates
# -------------------------------
df_prec <- bind_rows(
  data |> transmute(time_ym, est = d4, lower = LCI_4, upper = UCI_4,
                    model = "Mo-Su"),
  data |> transmute(time_ym, est = d5, lower = LCI_5, upper = UCI_5,
                    model = "Mo-Fr"),
  data |> transmute(time_ym, est = d6, lower = LCI_6, upper = UCI_6,
                    model = "Sa-Su")
)

df_prec <- df_prec |>
  arrange(time_ym) |>
  mutate(t_index = as.numeric(factor(time_ym, ordered = TRUE)))

vline_idx <- df_prec |>
  filter(time_ym == as.yearmon("2020-02")) |>
  distinct(t_index) |>
  pull(t_index)

p2 <- ggplot(df_prec,
             aes(x = t_index, y = est,
                 color = model,
                 linetype = model,
                 shape = model,
                 group = model)) +
  geom_vline(xintercept = vline_idx,
             color = "black", linetype="dashed") +
  geom_line(position = position_dodge(width = 0.5),
            linewidth = 1.5) +
  geom_point(position = position_dodge(width = 0.5),
             size = 4) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                position = position_dodge(width = 0.5),
                width = .01, linewidth=1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  
  # show ALL months (breaks) and labels
  scale_x_continuous(
    breaks = sort(unique(df_prec$t_index)),
    labels = format(df_prec$time_ym[match(sort(unique(df_prec$t_index)),
                                            df_prec$t_index)], "%Y-%m")
  ) +
  
  scale_y_continuous(
    limits = c(-0.4, 0.2),
    breaks = seq(-0.4, 0.2, by = 0.2)
  ) +
  
  scale_linetype_manual(values = c(
    "Mo-Su" = "solid",
    "Mo-Fr" = "longdash",
    "Sa-Su" = "dotdash" 
  )) +
  scale_shape_manual(values = c(
    "Mo-Su" = 16,
    "Mo-Fr" = 17,
    "Sa-Su" = 15
  )) +
  scale_color_manual(values = c(
    "Mo-Su" = "black",
    "Mo-Fr" = "grey80",
    "Sa-Su" = "grey40"
  )) +
  theme_bw() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.title = element_text(size = 30),
    axis.text = element_text(size = 30),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 30),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )
p2

ggsave(
  file = "../60_results/Fig-7-es-traffic_counts-covs.pdf",
  scale = 2,
  width = 12, 
  height = 7, 
  units = "in" 
)


