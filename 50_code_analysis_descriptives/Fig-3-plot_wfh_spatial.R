

# *************************************************************************
#   Creator: Tobias Eibinger and Sachintha Fernando
#   Project: Zero Fare
#   CREATE WFH PLOTS (Fig 3, 4)
#   Date: 2026-04-05
# *************************************************************************


rm(list=ls())

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(terra)
library(sf)
library(geodata)
library(exactextractr)
library(spData)
library(maps)
library(data.table)
library(raster)
library(RColorBrewer)
library(tidyr)
library(dplyr)
library(kableExtra)
library(knitr)
library(ggplot2)

############
#wfh
############

#wfh data
wfh <- read.csv("../30_data_analysis/homework-Dec24.csv")
head(wfh)

wfh <- wfh %>%
  group_by(Nuts2) %>%  # Group by NUTS2 region code
  arrange(Year, .by_group = TRUE) %>% 
  mutate(rwfh = usually/total) %>%
  mutate(Change_usual = (usually - lag(usually, default = NA))/lag(usually, default = NA)*100) %>%  # Calculate the change
  ungroup() 
head(wfh)



wfh <- wfh %>%
  dplyr::filter(Year%in%c(2020,2021))

wfh %>% dplyr::filter(Nuts2=="LU00")


#get NUTS 2 boundaries form eurostat

#sfile <- st_read("../10_data_raw/nuts2-shapefiles/NUTS_RG_01M_2021_4326.shp")  #eurostat nuts regions shape file
sfile <- st_read("../10_data_raw/nuts2-shapefiles/NUTS_RG_20M_2021_4326.shp")  #lower resolution for plotting
sfile0 <- sfile[sfile$LEVL_CODE==0,]           #nuts0
sfile2 <- sfile[sfile$LEVL_CODE==2,]           #nuts2


#list of countries
countries <- c("AT", "BE", "BG", "HR", "CY", "CZ", "DK", "EE", "FI", "FR", "DE", "GR",
               "HU", "IS", "IE", "IT", "LV", "LI", "LT", "LU", "MT", "NL", "NO", "PL", "PT", 
               "RO", "SK", "SI", "ES", "SE", "CH", "UK")


sfile0 <- sfile0 %>%
  dplyr::filter(CNTR_CODE%in%c(countries))

sfile2 <- sfile2 %>%
  dplyr::filter(CNTR_CODE%in%c(countries))

sfile2 <- sfile2 %>%
  dplyr::filter(!NUTS_ID%in%c("FRA", "FRY", "FRY4", "FRY1", "FRY2", "FRY3", 
                              "FRY5", "ES70", "PT20", "PT30", "NO0B"))

# Filter countries 
countries_fs <- sfile2 %>%
  filter(!CNTR_CODE%in%c("BE", "CZ", "DK", "FI", "FR", "EL", 
                         "HU", "IT", "LU", "NL", "PL", "PT",
                         "SK", "ES"))

#merge with commuting data
head(wfh)
colnames(wfh)[4] <- "NUTS_ID"
colnames(wfh)[3] <- "YEAR"

merge20 <- sfile2 %>% 
  left_join(wfh[wfh$YEAR==2020,], by = "NUTS_ID")
merge20$YEAR <- as.numeric(merge20$YEAR)

merge20$Change_usual <- as.numeric(merge20$Change_usual)
glimpse(merge20)

merge21 <- sfile2 %>% 
  left_join(wfh[wfh$YEAR==2021,], by = "NUTS_ID")
merge21$YEAR <- as.numeric(merge21$YEAR)

merge21$Change_usual <- as.numeric(merge21$Change_usual)
glimpse(merge21)


# Calculate the common range for the color scale
common_range <- range(c(min(merge20$Change_usual, na.rm = TRUE), 
                        min(merge21$Change_usual, na.rm = TRUE),
                        max(merge20$Change_usual, na.rm = TRUE), 
                        max(merge21$Change_usual, na.rm = TRUE)))
common_range
common_range <- c(-300,300)
breaks <- seq(common_range[1], common_range[2], length.out = 11) 
breaks <- round(breaks,0)

# Choose a diverging color palette
diverging_palette <- colorRampPalette(brewer.pal(11, "RdBu"))

# 1. Create a zoomed-in plot of Luxembourg
luxembourg_data20 <- merge20[merge20$NUTS_ID == "LU00", ]  

luxembourg_plot20 <- ggplot(luxembourg_data20) +
  scale_fill_gradientn(colors = diverging_palette(100), limits = common_range,) +
  geom_sf(aes(fill = Change_usual), color = "black", lwd=0.05) +
  theme_void() +
  theme(legend.position = "none",
        plot.background = element_rect(color = "black", fill = "white", linewidth =  0.5),
        plot.title = element_text(size=8)) +  
  labs(title = "LU00")

extents <- st_bbox(merge20)
nuts2_plot20 <- ggplot(merge20) +
  geom_sf(aes(fill = Change_usual), color = "black", lwd=0.05) + 
  scale_fill_gradientn(colors = diverging_palette(100), limits = common_range,
                       breaks = breaks, 
                       guide = guide_colourbar(barwidth = 20, 
                                               barheight = 1)) +
  geom_sf(data = countries_fs, fill = "grey90", color = "black", lwd=0.05) +
  labs(title = "",
       fill = "") +
  theme_minimal() +
  theme(legend.position = "bottom", 
        plot.title = element_blank(),
        axis.text.x = element_blank(),  
        axis.text.y = element_blank(),  
        axis.title.x = element_blank(),  
        axis.title.y = element_blank(),
        axis.ticks = element_blank()) +   
  annotation_custom(ggplotGrob(luxembourg_plot20), xmin = extents$xmin, ymin = extents$ymin,
                    xmax = extents$xmin + diff(extents[c("xmin", "xmax")])/4, 
                    ymax = extents$ymin + diff(extents[c("ymin", "ymax")])/4)

nuts2_plot20 <- nuts2_plot20 +
  geom_sf(data = sfile0, inherit.aes = FALSE, fill = NA, color = "black", lwd=0.5) +
  coord_sf(xlim = c(extents$xmin, extents$xmax), ylim = c(extents$ymin, extents$ymax))
nuts2_plot20

luxembourg_data21 <- merge21[merge21$NUTS_ID == "LU00", ]  

luxembourg_plot21 <- ggplot(luxembourg_data21) +
  scale_fill_gradientn(colors = diverging_palette(100), limits = common_range) +
  geom_sf(aes(fill = Change_usual), color = "black", lwd=0.05) +
  theme_void() +
  theme(legend.position = "none",
        plot.background = element_rect(color = "black", fill = "white", linewidth =  0.5),
        plot.title = element_text(size=8)) +
  labs(title = "LU00")

nuts2_plot21 <- ggplot(merge21) +
  geom_sf(aes(fill = Change_usual), color = "black", lwd=0.05) +  
  scale_fill_gradientn(colors = diverging_palette(100), limits = common_range,
                       breaks = breaks,  # More breaks for the legend
                       guide = guide_colourbar(barwidth = 20,  
                                               barheight = 1)) +
  geom_sf(data = countries_fs, fill = "grey90", color = "black", lwd=0.05) +
  labs(title = "",
       fill = "") +
  theme_minimal() +
  theme(legend.position = "bottom", 
        plot.title = element_blank(),
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(), 
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(), 
        axis.ticks = element_blank()) +  
  annotation_custom(ggplotGrob(luxembourg_plot21), xmin = extents$xmin, ymin = extents$ymin, 
                    xmax = extents$xmin + diff(extents[c("xmin", "xmax")])/4, 
                    ymax = extents$ymin + diff(extents[c("ymin", "ymax")])/4)

nuts2_plot21 <- nuts2_plot21 +
  geom_sf(data = sfile0, inherit.aes = FALSE, fill = NA, color = "black", lwd=0.5) +
  coord_sf(xlim = c(extents$xmin, extents$xmax), ylim = c(extents$ymin, extents$ymax))
nuts2_plot21

library(gridExtra)
grid.arrange(nuts2_plot20, nuts2_plot21, ncol = 2)

wfh %>% dplyr::filter(NUTS_ID=="LU00")

ggsave(file="../60_results/Fig-3-a-change_wfh_20_fs.png",
       plot = nuts2_plot20,
       scale=1,
       width = 4.5, 
       units = "in"
)

ggsave(file="../60_results/Fig-3-b-change_wfh_21_fs.png",
       plot = nuts2_plot21,
       scale=1,
       width = 4.5,  
       height = 5,  
       units = "in"
)




##########################
# commuting inflow
# additional plot
# not in paper / analysis
##########################

#wfh data
wfh <- read.csv("../30_data_analysis/wfh_never-Dec24.csv")
head(wfh)

wfh[wfh$Never_all=="NA",] <- 0

wfh <- wfh %>%
  group_by(COREG_2DW) %>%  # Group by NUTS2 region code
  arrange(YEAR, .by_group = TRUE) %>%  
  mutate(Change_Never = (Never_all - lag(Never_all, default = NA))/lag(Never_all, default = NA)*100) %>% 
  ungroup() 
head(wfh)

# create table

filtered_wfh <- wfh %>%
  dplyr::select(YEAR, COREG_2DW, Never_all, Change_Never) %>%
  filter(YEAR%in%c(2020,2021))

reshaped_wfh <- filtered_wfh %>%
  pivot_wider(
    names_from = YEAR,
    values_from = c(Never_all, Change_Never),
    names_sep = "_"
  )

ordered_columns <- c("COREG_2DW", 
                     "Never_all_2020", "Change_Never_2020", 
                     "Never_all_2021", "Change_Never_2021")

reshaped_wfh <- reshaped_wfh %>%
  dplyr::select(all_of(ordered_columns))

print(reshaped_wfh)

kable(reshaped_wfh, "latex", booktabs = TRUE, align = 'c', caption = "Data Summary") %>%
  kable_styling(latex_options = c("striped", "scale_down"))


# View the transformed data
print(reshaped_wfh)

wfh <- wfh %>%
  dplyr::filter(YEAR%in%c(2020,2021))

#get NUTS 2 boundaries form eurostat

#sfile <- st_read("../10_data_raw/nuts2-shapefiles/NUTS_RG_01M_2021_4326.shp")  #eurostat nuts regions shape file
sfile <- st_read("../10_data_raw/nuts2-shapefiles/NUTS_RG_20M_2021_4326.shp")  #lower resolution for plotting

sfile0 <- sfile[sfile$LEVL_CODE==0,]           #nuts0
sfile2 <- sfile[sfile$LEVL_CODE==2,]           #nuts2


#list of countries
countries <- c("AT", "BE", "BG", "HR", "CY", "CZ", "DK", "EE", "FI", "FR", "DE", "GR",
               "HU", "IS", "IE", "IT", "LV", "LI", "LT", "LU", "MT", "NL", "NO", "PL", "PT", 
               "RO", "SK", "SI", "ES", "SE", "CH", "UK")

sfile0 <- sfile0 %>%
  dplyr::filter(CNTR_CODE%in%c(countries))

sfile2 <- sfile2 %>%
  dplyr::filter(CNTR_CODE%in%c(countries))

sfile2 <- sfile2 %>%
  dplyr::filter(!NUTS_ID%in%c("FRA", "FRY", "FRY4", "FRY1", "FRY2", 
                              "FRY3", "FRY5", "ES70", "PT20", "PT30", "NO0B"))

# Filter countries 
countries_fs <- sfile2 %>%
  filter(!CNTR_CODE%in%c("BE", "CZ", "DK", "FI", "FR", "EL", 
                         "HU", "IT", "LU", "NL", "PL", "PT",
                         "SK", "ES"))

#merge with commuting data
head(wfh)
colnames(wfh)[3] <- "NUTS_ID"

merge20 <- sfile2 %>% 
  left_join(wfh[wfh$YEAR==2020,], by = "NUTS_ID")
merge20$YEAR <- as.numeric(merge20$YEAR)
merge20$Never_all <- as.numeric(merge20$Never_all)
merge20$Change_Never <- as.numeric(merge20$Change_Never)
glimpse(merge20)

merge21 <- sfile2 %>% 
  left_join(wfh[wfh$YEAR==2021,], by = "NUTS_ID")
merge21$YEAR <- as.numeric(merge21$YEAR)
merge21$Never_all <- as.numeric(merge21$Never_all)
merge21$Change_Never <- as.numeric(merge21$Change_Never)
glimpse(merge21)


# Calculate the common range for the color scale
common_range <- range(c(min(merge20$Change_Never, na.rm = TRUE), 
                        min(merge21$Change_Never, na.rm = TRUE),
                        max(merge20$Change_Never, na.rm = TRUE), 
                        max(merge21$Change_Never, na.rm = TRUE)))

common_range <- c(-50,50)
breaks <- seq(common_range[1], common_range[2], length.out = 11) 
breaks <- round(breaks,0)

# Choose a diverging color palette
diverging_palette <- colorRampPalette(brewer.pal(11, "RdBu"))

# 1. Create a zoomed-in plot of Luxembourg
luxembourg_data20 <- merge20[merge20$NUTS_ID == "LU00", ] 

luxembourg_plot20 <- ggplot(luxembourg_data20) +
  scale_fill_gradientn(colors = diverging_palette(100), limits = common_range,) +
  geom_sf(aes(fill = Change_Never), color = "black", lwd=0.05) +
  theme_void() +
  theme(legend.position = "none",
        plot.background = element_rect(color = "black", fill = "white", linewidth =  0.5),
        plot.title = element_text(size=8)) + 
  labs(title = "LU00")

extents <- st_bbox(merge20)
nuts2_plot20 <- ggplot(merge20) +
  geom_sf(aes(fill = Change_Never), color = "black", lwd=0.05) + 
  scale_fill_gradientn(colors = diverging_palette(100), limits = common_range,
                       breaks = breaks, 
                       guide = guide_colourbar(barwidth = 20, 
                                               barheight = 1)) +
  geom_sf(data = countries_fs, fill = "grey90", color = "black", lwd=0.05) +
  labs(title = "",
       fill = "") +
  theme_minimal() +
  theme(legend.position = "bottom", 
        plot.title = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(), 
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(), 
        axis.ticks = element_blank()) +   
  annotation_custom(ggplotGrob(luxembourg_plot20), xmin = extents$xmin, ymin = extents$ymin, 
                    xmax = extents$xmin + diff(extents[c("xmin", "xmax")])/4, 
                    ymax = extents$ymin + diff(extents[c("ymin", "ymax")])/4)

nuts2_plot20 <- nuts2_plot20 +
  geom_sf(data = sfile0, inherit.aes = FALSE, fill = NA, color = "black", lwd=0.5) +
  coord_sf(xlim = c(extents$xmin, extents$xmax), ylim = c(extents$ymin, extents$ymax))
nuts2_plot20

luxembourg_data21 <- merge21[merge21$NUTS_ID == "LU00", ] 

luxembourg_plot21 <- ggplot(luxembourg_data21) +
  scale_fill_gradientn(colors = diverging_palette(100), limits = common_range) +
  geom_sf(aes(fill = Change_Never), color = "black", lwd=0.05) +
  theme_void() +
  theme(legend.position = "none",
        plot.background = element_rect(color = "black", fill = "white", linewidth =  0.5),
        plot.title = element_text(size=8)) + 
  labs(title = "LU00")

nuts2_plot21 <- ggplot(merge21) +
  geom_sf(aes(fill = Change_Never), color = "black", lwd=0.05) +
  scale_fill_gradientn(colors = diverging_palette(100), limits = common_range,
                       breaks = breaks,  
                       guide = guide_colourbar(barwidth = 20, 
                                               barheight = 1)) +
  geom_sf(data = countries_fs, fill = "grey90", color = "black", lwd=0.05) +
  labs(title = "",
       fill = "") +
  theme_minimal() +
  theme(legend.position = "bottom", 
        plot.title = element_blank(),
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(), 
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),  
        axis.ticks = element_blank()) +   
  
  annotation_custom(ggplotGrob(luxembourg_plot21), xmin = extents$xmin, ymin = extents$ymin, 
                    xmax = extents$xmin + diff(extents[c("xmin", "xmax")])/4, 
                    ymax = extents$ymin + diff(extents[c("ymin", "ymax")])/4)

nuts2_plot21 <- nuts2_plot21 +
  geom_sf(data = sfile0, inherit.aes = FALSE, fill = NA, color = "black", lwd=0.5) +
  coord_sf(xlim = c(extents$xmin, extents$xmax), ylim = c(extents$ymin, extents$ymax))
nuts2_plot21

library(gridExtra)
grid.arrange(nuts2_plot20, nuts2_plot21, ncol = 2)


wfh %>% dplyr::filter(NUTS_ID=="LU00")

ggsave(file="../60_results/change_com_inflow_20_fs.png",
       plot = nuts2_plot20,
       scale=1,
       width = 4.5, 
       height = 5, 
       units = "in" 
)

ggsave(file="../60_results/change_com_inflow_21_fs.png",
       plot = nuts2_plot21,
       scale=1,
       width = 4.5,  
       height = 5, 
       units = "in" 
)




