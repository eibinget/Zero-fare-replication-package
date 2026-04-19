# *************************************************************************
#   Creator: Tobias Eibinger and Sachintha Fernando
#   Project: Zero Fare
#   CREATE CO2 PLOTS (Fig 1)
#   Date: 2026-04-05
# *************************************************************************

rm(list = ls())

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(dplyr)
library(magrittr)
library(terra)
library(sf)
library(ggplot2)
library(geodata)
library(exactextractr)
library(spData)
library(maps)
library(tidyverse)
library(data.table)
library(raster)
library(RColorBrewer)
library(latticeExtra)


#get NUTS 2 boundaries form eurostat

sfile <- st_read("../10_data_raw/nuts2-shapefiles/NUTS_RG_01M_2021_4326.shp")  #eurostat nuts regions shape file
sfile0 <- sfile[sfile$LEVL_CODE==0,]           #nuts0
sfile2 <- sfile[sfile$LEVL_CODE==2,]           #nuts2

#extract countries at specified nuts level and plot boundaries


#Belgium
BE <- sfile2[sfile2$CNTR_CODE == "BE",]
BE0 <- sfile0[sfile0$CNTR_CODE == "BE",]
#plot(BE$geometry)

#Czech Republic
CZ <- sfile2[sfile2$CNTR_CODE == "CZ",]
CZ0 <- sfile0[sfile0$CNTR_CODE == "CZ",]
#plot(CZ$geometry)

#Denmark
DK <- sfile2[sfile2$CNTR_CODE == "DK",]
DK0 <- sfile0[sfile0$CNTR_CODE == "DK",]
#plot(DK$geometry)

#Finland
FI <- sfile2[sfile2$CNTR_CODE == "FI",]
FI0 <- sfile0[sfile0$CNTR_CODE == "FI",]
#plot(FI$geometry)

#France
FR <- sfile2[sfile2$CNTR_CODE == "FR",]
FR0 <- sfile0[sfile0$CNTR_CODE == "FR",]
#plot(FR$geometry)

#Greece
GR <- sfile2[sfile2$CNTR_CODE == "EL",]
GR0 <- sfile0[sfile0$CNTR_CODE == "EL",]
#plot(GR$geometry)

#Hungary
HU <- sfile2[sfile2$CNTR_CODE == "HU",]
HU0 <- sfile0[sfile0$CNTR_CODE == "HU",]
#plot(HU$geometry)

#Italy
IT <- sfile2[sfile2$CNTR_CODE == "IT",]
IT0 <- sfile0[sfile0$CNTR_CODE == "IT",]
#plot(IT$geometry)

#Luxembourg
LU <- sfile2[sfile2$CNTR_CODE == "LU",]
LU0 <- sfile0[sfile0$CNTR_CODE == "LU",]
#plot(LU$geometry)

#Netherlands
NL <- sfile2[sfile2$CNTR_CODE == "NL",]
NL0 <- sfile0[sfile0$CNTR_CODE == "NL",]
#plot(NL$geometry)

#Poland
PL <- sfile2[sfile2$CNTR_CODE == "PL",]
PL0 <- sfile0[sfile0$CNTR_CODE == "PL",]
#plot(PL$geometry)

#Portugal
PT <- sfile2[sfile2$CNTR_CODE == "PT",]
PT0 <- sfile0[sfile0$CNTR_CODE == "PT",]
#plot(PT$geometry)

#Slovakia
SK <- sfile2[sfile2$CNTR_CODE == "SK",]
SK0 <- sfile0[sfile0$CNTR_CODE == "SK",]
#plot(SK$geometry)

#Spain
ES <- sfile2[sfile2$CNTR_CODE == "ES",]
ES0 <- sfile0[sfile0$CNTR_CODE == "ES",]
#plot(ES$geometry)


#define regional boundaries
regions <- c(BE$geometry,
             CZ$geometry,
             DK$geometry,
             FI$geometry,
             FR$geometry,
             GR$geometry,
             HU$geometry,
             IT$geometry,
             LU$geometry,
             NL$geometry,
             PL$geometry,
             PT$geometry,
             SK$geometry,
             ES$geometry)

#store names of regional boundaries
names <- c(BE$NUTS_NAME,
           CZ$NUTS_NAME,
           DK$NUTS_NAME,
           FI$NUTS_NAME,
           FR$NUTS_NAME,
           GR$NUTS_NAME,
           HU$NUTS_NAME,
           IT$NUTS_NAME,
           LU$NUTS_NAME,
           NL$NUTS_NAME,
           PL$NUTS_NAME,
           PT$NUTS_NAME,
           SK$NUTS_NAME,
           ES$NUTS_NAME)

#store associated country codes
country <- c(BE$CNTR_CODE,
             CZ$CNTR_CODE,
             DK$CNTR_CODE,
             FI$CNTR_CODE,
             FR$CNTR_CODE,
             GR$CNTR_CODE,
             HU$CNTR_CODE,
             IT$CNTR_CODE,
             LU$CNTR_CODE,
             NL$CNTR_CODE,
             PL$CNTR_CODE,
             PT$CNTR_CODE,
             SK$CNTR_CODE,
             ES$CNTR_CODE)

#store associated nuts codes
nuts <- c(BE$NUTS_ID,
          CZ$NUTS_ID,
          DK$NUTS_ID,
          FI$NUTS_ID,
          FR$NUTS_ID,
          GR$NUTS_ID,
          HU$NUTS_ID,
          IT$NUTS_ID,
          LU$NUTS_ID,
          NL$NUTS_ID,
          PL$NUTS_ID,
          PT$NUTS_ID,
          SK$NUTS_ID,
          ES$NUTS_ID)


####################
# 2023
####################

# import NetCDF file from EDGAR (gridded emissions)
ncin_raw23 <- terra::rast("../10_data_raw/EDGAR/edgar-road-ncdf-co2/EDGAR_2025_GHG_CO2_2023_TRO_emi.nc")

# compute sum for administrative regions
dtrcts_co223 <- exactextractr::exact_extract(
  ncin_raw23,
  sf::st_as_sf(regions),
  "sum"
)

# create df
dtrcts_co223 <- data.frame(
  year = rep(2023, length(dtrcts_co223)),
  co2 = dtrcts_co223,
  NUTS3Code = nuts,
  NUTS3 = names,
  Country = country
)

dtrcts_co2_all <- dtrcts_co223

####################
# 2022
####################

ncin_raw22 <- terra::rast("../10_data_raw/EDGAR/edgar-road-ncdf-co2/EDGAR_2025_GHG_CO2_2022_TRO_emi.nc")

dtrcts_co222 <- exactextractr::exact_extract(
  ncin_raw22,
  sf::st_as_sf(regions),
  "sum"
)

dtrcts_co222 <- data.frame(
  year = rep(2022, length(dtrcts_co222)),
  co2 = dtrcts_co222,
  NUTS3Code = nuts,
  NUTS3 = names,
  Country = country
)

dtrcts_co2_all <- rbind(dtrcts_co2_all, dtrcts_co222)


####################
# 2021
####################

ncin_raw21 <- terra::rast("../10_data_raw/EDGAR/edgar-road-ncdf-co2/EDGAR_2025_GHG_CO2_2021_TRO_emi.nc")

dtrcts_co221 <- exactextractr::exact_extract(
  ncin_raw21,
  sf::st_as_sf(regions),
  "sum"
)

dtrcts_co221 <- data.frame(
  year = rep(2021, length(dtrcts_co221)),
  co2 = dtrcts_co221,
  NUTS3Code = nuts,
  NUTS3 = names,
  Country = country
)

dtrcts_co2_all <- rbind(dtrcts_co2_all, dtrcts_co221)


####################
# 2020
####################

ncin_raw20 <- terra::rast("../10_data_raw/EDGAR/edgar-road-ncdf-co2/EDGAR_2025_GHG_CO2_2020_TRO_emi.nc")

dtrcts_co220 <- exactextractr::exact_extract(
  ncin_raw20,
  sf::st_as_sf(regions),
  "sum"
)

dtrcts_co220 <- data.frame(
  year = rep(2020, length(dtrcts_co220)),
  co2 = dtrcts_co220,
  NUTS3Code = nuts,
  NUTS3 = names,
  Country = country
)

dtrcts_co2_all <- rbind(dtrcts_co2_all, dtrcts_co220)


####################
# 2019
####################

ncin_raw19 <- terra::rast("../10_data_raw/EDGAR/edgar-road-ncdf-co2/EDGAR_2025_GHG_CO2_2019_TRO_emi.nc")

dtrcts_co219 <- exactextractr::exact_extract(
  ncin_raw19,
  sf::st_as_sf(regions),
  "sum"
)

dtrcts_co219 <- data.frame(
  year = rep(2019, length(dtrcts_co219)),
  co2 = dtrcts_co219,
  NUTS3Code = nuts,
  NUTS3 = names,
  Country = country
)

dtrcts_co2_all <- rbind(dtrcts_co2_all, dtrcts_co219)


####################
# 2018
####################

ncin_raw18 <- terra::rast("../10_data_raw/EDGAR/edgar-road-ncdf-co2/EDGAR_2025_GHG_CO2_2018_TRO_emi.nc")

dtrcts_co218 <- exactextractr::exact_extract(
  ncin_raw18,
  sf::st_as_sf(regions),
  "sum"
)

dtrcts_co218 <- data.frame(
  year = rep(2018, length(dtrcts_co218)),
  co2 = dtrcts_co218,
  NUTS3Code = nuts,
  NUTS3 = names,
  Country = country
)

dtrcts_co2_all <- rbind(dtrcts_co2_all, dtrcts_co218)


####################
# 2017
####################

ncin_raw17 <- terra::rast("../10_data_raw/EDGAR/edgar-road-ncdf-co2/EDGAR_2025_GHG_CO2_2017_TRO_emi.nc")

dtrcts_co217 <- exactextractr::exact_extract(
  ncin_raw17,
  sf::st_as_sf(regions),
  "sum"
)

dtrcts_co217 <- data.frame(
  year = rep(2017, length(dtrcts_co217)),
  co2 = dtrcts_co217,
  NUTS3Code = nuts,
  NUTS3 = names,
  Country = country
)

dtrcts_co2_all <- rbind(dtrcts_co2_all, dtrcts_co217)


####################
# 2016
####################

ncin_raw16 <- terra::rast("../10_data_raw/EDGAR/edgar-road-ncdf-co2/EDGAR_2025_GHG_CO2_2016_TRO_emi.nc")

dtrcts_co216 <- exactextractr::exact_extract(
  ncin_raw16,
  sf::st_as_sf(regions),
  "sum"
)

dtrcts_co216 <- data.frame(
  year = rep(2016, length(dtrcts_co216)),
  co2 = dtrcts_co216,
  NUTS3Code = nuts,
  NUTS3 = names,
  Country = country
)

dtrcts_co2_all <- rbind(dtrcts_co2_all, dtrcts_co216)
#################

colnames(dtrcts_co2_all) <- c("year", "co2", "nuts2code", "nuts2name", "country")


# plot LU emissions for 2022
LU_outline <- terra::vect(sf::st_as_sf(sf::st_make_valid(LU)))
ncin22 <- terra::crop(ncin_raw22, LU_outline, snap = "out")
ncin22 <- terra::mask(ncin22, LU_outline, touches = TRUE)
par(mfrow = c(1,1))
plot(ncin22[[1]], main = "LU 2022")
plot(LU_outline, add = TRUE, border = "black", lwd = 0.6)
plot(sf::st_geometry(LU$geometry), add = TRUE)


# compare emissions over time

ncin22 = terra::mask(terra::crop(ncin_raw22, LU_outline, snap = "out", extend=T), LU_outline, touches = T)
ncin21 = terra::mask(terra::crop(ncin_raw21, LU_outline, snap = "out", extend=T), LU_outline, touches = T)
ncin20 = terra::mask(terra::crop(ncin_raw20, LU_outline, snap = "out", extend=T), LU_outline, touches = T)
ncin19 = terra::mask(terra::crop(ncin_raw19, LU_outline, snap = "out", extend=T), LU_outline, touches = T)

rasters <- list(ncin19, ncin20, ncin21)

brks <- seq(0, 500000, by=50000)
pal <- colorRampPalette(brewer.pal(9, "YlOrRd"))(10)

par(mfrow=c(1,3))
for (r in seq_along(rasters)) {
  raster::plot(rasters[[r]][[1]], zlim=c(0, 400000), axes=FALSE, breaks=brks, col=pal,
               legend=FALSE, main=paste("LU", 2021 - length(rasters) + r))
  plot(LU_outline, add=TRUE, border="black", lwd=0.6)
  plot(st_geometry(LU$geometry), add=TRUE)
}



#############
# ggplot
# pre and post 
#############

LU_vect <- terra::as.polygons(ncin22[[1]], dissolve = FALSE)
LU_vect <- sf::st_as_sf(LU_vect)
# compute avgs
ncin22 = terra::mask(terra::crop(ncin_raw22, LU_outline, snap = "out", extend=T), LU_outline, touches = T)
ncin21 = terra::mask(terra::crop(ncin_raw21, LU_outline, snap = "out", extend=T), LU_outline, touches = T)
ncin20 = terra::mask(terra::crop(ncin_raw20, LU_outline, snap = "out", extend=T), LU_outline, touches = T)
ncin19 = terra::mask(terra::crop(ncin_raw19, LU_outline, snap = "out", extend=T), LU_outline, touches = T)

ncin18 = terra::mask(terra::crop(ncin_raw18, LU_outline, snap = "out", extend=T), LU_outline, touches = T)
ncin17 = terra::mask(terra::crop(ncin_raw17, LU_outline, snap = "out", extend=T), LU_outline, touches = T)
ncin16 = terra::mask(terra::crop(ncin_raw16, LU_outline, snap = "out", extend=T), LU_outline, touches = T)

# Group rasters by period
rasters_2016_2019 <- list(ncin16, ncin17, ncin18, ncin19)
rasters_2020_2022 <- list(ncin20, ncin21, ncin22)

raster_stack_2016_2019 <- rast(rasters_2016_2019)
raster_stack_2020_2022 <- rast(rasters_2020_2022)

# Compute the average for 2016-2019
avg_2016_2019 <- app(raster_stack_2016_2019, fun = mean)

# Compute the average for 2020-2021
avg_2020_2022 <- app(raster_stack_2020_2022, fun = mean)

# Compute the percentage change
percent_change = 100 * (avg_2020_2022 - avg_2016_2019) / avg_2016_2019


# Convert SpatRaster to data frame for plotting
df_avg_2016_2019 <- as.data.frame(avg_2016_2019, xy = TRUE)
df_percent_change <- as.data.frame(percent_change, xy = TRUE)


# Define the base plotting function
base_plot <- function(data, main = "", fill_palette, limits,
                      border1 = 0.08, border2 = 0.15, interpolate = TRUE) {
  
  use_linewidth <- utils::packageVersion("ggplot2") >= "3.4.0"
  
  border_args1 <- if (use_linewidth) list(linewidth = border1) else list(size = border1)
  border_args2 <- if (use_linewidth) list(linewidth = border2) else list(size = border2)
  
  ggplot(data) +
    geom_raster(aes(x = x, y = y, fill = mean), interpolate = interpolate) +
    do.call(geom_sf, c(list(data = LU_vect, fill = NA, color = "black"), border_args1)) +
    do.call(geom_sf, c(list(data = LU,      fill = NA, color = "black"), border_args2)) +
    scale_fill_gradientn(
      colors = fill_palette, limits = limits, name = "",
      guide = guide_colorbar(barwidth = 12, barheight = 1.2)
    ) +
    labs(title = main) +
    coord_sf() +
    scale_fill_gradientn(
      colors = fill_palette, limits = limits, name = "",
      guide = guide_colorbar(
        barwidth  = unit(0.95, "npc"),
        barheight = unit(0.6, "cm"),
        label.theme = element_text(size = 16)
      )
    ) +
    theme_minimal() +
    theme(
      axis.title = element_blank(),
      axis.text = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.title = element_text(hjust = 0.5),
      legend.position = "bottom"
    )
}

# Define color palettes
pal_avg_emissions <- colorRampPalette(brewer.pal(9, "Blues"))(100)
pal_percent_change <- colorRampPalette(brewer.pal(11, "RdBu"))(100)
limits_percent_change <- c(min(df_percent_change$mean, na.rm = TRUE), 
                           0)



# Plot the overall average emissions 2016-2021
plot_avg_2016_2019 <- base_plot(
  df_avg_2016_2019,
  main = "",
  fill_palette = pal_avg_emissions,
  limits = range(df_avg_2016_2019$mean, na.rm = TRUE),
  border1 = 0.03,
  border2 = 1,
  interpolate = TRUE
)
plot_avg_2016_2019 

ggsave(file="../60_results/Fig1b.png", 
       scale=1,
       width = 4,  
       height = 6,
       units = "in"
)

# Plot the percentage change between the averages
plot_percent_change <- base_plot(
  df_percent_change,
  main = "",
  fill_palette = pal_percent_change,
  limits = limits_percent_change,
  border1 = 0.03,
  border2 = 1,
  interpolate = TRUE
)
plot_percent_change

ggsave(file="../60_results/Fig1c.png", 
       scale=1,
       width = 4, 
       height = 6, 
       units = "in" 
)

# Arrange the plots side by side
gridExtra::grid.arrange(plot_avg_2016_2019, plot_percent_change, nrow = 1)


# panelview
library(panelView)

# Compute log-transformed CO2 values and normalize to start at zero in 2016
dtrcts_co2_all_std <- dtrcts_co2_all %>%
  group_by(nuts2code) %>%
  mutate(
    log_co2 = log(co2),
    co2_std = log_co2 - log_co2[year == 2016]
  ) %>%
  filter(year<=2022) %>%
  ungroup()

highlight_region <- "LU00"


ggplot(dtrcts_co2_all_std, aes(x = year, y = co2_std, group = nuts2code)) +
  geom_line(
    data = filter(dtrcts_co2_all_std, nuts2code != highlight_region),
    aes(color = "Other Regions", alpha = 0.2),
    size = 0.5
  ) +
  geom_line(
    data = filter(dtrcts_co2_all_std, nuts2code == highlight_region),
    aes(color = "Highlighted Region"),
    size = 1
  ) +
  scale_color_manual(
    values = c("Other Regions" = "grey75", "Highlighted Region" = "black"),
    guide = "none"
  ) +
  scale_alpha_identity() +
  labs(
    x = NULL,  
    y = NULL,
    title = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.x = element_blank(), 
    panel.grid.major.y = element_line(),
    panel.grid.minor = element_blank(), 
    axis.line = element_line(color = "black")
  )


ggsave(file="../60_results/Fig1a.pdf",
       scale=1,
       width = 10,
       height = 4,
       units = "in"
)


# LU change post vs pre
dtrcts_co2_all_std_lu <- dplyr::filter(dtrcts_co2_all_std, nuts2code == "LU00")

# post-treatment avg
avg_2020_2022_lu <- dtrcts_co2_all_std_lu %>%
  dplyr::filter(year%between%c(2020,2022)) %>%
  dplyr::summarise(avg_2020_2022 = mean(co2, na.rm = TRUE))

# pre-treatment avg
avg_2016_2019_lu <- dtrcts_co2_all_std_lu %>%
  dplyr::filter(year%between%c(2016,2019)) %>%
  dplyr::summarise(avg_2016_2019 = mean(co2, na.rm = TRUE))

# percentage change post vs. pre
100 * (avg_2020_2022_lu - avg_2016_2019_lu) / avg_2016_2019_lu




