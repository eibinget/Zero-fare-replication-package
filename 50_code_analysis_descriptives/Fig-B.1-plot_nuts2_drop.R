
# *************************************************************************
#   Creator: Tobias Eibinger and Sachintha Fernando
#   Project: Zero Fare
#   Bad controls
#   Create NUTS 2 regions Plot (Fig B.1)
#   Date: 2026-04-05
# *************************************************************************

rm(list = ls())

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(dplyr)
library(tidyr)
library(ggplot2)
library(sf)
library(grid)



sfile <- st_read("../10_data_raw/nuts2-shapefiles/NUTS_RG_20M_2021_4326.shp")  #eurostat nuts regions shape file
sfile0 <- sfile[sfile$LEVL_CODE==0,]           #nuts0
#sfile2 <- sfile[sfile$LEVL_CODE==1,]           #nuts1
sfile2 <- sfile[sfile$LEVL_CODE==2,]           #nuts2
#sfile2 <- sfile[sfile$LEVL_CODE==3,]          #nuts3


#list of countries
countries <- c("AT", "BE", "BG", "HR", "CY", "CZ", "DK", "EE", "FI", "FR", "DE", "GR",
               "HU", "IS", "IE", "IT", "LV", "LI", "LT", "LU", "MT", "NL", "NO", "PL", "PT", 
               "RO", "SK", "SI", "ES", "SE", "CH", "UK")

sfile0 <- sfile0 %>%
  dplyr::filter(CNTR_CODE%in%c(countries))

sfile2 <- sfile2 %>%
  dplyr::filter(CNTR_CODE%in%c(countries))

sfile2 <- sfile2 %>%
  dplyr::filter(!NUTS_ID%in%c("FRA", "FRY", "FRY4", "FRY1", "FRY2", "FRY3", "FRY5", 
                              "ES70", "PT20", "PT30", "NO0B"))

# List of regions to color
regions_to_color0 <- c("BE34", "BE33", "DEB2", "DEC0", "FRF3")

# Fully Free PT
regions_to_color1 <- c("PL84", "PL51", "PL22", "PL41", "PL63", "PL52",
                       "PL71", "PL91", "PL92",
                       "PL62","PL81", "PL21",
                       "PT17", "FRE1")

# Partially free PT / highly reduced fees
regions_to_color2 <- c("FRG0", "FRF1", "EL30", "FR10")


# Add a new column to indicate the color category
sfile2 <- sfile2 %>%
  mutate(color_region = case_when(
    NUTS_ID %in% regions_to_color0 ~ "color0",
    NUTS_ID %in% regions_to_color1 ~ "color1",
    NUTS_ID %in% regions_to_color2 ~ "color2",
    CNTR_CODE == "EE" ~ "color1",
    CNTR_CODE %in% c("AT", "DE") ~ "color_hidden",
    TRUE ~ "no_color"
  ))

# Everything that should be filled on the map
colored_regions <- sfile2 %>% 
  filter(color_region != "no_color")

# Only regions that should get numbers and appear in the right-hand list
numbered_regions <- sfile2 %>% 
  filter(color_region %in% c("color0", "color1", "color2")) %>%
  mutate(region_number = row_number())

# Get centroids for label placement
centroids <- st_centroid(numbered_regions) %>% 
  st_as_sf() %>%
  mutate(
    x = st_coordinates(geometry)[,1],
    y = st_coordinates(geometry)[,2]
  )

# Adjust the position for region PL92
centroids <- centroids %>%
  mutate(
    x_adjusted = ifelse(NUTS_ID == "PL92", x + 0.1, x),
    y_adjusted = ifelse(NUTS_ID == "PL92", y + 0.5, y)
  )


library(ggtext)
# Create the plot
legend_labels <- c("NUTS 2 ring around LU", "Fully free fares", "Partially free / reduced fares")
legend_colors <- c("color0", "color1", "color2")

n2plot <- ggplot() +
  geom_sf(data = sfile2, fill = "white", color = "black", size = 0.1, lwd = 0.1) +  
  geom_sf(data = colored_regions, aes(fill = color_region), color = "black", size = 0.4) +
  scale_fill_manual(
    values = c(
      color0 = "lightsalmon",
      color1 = "steelblue3",
      color2 = "lightblue",
      color_hidden = "lightblue"
    ),
    breaks = legend_colors,
    labels = legend_labels
  ) +
  theme_void() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.margin = unit(c(0,-5,0,-2), "cm"),
    legend.position = "bottom",
    legend.title = element_blank()
  )

# Get the bounding box of the colored regions
extents <- st_bbox(colored_regions)

# Add the adjusted text labels to the plot
n2plotn0 <- n2plot +
  geom_sf(data = sfile0, inherit.aes = FALSE, fill = NA, color = "black", lwd = 0.5) +
  coord_sf(xlim = c(extents$xmin, extents$xmax), ylim = c(extents$ymin, extents$ymax)) +
  geom_text(
    data = centroids,
    aes(x = x_adjusted, y = y_adjusted, label = region_number),
    color = "black", size = 4, nudge_y = 0.1, fontface = "bold"
  )

# Create the custom legend as a separate plot and sort by region number
legend_data <- as.data.frame(
  numbered_regions %>%
    st_drop_geometry() %>%
    dplyr::select(region_number, NUTS_ID)
) %>%
  arrange(region_number)

legend_plot <- ggplot(legend_data, aes(x = 1, y = region_number)) +
  geom_text(aes(label = paste(region_number, NUTS_ID, sep = " - ")), hjust = 0, size = 4) +
  scale_y_reverse() +
  theme_void() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.margin = unit(c(0, 0, 0, 0), "lines")
  )

library(cowplot)
# Combine the map plot and the legend plot using cowplot
combined_plot <- plot_grid(n2plotn0, legend_plot, ncol = 2, rel_widths = c(0.75, 0.25))

# Display the combined plot
print(combined_plot)

ggsave(file="../60_results/Fig-B.1-nuts2_drop_col.pdf", 
       plot=combined_plot,
       scale=1,
       width = 8,
       height = 6,
       units = "in"
)
  
