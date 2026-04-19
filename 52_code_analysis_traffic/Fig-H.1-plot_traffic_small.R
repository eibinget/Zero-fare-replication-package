

# *************************************************************************
#   Creator: Tobias Eibinger and Sachintha Fernando
#   Project: Zero Fare
#   PLOT TRAFFIC COUNTERS (Fig F.1)
#   Date: 2026-04-05
# *************************************************************************

rm(list=ls())


library(dplyr)
library(osmextract)
library(ggplot2)
library(sf)
library(haven)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

sfile <- terra::vect("../10_data_raw/nuts2-shapefiles/NUTS_RG_01M_2021_4326.shp") 
#sfile <- sfile[sfile$CNTR_CODE == "NL" & sfile$LEVL_CODE == 2, ]

nuts_sf <- st_as_sf(sfile)  # converts SpatVector -> sf


##########################
########## LUX
##########################

nuts2_lux <- nuts_sf %>%
  filter(CNTR_CODE == "LU", LEVL_CODE == 2)

lux_country <- nuts_sf %>%
  filter(CNTR_CODE == "LU", LEVL_CODE == 0) %>%
  st_union() %>%
  st_as_sf()

classes_lux <- c(
  "motorway", "motorway_link",
  "trunk", "trunk_link",
  "primary", "primary_link"
)

roads_lux <- oe_get(
  place = "Luxembourg",
  layer = "lines",
  provider = "geofabrik",
  extra_tags = c("highway", "name", "ref")
) |>
  filter(!is.na(highway), highway %in% classes_lux)

unique(roads_lux$ref)

roads_lux <- roads_lux %>%
  mutate(
    ref_clean = toupper(gsub("\\s+", "", ref))   # "A 3" -> "A3"
  ) %>%
  filter(
    !is.na(ref_clean),
    grepl("^[ABN]", ref_clean)
  )

roads_lux_crop <- st_crop(roads_lux, st_bbox(lux_country))

ggplot() +
  geom_sf(data = nuts2_lux, fill = "grey95", color = "grey60", linewidth = 0.2) +
  geom_sf(data = roads_lux_crop, color = "black", linewidth = 0.25, alpha = 0.7) +
  coord_sf(expand = FALSE) +
  theme_void()


# read LUX station points
stations_sf <- readRDS("../20_data_derived/Traffic/lux-stations_sf.rds")

ggplot() +
  geom_sf(data = nuts2_lux, fill = "grey95", color = "grey60", linewidth = 0.2) +
  geom_sf(data = roads_lux, color = "black", linewidth = 0.35, alpha = 0.8) +
  geom_sf(data = stations_sf, aes(color = NUTS_ID), size = 1.8, alpha = 0.9) +
  coord_sf(expand = FALSE) +
  theme_void() +
  guides(color = guide_legend(title = "NUTS2"))

ggsave(file="../60_results/Fig-H.1-Lu-traffic.pdf", 
       scale=1,
       width = 4,
       height = 4,
       units = "in"
)

##########################
# DK
##########################

# DK polygons
nuts2_dk <- nuts_sf %>% filter(CNTR_CODE == "DK", LEVL_CODE == 2)

dk_country <- nuts_sf %>%
  filter(CNTR_CODE == "DK", LEVL_CODE == 0) %>%
  st_union() %>%
  st_as_sf()

# DK roads (main network via OSM highway classes)
main_classes <- c("motorway","motorway_link",
                  "trunk","trunk_link",
                  "primary","primary_link")

roads_dk <- oe_get(
  place = "Denmark",
  layer = "lines",
  provider = "geofabrik",
  extra_tags = c("highway","name","ref")
) %>%
  filter(!is.na(highway), highway %in% main_classes)

roads_dk <- st_crop(roads_dk, st_bbox(dk_country))

ggplot() +
  geom_sf(data = nuts2_dk, fill = "grey95", color = "grey60", linewidth = 0.2) +
  geom_sf(data = roads_dk, color = "black", linewidth = 0.25, alpha = 0.7) +
  coord_sf(expand = FALSE) +
  theme_void()

# read DK station points
stations_sf_dk <- readRDS("../20_data_derived/Traffic/dk-stations_sf.rds")

ggplot() +
  geom_sf(data = nuts2_dk, fill = "grey95", color = "grey60", linewidth = 0.2) +
  geom_sf(data = roads_dk, color = "black", linewidth = 0.35, alpha = 0.8) +
  geom_sf(data = stations_sf_dk, aes(color = NUTS_ID), size = 1.8, alpha = 0.9) +
  coord_sf(expand = FALSE) +
  theme_void() +
  guides(color = guide_legend(title = "NUTS2"))


ggsave(file="../60_results/Fig-H.1-DK-traffic.pdf", 
       scale=1,
       width = 4,
       height = 4,
       units = "in"
)

##################################
# FI
##################################


# FI polygons
nuts2_fi <- nuts_sf %>% filter(CNTR_CODE == "FI", LEVL_CODE == 2)

fi_country <- nuts_sf %>%
  filter(CNTR_CODE == "FI", LEVL_CODE == 0) %>%
  st_union() %>%
  st_as_sf()

# FI roads (main network via OSM highway classes)
main_classes <- c("motorway","motorway_link",
                  "trunk","trunk_link",
                  "primary","primary_link")

roads_fi <- oe_get(
  place = "Finland",
  layer = "lines",
  provider = "geofabrik",
  extra_tags = c("highway","name","ref")
) %>%
  filter(!is.na(highway), highway %in% main_classes)

#-------------------------------------------------------------------------------
# the code lines below need to be run after the oe_get() function ran
#-------------------------------------------------------------------------------

roads_fi <- st_crop(roads_fi, st_bbox(fi_country))

ggplot() +
  geom_sf(data = nuts2_fi, fill = "grey95", color = "grey60", linewidth = 0.2) +
  geom_sf(data = roads_fi, color = "black", linewidth = 0.25, alpha = 0.7) +
  coord_sf(expand = FALSE) +
  theme_void()


#metadata

library(httr)
url <- "https://tie.digitraffic.fi/api/tms/v1/stations"  # TMS stations endpoint
resp <- GET(url, add_headers(`Digitraffic-User` = "te/zerofare-fi"))
stopifnot(status_code(resp) == 200)

geojson_txt <- content(resp, as = "text", encoding = "UTF-8")

tmp <- tempfile(fileext = ".geojson")
writeLines(geojson_txt, tmp)

tms_sf <- st_read(tmp, quiet = TRUE)
tms_sf
names(tms_sf)


# read FI station points
fi_station_list <- readRDS("../20_data_derived/Traffic/fi-station_list.rds")


tms_keyed <- tms_sf %>%
  mutate(name_key = toupper(trimws(name))) %>%
  st_zm(drop = TRUE)

fi_stations_pts <- tms_keyed %>%
  inner_join(fi_station_list, by = c("name_key" = "location_key"))

# Finland country polygon in 4326
fi_country <- nuts_sf %>%
  filter(CNTR_CODE == "FI", LEVL_CODE == 0) %>%
  st_union() %>%
  st_as_sf() %>%
  st_transform(4326)

# ensure points are 4326
fi_stations_pts <- st_transform(fi_stations_pts, 4326)

# intersect 
fi_stations_pts <- fi_stations_pts[
  st_intersects(fi_stations_pts, fi_country, sparse = FALSE)[, 1],
]

fi_stations_pts <- st_join(
  fi_stations_pts,
  nuts2_fi %>% dplyr::select(NUTS_ID, NUTS_NAME),
  join = st_intersects,
  left = TRUE
)

ggplot() +
  geom_sf(data = nuts2_fi, fill = "grey95", color = "grey60", linewidth = 0.2) +
  geom_sf(data = roads_fi, color = "black", linewidth = 0.35, alpha = 0.8) +
  geom_sf(data = fi_stations_pts, aes(color = NUTS_ID), size = 1.2, alpha = 0.9) +
  coord_sf(expand = FALSE) +
  theme_void() +
  guides(color = guide_legend(title = "NUTS2"))


ggsave(file="../60_results/Fig-H.1-FI.pdf", 
       scale=1,
       width = 4,
       height = 4,
       units = "in"
)

##################################
# NL
##################################


# NL polygons
nuts2_nl <- nuts_sf %>% filter(CNTR_CODE == "NL", LEVL_CODE == 2)

nl_country <- nuts_sf %>%
  filter(CNTR_CODE == "NL", LEVL_CODE == 0) %>%
  st_union() %>%
  st_as_sf()

# NL roads (main network via OSM highway classes)
main_classes <- c("motorway","motorway_link",
                  "trunk","trunk_link",
                  "primary","primary_link")

roads_nl <- oe_get(
  place = "Netherlands",
  layer = "lines",
  provider = "geofabrik",
  extra_tags = c("highway","name","ref")
) %>%
  filter(!is.na(highway), highway %in% main_classes)

#-------------------------------------------------------------------------------
# the code lines below need to be run after the oe_get() function ran
#-------------------------------------------------------------------------------

roads_nl <- st_crop(roads_nl, st_bbox(nl_country))

ggplot() +
  geom_sf(data = nuts2_nl, fill = "grey95", color = "grey60", linewidth = 0.2) +
  geom_sf(data = roads_nl, color = "black", linewidth = 0.25, alpha = 0.7) +
  coord_sf(expand = FALSE) +
  theme_void()


# read NL station points
stations_sf_nl <- readRDS("../20_data_derived/Traffic/nl-stations_sf.rds")

ggplot() +
  geom_sf(data = nuts2_nl, fill = "grey95", color = "grey60", linewidth = 0.2) +
  geom_sf(data = roads_nl, color = "black", linewidth = 0.35, alpha = 0.8) +
  geom_sf(data = stations_sf_nl, aes(color = nuts2_id), size = 1.2, alpha = 0.9) +
  coord_sf(expand = FALSE) +
  theme_void() +
  guides(color = guide_legend(title = "NUTS2"))

ggsave(file="../60_results/Fig-H.1-NL.pdf", 
       scale=1,
       width = 4,
       height = 4,
       units = "in"
)



