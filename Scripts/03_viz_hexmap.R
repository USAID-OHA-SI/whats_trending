## TEST HEX MAP

library(tidyverse)
library(vroom)
library(rnaturalearth)
library(sf)

dataout <- "Dataout"

ctry_sel <- "Nigeria"

#import dataset
df_repgap <- vroom(file.path(dataout, "HFR_TXCURR_munged.zip"))

#country border
  ctry_adm0 <- ne_countries(country = ctry_sel, scale = 'medium', returnclass = 'sf') %>% 
    st_geometry(NULL) %>% 
    st_transform(crs = st_crs(3857)) %>% 
    st_as_sf()

#create hex grid for country
  ctry_hex <- ctry_adm0 %>% 
    st_make_grid(what = 'polygons', cellsize = 30000, square = F) %>% 
    st_as_sf() 

#create id for merging
  ctry_hex <- ctry_hex %>% 
    mutate(id = row_number())

#limit dataset to country, ensure coords exist & transform from decimal degrees to projection in meters
  df_mapdata <- df_repgap %>% 
    filter(countryname == ctry_sel) %>% 
    filter_at(vars(latitude, longitude), any_vars(!is.na(.))) %>% 
    st_as_sf(coords = c("longitude", "latitude"),
             crs = st_crs(4326)) %>% 
    st_transform(crs = st_crs(3857))
  
#bind hex ids onto data for join post aggregation
  df_mapdata <- st_join(df_mapdata, ctry_hex, join = st_intersects)
  
#clip hexes to country border, BK => do this after the st_join
  ctry_hex <- ctry_hex %>% 
    st_intersection(ctry_adm0) 
  
#how many sites didn't map to a bin?
  df_mapdata %>% 
    select(-geometry) %>% 
    as_tibble() %>% 
    distinct(orgunituid, id) %>% 
    count(is.na(id))
  
  df_mapdata %>% 
    filter(is.na(id)) %>% 
    ggplot() +
    geom_sf() +
    geom_sf(data=ctry_adm0, fill=NA) +
    coord_sf() +
    theme_minimal()

#remove geometry and aggregate to calc hex completeness
  df_mapdata <- df_mapdata %>% 
    select(-geometry) %>% 
    as_tibble() %>% 
    group_by(countryname, iso, id, type) %>% 
    summarise_at(vars(has_hfr_reporting, is_datim_site), sum, na.rm = TRUE) %>% 
    ungroup()

#create completeness pre/post and reshape to get one variable
  df_mapdata <- df_mapdata %>% 
    mutate(reporting_rate = has_hfr_reporting/is_datim_site,
           status = case_when(reporting_rate <= .25 ~ paste0(type, "-low"),
                              reporting_rate < .75 ~ paste0(type, "-med"),
                              TRUE ~ paste0(type, "-high"))) %>% 
    select(-has_hfr_reporting, -is_datim_site, -reporting_rate)  %>% 
    spread(type, status) %>% 
    unite(position, c(pre, post), sep = ", ")


#fill colors for bivarate plot
  bivar_map <- c("pre-high, post-high" = "#e8e8e8",
                 "pre-low, post-high" = "#5ac8c8",
                 "pre-med, post-high" = "#ace4e4",
                 "pre-high, post-low" = "#be64ac",
                 "pre-low, post-low" = "#3b4994",
                 "pre-med, post-low" = "#8c62aa",
                 "pre-high, post-med" = "#dfb0d6",
                 "pre-low, post-med" = "#5698b9",
                 "pre-med, post-med" = "#a5add3")


#join aggregated data to hex
  df_mapdata <- left_join(ctry_hex, df_mapdata)

#inset map
  map1 <- df_mapdata %>% 
    ggplot() +
    geom_sf(aes(fill = position)) +
    scale_fill_manual(values = bivar_map) +
    theme_void()
  
  map2 <- ggplotGrob(
    ggplot() +
      geom_sf(data=ctry_hex, fill=NA) +
      theme_void()
  )

  inset = data.frame(
    lat = c(6, 6, 4, 4),
    lon = c(12, 14, 14, 12)
  )
  
  inset <- inset %>% 
    st_as_sf(coords = c("lon", "lat"), crs = st_crs(4326)) %>% 
    st_transform(crs = st_crs(3857)) %>% 
    st_bbox()
  
  map <- map1 +
    annotation_custom(grob = map2, 
                      xmin= inset$xmin, 
                      xmax = inset$xmax, 
                      ymin = inset$ymin, 
                      ymax = inset$ymax)

  map
  