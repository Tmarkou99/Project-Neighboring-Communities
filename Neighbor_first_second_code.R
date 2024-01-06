


# Finding the First and Second Degree Neighbors ---------------------------



# Libraries
library(tidyverse)
library(sf)





# ----------------------------------------------------------------------------
# 1) Finding the total Neighbors --------------------------------------------




# Χρειάζεται το αρχείο της ΕΛΣΤΑΤ με τα πολύγωνα των κοινοτήτων

shp <- shp %>% 
  select(KALCODE,geometry)


neighbors <- as.data.frame(st_touches(shp$geometry)) 

colnames(neighbors) <- c("KALCODE", "Neighbor")


shp_geom <- shp %>%     # κρατάω μόνο τα πολύγωνα
  select(KALCODE, geometry) 

shp_num <- shp_geom %>%  # κρατάω μόνο το ID της κάθε κοινότητας
  mutate(ID = row_number()) %>% 
  st_drop_geometry()

neighbors_df <- inner_join(neighbors, shp_num, by = c("KALCODE" = "ID"))

neighbors <- inner_join(neighbors_df, shp_num, by = c("Neighbor" = "ID"))


neighbors <- neighbors %>% 
  select("KALCODE" = KALCODE.y, "NEIGHBOR" = KALCODE.y.y) 


View(neighbors)



# ----------------------------------------------------------------------------

 # 2) Second Degree Neighbors  ------------------------------------------------

# Με αυτόν τον τρόπο βρίσκω όλους τους δεύτερους γείτονες όλων των κοινοτήτων


koin_all <- final %>%  
  select(KOD11)

# Πρώτοι γείτονες
neighbors_first_all <- neighbors %>%
  inner_join(koin_all, by = c("KALCODE" = "KOD11")) %>%
  select(KALCODE, NEIGHBOR)

View(neighbors_first_all)

# Δεύτεροι γείτονες
neighbors_second_all <- neighbors %>%
  inner_join(neighbors_first_all,
             by = "KALCODE", 
             relationship = "many-to-many") %>%
  select("KOD11" = KALCODE, 
         "neighbor_first" = NEIGHBOR.x,
         "neighbor_second" = NEIGHBOR.y)



View(neighbors_second_all)

glimpse(neighbors_first_all)
glimpse(neighbors_second_all)

length(unique(neighbors_first_all$KALCODE))  # 6090 Κοινότητες
length(unique(neighbors_second_all$KOD11)) # 6090 κοινότητες

length(unique(neighbors_second_all$neighbor_second))


# Keep only the Second Degree Urban Neighbors  --------------------------------


urban_koin <- final %>% 
  filter(tot_Pop21 > 2000) %>% 
  select(KOD11)    # 530 αστικές κοινότητες


urban_koin_nei <- inner_join(neighbors_second_all, urban_koin, by = "KOD11")

View(urban_koin_nei)      # αυτό μας ενδιαφέρει
glimpse(urban_koin_nei)
length(unique(urban_koin_nei$KOD11))  #συνολικά 519 αστικές κοινότητες έχουν 
                                      #δεύτερο γείτονα



# ---------------------------------------------------------------------------
# Adding polygons -----------------------------------------------------


urban_koin_shp <- inner_join(urban_koin_nei, shp,
                             by = c("neighbor_second" = "KALCODE"))

urban_koin_shp <- st_as_sf(urban_koin_shp)


coordinates2011 <- coordinates2011 %>%
  filter(LEV11 == 8 & str_ends(KOD11, "01"))

coordinates2011 <- coordinates2011 %>%
  mutate(KOD11 = substr(KOD11, 1, 8)) %>% 
  select(KOD11,lat,lon)

urban_koin_shp <- inner_join(urban_koin_shp, coordinates2011, by = "KOD11")
