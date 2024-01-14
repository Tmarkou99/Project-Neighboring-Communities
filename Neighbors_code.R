library(sf)
library(tidyverse)
library(beepr)



# Data Preperation -------------------------------------------------

 shp <- st_read("C:/My_Files/University/ΠΜΣ/📑Thesis/top_dim_koinotites(ELSTAT)/TOP_DHM_KOIN.shp")  # shp data

 shp_backup <- shp
 
 shp <- shp %>% 
   select(KALCODE,geometry)
 
 
# First Degree Neighbors -------------------------------------------


{
  start.time <- Sys.time()
  
  neighbors <- as.data.frame(st_touches(shp$geometry))
  
  colnames(neighbors) <- c("KALCODE", "Neighbor")
  
  end.time <- Sys.time()
  time.taken <- round(end.time - start.time, 2)
  print(time.taken)


neighbors

shp_geom <- shp %>%
  select(KALCODE, geometry) 

shp_num <- shp_geom %>% 
  mutate(ID = row_number()) %>% 
  st_drop_geometry()

neighbors_df <- inner_join(neighbors, shp_num, by = c("KALCODE" = "ID"))
neighbors <- inner_join(neighbors_df, shp_num, by = c("Neighbor" = "ID"))


neighbors <- neighbors %>% 
  select("KALCODE" = KALCODE.y, "NEIGHBOR" = KALCODE.y.y) 
beep()
}



# The "Best" Neighbor -----------------------------------------------------



{
  start.time <- Sys.time()
  
  shp60 <- shp %>%
    select(KALCODE, geometry)
  
  
  find_best_neighbor <- function(community, shp, neighbors) {
    
    community_neighbors <- neighbors %>%
      filter(KALCODE == community$KALCODE) %>%
      pull(NEIGHBOR)
    
    
    neighbor_data <- shp %>%
      filter(KALCODE %in% community_neighbors)
    
    
    intersections <- st_intersection(community, neighbor_data, by_feature = TRUE)
    
    if (nrow(intersections) > 0) {
      
      border_lengths <- st_length(intersections)
      
      
      best_neighbor <- neighbor_data[which.max(border_lengths), ]
      
      
      result_df <- data.frame(
        KALCODE = community$KALCODE,
        max_border_length = max(border_lengths),
        neighbor_KALCODE = best_neighbor$KALCODE,
        stringsAsFactors = FALSE
      )
    } else {
      
      result_df <- data.frame(
        KALCODE = community$KALCODE,
        max_border_length = NA,
        neighbor_KALCODE = NA,
        stringsAsFactors = FALSE
      )
    }
    
    return(result_df)
  }
  
  
  result_df <- lapply(1:nrow(shp60), function(i) {
    find_best_neighbor(shp60[i, ], shp, neighbors)
  })
  
  
  result_df <- do.call(rbind, result_df)
  
  
  View(result_df)
  beep(sound = 8)
  end.time <- Sys.time()
  time.taken <- round(end.time - start.time, 2)
  print(time.taken)
}



# Second Degree Neighbor --------------------------------------------------

# Κρατάμε μόνο τις κοινότητες που έχουν πληθυσμό μεγαλύτερο από 2.000
#κάτοικους στην απογραφή του 2021

# Αυτές οι κοινότητες είναι οι αστικές ή urban 
# Κάτω από 2.000 θεωρούνται αγροτικές κοινότητες


urban_koin <- final %>% 
  filter(tot_Pop21 > 2000) %>% 
  select(KOD11)


glimpse(urban_koin)
glimpse(neighbors)



urban_koin_filtered <- urban_koin %>%
  filter(KOD11 %in% neighbors$KALCODE)

# First-degree neighbors
neighbors_first <- neighbors %>%
  inner_join(urban_koin_filtered, by = c("KALCODE" = "KOD11")) %>%
  select(KALCODE, NEIGHBOR)

# Second-degree
neighbors_second <- neighbors %>%
  inner_join(neighbors_first, by = c("KALCODE" = "NEIGHBOR"), relationship =
               "many-to-many") %>%
  select(KALCODE, NEIGHBOR)


urban_koin_nei <- neighbors_second %>%
  filter(KALCODE %in% urban_koin_filtered$KOD11)



glimpse(neighbors_first)
glimpse(neighbors_second)
glimpse(urban_koin_filtered)
View(neighbors_second)
View(urban_koin_nei)


length(unique(urban_koin_nei$KALCODE))


anti_join(urban_koin,urban_koin_nei, by = c("KOD11" = "KALCODE"))

anti_join(urban_koin_nei, urban_koin, by = c("KALCODE" = "KOD11"))


# Διγραμματική απεικόνηση
neighbors_second_shp <-  inner_join(neighbors_second, shp_geom, 
                                    by = c("NEIGHBOR" = "KALCODE"))


coordinates2011 <- coordinates2011 %>%
  filter(LEV11 == 8 & str_ends(KOD11, "01"))

coordinates2011 <- coordinates2011 %>%
  mutate(KOD11 = substr(KOD11, 1, 8))

neighbors_second_cord <- inner_join(neighbors_second, coordinates2011, 
                                    by = c("NEIGHBOR" = "KOD11")) 








# TELIKO) First & Second Degree Neighbors for Urban areas -------------------------

# Κρατάμε μόνο τις κοινότητες που έχουν πληθυσμό μεγαλύτερο από 2.000
#κάτοικους στην απογραφή του 2021.

# Αυτές οι κοινότητες είναι οι αστικές ή urban 
# Κάτω από 2.000 θεωρούνται αγροτικές κοινότητες.

urban_koin <- final %>%      # Αστικές κοινότητες
  filter(tot_Pop21 > 2000) %>% 
  select(KOD11)



#  Βλέπω τους πρώτους γείτονες των αστικών κοινοτήτων 
urban_koin_filtered <- urban_koin %>%
  filter(KOD11 %in% neighbors$KALCODE)

# Αποθηκεύω τους πρώτους γείτονες των αστικών κοινοτήτων
neighbors_first <- neighbors %>%
  inner_join(urban_koin_filtered, by = c("KALCODE" = "KOD11")) %>%
  select(KALCODE, NEIGHBOR)

# Για τις ίδιες κοινότητες βρίσκω τους δεύτερους γείτονες
neighbors_second <- neighbors %>%
  inner_join(neighbors_first, by = c("KALCODE" = "NEIGHBOR"), relationship =
               "many-to-many") %>%
  select(KALCODE, NEIGHBOR)

# Παίρνω μόνο αυτές που είναι αστικές 
# Αυτό μας ενδιαφέρει.
urban_koin_nei <- neighbors_second %>%
  filter(KALCODE %in% urban_koin_filtered$KOD11)

View(urban_koin_nei)


length(unique(neighbors_first$KALCODE))
length(unique(urban_koin_nei$KALCODE))
length(unique(urban_koin$KOD11))




urban_koin_shp <- inner_join(urban_koin_nei, shp,
                             by = c("neighbor_second" = "KALCODE"))

urban_koin_shp <- st_as_sf(urban_koin_shp)


coordinates2011 <- coordinates2011 %>%
  filter(LEV11 == 8 & str_ends(KOD11, "01"))

coordinates2011 <- coordinates2011 %>%
  mutate(KOD11 = substr(KOD11, 1, 8)) %>% 
  select(KOD11,lat,lon)

urban_koin_shp <- inner_join(urban_koin_shp, coordinates2011, by = "KOD11")

#Ελέγχω οποιαδήποτε τυχαία κοινότητα, έστω η 28010101

community <- urban_koin_shp %>%
  filter(KOD11 == "28010101") %>% 
  st_as_sf()

# leaflet map
map <- leaflet() %>%
  addTiles() 

map <- map %>%
  addPolygons(
    data = community,
    fillColor = "blue",  
    fillOpacity = 0.5,   
    color = "black",     
    weight = 1            
  )


map


