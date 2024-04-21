
# Libraries ---------------------------------------------------------------




library(tidyverse)

library(ggplot2)

library(leaflet)

library(sf)

library(beepr)



# Data input --------------------------------------------------------------
start.time <- Sys.time()

shp <- st_read("C:/My_Files/University/ΠΜΣ/📑Thesis/top_dim_koinotites(ELSTAT)/TOP_DHM_KOIN.shp")  # shp data

shp_backup <- shp

shp <- shp %>% 
  select(KALCODE,geometry)


shp_geom <- shp %>%
  select(KALCODE, geometry) 

shp_num <- shp_geom %>% 
  mutate(ID = row_number()) %>% 
  st_drop_geometry()



# First and Second Neighbors ----------------------------------------------



  
first_degree_neighbors <- as.data.frame(st_touches(shp$geometry))


colnames(first_degree_neighbors) <- c("KALCODE", "First_Neighbor")
  
# Finding the second degree neighbors by joining with the first degree neighbors
neighboring_communities <- inner_join(first_degree_neighbors, first_degree_neighbors,
                                      by = c("First_Neighbor" = "KALCODE"),
                                      suffix = c(".1", ".2"),
                                      copy = TRUE)
  

colnames(neighboring_communities) <- c("KALCODE", "first_neighbor", "second_neighbor")
  

neighboring_communities <- inner_join(neighboring_communities,shp_num,by = c("KALCODE"="ID"))

neighboring_communities <- inner_join(neighboring_communities,shp_num,by = c("first_neighbor"="ID"))


neighboring_communities <- inner_join(neighboring_communities,shp_num,by = c("second_neighbor"="ID"))


neighboring_communities <- neighboring_communities %>% 
  select(KALCODE = "KALCODE.y",
         first_neighbor = "KALCODE.y.y",
         second_neighbor = "KALCODE")


# Best Neighbors ----------------------------------------------------------

{ # Is suggested to run this code as a whole for faster results 
  
  shp60 <- shp %>%
    select(KALCODE, geometry)
  
  neighbors <- neighboring_communities %>% 
    select(KALCODE, NEIGHBOR = "first_neighbor")
  
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
}


neighboring_communities <- result_df %>% 
  select(KALCODE, best_neighbor = "neighbor_KALCODE") %>% 
  inner_join(neighboring_communities, by = "KALCODE")


# Visualization -----------------------------------------------------------

community <- neighboring_communities %>% 
  filter(KALCODE == "18010101") %>% 
  select(KALCODE) %>%
  distinct() %>% 
  inner_join(shp_geom,by = "KALCODE")

community <- st_as_sf(community)

first_neighbor <- neighboring_communities %>% 
  filter(KALCODE == "18010101") %>% 
  select(first_neighbor) %>% 
  distinct() %>% 
  inner_join(shp_geom, by = c("first_neighbor" = "KALCODE"))

first_neighbor <- st_as_sf(first_neighbor)

best_neighbor <- neighboring_communities %>%
  filter(KALCODE == "18010101") %>%
  select(best_neighbor) %>% 
  distinct() %>% 
  inner_join(shp_geom, by = c("best_neighbor" = "KALCODE"))

best_neighbor <- st_as_sf(best_neighbor)

second_neighbor <- neighboring_communities  %>%
  filter(KALCODE == "18010101") %>%
  select(second_neighbor) %>% 
  distinct() %>% 
  inner_join(shp_geom, by = c("second_neighbor" = "KALCODE"))


second_neighbor <- st_as_sf(second_neighbor)


ggplot() +
  geom_sf(data = second_neighbor,fill = "white")+
  geom_sf(data = first_neighbor, fill = "lightblue") +
  geom_sf(data = best_neighbor, fill = "red") +
  geom_sf(data = community, fill = "yellow",) +
  labs(
    title = "Spatial Analysis of Ioannina",
    fill = "Legend",
    caption = "Lightblue: Neighbor\nRed: Best Neighbor\nYellow: Ioannina\nWhite:Second Degree Neighbors"
  ) +
  theme_classic()

