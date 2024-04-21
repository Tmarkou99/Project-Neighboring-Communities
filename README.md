# Project: Neighboring Communities

This code manipulates shapefiles (.shp files) containing the geometry of specific objects to identify neighboring objects. Additionally, it determines the 'best neighbor' for each object, defined as the one sharing the most border length.

The analysis utilizes actual data sourced from the official shape file provided by the Hellenic Statistical Authority, focusing specifically on municipal communities.

The package called "sf" contains everything in order to manipulate shape files and also many more functions, an sf object in R is based on the dataframe and has multiple columns with different variables, as well as a geometry column containing the spatial vector geometry.

You can find more info on the following link \
<https://r-spatial.github.io/sf/>

In our case, the KALCODE of each community is an identifier for every community that Hellenic Statistic Authority is using to refer to a certain administrative unit.

