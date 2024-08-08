# This script is used to extract GBIF occurrences (global or european) for species of interest and plot them on a map (world or Europe) alongside with occurrences on ARMS unit

# Install if needed, and load the following packages:
library(rgbif)
library(sf)
library(rnaturalearth)
library(ggplot2)

# Set working directory
setwd("~/invasivePaper/scripts/GBIF_AOO")

# Retrieve world map data
world_map <- ne_countries(scale = "medium", returnclass = "sf")

# Set frame for european maps
europe_bounds <- list(
  xmin = -30,  # Western limit
  xmax = 50,   # Eastern limit
  ymin = 18,   # Southern limit
  ymax = 80    # Northern limit
)

##-----SPECIES 1-----------------------------------
# Generate the convex hull polygon from the coordinates

# GBIF data preparation

# If EU focused, use polygons
polygon <- "POLYGON((-30 81, 50 81, 50 12, -30 12, -30 81))"

# Retrieve data /!\ previously check the number of occurrences on GBIF for each species and adjust limit (default is 500)
res <- occ_search(scientificName = "Ostreopsis ovata", limit = 1000) # World
num_records <- nrow(res$data) 
res <- occ_search(scientificName = "Ostreopsis ovata", limit = 1000, geometry = polygon) # For Europe

# Get metadata
date_info <- res$data$eventDate
summary(as.Date(date_info))

# Remove NA values from the data
gbif_coords <- na.omit(res$data[, c('decimalLongitude', 'decimalLatitude')])
# Calculate the convex hull indices
hpts <- chull(gbif_coords$decimalLongitude, gbif_coords$decimalLatitude)
# Close the loop on the convex hull by adding the first point at the end
hpts <- c(hpts, hpts[1])

# Load point of detection by ARMS
ARMS <- read.delim("~/invasivePaper/scripts/GBIF_AOO/O.ovata.txt")

## Build the map
gg <- ggplot() +
  geom_sf(data = world_map) +
  labs(title = "Global Occurrences of Ostreopsis ovata",
       x = "Longitude", y = "Latitude") +
  theme(legend.position = "top")

# OR Focused on EU
gg <- ggplot(data = world_map) +
  geom_sf() +
  coord_sf(xlim = c(europe_bounds$xmin, europe_bounds$xmax), ylim = c(europe_bounds$ymin, europe_bounds$ymax)) +
  ggtitle("Ostreopsis ovata in Europe") +
  theme_minimal()

# Add GBIF data points with a specific color and label for legend
gg <- gg + geom_point(data = gbif_coords, aes(x = decimalLongitude, y = decimalLatitude, color = "GBIF Occurrences"), size = 2, alpha = 0.8)

# Add ARMS data points with a different color and label for legend
gg <- gg + geom_point(data = ARMS, aes(x = decimalLongitude, y = decimalLatitude, color = "ARMS Occurrences"), size = 2, alpha = 0.8)

# Add convex hull lines to the plot using the GBIF data, ensuring consistent labeling for legend
gg <- gg + geom_polygon(data = gbif_coords[hpts, ], aes(x = decimalLongitude, y = decimalLatitude, color = "GBIF Extent of Occurrence"), fill = NA, size = 1)

# Define manual color scale with exact names used in the aes() mappings
gg <- gg + scale_color_manual(values = c("GBIF Occurrences" = "red", "ARMS Occurrences" = "blue", "GBIF Extent of Occurrence" = "red"),
                              name = "LEGEND:", 
                              breaks = c("GBIF Occurrences", "ARMS Occurrences", "GBIF Extent of Occurrence"),
                              labels = c("GBIF Occurrences", "ARMS Occurrences", "GBIF Extent of Occurrence"))


# Display the map
print(gg)

# Save the plot to a PDF file
ggsave("Ostreopsis_ovata_GBIF_EU.pdf", plot = gg, device = "pdf", width = 10, height = 8, units = "in")


##-----SPECIES 2-----------------------------------
# Generate the convex hull polygon from the coordinates

# GBIF data preparation

# If EU focused, use polygons
polygon <- "POLYGON((-30 81, 50 81, 50 12, -30 12, -30 81))"

# Retrieve data 
res <- occ_search(scientificName = "Bugula neritina", limit = 4000)
res <- occ_search(scientificName = "Bugula neritina", limit = 4000, geometry = polygon)

num_records <- nrow(res$data)
date_info <- res$data$eventDate
summary(as.Date(date_info))
# Remove NA values from the data
gbif_coords <- na.omit(res$data[, c('decimalLongitude', 'decimalLatitude')])
# Calculate the convex hull indices
hpts <- chull(gbif_coords$decimalLongitude, gbif_coords$decimalLatitude)
# Close the loop on the convex hull by adding the first point at the end
hpts <- c(hpts, hpts[1])


# Load point of detection by ARMS
ARMS <- read.delim("~/invasivePaper/scripts/GBIF_AOO/B.neritina.txt")

## Build the map
## Build the map
gg <- ggplot() +
  geom_sf(data = world_map) +
  labs(title = "Global Occurrences of Bugula neritina",
       x = "Longitude", y = "Latitude") +
  theme(legend.position = "top")

# If EU focus
gg <- ggplot(data = world_map) +
  geom_sf() +
  coord_sf(xlim = c(europe_bounds$xmin, europe_bounds$xmax), ylim = c(europe_bounds$ymin, europe_bounds$ymax)) +
  ggtitle("Bugula neritina in Europe") +
  theme_minimal()

# Add GBIF data points with a specific color and label for legend
gg <- gg + geom_point(data = gbif_coords, aes(x = decimalLongitude, y = decimalLatitude, color = "GBIF Occurrences"), size = 2, alpha = 0.8)

# Add ARMS data points with a different color and label for legend
gg <- gg + geom_point(data = ARMS, aes(x = decimalLongitude, y = decimalLatitude, color = "ARMS Occurrences"), size = 2, alpha = 0.8)

# Add convex hull lines to the plot using the GBIF data, ensuring consistent labeling for legend
gg <- gg + geom_polygon(data = gbif_coords[hpts, ], aes(x = decimalLongitude, y = decimalLatitude, color = "GBIF Extent of Occurrence"), fill = NA, size = 1)

# Define manual color scale with exact names used in the aes() mappings
gg <- gg + scale_color_manual(values = c("GBIF Occurrences" = "red", "ARMS Occurrences" = "blue", "GBIF Extent of Occurrence" = "red"),
                              name = "LEGEND:", 
                              breaks = c("GBIF Occurrences", "ARMS Occurrences", "GBIF Extent of Occurrence"),
                              labels = c("GBIF Occurrences", "ARMS Occurrences", "GBIF Extent of Occurrence"))

# Display the map
print(gg)

# Save the plot to a PDF file
ggsave("Bugula_neritina_GBIF_EU_nohull.pdf", plot = gg, device = "pdf", width = 10, height = 8, units = "in")

##-----SPECIES 3-----------------------------------

# Generate the convex hull polygon from the coordinates

# GBIF data preparation

# If EU focused, use polygons
polygon <- "POLYGON((-30 81, 50 81, 50 12, -30 12, -30 81))"

# Retrieve data 
res <- occ_search(scientificName = "Bougainvillia muscus", limit = 2000)
res <- occ_search(scientificName = "Bougainvillia muscus", limit = 2000, geometry = polygon)

num_records <- nrow(res$data)
date_info <- res$data$eventDate
summary(as.Date(date_info))
# Remove NA values from the data
gbif_coords <- na.omit(res$data[, c('decimalLongitude', 'decimalLatitude')])
# Calculate the convex hull indices
hpts <- chull(gbif_coords$decimalLongitude, gbif_coords$decimalLatitude)
# Close the loop on the convex hull by adding the first point at the end
hpts <- c(hpts, hpts[1])


# Load point of detection by ARMS
ARMS <- read.delim("~/invasivePaper/scripts/GBIF_AOO/B.muscus.txt")

## Build the map
## Build the map
gg <- ggplot() +
  geom_sf(data = world_map) +
  labs(title = "Global Occurrences of Bougainvillia muscus",
       x = "Longitude", y = "Latitude") +
  theme(legend.position = "top")

# If EU focus
gg <- ggplot(data = world_map) +
  geom_sf() +
  coord_sf(xlim = c(europe_bounds$xmin, europe_bounds$xmax), ylim = c(europe_bounds$ymin, europe_bounds$ymax)) +
  ggtitle("Bougainvillia muscus in Europe") +
  theme_minimal()

# Add GBIF data points with a specific color and label for legend
gg <- gg + geom_point(data = gbif_coords, aes(x = decimalLongitude, y = decimalLatitude, color = "GBIF Occurrences"), size = 2, alpha = 0.8)

# Add ARMS data points with a different color and label for legend
gg <- gg + geom_point(data = ARMS, aes(x = decimalLongitude, y = decimalLatitude, color = "ARMS Occurrences"), size = 2, alpha = 0.8)

# Add convex hull lines to the plot using the GBIF data, ensuring consistent labeling for legend
gg <- gg + geom_polygon(data = gbif_coords[hpts, ], aes(x = decimalLongitude, y = decimalLatitude, color = "GBIF Extent of Occurrence"), fill = NA, size = 1)

# Define manual color scale with exact names used in the aes() mappings
gg <- gg + scale_color_manual(values = c("GBIF Occurrences" = "red", "ARMS Occurrences" = "blue", "GBIF Extent of Occurrence" = "red"),
                              name = "LEGEND:", 
                              breaks = c("GBIF Occurrences", "ARMS Occurrences", "GBIF Extent of Occurrence"),
                              labels = c("GBIF Occurrences", "ARMS Occurrences", "GBIF Extent of Occurrence"))

# Display the map
print(gg)

# Save the plot to a PDF file
ggsave("Bougainvillia_muscus_GBIF_EU.pdf", plot = gg, device = "pdf", width = 10, height = 8, units = "in")

##-----SPECIES 4-----------------------------------
# Generate the convex hull polygon from the coordinates

# GBIF data preparation

# If EU focused, use polygons
polygon <- "POLYGON((-30 81, 50 81, 50 12, -30 12, -30 81))"

# Retrieve data 
res <- occ_search(scientificName = "Eucheilota menoni")
res <- occ_search(scientificName = "Eucheilota menoni", geometry = polygon)
num_records <- nrow(res$data)
date_info <- res$data$eventDate
summary(as.Date(date_info))
# Remove NA values from the data
gbif_coords <- na.omit(res$data[, c('decimalLongitude', 'decimalLatitude')])
# Calculate the convex hull indices
hpts <- chull(gbif_coords$decimalLongitude, gbif_coords$decimalLatitude)
# Close the loop on the convex hull by adding the first point at the end
hpts <- c(hpts, hpts[1])


# Load point of detection by ARMS
ARMS <- read.delim("~/invasivePaper/scripts/GBIF_AOO/E.menoni.txt")

## Build the map
## Build the map
gg <- ggplot() +
  geom_sf(data = world_map) +
  labs(title = "Global Occurrences of Eucheilota menoni",
       x = "Longitude", y = "Latitude") + 
  theme(legend.position = "top")

# If EU focus
gg <- ggplot(data = world_map) +
  geom_sf() +
  coord_sf(xlim = c(europe_bounds$xmin, europe_bounds$xmax), ylim = c(europe_bounds$ymin, europe_bounds$ymax)) +
  ggtitle("Eucheilota menoni in Europe") +
  theme_minimal()

# Add GBIF data points with a specific color and label for legend
gg <- gg + geom_point(data = gbif_coords, aes(x = decimalLongitude, y = decimalLatitude, color = "GBIF Occurrences"), size = 2, alpha = 0.8)

# Add ARMS data points with a different color and label for legend
gg <- gg + geom_point(data = ARMS, aes(x = decimalLongitude, y = decimalLatitude, color = "ARMS Occurrences"), size = 2, alpha = 0.8)

# Add convex hull lines to the plot using the GBIF data, ensuring consistent labeling for legend
gg <- gg + geom_polygon(data = gbif_coords[hpts, ], aes(x = decimalLongitude, y = decimalLatitude, color = "GBIF Extent of Occurrence"), fill = NA, size = 1)

# Define manual color scale with exact names used in the aes() mappings
gg <- gg + scale_color_manual(values = c("GBIF Occurrences" = "red", "ARMS Occurrences" = "blue", "GBIF Extent of Occurrence" = "red"),
                              name = "LEGEND:", 
                              breaks = c("GBIF Occurrences", "ARMS Occurrences", "GBIF Extent of Occurrence"),
                              labels = c("GBIF Occurrences", "ARMS Occurrences", "GBIF Extent of Occurrence"))

# Display the map
print(gg)

# Save the plot to a PDF file
ggsave("Eucheilota_menoni_GBIF_EU.pdf", plot = gg, device = "pdf", width = 10, height = 8, units = "in")


##-----SPECIES 5-----------------------------------
# Generate the convex hull polygon from the coordinates

# GBIF data preparation

# If EU focused, use polygons
polygon <- "POLYGON((-30 81, 50 81, 50 12, -30 12, -30 81))"

# Retrieve data 
res <- occ_search(scientificName = "Herdmania momus", limit = 1000, geometry = polygon)
res <- occ_search(scientificName = "Herdmania momus", limit = 1000)
num_records <- nrow(res$data)
date_info <- res$data$eventDate
summary(as.Date(date_info))
# Remove NA values from the data
gbif_coords <- na.omit(res$data[, c('decimalLongitude', 'decimalLatitude')])
# Calculate the convex hull indices
hpts <- chull(gbif_coords$decimalLongitude, gbif_coords$decimalLatitude)
# Close the loop on the convex hull by adding the first point at the end
hpts <- c(hpts, hpts[1])


# Load point of detection by ARMS
ARMS <- read.delim("~/invasivePaper/scripts/GBIF_AOO/H.momus.txt")

## Build the map
## Build the map
gg <- ggplot() +
  geom_sf(data = world_map) +
  labs(title = "Global Occurrences of Herdmania momus",
       x = "Longitude", y = "Latitude") +
  theme(legend.position = "top")

# If EU focus
gg <- ggplot(data = world_map) +
  geom_sf() +
  coord_sf(xlim = c(europe_bounds$xmin, europe_bounds$xmax), ylim = c(europe_bounds$ymin, europe_bounds$ymax)) +
  ggtitle("Herdmania momus in Europe") +
  theme_minimal()

# Add GBIF data points with a specific color and label for legend
gg <- gg + geom_point(data = gbif_coords, aes(x = decimalLongitude, y = decimalLatitude, color = "GBIF Occurrences"), size = 2, alpha = 0.8)

# Add ARMS data points with a different color and label for legend
gg <- gg + geom_point(data = ARMS, aes(x = decimalLongitude, y = decimalLatitude, color = "ARMS Occurrences"), size = 2, alpha = 0.8)

# Add convex hull lines to the plot using the GBIF data, ensuring consistent labeling for legend
gg <- gg + geom_polygon(data = gbif_coords[hpts, ], aes(x = decimalLongitude, y = decimalLatitude, color = "GBIF Extent of Occurrence"), fill = NA, size = 1)

# Define manual color scale with exact names used in the aes() mappings
gg <- gg + scale_color_manual(values = c("GBIF Occurrences" = "red", "ARMS Occurrences" = "blue", "GBIF Extent of Occurrence" = "red"),
                              name = "LEGEND:", 
                              breaks = c("GBIF Occurrences", "ARMS Occurrences", "GBIF Extent of Occurrence"),
                              labels = c("GBIF Occurrences", "ARMS Occurrences", "GBIF Extent of Occurrence"))

# Display the map
print(gg)

# Save the plot to a PDF file
ggsave("Herdmania_momus_GBIF_EU.pdf", plot = gg, device = "pdf", width = 10, height = 8, units = "in")

##-----SPECIES 6-----------------------------------
# Generate the convex hull polygon from the coordinates

# GBIF data preparation

# If EU focused, use polygons
polygon <- "POLYGON((-30 81, 50 81, 50 12, -30 12, -30 81))"

# Retrieve data 
res <- occ_search(scientificName = "Fenestrulina delicia")
res <- occ_search(scientificName = "Fenestrulina delicia", geometry = polygon)
num_records <- nrow(res$data)
date_info <- res$data$eventDate
summary(as.Date(date_info))
# Remove NA values from the data
gbif_coords <- na.omit(res$data[, c('decimalLongitude', 'decimalLatitude')])
# Calculate the convex hull indices
hpts <- chull(gbif_coords$decimalLongitude, gbif_coords$decimalLatitude)
# Close the loop on the convex hull by adding the first point at the end
hpts <- c(hpts, hpts[1])


# Load point of detection by ARMS
ARMS <- read.delim("~/invasivePaper/scripts/GBIF_AOO/F.delicia.txt")

## Build the map
## Build the map
gg <- ggplot() +
  geom_sf(data = world_map) +
  labs(title = "Global Occurrences of Fenestrulina delicia",
       x = "Longitude", y = "Latitude") +
  theme(legend.position = "top")

# If EU focus
gg <- ggplot(data = world_map) +
  geom_sf() +
  coord_sf(xlim = c(europe_bounds$xmin, europe_bounds$xmax), ylim = c(europe_bounds$ymin, europe_bounds$ymax)) +
  ggtitle("Fenestrulina delicia in Europe") +
  theme_minimal()

# Add GBIF data points with a specific color and label for legend
gg <- gg + geom_point(data = gbif_coords, aes(x = decimalLongitude, y = decimalLatitude, color = "GBIF Occurrences"), size = 2, alpha = 0.8)

# Add ARMS data points with a different color and label for legend
gg <- gg + geom_point(data = ARMS, aes(x = decimalLongitude, y = decimalLatitude, color = "ARMS Occurrences"), size = 2, alpha = 0.8)

# Add convex hull lines to the plot using the GBIF data, ensuring consistent labeling for legend
gg <- gg + geom_polygon(data = gbif_coords[hpts, ], aes(x = decimalLongitude, y = decimalLatitude, color = "GBIF Extent of Occurrence"), fill = NA, size = 1)

# Define manual color scale with exact names used in the aes() mappings
gg <- gg + scale_color_manual(values = c("GBIF Occurrences" = "red", "ARMS Occurrences" = "blue", "GBIF Extent of Occurrence" = "red"),
                              name = "LEGEND:", 
                              breaks = c("GBIF Occurrences", "ARMS Occurrences", "GBIF Extent of Occurrence"),
                              labels = c("GBIF Occurrences", "ARMS Occurrences", "GBIF Extent of Occurrence"))

# Display the map
print(gg)

# Save the plot to a PDF file
ggsave("Fenestrulina_delicia_GBIF_EU.pdf", plot = gg, device = "pdf", width = 10, height = 8, units = "in")


##-----SPECIES 7-----------------------------------
# Generate the convex hull polygon from the coordinates

# GBIF data preparation

# If EU focused, use polygons
polygon <- "POLYGON((-30 81, 50 81, 50 12, -30 12, -30 81))"

# Retrieve data 
res <- occ_search(scientificName = "Apionsoma (Apionsoma) misakianum")
res <- occ_search(scientificName = "Apionsoma (Apionsoma) misakianum", geometry = polygon)
num_records <- nrow(res$data) 
date_info <- res$data$eventDate
summary(as.Date(date_info))
# Remove NA values from the data
gbif_coords <- na.omit(res$data[, c('decimalLongitude', 'decimalLatitude')])
# Calculate the convex hull indices
hpts <- chull(gbif_coords$decimalLongitude, gbif_coords$decimalLatitude)
# Close the loop on the convex hull by adding the first point at the end
hpts <- c(hpts, hpts[1])


# Load point of detection by ARMS
ARMS <- read.delim("~/invasivePaper/scripts/GBIF_AOO/A.misakianum.txt")

## Build the map
## Build the map
gg <- ggplot() +
  geom_sf(data = world_map) +
  labs(title = "Global Occurrences of Apionsoma (Apionsoma) misakianum",
       x = "Longitude", y = "Latitude") +
  theme(legend.position = "top")

# If EU focus
gg <- ggplot(data = world_map) +
  geom_sf() +
  coord_sf(xlim = c(europe_bounds$xmin, europe_bounds$xmax), ylim = c(europe_bounds$ymin, europe_bounds$ymax)) +
  ggtitle("Apionsoma (Apionsoma) misakianum in Europe") +
  theme_minimal()

# Add GBIF data points with a specific color and label for legend
gg <- gg + geom_point(data = gbif_coords, aes(x = decimalLongitude, y = decimalLatitude, color = "GBIF Occurrences"), size = 2, alpha = 0.8)

# Add ARMS data points with a different color and label for legend
gg <- gg + geom_point(data = ARMS, aes(x = decimalLongitude, y = decimalLatitude, color = "ARMS Occurrences"), size = 2, alpha = 0.8)

# Add convex hull lines to the plot using the GBIF data, ensuring consistent labeling for legend
gg <- gg + geom_polygon(data = gbif_coords[hpts, ], aes(x = decimalLongitude, y = decimalLatitude, color = "GBIF Extent of Occurrence"), fill = NA, size = 1)

# Define manual color scale with exact names used in the aes() mappings
gg <- gg + scale_color_manual(values = c("GBIF Occurrences" = "red", "ARMS Occurrences" = "blue", "GBIF Extent of Occurrence" = "red"),
                              name = "LEGEND:", 
                              breaks = c("GBIF Occurrences", "ARMS Occurrences", "GBIF Extent of Occurrence"),
                              labels = c("GBIF Occurrences", "ARMS Occurrences", "GBIF Extent of Occurrence"))

# Display the map
print(gg)

# Save the plot to a PDF file
ggsave("Apionsoma_misakianum_GBIF_EU.pdf", plot = gg, device = "pdf", width = 10, height = 8, units = "in")
