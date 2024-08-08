## This script is used for graphical visualization of taxa in the NIS list ##

# Install if needed, and load required libraries
library(readxl)   
library(dplyr)    
library(ggplot2)  
library(scales)
library(forcats)
library(tidyr)
library(RColorBrewer)
library(ggpubr)

# Set working directory
setwd("~/invasivePaper/finalData")

# Build a file containing full taxonomic information of NIS
# Load taxonomy table
data <- read.csv("NIS_taxonomy.csv",header=T)

# If both "Acartia tonsa" and "Acartia (Acanthacartia) tonsa" are still present, remove one.
data <- subset(data, Species != "Acartia tonsa")

# Change classification in the phylum/class level column to correct phylum name
# This was done based on web-based search using WoRMS or other scientific publications
data$Phylum<-ifelse(grepl("Urochordata|Craniata|Cephalochordata", data$Phylum),"Chordata",data$Phylum)
data$Phylum<-ifelse(grepl("Gregarnimorphea|Dinophyceae|Coccidiomorphea|Syndiniales|Colpodellidea|Perkinsida|Ellobiophyceae|Gregarinomorphea", data$Phylum),"Myzozoa",data$Phylum)
data$Phylum<-ifelse(grepl("Phaeophyceae|Dictyochophyceae|Chrysophyceae|Synurophyceae|Chrysomerophyceae|Raphidophyceae|Pelagophyceae|Eustigmatophyceae|MOCH-5|Pinguiophyceae|Xanthophyceae", data$Phylum),"Ochrophyta",data$Phylum)
data$Phylum<-ifelse(grepl("Florideophyceae|Bangiophyceae|Compsopogonophyceae|Rhodellophyceae|Porphyridiophyceae", data$Phylum),"Rhodophyta",data$Phylum)
data$Phylum<-ifelse(grepl("Ulvophyceae|Trebouxiophyceae|Pyramimonadophyceae|Embryophyceae|Chlorophyceae|Mamiellophyceae|Chloropicophyceae|Chlorodendrophyceae|Pedinophyceae|Nephroselmidophyceae|Scotinosphaera", data$Phylum),"Chlorophyta",data$Phylum)
data$Phylum<-ifelse(grepl("Spirotrichea|Phyllopharyngea|Oligohymenophorea|Heterotrichea|Litostomatea|CONTH|CONThreeP|Cariacotrichea|Colpodea|Protocruziidae|Prostomatea|Karyorelictea|Nassophorea|Plagiopylea", data$Phylum),"Ciliophora",data$Phylum)
data$Phylum<-ifelse(grepl("Labyrinthulomycetes|Bicoecea|Placidideae", data$Phylum),"Bigyra",data$Phylum)
data$Phylum<-ifelse(grepl("Developea", data$Phylum),"Gyrista",data$Phylum)
data$Phylum<-ifelse(grepl("Thecofilosea|Endomyxa-Ascetosporea|Imbricatea|Endomyxa|Phytomyxea|Filosa-Sarcomonadea|Filosa-Granofilosea|Chlorarachniophyceae|Filosa|Novel-clade-10-12|Metromonadea|Filosa-Thecofilosea|Filosa-Imbricatea", data$Phylum),"Cercozoa",data$Phylum)
data$Phylum<-ifelse(grepl("Ichthyosporea|Choanoflagellatea", data$Phylum),"Choanozoa",data$Phylum)
data$Phylum<-ifelse(grepl("Monothalamids|Globothalamea|Allogromida|Tubothalamea", data$Phylum),"Foraminifera",data$Phylum)
data$Phylum<-ifelse(grepl("Telonemia-Group-2|Telonemia-Group-1|Katablepharidaceae|Cryptophyceae", data$Phylum),"Cryptophyta",data$Phylum)
data$Phylum<-ifelse(grepl("Group-1", data$Phylum),"Apusozoa",data$Phylum)
data$Phylum<-ifelse(grepl("Myxozoa", data$Phylum),"Cnidaria",data$Phylum)
data$Phylum<-ifelse(grepl("Loxomitra", data$Phylum),"Entoprocta",data$Phylum)
data$Phylum<-ifelse(grepl("Crasiella", data$Phylum),"Gastrotricha",data$Phylum)
data$Phylum<-ifelse(grepl("Prymnesiophyceae|HAP5|Pavlovophyceae", data$Phylum),"Haptophyta",data$Phylum)
data$Phylum<-ifelse(grepl("Planomonadida|Subulatomonas-lineage|Mantamonadida|YS16Ec34-lineage", data$Phylum),"Sulcozoa",data$Phylum)
data$Phylum<-ifelse(grepl("Tubulinea|Discosea-Flabellinia|Stygamoebida|LKM74-lineage|Variosea", data$Phylum),"Amoebozoa",data$Phylum)
data$Phylum<-ifelse(grepl("Diplonemea|Kinetoplastea|Euglenida|Symbiontida", data$Phylum),"Euglenozoa",data$Phylum)
data$Phylum<-ifelse(grepl("Prasinodermophyceae", data$Phylum),"Prasinodermophyta",data$Phylum)
data$Phylum<-ifelse(grepl("Pterocystida", data$Phylum),"Heliozoa",data$Phylum)
data$Phylum<-ifelse(grepl("Zygnematophyceae|Zygnemophyceae", data$Phylum),"Charophyta",data$Phylum)
data$Phylum<-ifelse(grepl("Pirsonia", data$Phylum),"Hyphochytridiomycota",data$Phylum)
data$Phylum<-ifelse(grepl("RAD-B|Polycystinea", data$Phylum),"Radiozoa",data$Phylum)
data$Phylum<-ifelse(grepl("Baseodiscus", data$Phylum),"Nemertea",data$Phylum)
data$Phylum<-ifelse(grepl("Heterolobosea", data$Phylum),"Percolozoa",data$Phylum)
data$Phylum<-ifelse(grepl("Preaxostyla", data$Phylum),"Metamonada",data$Phylum)
data$Phylum<-ifelse(grepl("Gnosonesima", data$Phylum),"Platyhelminthes",data$Phylum)
data$Phylum<-ifelse(grepl("twista", data$Phylum),"Alveidia",data$Phylum)
data$Phylum<-ifelse(grepl("Hyphochytridiomycota", data$Phylum),"Hyphochytriomyceta",data$Phylum)
data$Phylum<-ifelse(grepl("Bacillariophyta", data$Phylum),"Gyrista",data$Phylum)


# See how many unique taxa there are
unique(data$Phylum)
unique(data$Class)
unique(data$Family)
unique(data$Order)
unique(data$Genus)
unique(data$Species)

# Calculate total number of observations
total_obs <- nrow(data)

# Calculate counts
count_phylum <- data %>%
  count(Phylum)

count_phylum <- dplyr::count(data, Phylum)

count_phylum_ordered <- count_phylum[order(-count_phylum$n),]

# Plot the pie chart using the base R function
pie_colors <- brewer.pal(length(count_phylum_ordered$n), "Paired")


count_phylum_ordered$Phylum <- factor(count_phylum_ordered$Phylum, levels = rev(as.character(count_phylum_ordered$Phylum)))
count_phylum_ordered$Phylum <- fct_inorder(count_phylum_ordered$Phylum)

p1 <- ggplot(count_phylum_ordered, aes(x = "", y = n, fill = Phylum)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) + # This transforms the bar chart into a pie chart
  scale_fill_manual(values = pie_colors) + # Use the manually defined colors
  theme_void() + # Remove background, grid lines, and text
  theme(legend.position = "left", # Adjust the position of the legend
        legend.text = element_text(size = 20), # Increase the text size in the legend
        legend.title = element_text(size = 22, face = "bold"), # Increase the title size and make it bold
        legend.key.size = unit(3, "lines")) + # Increase the size of the legend keys) 
  labs(fill = "Phylum") + # Label the legend
  geom_text(aes(x = 1.6, label = n), position = position_stack(vjust = 0.5), size = 8) # Add text labels

print(p1)


ggsave("phylum_pie_chart.pdf", width = 10, height = 8, dpi = 300) # Save the plot


## Counting and plotting the number of NIS found in each observatory ##

# Set working directory
setwd("~/invasivePaper/finalData")

## Count number of NIS found in each observatory
# Load Presence/Absence table 
finalTable <- read.csv("ARMS_final_NIS_presence_absence_cleaned.csv", header=T)

# Load coordinates table for sampling events with at least one NIS present
finalARMS <- read.table("ARMS_final_NIS_coordinates.txt",sep="\t",header=T)

# Reshape finalTable to long format
finalTable_long <- finalTable %>%
  pivot_longer(-Species, names_to = "event", values_to = "Presence")

# Merge finalTable_long with finalARMS based on Event
merged_data <- merge(finalTable_long, finalARMS, by = "event")

# Aggregate data by Observatory to get presence/absence of each species per observatory
observatory_presence <- merged_data %>%
  group_by(Species, Observatory) %>%
  summarize(Presence = max(Presence)) %>%
  ungroup()

# Reshape the data to get presence/absence matrix per observatory
presence_matrix <- observatory_presence %>%
  pivot_wider(names_from = Observatory, values_from = Presence, values_fill = list(Presence = 0))

# Calculate total observatory count per species
species_observatory_count <- observatory_presence %>%
  group_by(Species) %>%
  summarize(Total_Observatories = sum(Presence > 0)) %>%
  ungroup()

write.table(species_observatory_count, file = "obs_per_NIS.csv")

## Plot the 5 main species

# Set working directory
setwd("~/invasivePaper/finalData")

# Load table with number of observatories where each NIS has been found
ObsPerNIS <- read.csv("obs_per_NIS.csv",sep=",")

# Select the top 5 most abundant species
top_species <- head(ObsPerNIS[order(-ObsPerNIS$Total_Observatories), ], 7)


# Convert Species to a factor and order it by Total_Observatories in descending order
top_species$Species <- factor(top_species$Species, levels = top_species$Species[order(-top_species$Total_Observatories)])


# Now adjust the labels manually within the levels
levels(top_species$Species) <- c(
  "Amphibalanus\nimprovisus", "Bonnemaisonia\nhamifera", "Corella sp.", "Bugula\nneritina", 
  "Balanus\ntrigonus", "Hydroides\nelegans", "Acartia\n(Acanthacartia)\ntonsa"
)


# Create the plot with modifications
p2 <- ggplot(top_species, aes(x = Species, y = Total_Observatories, fill = Species)) +
  geom_bar(stat = "identity", width = 0.6, colour = "black") +
  geom_text(aes(label = Total_Observatories), vjust = -0.5, size = 6, color = "black") +
  scale_fill_brewer(palette = "Dark2") +
  labs(x = "", y = "No. of observatories") +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 15, vjust = 1, face = "italic", color = "black"), # Adjusted font family and angle
    axis.text.y = element_text(size = 14, color = "black"),
    axis.title.y = element_text(size = 17),
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  ylim(0, 15)

# Check the plot
print(p2)

# Adjust the size of the saved plot and increase text size
ggsave("top_species_barplot_3.pdf", plot = p, width = 7.5, height = 5, dpi = 300)  # Smaller dimensions but retain clarity

# Add a border/frame to one plot
p2 <- p2 + theme(plot.background = element_rect(color = "black", size = 1, fill = NA))


## Combine the two plots
combined_plot <- ggarrange(p1, p2, 
                           ncol = 2, nrow = 1, # Define the layout: 2 columns, 1 row
                           labels = c("a", "b"), # Optionally add labels to each plot
                           font.label = list(size = 22, face = "bold"),
                           common.legend = F) # Share a common legend if applicable


# Display the combined plot
print(combined_plot)

# Save the combined plot
ggsave("combined_plot.pdf", combined_plot, width = 16, height = 8)

