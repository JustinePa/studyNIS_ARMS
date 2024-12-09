## This script is used to make the UpSet plots ##
# Author: Justine Pagnier
# Date: July 2024

# Install if needed and load the packages
required_packages <- c("UpSetR", "dplyr", "phyloseq", "ggplot2", "tibble")
installed_packages <- installed.packages()
for (pkg in required_packages) {
  if (!(pkg %in% installed_packages)) install.packages(pkg)
}
library(UpSetR)
library(dplyr)
library(phyloseq)
library(ggplot2)
library(tibble)

# Set working directory
setwd("~/invasivePaper/finalData")

## Build a phyloseq object
# Load elements to build a phyloseq object
otu_mat<- read.csv("ARMS_final_NIS_presence_absence_cleaned_filtered.csv") # pick "_cleaned_filtered" for only having true NIS occurrences (occurrences in locations where the species is introduced), for all occurrences pick "_cleaned.csv"
tax_mat<- read.csv("NIS_taxonomy.csv")
samples_df <- read.csv("Observatories.csv")

# Define the row names for each elements
otu_mat <- otu_mat %>%
  tibble::column_to_rownames("Species") 
tax_mat <- tax_mat %>% 
  tibble::column_to_rownames("Species")
samples_df <- samples_df %>% 
  tibble::column_to_rownames("Event")

# Transform otu and tax tables into matrixes before assembling the phyloseq object
otu_mat <- as.matrix(otu_mat)
tax_mat <- as.matrix(tax_mat)

# Transform to phyloseq object
ps_NIS <- phyloseq(otu_table(otu_mat, taxa_are_rows = TRUE),tax_table(tax_mat), sample_data(samples_df))

# Aggregate species counts per observatories
ps_obs_grouped <- merge_samples(ps_NIS, group= "Observatory",fun=sum)

# Convert OTU table to binary (Presence/Absence)
t(otu_table(ps_obs_grouped))
otu_table <- as.data.frame(otu_table(ps_obs_grouped))
binary_table <- ifelse(otu_table > 0, 1, 0)
binary_table <- as.data.frame(binary_table)
binary_table <- t(binary_table)
binary_table <- as.data.frame(binary_table)

# Save presence absence matrix per observatory
write.table(binary_table,"presence_absence_per_obs.txt",sep="\t",row.names=T)


## Create the UpSet plot

# Define colors from the image and pick three
colors <- c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D")

# By frequency
upsetfreq <- upset(
  binary_table, 
  nsets=19, 
  nintersects = nrow(binary_table), 
  main.bar.color = colors[3], 
  sets.bar.color = colors[4], 
  matrix.color = colors[2], 
  order.by = "freq", 
  decreasing = T, 
  set_size.show = T, 
  mainbar.y.label = "Locations Intersections\n(Number of species)", 
  sets.x.label = "NIS per Observatory", 
  text.scale = c(4, 4, 3, 3, 3, 3)  # Adjusted sizes
)

# Display
upsetfreq

# Save
pdf("upset_plot_freq_unfiltered.pdf", width = 24, height = 20)
upsetfreq
dev.off()

## How to know which species are only found in a specific observatory?
# which species are only found in, for example, Ravenna?
ravenna_samples <- subset_samples(ps_NIS, Observatory == "RavennaH")
ravenna_taxa_present <- prune_taxa(taxa_sums(ravenna_samples) > 0, ravenna_samples)
species_in_ravenna<- taxa_names(ravenna_taxa_present)

non_ravenna_samples <- subset_samples(ps_NIS, Observatory != "RavennaH")
non_ravenna_taxa_present <- prune_taxa(taxa_sums(non_ravenna_samples) > 0, non_ravenna_samples)
species_not_in_ravenna <- taxa_names(non_ravenna_taxa_present)

unique_species_ravenna <- setdiff(species_in_ravenna, species_not_in_ravenna)

#which species are only found in, for example, Plymouth?
plymouth_samples <- subset_samples(ps_NIS, Observatory == "Plymouth")
plymouth_taxa_present <- prune_taxa(taxa_sums(plymouth_samples) > 0, plymouth_samples)
species_in_plymouth <- taxa_names(plymouth_taxa_present)

non_plymouth_samples <- subset_samples(ps_NIS, Observatory != "Plymouth")
non_plymouth_taxa_present <- prune_taxa(taxa_sums(non_plymouth_samples) > 0, non_plymouth_samples)
species_not_in_plymouth <- taxa_names(non_plymouth_taxa_present)

unique_species_plymouth <- setdiff(species_in_plymouth, species_not_in_plymouth)

#which species are only found in, for example, Piran?
piran_samples <- subset_samples(ps_NIS, Observatory == "Gulf Of Piran")
piran_taxa_present <- prune_taxa(taxa_sums(piran_samples) > 0, piran_samples)
species_in_piran<- taxa_names(piran_taxa_present)

non_piran_samples <- subset_samples(ps_NIS, Observatory != "Gulf Of Piran")
non_piran_taxa_present <- prune_taxa(taxa_sums(non_piran_samples) > 0, non_piran_samples)
species_not_in_piran<- taxa_names(non_piran_taxa_present)

unique_species_piran <- setdiff(species_in_piran, species_not_in_piran)

#which species are only found in, for example, TZS?
tzs_samples <- subset_samples(ps_NIS, Observatory == "TZS")
tzs_taxa_present <- prune_taxa(taxa_sums(tzs_samples) > 0, tzs_samples)
species_in_tzs <- taxa_names(tzs_taxa_present)

non_tzs_samples <- subset_samples(ps_NIS, Observatory != "TZS")
non_tzs_taxa_present <- prune_taxa(taxa_sums(non_tzs_samples) > 0, non_tzs_samples)
species_not_in_tzs <- taxa_names(non_tzs_taxa_present)

unique_species_tzs <- setdiff(species_in_tzs, species_not_in_tzs)

#which species are only found in, for example, TZS?
gdynia_samples <- subset_samples(ps_NIS, Observatory == "Gdynia")
gdynia_taxa_present <- prune_taxa(taxa_sums(gdynia_samples) > 0, gdynia_samples)
species_in_gdynia <- taxa_names(gdynia_taxa_present)

non_gdynia_samples <- subset_samples(ps_NIS, Observatory != "Gdynia")
non_gdynia_taxa_present <- prune_taxa(taxa_sums(non_gdynia_samples) > 0, non_gdynia_samples)
species_not_in_gdynia <- taxa_names(non_gdynia_taxa_present)

unique_species_gdynia <- setdiff(species_in_gdynia, species_not_in_gdynia)


## Are there any species detected in all observatories?
# List all observatories
all_observatories <- unique(sample_data(ps_NIS)$Observatory)

# Initialize a list to store taxa from each observatory
taxa_lists <- list()

# Loop through each observatory and store the taxa present
for (obs in all_observatories) {
  obs_samples <- subset_samples(ps_NIS, Observatory == obs)
  obs_taxa_present <- prune_taxa(taxa_sums(obs_samples) > 0, obs_samples)
  taxa_lists[[obs]] <- taxa_names(obs_taxa_present)
}

# Find the intersection of all lists - species found in all observatories
species_in_all <- Reduce(intersect, taxa_lists)

# Print species found in all observatories
print(species_in_all)
