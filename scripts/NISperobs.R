# This script is used to calculate and plot the number of NIS found in each observatory

# Load, and install if needed, the R packages
library(phyloseq)
library(dplyr)
library(tibble)
library(ggplot2)

# Set working directory
setwd("C:/Users/Justine/OneDrive/Documents/ARMS_FELLOWSHIP/invasivePaper/finalData")

# Load elements to build a phyloseq object
otu_mat<- read.csv("ARMS_final_NIS_presence_absence_cleaned_filtered.csv") # pick "_cleaned_filtered" if you only want occurrences of true NIS (i.e. that are non-indigenous in the observatory in question)
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

# Sum the binary NIS data per observatory
nis_counts_per_obs <- colSums(binary_table)

# Convert to a data frame for plotting
nis_data <- data.frame(Observatory = names(nis_counts_per_obs), Count = nis_counts_per_obs)

# Reorder the factor levels based on the count in descending order
nis_data$Observatory <- factor(nis_data$Observatory, levels = nis_data$Observatory[order(-nis_data$Count)])

# Create the bar plot
ggplot(nis_data, aes(x = Observatory, y = Count, fill = Observatory)) +
  geom_bar(stat = "identity", show.legend = FALSE) +  # Draw the bars
  geom_text(aes(label = Count), vjust = -0.5, color = "black") +  # Add text labels above bars
  theme_minimal() +
  labs(x = "Observatory",
       y = "Count of NIS") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for better readability
        panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank())  # Remove minor grid lines

# Save the plot
ggsave("NIS_per_Observatory.pdf", width = 10, height = 8)
