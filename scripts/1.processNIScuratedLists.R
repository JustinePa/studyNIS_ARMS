# This script is used to process and reformat the curated ASV counts tables coming from the protocol "ARMS-MBON 18SrRNA and COI gene metabarcoding: scanning for non indigenous species (Daraghmeh, 2024)

# Install if needed, and load packages
library(dplyr)
library(plyr)
library(tidyr)


## COI ##

setwd("C:/Users/Justine/OneDrive/Documents/ARMS_FELLOWSHIP/invasivePaper/finalData")

# Get the table manually curated in Excel
nis_taxa_counts<-read.table("NIS_MOTU_ASV_counts_COI_new_curated.txt",sep="\t",header=T,check.names= F)
nis_taxa_counts<-nis_taxa_counts[,-3] # remove ASV column

# Aggregate counts based on MOTU
nis_taxa_counts<-aggregate(.~ MOTU + Species, data = nis_taxa_counts,FUN=sum)

# Transform to presence-absence
nis_taxa_counts <- nis_taxa_counts %>%
  mutate_if(is.numeric, ~1 * (. >= 1))

write.table(nis_taxa_counts,"COI_NIS_presence_absence_all_events.txt",sep="\t",row.names=F)

column_sums <- colSums(nis_taxa_counts[,3:ncol(nis_taxa_counts)], na.rm = TRUE)
empty_columns <- names(column_sums[column_sums == 0])
row_sums <- rowSums(nis_taxa_counts[,3:ncol(nis_taxa_counts)], na.rm = TRUE)
empty_rows <- names(row_sums[row_sums == 0])

#Remove empty events
nis_taxa_counts <- nis_taxa_counts[, !(colnames(nis_taxa_counts) %in% empty_columns)]

# Write final table to file
write.table(nis_taxa_counts,"COI_NIS_presence_absence_cleaned.txt",sep="\t",row.names=F)

## 18S

# Get the table manually curated in Excel
nis_taxa_counts_2<-read.table("NIS_MOTU_ASV_counts_18S_new_curated.txt",sep="\t",header=T,check.names= F)
nis_taxa_counts_2<-nis_taxa_counts_2[,-3] # remove ASV column

# Aggregate counts based on MOTU
nis_taxa_counts_2<-aggregate(.~ MOTU + Species, data = nis_taxa_counts_2,FUN=sum)

# Transform to presence-absence
nis_taxa_counts_2 <- nis_taxa_counts_2 %>%
  mutate_if(is.numeric, ~1 * (. >= 1))

write.table(nis_taxa_counts_2,"18S_NIS_presence_absence_all_events.txt",sep="\t",row.names=F)

# Spot empty columns or rows
column_sums_2 <- colSums(nis_taxa_counts_2[,3:ncol(nis_taxa_counts_2)], na.rm = TRUE)
empty_columns_2 <- names(column_sums_2[column_sums_2 == 0])
row_sums_2 <- rowSums(nis_taxa_counts_2[,3:ncol(nis_taxa_counts_2)], na.rm = TRUE)
empty_rows_2 <- names(row_sums_2[row_sums_2 == 0])

#Remove empty events
nis_taxa_counts_2 <- nis_taxa_counts_2[, !(colnames(nis_taxa_counts_2) %in% empty_columns_2)]

# Write final table to file
write.table(nis_taxa_counts_2,"18S_NIS_presence_absence_cleaned.txt",sep="\t",row.names=F)

# MERGE 18S and COI
# Read cleaned NIS tables for 18S and COI
nis_18s<-read.table("18S_NIS_presence_absence_cleaned.txt",sep="\t",header=T,check.names=FALSE)
nis_coi<-read.table("COI_NIS_presence_absence_cleaned.txt",sep="\t",header=T,check.names=FALSE)
# Remove MOTU names
nis_18s<-nis_18s[,-1]
nis_coi<-nis_coi[,-1]
# Combine both tables
nis_all<-rbind.fill(nis_18s,nis_coi)
# Set NAs to zero and aggregate based on species name
nis_all[is.na(nis_all)]<-0
nis_all <- aggregate(. ~ Species, data = nis_all, FUN = sum)
# Set all numeric values above zero to 1
nis_all<-nis_all %>% mutate_if(is.numeric, ~1 * (. > 0))
write.table(nis_all,"ARMS_final_NIS_presence_absence_cleaned.txt",sep="\t",row.names =F) #

# Read uncleaned NIS tables for 18S and COI
#nis_18s_all_events<-read.table("18S_NIS_presence_absence_all_events.txt",sep="\t",header=T,check.names=FALSE)
#nis_coi_all_events<-read.table("COI_NIS_presence_absence_all_events.txt",sep="\t",header=T,check.names=FALSE)
# Remove MOTU names
#nis_18s_all_events<-nis_18s_all_events[,-1]
#nis_coi_all_events<-nis_coi_all_events[,-1]
# Combine both tables
#nis_both_all_events<-rbind.fill(nis_18s_all_events,nis_coi_all_events)
# Set NAs to zero and aggregate based on species name
#nis_both_all_events[is.na(nis_both_all_events)]<-0
#nis_both_all_events <- aggregate(. ~ Species, data = nis_both_all_events, FUN = sum)
# Set all numeric values above zero to 1
#nis_both_all_events<-nis_both_all_events %>% mutate_if(is.numeric, ~1 * (. > 0))
#write.table(nis_both_all_events,"ARMS_final_NIS_presence_absence_all_events.txt",sep="\t",row.names =F) #
# /!\ manually remove "Katza1 _20181024_20200706"

test <- rbind.fill(nis_taxa_counts,nis_taxa_counts_2)
test <- test[,-1]
test <- test %>%
  mutate(Species = if_else(Species == "Watersipora sp.", 
                           paste("Watersipora sp.", ave(Species == "Watersipora sp.", Species, FUN = seq_along), sep = "_"),
                           Species))
test[is.na(test)]<-0
test <- aggregate(. ~ Species, data = test, FUN = sum)



## Write table with coordinates of ARMS locations
arms_events<-as.data.frame(colnames(nis_all[,-1]))
colnames(arms_events)<-"event"
arms_events$event2 <- arms_events$event
arms_events<-separate_wider_delim(arms_events,event2,delim="_",names_sep="")
colnames(arms_events)[2:4]<-c("ARMS","Deployment","Retrieval")
coord<-read.table("arms_coordinates.txt",sep="\t",header=T)
coord<-coord %>% filter(ARMS %in% arms_events$ARMS)
arms_events<-merge(arms_events,coord)
arms_events<-arms_events %>% relocate(event)
write.table(arms_events,"ARMS_final_NIS_coordinates.txt",row.names = F)
