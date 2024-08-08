# studyNIS_ARMS

## Project Description

This repository contains scripts and their inputs used for the analysis performed for the article titled "Assessing the effectiveness of genetic observatory networks in detecting and monitoring marine non-indigenous species."

## Contents

### Scripts
This directory contains the five different scripts used after the manual curation of the NIS list:

1. **`processNIScuratedLists.R`**  
   Process the COI and 18S ASVs count tables after curation (i.e., removal of ASVs/mOTUs that did not make it through the taxonomic check or the introduction status check), merge them, and clean them (i.e., remove sampling events where no NIS were detected - empty columns).

2. **`graph_taxa.R`**  
   Identify the main phyla and the 5 most widespread NIS and make graphs.

3. **`NISperobs.R`**  
   Calculate the number of NIS detected in each observatory (both options possible: all occurrences of all NIS, or occurrences of NIS only in observatories where considered alien).

4. **`upSetPlot.R`**  
   Build a phyloseq object to then build an Upset plot (both options possible: all occurrences of all NIS, or occurrences of NIS only in observatories where considered alien).

5. **`GBIF_EOO.R`**  
   For NIS potentially detected in new areas, get occurrences from GBIF, map the known area of extent together with occurrences from ARMS observatories to identify one occurring outside.

### Inputs

Contains all inputs needed to run the above scripts.

### Outputs

Output from scripts, including tables and figures.

