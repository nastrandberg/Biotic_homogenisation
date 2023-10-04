# Biotic_homogenisation

R scripts and data for “Floristic homogenisation on South Pacific islands during the last 5000 years”.
The R scripts were written by Nichola Strandberg https://orcid.org/0000-0003-1268-2080  Manuel Steinbauer (https://github.com/ManuelSteinbauer) https://orcid.org/0000-0002-7142-9272 and Anna Walentowitz (https://github.com/AnnaJever) https://orcid.org/0000-0001-9720-9078 

Folders:

“Bacon_runs” contains the data for the age depth models which were not obtained from Neotoma.

“Original_pollen_data” contains all the original pollen data including those downloaded from Neotoma.

“Outputs” contains all the .csv files generated during the analysis. Note that “Stand1_unbinned_int.csv” and “Stand2_unbinned_int.csv” include a column with the mean interval age. “harmonisation.csv” contains the standardisation list for standardisation 1 and 2. “metadata.csv” contains all the site metadata.

“Pollen_data_with_ages” contains all the pollen data with the associated ages either from rbacon or Neotoma.

Scripts:

“1_rbacon_age_depth_models” was used to generate the age-depth models for the datasets not retrieved from Neotoma.

“2_Assign_ages_to_pollen_data” was used to download the remaining datasets from Neotoma and to assign ages to all the pollen samples. 

“3_Create_standardisation_table” was used to create the basis of the table used for the standardisations (harmonisations).

“4_Validation_with_POWO” was used to validate all taxa names in standardisation 1 and 2.

“5_Combine_datasets” was used to combine all the pollen datasets and to replace the original pollen and spore names with the updated names in standardisation 1 and 2. 

“6_Binning” was used to convert the pollen data to percentages, average pollen data, and put all pollen samples within 500-year bins. It was also used to create a Fig. S9.

“6b_Binning_subset_300_min_pollen_sum” was used to remove pollen samples with counts <300 and then to place the pollen samples in 500-year intervals.

“7_Rank” was used to obtain the rank of each taxa using Taxize. You should obtain your own ENTREZ_KEY.

“8_Stand1_similarity” was used to conduct the pairwise similarity analysis on the standardisation 1 dataset. It was also used to produce the pie charts for Fig. 1 and the boxplots for Fig. 2. The pie charts were made into a figure using Adobe Illustrator. 

“8_Stand2_similarity” was used to conduct the pairwise similarity analysis on the standardisation 2 dataset. It was also used to produce Figs. S10 and S11. At the end of the script the slope coefficient with the minimum number of points is calculated.

“8b_Similarity_by_rank” was used to split the standardisation 1 and 2 datasets into ranks (family, genus, and species) and to conduct the similarity analysis on each rank. The score were then merged back together.

“8c_Subset_min_300_counts_similarity” was used to do conduct the similarity analysis on 

“9_Fig3” was used to make Fig. 3 and Fig. S12.

“9_Sensitivity_test_figs” was used to produce Figs. S13 and S14. 

“10_Rarefaction” was used to obtain the mean counts of the original pollen data (after non-pollen and unknown types were removed), to rarefy the pollen data, and to obtain the total (or alpha) diversity of the pollen samples. It was also used to produce Fig. S7. 

“11_Accumulation_time” was used to compare the accumulation times of sediment samples from three sites with total and rarefied richness. It was used to create Fig. S8.
