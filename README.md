# Biotic_homogenisation

R scripts and data for “Floristic homogenisation of South Pacific islands commenced with human arrival”.
The R scripts were written by Nichola Strandberg [Orcid](https://orcid.org/0000-0003-1268-2080), Manuel Steinbauer [Github](https://github.com/ManuelSteinbauer) [Orchid](https://orcid.org/0000-0002-7142-9272), and Anna Walentowitz [Github](https://github.com/AnnaJever) [Orchid](https://orcid.org/0000-0001-9720-9078)  

***

## Folders:

* “Bacon_runs” contains the data for the age-depth models which were not obtained from Neotoma.

* “Figures” contains all the figures from the paper and SI in PDF format.

* “Original_pollen_data” contains all the original pollen data including those downloaded from Neotoma.

* “Outputs” contains all the .csv files generated during the analysis. Note that “Stand1_unbinned_int.csv” and “Stand2_unbinned_int.csv” include a column with the mean interval age. “harmonisation.csv” contains the standardisation list for standardisation 1 and 2. “metadata.csv” contains all the site metadata.

* “Pollen_data_with_ages” contains all the pollen data with the associated ages either from *rbacon* or Neotoma.

***

## Scripts:

* “1_rbacon_age_depth_models” was used to generate the *rbacon* age-depth models for the datasets not retrieved from Neotoma.

* “2_Assign_ages_to_pollen_data” was used to download the remaining datasets from Neotoma and to assign ages to all the pollen samples. 

* “3_Create_standardisation_table” was used to create the basis of the table used for the standardisations (harmonisations).

* “4_Validation_with_POWO” was used to validate all taxa names in standardisation 1 and 2.

* “5_Combine_datasets” was used to combine all the pollen datasets and to replace the original pollen and spore names with the updated names in standardisation 1 and 2. 

* “6_Binning” was used to convert the pollen data to percentages, average pollen data (mean values), and put all pollen samples within 500-year bins (intervals). It was also used to create a Fig. S9.

* “6a_Binning_subset_300_min_pollen_sum” was used to remove pollen samples with counts <300 and to place the pollen samples in 500-year intervals.

* “7_Rank” was used to obtain the rank of each taxa using *taxize*. You should obtain your own ENTREZ_KEY.

* “8_Stand1_similarity” was used to conduct the pairwise similarity analysis on the standardisation 1 dataset. It was also used to produce the pie charts for Fig. 1 and the boxplots for Fig. 2. The pie charts were made into a figure using Adobe Illustrator V27.8. It was also used to make Fig. S10.

* “8_Stand2_similarity” was used to conduct the pairwise similarity analysis on the standardisation 2 dataset. It was also used to produce Figs. S11 and S12. At the end of the script the slope coefficient with the minimum number of points is calculated.

* “8a_Similarity_by_rank” was used to split the standardisation 1 and 2 datasets into ranks (family, genus, and species) and to conduct the similarity analysis on each rank. The similarity scores were then merged back together into one dataset.

* “8b_Subset_min_300_counts_similarity” was used to conduct the similarity analysis on the datasets with samples with counts <300 excluded.

* "8c_mean_taxa_abundance" was used to calculate the mean abundance (%) of each taxon before and after human colonisation i.e., either 3000 or 700 cal. years BP.

* "8c_Similarity_without_most_abundant_taxa" was used to conduct the similarity analysis on stand1 and stand2 with Cyperaceae and then Poaceae pollen excluded. Pollen percentages were rescaled to 100% after the removal of both taxa.

* “9_Fig3” was used to make Fig. 3 and Fig. S13.

* “9_Sensitivity_analysis_figs” was used to produce Figs. S14, S15, and S16.

* “10_Rarefaction” was used to obtain the mean counts of the original pollen data (after non-pollen and unknown types were removed), to rarefy the pollen data, and to obtain the total (or alpha) diversity of the pollen samples. It was also used to produce Fig. S7. 

* “11_Accumulation_time” was used to compare the accumulation times of sediment samples from three sites with total and rarefied richness. It was used to create Fig. S8.

* "12_Jaccard_index" was used to investigate how using Jaccard index `method= "jaccard", binary=T` instead of Bray-Curtis impacted the similarity results. The use of `binary=TRUE` means that only the presence or absence of a taxa was used (rather than abundance). Note that only the Bray-Curtis analyses were presented in the main paper and SI.