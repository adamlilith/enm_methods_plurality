# enm_methods_plurality
Analysis of species distribution models for the same species submitted by multiple, independent teams

## Files

- **00_shared_constants_and_functions.r** — Defines shared constants, sets up the working environment, loads required libraries, and contains custom functions used across all analyses.

- **01_assess_candidate_species_for_enm_methods_plurality_study.r** — Assesses candidate species for the plurality of SDM workflows project and creates maps and tallies occurrences of selected species.

- **01_extract_predictions_to_random_points.r** — Extracts predictions from rasters to random points within the common spatial extent across all team rasters.

- **01_maps_of_rasters_for_inspection.r** — Creates PDF maps of each team's rasters for visual inspection across different time periods (present, mid-century, late century).

- **02_exploratory_cluster_analysis_of_teams.r** — Calculates statistics for testing whether predictions cluster by team and determines the optimal number of clusters using multiple methods (VAT, Hopkins, NbClust).

- **03_cluster_analysis_of_teams_hierarchical.r** — Performs hierarchical clustering on team predictions and creates dendrograms and PCA plots showing relationships between teams.

- **04_heatmaps_of_correlations_between_predictions.r** — Creates heatmaps of Spearman correlations between predictions made by each team for each time period.

- **05_distributions_of_predictions_by_raster.r** — Generates violin plots showing the distributions of prediction values for each team/raster.

- **06_univariate_workflow_attributes_vs_raster_clusters.r** — Tests for associations between unsupervised clusters of rasters and individual workflow attributes using various statistical methods.

- **07_multivariate_workflow_attributes_vs_raster_dissimilarity.r** — Measures associations between raster dissimilarity (in PCA space) and workflow attribute distances using Mantel tests and multiple regression on distance matrices.

- **08_cluster_analysis_of_teams_hierarchical_with_attributes.r** — Creates a PCA biplot of team predictions with workflow attributes overlaid as vectors to show which attributes relate to variation in predictions.

- **09_pca_of_predictions_vs_teams.r** — Performs principal component analysis on team predictions and creates a biplot showing team loadings and spatial maps of PC scores.

- **10_spatial_consensus_across_rasters.r** — Analyzes spatial consensus across rasters, including consensus in ranking sites, assigning to suitability quartiles, and identifying climate change refugia.

- **11_select_plots_for_publication.r** — Selects and prepares figures and plots for the main text and supplementary materials of publications.

- **DEPRECATED_03_cluster_analysis_of_teams_dbscan.r** — Deprecated version of cluster analysis using DBSCAN clustering instead of hierarchical clustering.
