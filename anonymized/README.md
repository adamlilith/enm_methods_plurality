# enm_methods_plurality
Analysis of species distribution models for the same species submitted by multiple, independent teams

These scripts conduct part of the analysis in Thonis et al., a study in which multiple independent teams were asked to construct species distribution models for the same two species. Most of the scripts in this repository are intended to analyze clustering among predictions and whether or not these clusters are related to workflow attributes.

## Files

The numbered scripts below contain their own `CONTENTS` blocks; those tables of contents are reproduced here. The `xx` scripts are supporting, publication, presentation, or utility scripts and are listed last. Deprecated scripts are not included.

### Numbered scripts

#### `00_shared_constants_and_functions.r`

Defines shared setup, analysis-wide settings, and custom functions used throughout the project.

**Contents:**

- analysis-wide settings
- custom functions

#### `01_assess_candidate_species_for_enm_methods_plurality_study.r`

Assesses candidate species for the plurality study by mapping species occurrences and tallying records across several candidate groups.

**Contents:**

- make maps and tally occurrences of selected species: cycads of the world
- make maps and tally occurrences of selected species: selected cycads
- make maps and tally occurrences of selected species: mammals of Thailand
- make maps and tally occurrences of selected species: Nepenthes

#### `02_extract_predictions_to_random_points.r`

Creates shared random sampling locations, maps the common raster area, and extracts predictions from all team rasters at those locations.

**Contents:**

- create random points from which to draw predictions across all team rasters
- make nice map of area in common between all rasters
- extract values for predictions at same sites across all rasters

#### `03_maps_of_rasters_for_inspection.r`

Creates inspection maps for the rasters submitted by each team.

**Contents:**

- make maps of all rasters for inspection

#### `04_exploratory_cluster_analysis_of_teams.r`

Calculates exploratory statistics to assess whether predictions cluster by team and to estimate the appropriate number of clusters.

**Contents:**

- assess propensity to cluster and optimal number of clusters

#### `05_hierarchical_cluster_analysis_of_predictions.r`

Produces dendrograms and PCA-based figures for hierarchical clustering of team predictions and raster sets.

**Contents:**

- dendrogram and PCA of teams
- clustering each set of rasters from a team
- PCA on teams with rasters coded by workflow attribute
- not used but may be useful

#### `06_heatmaps_of_correlations_between_predictions.r`

Creates heatmaps showing correlations among predictions produced by the teams.

**Contents:**

- heatmaps of correlations between predictions

#### `07_distributions_of_predictions_by_raster.r`

Creates violin plots showing the distributions of predictions by raster and team.

**Contents:**

- distributions of predictions by team

#### `08_unsupervised_clustering_and_associations_with_workflow_attributes.r`

Tests associations between unsupervised raster clusters and workflow attributes, then prepares tables, PCA biplots, and a workflow-quality analysis.

**Contents:**

- test for associations between unsupervised clusters of rasters and workflow attributes
- reshape results associating workflow attributes with clusters into table for display
- make PCA biplots with rasters coded by select workflow attributes for figures in main text
- calculate correlation between workflow quality and self-reported AUC

#### `09_supervised_clustering_of_predictions_and_multivariate_random_forests.r`

Uses multivariate random forests to estimate the importance of workflow attributes for distances among rasters in principal-component space and graphs the results.

**Contents:**

- multivariate RF on PC scores in 1st two PC axes
- graph of variable importance
- plot of variable importance and selected biplots for main text for *Prionailurus bengalensis*
- selected biplots for main text for *Prionailurus bengalensis*
- selected biplots for main text for *Zamia prasina*
- plot of variable importance and selected biplots for main text for both species together

#### `10_spatial_consensus_across_rasters.r`

Analyzes agreement among rasters in site rankings, suitability quartiles, and climate-change refugia, including maps of consensus patterns.

**Contents:**

- consensus in ranking sites
- consensus in assigning locations to quartiles of suitability
- maps illustrating calculation of consensus in assigning locations to quartiles of suitability
- consensus in identifying climate change refugia

#### `11_sdm_standards_assessment.r`

Assesses consistency among raters of SDM standards, summarizes ODMAP scores, and plots standards by criterion.

**Contents:**

- assess consistency between raters of SDM standards
- calculate mean score for each study and ODMAP criterion across assessors
- plot of SDM standard scores by standard

#### `12_select_plots_for_publication.r`

Selects and assembles plots for the main publication, including spatial-consensus figures and associations between raster predictions and workflow attributes.

**Contents:**

- expected null proportions for positive/negative agreement between rasters in spatial consensus analysis
- figure of consensus in assigning locations to quartiles of suitability for main text
- multi-panel figure of select associations between rasters in PC space and workflow decisions/attributes

### `xx` scripts

#### `xx_anonymize.r`

Anonymizes raster outputs and ODMAP team names for sharing or analysis where contributor identities should be hidden.

**Contents:**

- anonymize rasters
- anonymize ODMAP team names

#### `xx_map_of_contributors_by_country.r`

Loads country boundaries, selects countries represented among contributors, and creates a map of contributor countries.

**Contents:**

- load the required country-boundary packages
- retrieve world country outlines
- identify contributor countries
- plot the contributor-country map

#### `xx_maps_of_predictions.r`

Creates publication-quality maps of each team’s prediction rasters and related summaries.

**Contents:**

- make maps of rasters for publication

#### `xx_pca_of_predictions_vs_attributes_for_presentations.r`

Creates presentation figures showing dendrograms and PCA clustering of rasters, including workflow-attribute overlays.

**Contents:**

- cluster analysis of teams: all periods

#### `xx_plots_for_presentations.r`

Creates presentation plots summarizing the distributions of individual workflow decisions across sample size, predictors, software, climate data, algorithms, boundaries, and resolution.

**Contents:**

- sample size
- predictors
- software
- climate data
- algorithm
- boundary
- resolution
- format and save
