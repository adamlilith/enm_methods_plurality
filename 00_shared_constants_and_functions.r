### SDM METHODOLOGICAL PLURALITY
### Anna Thonis, Nikki Calavari, Uzma Ashraf, Toni Lyn Morelli, and Adam B. Smith*
### * adam.smith@mobot.org | Missouri Botanical Garden | 2025-10
###
### source('C:/Kaji/Research Group/ENMs - Plurality of Modeling Workflows (Anna Thonis)/enm_methods_plurality/00_shared_constants_and_functions.r')
###
### CONTENTS ###
### setup ###
### analysis-wide settings ###
### custom functions ###

#############
### setup ###
#############

	setwd('C:/Kaji/Research Group/ENMs - Plurality of Modeling Workflows (Anna Thonis)')

	library(cluster) # clustering
	library(cowplot) # combining ggplots
	library(data.table) # fast data frames
	library(ecodist) # multiple regression on distance matrices
	library(enmSdmX) # SDMing and GIS
	library(ggplot2) # graphics
	library(ggspatial) # spatial graphics
	library(omnibus) # utilities
	library(patchwork) # combine ggplots
	library(readxl) # open Excel documents
	library(reshape2) # wide <--> long data frames
	# library(rnaturalearth) # GIS data
	library(rnaturalearthdata) # GIS data
	library(shadowtext) # shadows for text in ggplot2
	library(terra) # GIS
	library(vegan) # ecology (esp. Mantel and PERMANOVA)
	library(viridis) # for magma palette
	
	# library(clustertend) # clustering: deprecated
	library(clValid) # cluster validity
	library(dbscan) # DBSCAN clustering
	library(distances) # fast distance calculation
	# library(FactoMineR) # ggplot PCA
	library(factoextra) # cluster ggplots
	library(ggdendro) # plotting dendrograms
	library(hopkins) # Hopkins statistic for clustering
	library(NbClust) # optimal number of clusters
	library(rpart) # CART
	library(rpart.plot) # CART plotting
	library(tree) # classification and regression trees

	drive <- 'C:/Kaji/'

	setwd(paste0(drive, '/Research Group/ENMs - Plurality of Modeling Workflows (Anna Thonis)'))

##############################
### analysis-wide settings ###
##############################

	species_focal <- 'Priona'
	# species_focal <- 'Zamia'

	# spreadsheet that scores attributes for each workflow
	fields_file_name <- './Data/Model_choices_2026_01_25.xlsx'

	# number of random points to keep that have predictions across all teams' rasters
	n_rand_points_to_keep <- 100000

	say('Using workflow attributes file: ', fields_file_name, level = 1, deco = '!')

	# min_variance <- 0.85 # for clustering by PC loadings, keep axes that explain at least this much variance
	# max_height <- 0.5 # in dendrogram of teams, if groups of teams are more different than this, then define them as different clusters

	# colors for each period
	period_colors <- c('Present' = '#66c2a5', 'Mid' = '#8da0cb', 'Late' = '#fc8d62')

	species_full <- if (species_focal == 'Priona') {
		'Prionailurus bengalensis'
	} else {
		'Zamia prasina'
	}

	say('Analyzing ', species_full, level = 1, deco = '!')

	out_dir <- paste0('./Outputs ', species_full)
	dirCreate(out_dir)

########################
### custom functions ###
########################

	# get_ks(): Series of number of clusters from 2 to optimum; becomes sparser at higher k
	# scrub_period(): Remove the period name from raster names (e.g., "_present", "_mid", "_late")
	# get_nice_period(): Convert period ('present', 'mid', and 'late') to nice version for folder names
	# load_team_codes(): loads table with team codes
	# load_rast_fields(): loads workbook with attributes of each team's rasters
	# load_team_fields(): loads workbook with attributes of each team
	# load_predictions(): loads SDM predictions
	# fill_terrestrial_NAs(): fills terrestrial NA cells in a projection raster with 0s based on appropriate land/sea mask and resolution
	# kw_test(): Implement Kruskal-Wallis test for numerical covariates of cluster identity
	# contingency_test(): Implement contingency_test test for categorical covariates of cluster identity

	# Series of number of clusters from 2 to optimum; becomes sparser at higher k
	get_ks <- function() {
		# opt_k <- readRDS(paste0('./Outputs ', species_full, '/Cluster Analysis of Rasters/Optimal Cluster Number from Silhouette Width vs k Hierarchical.rds'))
		# ks <- sort(unique(c(2:24, round(opt_k * c(0.5, 0.75)), opt_k)))
		2:24
		# ks
	}

	### remove the period name from raster names (e.g., "_present", "_mid", "_late")
	# x Character vector
	scrub_period <- function(x) {
	
		x <- sub(x, pattern = '_present', replacement = '')
		x <- sub(x, pattern = '_mid', replacement = '')
		x <- sub(x, pattern = '_late', replacement = '')
		x
	
	}

	### Convert period ('present', 'mid', and 'late') to nice version for folder names
	get_nice_period <- function(period) {
		if (period == 'present') {
			'Present'
		} else if (period == 'mid') {
			'Mid-20th Century'
		} else if (period == 'late') {
			'Late 20th Century'
		} else {
			stop('Bad period name.')
		}
	}

	### loads table with team codes
	load_team_codes <- function() fread('./Outputs Shared/team_info.csv')

	# function to load data on workflow choices
	# species_focal		Either 'Priona' or 'Zamia'
	#
	# returns data.table of modeling choices
	load_rast_fields <- function(species_focal) {

		if (species_focal == 'Priona') {
			fields <- read_xlsx(fields_file_name, sheet = 'Scoring by Raster CAT')
		} else if (species_focal == 'Zamia') {
			fields <- read_xlsx(fields_file_name, sheet = 'Scoring by Raster CYCAD')
		}
		fields <- as.data.table(fields)
		fields
	}

	load_team_fields <- function(species_focal) {

		if (species_focal == 'Priona') {
			fields <- read_xlsx(fields_file_name, sheet = 'Scoring by Team CAT')
		} else if (species_focal == 'Zamia') {
			fields <- read_xlsx(fields_file_name, sheet = 'Scoring by Team CYCAD')
		}
		fields <- as.data.table(fields)
		fields
	}

	### load predictions at sites shared by all rasters
	# species_focal 'Priona' or 'Zamia'
	# period		'all' (all periods), 'present', 'mid', or 'late'
	# scaled		TRUE ==> scale predictions to [0, 1], FALSE ==> return raw scores
	# subset_teams  TRUE ==> return data frame with just columns with predictions, FALSE ==> return all columns in extraction frame (e.g., coordinates)
	# discardNAs	TRUE ==> remove rows in which there is at least one NA
	load_predictions <- function(species_focal, period = 'all', scale = TRUE, subset_teams = TRUE) {

		species_full <- if (species_focal == 'Priona') {
			'Prionailurus bengalensis'
		} else {
			'Zamia prasina'
		}

		out <- readRDS(paste0('./Outputs ', species_full, '/Extractions to Random Sites ', species_focal, '.rds'))
		if (subset_teams) out[ , c('longitude', 'latitude') := NULL]
		if (period != 'all') {
			col_indices <- which(grepl(names(out), pattern = paste0('_', period)))
			if (!subset_teams) col_indices <- c(1:2, col_indices) # 1 and 2 are longitude & latitude
			out <- out[ , ..col_indices]
		}

		if (scale) {
			
			fields <- load_rast_fields(species_focal)

			min_max <- readRDS(paste0('./Outputs ', species_full, '/Extractions to Random Sites - Minimum & Maximum Values across Rasters - ', species_focal, '.rds'))
			min_max$base_raster <- substr(min_max$raster, 1, 1)
			team_info <- fread('./Outputs Shared Anonymized/team_info_anonymized.csv')
			codes <- team_info$code

			column_base_codes <- substr(names(out), 1, 1)
			column_base_codes_subs <- substr(names(out), 1, 2)
			column_base_codes_subs <- sub(column_base_codes_subs, pattern = '_', replacement = '')

			# get min/max value for each team from continuous rasters
			# NB We may need to match rasters across time periods
			# For example, a team submits one present-day raster, then two for each future. Here, it's OK to standardize all using the same values.
			# Counter-example: A team submits two present-day rasters using two different SDM algorithms. The present and future rasters should be scaled independently of one another by the algorithm that was used.
			# To do this, we use the "standardize_group" field in the "Scoring by Raster" sheets
			for (i in seq_along(codes)) {

				this_team_code <- codes[i]
				stand_groups <- fields$standardize_group[fields$team_code == this_team_code]
				stand_groups <- unique(stand_groups)

				min_max_team <- min_max[base_raster == this_team_code]

				# just one group of rasters by the team we need to standardize
				if (length(stand_groups) == 1) {
				
					min_val <- min(min_max_team$min, na.rm = TRUE)
					max_val <- max(min_max_team$max, na.rm = TRUE)

					cols <- which(column_base_codes == this_team_code)
					for (col in cols) {
						out[[col]] <- (out[[col]] - min_val) / (max_val - min_val)
					}
				
				} else { # more than one group to standardize

					for (group in seq_along(stand_groups)) {
						
						stand_group <- stand_groups[group]
						rasts_in_group <- fields$raster_code[fields$team_code == this_team_code & fields$standardize_group == stand_group]
						rasts_in_group <- paste0(this_team_code, rasts_in_group)

						this_min_max_team <- min_max_team[raster %in% rasts_in_group]

						min_val <- min(this_min_max_team$min, na.rm = TRUE)
						max_val <- max(this_min_max_team$max, na.rm = TRUE)

						cols <- which(column_base_codes_subs %in% rasts_in_group)
						for (col in cols) {
							out[[col]] <- (out[[col]] - min_val) / (max_val - min_val)
						}

					}
				
				}


			}
	
		}
		out <- as.data.table(out)
		out

	}

	# replace NA cells in projection raster with 0s based on land/sea mask using appropriate data source and resolution
	# r 			projection raster
	# team_code		team code (e.g., "A", "B", etc.)			
	# team_info 	output of load_team_fields()
	fill_terrestrial_NAs <- function(r, team_code, team_info) {

		climate_source <- team_info$predictors_climate_source[team_info$team_code == team_code]
		resol <- team_info$res[team_info$team_code == team_code]

		r_name <- names(r)

		extent <- ext(r)
		extent <- as.polygons(extent, crs = getCRS(r))
		extent <- buffer(extent, 80000)

		if (climate_source == 'CHELSA') {
			zeros <- rast('./Data/Land Mask CHELSA/landseamask.tif')
		} else if (climate_source == 'WorldClim') {
			zeros <- rast('./Data/Land Mask WorldClim/worldclim_21_mask.tif')
		}
		
		zeros <- crop(zeros, extent)

		if (resol == '2.5 arcmin') {
			zeros <- aggregate(zeros, fact = 5, fun = 'mean')
		} else if (resol == '5 arcmin') {
			zeros <- aggregate(zeros, fact = 10, fun = 'mean')
		} else if (resol == '10 arcmin') {
			zeros <- aggregate(zeros, fact = 20, fun = 'mean')
		} else if (resol == '0.5 deg') {
			zeros <- aggregate(zeros, fact = 60, fun = 'mean')
		} else if (resol == '1000×1000 m') {
			zeros <- resample(zeros, r, method = 'mode')
		} else if (resol != '30 arcsec') {
			stop('Do not know how to proceed with this resolution.')
		}

		# combine prediction raster and 0s raster
		zeros <- resample(zeros, r, method = 'mode')
		r_zeros <- c(r, zeros)
		r <- sum(r_zeros, na.rm = TRUE)		
		names(r) <- r_name
		r

	}

