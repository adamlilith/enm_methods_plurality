### SDM METHODOLOGICAL PLURALITY
### Anna Thonis, Nikki Calavari, Uzma Ashraf, Toni Lyn Morelli, and Adam B. Smith*
### * adam.smith@mobot.org | Missouri Botanical Garden | 2025-10
###
### Measure associations between clusters of teams and collections of workflow attributes:
### * Tabulate scoring of workflow decisions into a single data.table called "attributes." Some decisions are scored as binary, some numeric, and some categorical.
### * Implement Mantel test between distances between team predictions calculated in PCA space and workflow distances calculated using Gower distance.
### * Implement multiple regression of distance matrices, where the response is distances between team predictions in PCA space and predictors are distance matrices between teams in terms of individual workflow decisions.
###
### source('C:/Kaji/Research Group/ENMs - Plurality of Modeling Workflows (Anna Thonis)/enm_methods_plurality/07_multivariate_workflow_attributes_vs_raster_dissimilarity.r')
###
### CONTENTS
### multiple regression on distance matrices ###
###
#############
### setup ###
#############

	rm(list = ls())
	source('C:/Kaji/Research Group/ENMs - Plurality of Modeling Workflows (Anna Thonis)/enm_methods_plurality/00_shared_constants_and_functions.r')

	this_out_dir <- paste0(out_dir, '/Raster Distances ~ Decision Distances')
	dirCreate(this_out_dir)

	set.seed(1)

	# When concatenating multiple fields into one, sometimes we get entries like "NA, NA" or "NA, NA, NA" and so on. Replace these with just `NA`.
	#
	# y				Character vector.
	# field_names	Names of fields. Used to know how many NAs in a row there can be.
	#
	# Returns y.
	replace_y_NAs <- function(y, field_names) {
	
		for (i in 2:length(field_names)) {
		
			nas <- paste(rep('NA', i), collapse = ', ')
			these <- which(y == nas)
			if (length(these) > 0) y[these] <- NA
		
		}
		y
	
	}

	# Match attribute to rasters
	#
	# match_on 		'team' ==> attribute is a team-level attribute; 'raster' ==> attribute is a raster-level attribute
	# y 			Vector of attribute values.
	# attributes	Data frame with workflow attributes being collated.
	match_single_y <- function(match_on, y, attributes) {
	
		if (match_on == 'team') {
		
			x <- team_fields$team_code
			y <- y[match(attributes$team_code, x)]
		
		} else if (match_on == 'raster') {
		
			y_match <- y
		
		} else {
			stop('Bad `match_on` argument.')
		}
		y
	
	}

	# For a set of fields, make one column each in `attributes` and populate cells.
	#
	# match_on		'team' ==> attribute is scored by team; 'raster' ==> attribute is scored by raster
	# field_names	Character vector of field names.
	# attributes	Table for which to match the attributes.
	# prepend		Either NULL or character to pre-pend to new field names in `attributes`.
	match_field_by_field <- function(match_on, field_names, attributes, prepend) {

		for (field_name in field_names) {
		
			y_name <- field_name
			if (!is.null(prepend)) y_name <- paste0(prepend, '_', y_name)
			y <- if (match_on == 'team') {
				team_fields[[field_name]]
			} else if (match_on == 'raster') {
				rast_fields[[field_name]]
			} else {
				stop('Bad `match_on` argument.')
			}
			if (is.character(y)) y[y == 'NA'] <- NA_character_
			y <- as.numeric(y)
			y_match <- match_single_y(match_on = match_on, y = y, attributes = attributes)

			attributes[ , (y_name) := y_match]
			names(attributes)[ncol(attributes)] <- y_name
		
		}
		attributes

	}		

say('################################################')
say('### multiple regression on distance matrices ###')
say('################################################')

	# number of permutations used to assess significance
	nperm <- 99999
	# nperm <- 999 # for development.. fast!

	# p value for a variable to be dropped in backwards selection stage
	p_value_backward <- 0.05

	# p value for a variable to be added in forwards selection stage
	p_value_forward <- 0.01

	rast_fields <- load_rast_fields(species_focal = species_focal)
	team_fields <- load_team_fields(species_focal = species_focal)

	### add MEAN ODMAP score and MINIMUM SCORE ACROSS CATEGORIES to fields
	######################################################################

	odmap <- readRDS('./Outputs Shared Anonymized/ODMAP Scoring Anonymized.rds')

	y <- odmap[['means']]

	y <- y[grepl(y$species, pattern = species_full)]
	criteria <- c(paste0(1, LETTERS[1:5]), paste0(2, LETTERS[1:3]), paste0(3, LETTERS[1:4]), paste0(4, LETTERS[1:3]))
	odmap_scores <- y[ , ..criteria]
	
	odmap_means <- rowMeans(odmap_scores)
	odmap_mins <- apply(odmap_scores, 1, max) # taking max bc 1 = gold, 0 = deficient

	odmap_means <- 4 - odmap_means # reverse ranks so 4 = gold, 0 = deficient
	odmap_mins <- 4 - odmap_mins # reverse ranks so 4 = gold, 0 = deficient

	team_codes <- odmap$means$team_code[odmap$means$species == species_full]
	names(odmap_means) <- team_codes
	names(odmap_mins) <- team_codes

	team_fields$odmap_mean <- odmap_means[match(team_fields$team_code, team_codes)]
	team_fields$odmap_min <- odmap_mins[match(team_fields$team_code, team_codes)]

	### distances between rasters in PCA space
	##########################################

	preds <- load_predictions(species_focal = species_focal, period = 'all', scale = TRUE, subset_teams = TRUE)
	preds_trans <- t(preds)

	pca <- prcomp(preds_trans)
	scores <- pca$x[ , 1:2]
	scores <- as.data.frame(scores)

	rast_dists <- daisy(scores, metric = 'euclidean')

	### compile workflow attribute data
	###################################

	attributes <- rast_fields[ , c('team_code', 'raster_name', 'time_period')]
	rast_names_with_period <- paste(attributes$raster_name, attributes$time_period, sep = '_')
	
	names(attributes)[names(attributes) == 'time_period'] <- '4_time_period'

	### "team"

		y_name <- '0_team'
		match_on <- 'team'

		y <- team_fields$team_code
		
		y_match <- match_single_y(match_on, y, attributes)
		attributes[ , (y_name) := y_match]
		
	### "team" x raster output type (continuous vs thresholded)

		y_name <- '0_team'
		match_on <- 'raster'

		y <- rast_fields$team_code
		y[rast_fields$raster_name == 'M1'] <- 'Mc'
		y[rast_fields$raster_name == 'M2'] <- 'Mt'
		if (species_focal == 'Priona') {
			y[rast_fields$raster_name %in% c('N1', 'N2')] <- 'Nc'
			y[rast_fields$raster_name %in% c('N3', 'N4', 'N3a', 'N4a', 'N3b', 'N4b')] <- 'Nt'
		} else if (species_focal == 'Zamia') {
			y[rast_fields$raster_name %in% c('N1', 'N2', 'N3', 'N1a', 'N2a', 'N3a', 'N1b', 'N2b', 'N3b')] <- 'Nc'
			y[rast_fields$raster_name %in% c('N4', 'N5', 'N6', 'N4a', 'N5a', 'N6a', 'N4b', 'N5b', 'N6b')] <- 'Nt'
		}
		
		y_match <- match_single_y(match_on, y, attributes)
		attributes[ , (y_name) := y_match]
		
	### ODMAP *mean* score

		y_name <- '0_standards_mean_score'
		match_on <- 'team'

		y <- as.numeric(team_fields$odmap_mean)
		
		y_match <- match_single_y(match_on, y, attributes)
		attributes[ , (y_name) := y_match]
		
	# ### ODMAP *minimum* of mean score
	# # Not doing bc few teams >0

	# 	y_name <- '0_standards_minimum_score'
	# 	match_on <- 'team'

	# 	y <- as.numeric(team_fields$odmap_mean)

	# 	y_match <- match_single_y(match_on = match_on, y = y, attributes = attributes)
	# 	attributes[ , (y_name) := y_match]

	### number of occurrences

		y_name <- '1_number_of_occurrences_log10'
		nice <- 'Occurrences: Number of occurrences'
		match_on <- 'team'

		y <- as.numeric(team_fields$num_occurrences_minimum)
		y <- log10(y)

		y_match <- match_single_y(match_on = match_on, y = y, attributes = attributes)
		attributes[ , (y_name) := y_match]

	### number of predictors

		y_name <- '1_number_of_predictors_log10'
		match_on <- 'team'

		y <- as.numeric(team_fields$predictors_climate_nonclimate_num_total)
		y <- log10(y)

		y_match <- match_single_y(match_on = match_on, y = y, attributes = attributes)
		attributes[ , (y_name) := y_match]

	### climate: number of predictors

		y_name <- '1_number_of_climate_predictors_log10'
		match_on <- 'team'

		y <- as.numeric(team_fields$predictors_climate_num)
		y <- log10(y)

		y_match <- match_single_y(match_on = match_on, y = y, attributes = attributes)
		attributes[ , (y_name) := y_match]

	### non-climate: number of predictors

		y_name <- '1_number_of_non_climate_predictors_log10p'
		match_on <- 'team'

		y <- as.numeric(team_fields$predictors_nonclimate_num)
		y <- log10(y + 1)

		y_match <- match_single_y(match_on = match_on, y = y, attributes = attributes)
		attributes[ , (y_name) := y_match]

	### climate predictors

		prepend <- '1'
		match_on <- 'team'

		field_names <- c(paste0('predictors_climate_bio', 1:19), 'predictors_climate_num_other')
		attributes <- match_field_by_field(match_on = match_on, field_names = field_names, attributes = attributes, prepend = prepend)

	### non-climate predictors

		prepend <- '1'
		match_on <- 'team'

		field_names <- c('predictors_nonclimate_elevation', 'predictors_nonclimate_slope', 'predictors_nonclimate_aspect', 'predictors_nonclimate_terrain_ruggedness_position', 'predictors_nonclimate_forest_cover', 'predictors_nonclimate_land_cover', 'predictors_nonclimate_soil', 'predictors_nonclimate_human_impact', 'predictors_nonclimate_npp_ndvi', 'predictors_nonclimate_other')

		attributes <- match_field_by_field(match_on = match_on, field_names = field_names, attributes = attributes, prepend = prepend)

	### source of climate predictors

		y_name <- '1_climate_predictor_source'
		match_on <- 'team'

		y <- team_fields$predictors_climate_source
		y_match <- match_single_y(match_on = match_on, y = y, attributes = attributes)
		attributes[ , (y_name) := y_match]

	### spatial resolution: cell size
		
		y_name <- '1_spatial_resolution_km2_log10'
		match_on <- 'team'

		y <- as.numeric(team_fields$res_km2)
		y <- log10(y)

		y_match <- match_single_y(match_on = match_on, y = y, attributes = attributes)
		attributes[ , (y_name) := y_match]

	### collinearity: method

		prepend <- '2'
		y_name <- 'collinearity_method'
		match_on <- 'team'

		field_names <- c('collinearity_pca', 'collinearity_correlation', 'collinearity_vif', 'collinearity_other_method')
		attributes <- match_field_by_field(match_on = match_on, field_names = field_names, attributes = attributes, prepend = prepend)

	### modeling_software

		prepend <- '3'
		y_name <- 'sdm_software'
		match_on <- 'team'

		field_names <- c('modeling_software_enmeval', 'modeling_software_enmtools', 'modeling_software_wallace', 'modeling_software_biomod2', 'modeling_software_sabinansdm', 'modeling_software_sdm', 'modeling_software_miamaxent', 'modeling_software_enmsdmx', 'modeling_software_flexsdm', 'modeling_software_sdmtune', 'modeling_software_piecemeal')

		attributes <- match_field_by_field(match_on = match_on, field_names = field_names, attributes = attributes, prepend = prepend)

	### modeling_software used by developers

		prepend <- '3'
		y_name <- 'sdm_software_used_by_developers'
		match_on <- 'raster'

		y <- rep(0, nrow(rast_fields))
		y[rast_fields$team_code %in% c('E', 'B', 'C', 'H', 'D')] <- 1
		
		y_match <- match_single_y(match_on = match_on, y = y, attributes = attributes)
		attributes[ , (y_name) := y_match]

	### algorithm: number of algorithms used in ensemble (including 0)

		y_name <- '3_number_of_ensemble_algorithms_log10p'
		match_on <- 'raster'

		y <- as.numeric(rast_fields$algo_ensemble_number_of_models)
		y <- log10(y + 1)

		y_match <- match_single_y(match_on = match_on, y = y, attributes = attributes)
		attributes[ , (y_name) := y]

	### algorithm: identity

		y_name <- '3_sdm_algorithm'
		match_on <- 'raster'
		field_names <- c('algo_ensemble', 'algo_maxent', 'algo_maxnet', 'algo_glm', 'algo_gam', 'algo_rf')

		y <- rast_fields[ , ..field_names]
		y <- y[ , lapply(.SD, as.numeric)]
		y <- apply(y, 1, function(row) {
			cols_with_1 <- field_names[row == 1]
			if (length(cols_with_1) == 0) return(NA)
			paste(cols_with_1, collapse = ", ")
		})
		y <- replace_y_NAs(y = y, field_names = field_names)

		y_match <- match_single_y(match_on = match_on, y = y, attributes = attributes)
		attributes[ , (y_name) := y_match]

	### bias correction: method

		prepend <- '2'
		match_on <- 'team'

		field_names <- c('bias_correction_spatial_thinning', 'bias_correction_environmental_thinning', 'bias_correction_target_background', 'bias_correction_nonrandom_background')

		attributes <- match_field_by_field(match_on = match_on, field_names = field_names, attributes = attributes, prepend = prepend)

	### non-presence type

		y_name <- '2_non_presence_type'
		match_on <- 'team'

		field_names <- c('nonpres_type_background', 'nonpres_type_pseudoabsence', 'nonpres_type_target_background')

		y <- team_fields[ , ..field_names]
		y <- y[ , lapply(.SD, as.numeric)]
		y <- apply(y, 1, function(row) {
			cols_with_1 <- field_names[row == 1]
			if (length(cols_with_1) == 0) return(NA)
			paste(cols_with_1, collapse = ", ")
		})
		y <- replace_y_NAs(y = y, field_names = field_names)

		y_match <- match_single_y(match_on = match_on, y = y, attributes = attributes)
		attributes[ , (y_name) := y_match]

	### calibration region boundary
	
		prepend <- '2'
		match_on <- 'team'

		# non-presences: type
		field_names <- c('boundary_rectangle', 'boundary_natural', 'boundary_convex_hull', 'boundary_range_map', 'boundary_political', 'boundary_buffer_around_occurrences')

		attributes <- match_field_by_field(match_on = match_on, field_names = field_names, attributes = attributes, prepend = prepend)

	### calibration region extent
		
		y_name <- '2_calibration_extent_km2_log10'
		match_on <- 'team'

		y <- as.numeric(team_fields$extent_calibration_sans_water_km2)
		y <- log10(y)
	
		y_match <- match_single_y(match_on = match_on, y = y, attributes = attributes)
		attributes[ , (y_name) := y_match]

	# ### AUC
	# # NB fails!
		
	# 	y_name <- '4_auc'
	# 	match_on <- 'raster'

	# 	y <- as.numeric(rast_fields$eval_metric_auc_roc_value)

	# 	y_match <- match_single_y(match_on = match_on, y = y, attributes = attributes)
	# 	attributes[ , (y_name) := y_match]

	### thresholded predictions

		y_name <- '4_continuous_vs_thresholded'
		match_on <- 'raster'

		field_names <- c('prediction_type_continuous', 'prediction_type_binary_threshold', 'prediction_type_multi_threshold')

		y <- rast_fields[ , ..field_names]
		y <- y[ , lapply(.SD, as.numeric)]
		y <- apply(y, 1, function(row) {
			cols_with_1 <- field_names[row == 1]
			if (length(cols_with_1) == 0) return(NA)
			paste(cols_with_1, collapse = ", ")
		})
		y <- replace_y_NAs(y = y, field_names = field_names)

		y_match <- match_single_y(match_on = match_on, y = y, attributes = attributes)
		attributes[ , (y_name) := y_match]

	### time period

		y_name <- '4_time_period'
		match_on <- 'team'
		time_period <- 'late'

		y <- team_fields$`4_time_period`

		y_match <- match_single_y(match_on = match_on, y = y, attributes = attributes)
		attributes[ , (y_name) := y_match]

	# ### late-century time period
	# # present and mid-century time period are redundant with climate data source, so not doing them
	# # NB not using this bc highly correlated with climate data source.

	# 	y_name <- '4_late_century_time_period'
	# 	match_on <- 'team'
	# 	time_period <- 'late'

	# 	y <- team_fields$future_scenario_latecentury_year

	# 	y_match <- match_single_y(match_on = match_on, y = y, attributes = attributes)
	# 	attributes[ , (y_name) := y_match]

	### future: emission scenario
		
		y_name <- '4_emissions_scenario'
		match_on <- 'raster'
		
		field_names <- c('future_scenario_ensemble', 'future_scenario_ssp126', 'future_scenario_ssp245', 'future_scenario_ssp370', 'future_scenario_ssp585', 'future_scenario_rcp45', 'future_scenario_rcp85')

		y <- rast_fields[ , ..field_names]
		y <- y[ , lapply(.SD, as.numeric)]
		y <- apply(y, 1, function(row) {
			cols_with_1 <- field_names[row == 1]
			if (length(cols_with_1) == 0) return(NA)
			paste(cols_with_1, collapse = ", ")
		})
		y <- replace_y_NAs(y = y, field_names = field_names)
		y[rast_fields$time_period == 'present'] <- 'present'

		y_match <- match_single_y(match_on = match_on, y = y, attributes = attributes)
		attributes[ , (y_name) := y_match]

	### extrapolation: individual methods

		prepend <- '4'
		y_name <- 'extrapolation_method'
		match_on <- 'raster'
		
		field_names <- c('extrapolation_clamping_masking_clipping', 'extrapolation_exdet', 'extrapolation_mess', 'extrapolation_shape', 'extrapolation_area_of_applicability', 'extrapolation_response_curve_inspection', 'extrapolation_kissmig')

		y_match <- match_field_by_field(match_on = match_on, field_names = field_names, attributes = attributes, prepend = prepend)

	### taxonomy: accounted for subspecies
		
		if (species_focal == 'Priona') {

			y_name <- '1_modeled_only_mainland'
			match_on <- 'team'

			y <- as.numeric(team_fields$taxonomy_mainland_only)

			y_match <- match_single_y(match_on = match_on, y = y, attributes = attributes)
			attributes[ , (y_name) := y_match]

		}

	### save
	########

	saveRDS(attributes, paste0(this_out_dir, '/workflow_attributes.rds'))

	### Mantel test between team distances and workflow distances
	#############################################################

	removes <- c('team_code', 'raster_name')
	atts <- attributes[ , !removes, with = FALSE]

	# force characters to factors
	for (col in names(atts)) {
		if (is.character(atts[[col]])) {
			atts[[col]] <- factor(atts[[col]])
		}# else if (is.numeric(atts[[col]])) {
		#	atts[[col]] <- atts[[col]] / sd(atts[[col]], na.rm = TRUE)
		#}
	}

	# Gower distance on attributes
	att_dists <- cluster::daisy(atts, metric = 'gower')

	sink(paste0(this_out_dir, '/Multivariate Mantel Test on Distance between Rasters and Workflows.txt'), split = TRUE)
	say('MANTEL TEST: DISSIMILARITY BETWEEN RASTERS ~ DISSIMILARITY IN WORKFLOWS')
	say(date(), post = 1)
	mant <- vegan::mantel(rast_dists, att_dists, method = 'pearson', permutations = nperm)
	print(mant)
	sink()

	# Sets of decisions to analyze:
	# all					All decisions
	# all_but_predictors	All decisions but individual predictors
	# bioclims				BIOCLIM predictors only
	# predictors			BIOCLIM, non-bioclim climatic, and non-climatic predictors only
	# 1_sans_predictors		Stage "1" decisions excluding predictors (BIOCLIM, non-BIOCLIM climate, non-climate)
	# 0						SDM standards
	# 1						Data
	# 2						Modeling setup
	# 3						Algorithms
	# 4						Post-training / prediction / projection

	results <- data.table()
	sets <- c('all', 'all_but_predictors', 'bioclims', 'predictors', '1_sans_predictors', 'modeling_software', 0:4)
	# sets <- 'bioclims'
	for (set in sets) {

		say('MULTIPLE REGRESSION ON DISTANCE MATRICES')
		say(date(), post = 2)

		say(set, level = 2)

		if (set == 'all') {
			this_atts <- atts
		} else if (set == 'bioclims') {
		
			names <- names(atts)
			keeps <- grepl(names, pattern = 'predictors_climate_bio')
			this_atts <- atts[ , ..keeps]

		} else if (set == 'predictors') {
		
			names <- names(atts)
			keeps1 <- grepl(names, pattern = 'predictors_climate_bio')
			keeps2 <- grepl(names, pattern = 'predictors_nonclimate')
			keeps3 <- grepl(names, pattern = 'predictors_climate_num_other')
			keeps <- keeps1 | keeps2 | keeps3
			this_atts <- atts[ , ..keeps]

		} else if (set == 'all_but_predictors') {
		
			this_atts <- atts

			names <- names(this_atts)
			keeps1 <- !grepl(names, pattern = 'predictors_climate_bio')
			keeps2 <- !grepl(names, pattern = 'predictors_nonclimate')
			keeps3 <- !grepl(names, pattern = 'predictors_climate_num_other')
			keeps <- keeps1 & keeps2 & keeps3
			this_atts <- this_atts[ , ..keeps]

		} else if (set == '1_sans_predictors') {
		
			starts <- substr(names(atts), 1, 1)
			keeps <- which(starts == '1')
			this_atts <- atts[ , ..keeps]

			names <- names(this_atts)
			keeps1 <- !grepl(names, pattern = 'predictors_climate_bio')
			keeps2 <- !grepl(names, pattern = 'predictors_nonclimate')
			keeps3 <- !grepl(names, pattern = 'predictors_climate_num_other')
			keeps <- keeps1 & keeps2 & keeps3
			this_atts <- this_atts[ , ..keeps]

		} else if (set == 'modeling_software') {
		
			names <- names(atts)
			keeps <- grepl(names, pattern = 'modeling_software')
			this_atts <- atts[ , ..keeps]

		} else {

			starts <- substr(names(atts), 1, 1)
			keeps <- which(starts == set)
			this_atts <- atts[ , ..keeps]

		}

		# remove columns with zero variances
		this_atts <- this_atts[ , sapply(this_atts, function(x) length(unique(x)) > 1), with = FALSE]

		if (ncol(this_atts) == 0) {
			say('No attributes remaining after filtering.')
			next
		}

		# calculate individual distance matrices for each attribute
		n_atts <- ncol(this_atts)
		dist_matrices <- list()
		
		for (i in 1:n_atts) {
		
			x_name <- names(this_atts)[i]
			att_class <- class(this_atts[[i]])
			metric <- if (att_class == 'numeric') {
				'euclidean'
			} else {
				'gower'
			}

			single_col <- this_atts[ , i, with = FALSE]
			this_dist <- daisy(single_col, metric = metric)
			dist_matrices[[x_name]] <- this_dist
		
		}

		# remove matrices with zero variance (all values equal) highly correlated distance matrices
		if (length(dist_matrices) > 1) {

			### remove distance matrices with zero variance
			zero_var_vars <- c()
			for (x_name in names(dist_matrices)) {
				var_val <- var(as.vector(dist_matrices[[x_name]]), na.rm = TRUE)
				if (is.na(var_val) || var_val == 0) {
					zero_var_vars <- c(zero_var_vars, x_name)
				}
			}
			
			if (length(zero_var_vars) > 0) {
				
				say('Removing ', length(zero_var_vars), ' distance matrix(ices) with zero variance: ', paste(zero_var_vars, collapse = ', '))
				dist_matrices <- dist_matrices[!names(dist_matrices) %in% zero_var_vars]

			}

			### combine highly correlated matrices
			cor_threshold <- 0.7
			dist_mat_matrix <- sapply(dist_matrices, as.vector)
			cor_mat <- cor(dist_mat_matrix, method = 'spearman', use = 'pairwise.complete.obs')

			# find redundant variables and combine names
			name_mapping <- names(dist_matrices)
			names(name_mapping) <- names(dist_matrices)
			
			to_remove <- c()
			for (i in 1:(ncol(cor_mat) - 1)) {
				for (j in (i + 1):ncol(cor_mat)) {
					if (!is.na(cor_mat[i, j]) && abs(cor_mat[i, j]) > cor_threshold) {
						# append name of removed matrix (j) to kept matrix (i)
						retained_name <- names(dist_matrices)[i]
						removed_name <- names(dist_matrices)[j]
						name_mapping[retained_name] <- paste(name_mapping[retained_name], removed_name, sep = '_')
						to_remove <- c(to_remove, j)
					}
				}
			}
			
			if (length(to_remove) > 0) {

				to_remove <- unique(to_remove)
				removed_names <- names(dist_matrices)[to_remove]
				
				# rename retained matrices with combined names
				for (i in 1:length(dist_matrices)) {
					if (!(i %in% to_remove)) {
						names(dist_matrices)[i] <- name_mapping[names(dist_matrices)[i]]
					}
				}
				
				dist_matrices <- dist_matrices[-to_remove]
				say('Removed ', length(to_remove), ' highly correlated distance matrix(ices): ', paste(removed_names, collapse = ', '))
			}

		}

		# assign distance matrices to environment
		for (x_name in names(dist_matrices)) {
			assign(paste0('att_dist_', x_name), dist_matrices[[x_name]])
		}

		#################################
		### backwards model selection ###
		#################################

		say('backwards model selection', level = 2)

			# build formula with remaining distance matrices 36, 59
			if (species_focal == 'Zamia') { # These two distance matrices cause problems
				dist_matrices$`2_collinearity_other_method_3_modeling_software_enmtools` <- NULL
				dist_matrices$`2_collinearity_other_method` <- NULL
			}
			form <- 'rast_dists ~ '
			for (x_name in names(dist_matrices)) {
					assign(paste0('att_dist_', x_name), dist_matrices[[x_name]])
					form <- paste0(form, ' + att_dist_', x_name)
			}
			form <- as.formula(form)

			# NB if we use library(ecodist), there's some conflict that throws an error
			mrm <- ecodist::MRM(form, nperm = nperm, mrank = TRUE)
			
			say('FULL MODEL:')
			print(mrm)
			say('')

			simplified_vars <- names(dist_matrices)
			current_form <- form
			
			this_dist_matrices <- dist_matrices

			# drop variables in order from highest to lowest p value until all remaining are significant
			# (but stop if just one variable left)
			keep_simplifying <- any(mrm$coef[rownames(mrm$coef) != 'Int', 'pval'] > p_value_backward)
			while (length(this_dist_matrices) > 1 & keep_simplifying) {

				coef <- mrm$coef[rownames(mrm$coef) != 'Int', 'pval']
				if (!any(coef > p_value_backward)) {
					keep_simplifying <- FALSE
				} else {
					drop <- which.max(mrm$coef[rownames(mrm$coef) != 'Int', 'pval'])
					say('Dropping ', names(this_dist_matrices)[drop], '...')
					this_dist_matrices <- this_dist_matrices[-drop]

					form <- 'rast_dists ~ '
					for (x_name in names(this_dist_matrices)) {
						assign(paste0('att_dist_', x_name), this_dist_matrices[[x_name]])
						form <- paste0(form, ' + att_dist_', x_name)
					}
					form <- as.formula(form)

					# NB if we use library(ecodist), there's some conflict that throws an error
					mrm <- ecodist::MRM(form, nperm = nperm, mrank = TRUE)

				}

			}

			# if just one variable left and it's not significant
			coef <- mrm$coef[rownames(mrm$coef) != 'Int', 'pval']
			if (length(this_dist_matrices) == 1 & all(coef > 0.05)) {
			
				results <- rbind(
					results,
					data.table(
						model_sel_method = 'backward',
						variable = '(None)',
						rast_dists = NA_real_,
						pval = NA_real_,
						sig = NA_character_,
						set = set,
						F = NA_real_,
						p_model = NA_real_,
						R2 = NA_real_
					)
					
				)
			
			} else {
				# we ended with a model (at least one significant predictor)

				say('OPTIMAL BACKWARDS-SELECTION MODEL:', pre = 1)
				print(mrm)
				say('')
			
				# remember
				this_results <- mrm$coef
				vars <- rownames(this_results) 
				this_results <- as.data.table(this_results)
				vars <- data.table(variable = vars)
				this_results <- cbind(vars, this_results)

				this_results[ , sig := ifelse(pval < 0.05, '*', '-')]
				this_results[ , set := set]
				this_results[ , F := mrm$F.test[1]]
				this_results[ , p_model := mrm$F.test[2]]
				this_results[ , R2 := mrm$r.squared[1]]
				this_results[ , model_sel_method := 'backwards']
				
				results <- rbind(results, this_results)

			}

		#############################################
		### forward then backward model selection ###
		#############################################

		say('forward/backward model selection', level = 2)

			# add first variable to model
			temp_results <- data.table()
			for (i in seq_along(dist_matrices)) {

				variable <- names(dist_matrices)[i]
				form <- paste0('rast_dists ~ ', paste0('att_dist_', variable))
				form <- as.formula(form)

				# NB if we use library(ecodist), there's some conflict that throws an error
				mrm <- ecodist::MRM(form, nperm = nperm, mrank = TRUE)

				pval <- mrm$coef[paste0('att_dist_', variable), 'pval']
				temp_results <- rbind(temp_results, data.table(variable = variable, pval = pval))

			} # next variable 

			# not even one significant variable :(
			if (all(temp_results$pval > p_value_forward)) {
			
				results <- rbind(
					results,
					data.table(
						model_sel_method = 'forward then backward',
						variable = '(None)',
						rast_dists = NA_real_,
						pval = NA_real_,
						sig = NA_character_,
						set = set,
						F = NA_real_,
						p_model = NA_real_,
						R2 = NA_real_
					)
				)
			
			} else {
			
				### build model variable-by-variable
				# include only significant variables
				# include variable that adds most to R2 each step
				# stop when no more significant variables

				best <- which.min(temp_results$pval) 
				ins <- temp_results$variable[best]
				temp_results <- temp_results[-best]
				base_test_form <- paste0('rast_dists ~ 1 + ', paste0('att_dist_', ins))

				should_stop <- all(temp_results$pval > p_value_forward)
				while (!should_stop) {
				
					outs <- setdiff(names(dist_matrices), ins)
					if (length(outs) == 0) {
						should_stop <- TRUE
					} else {

						# add single variables to model
						temp_results <- data.table()
						for (i in seq_along(outs)) {

							variable <- outs[i]
							dist_matrix <- dist_matrices[[variable]]
							test_form <- paste0(base_test_form, ' + ', paste0('att_dist_', variable))
							test_form <- as.formula(test_form)

							# NB if we use library(ecodist), there's some conflict that throws an error
							mrm <- ecodist::MRM(test_form, nperm = nperm, mrank = TRUE)

							pval <- mrm$coef[paste0('att_dist_', variable), 'pval']
							temp_results <- rbind(temp_results, data.table(variable = variable, pval = pval))

						} # next variable

						temp_results <- temp_results[pval <= p_value_forward]
						if (nrow(temp_results) > 0) {
						
							best <- which.min(temp_results$pval)
							ins <- c(ins, temp_results$variable[best])
						
							base_test_form <- paste0('rast_dists ~ ', paste(paste0('att_dist_', ins), collapse = ' + '))

						} else {
							should_stop <- TRUE
						}

					} # we could add a new variable
				
				} # can we continue adding variables?
			
				# NB if we use library(ecodist), there's some conflict that throws an error
				mrm <- ecodist::MRM(base_test_form, nperm = nperm, mrank = TRUE)

				### backwards selection step to retain all significant variables
				################################################################

				coef <- mrm$coef[rownames(mrm$coef) != 'Int', 'pval']
				keep_simplifying <- any(coef > p_value_backward)
				while (keep_simplifying) {

					coef <- mrm$coef[rownames(mrm$coef) != 'Int', 'pval']
					if (!any(coef > p_value_backward)) {
						keep_simplifying <- FALSE
					} else {

						drop <- which.max(mrm$coef[rownames(mrm$coef) != 'Int', 'pval'] > p_value_backward)# + 1
						drop_var <- substr(names(drop), 10, nchar(names(drop)))
						say('Dropping ', drop_var, '...')
						ins <- ins[ins != drop_var]
						
						form <- paste0('rast_dists ~ ', paste(paste0('att_dist_', ins), collapse = ' +'))
						form <- as.formula(form)

						# NB if we use library(ecodist), there's some conflict that throws an error
						mrm <- ecodist::MRM(form, nperm = nperm, mrank = TRUE)

					}
				
				}

				# remember
				this_results <- mrm$coef
				vars <- rownames(this_results) 
				this_results <- as.data.table(this_results)
				vars <- data.table(variable = vars)
				this_results <- cbind(vars, this_results)

				this_results[ , sig := ifelse(pval < 0.05, '*', '-')]
				this_results[ , set := set]
				this_results[ , F := mrm$F.test[1]]
				this_results[ , p_model := mrm$F.test[2]]
				this_results[ , R2 := mrm$r.squared[1]]
				this_results[ , model_sel_method := 'forward then backward']
				
				results <- rbind(results, this_results)

			} # in initial model, could we add at least one variable?

	} # next set

	colnames(results)[colnames(results) == 'rast_dists'] <- 'coefficient'
	colnames(results)[colnames(results) == 'pval'] <- 'p_coefficient'
	results[ , variable := sub(variable, pattern = 'att_dist_', replacement = '')]
	fwrite(results, paste0(this_out_dir, '/Multivariate Regression on Distance Matrices.csv'))


say('DONE', level = 1)
