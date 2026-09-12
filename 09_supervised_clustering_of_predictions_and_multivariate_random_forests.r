### SDM METHODOLOGICAL PLURALITY
### Anna Thonis, Nikki Calavari, Uzma Ashraf, Toni Lyn Morelli, and Adam B. Smith*
### * adam.smith@mobot.org | Missouri Botanical Garden | 2025-10
###
### Estimate importance of workflow attributes on distances between rasters in PC space using multivariate RF, and graph results.
###
### source('C:/Kaji/Research Group/ENMs - Plurality of Modeling Workflows (Anna Thonis)/enm_methods_plurality/09_supervised_clustering_of_predictions_and_multivariate_random_forests.r')
###
### CONTENTS
### multivariate RF on PC scores in 1st two PC axes ###
### graph of variable importance ###
### plot of variable importance and selected biplots for main text for Prionailurus bengalensis ###
### selected biplots for main text for Prionailurus bengalensis ###
### selected biplots for main text for Zamia prasina ###
### plot of variable importance and selected biplots for main text for BOTH SPECIES together ###
###
#############
### setup ###
#############

	rm(list = ls())
	source('C:/Kaji/Research Group/ENMs - Plurality of Modeling Workflows (Anna Thonis)/enm_methods_plurality/00_shared_constants_and_functions.r')

	this_out_dir <- paste0(out_dir, '/Multivariate RF on Differences in PC Space')
	dirCreate(this_out_dir)

	RNGkind("L'Ecuyer-CMRG")
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

say('#######################################################')
say('### multivariate RF on PC scores in 1st two PC axes ###')
say('#######################################################')

	rast_fields <- load_rast_fields(species_focal = species_focal)
	team_fields <- load_team_fields(species_focal = species_focal)

	### add MEAN ODMAP score ACROSS CATEGORIES to fields
	####################################################

	odmap <- readRDS('./Outputs Shared Anonymized/ODMAP Scoring Anonymized.rds')

	y <- odmap[['means']]
	y <- y[grepl(y$species, pattern = species_full)]
	criteria <- c(paste0(1, LETTERS[1:5]), paste0(2, LETTERS[1:3]), paste0(3, LETTERS[1:4]), paste0(4, LETTERS[1:3]))
	odmap_scores <- y[ , ..criteria]
	
	odmap_means <- rowMeans(odmap_scores)
	odmap_means <- 4 - odmap_means # reverse ranks so 4 = gold, 0 = deficient

	team_codes <- odmap$means$team_code[odmap$means$species == species_full]
	names(odmap_means) <- team_codes

	team_fields$odmap_mean <- odmap_means[match(team_fields$team_code, team_codes)]

	### PCA on predictions
	######################

	preds <- load_predictions(species_focal = species_focal, period = 'all', scale = TRUE, subset_teams = TRUE)
	preds_trans <- t(preds)

	pca <- prcomp(preds_trans)
	scores <- pca$x[ , 1:2]
	scores <- as.data.table(scores)

	### compile workflow attribute data
	###################################

	attributes <- rast_fields[ , c('team_code', 'raster_name', 'time_period')]
	rast_names_with_period <- paste(attributes$raster_name, attributes$time_period, sep = '_')
	
	names(attributes)[names(attributes) == 'time_period'] <- '4_time_period'
	nice_names <- data.table(computer = '4_time_period', nice = c('Time Period'))

	### "team"

		y_name <- '0_team'
		match_on <- 'team'

		y <- team_fields$team_code
		
		y_match <- match_single_y(match_on, y, attributes)
		attributes[ , (y_name) := y_match]
		nice_names <- rbind(nice_names, data.table(computer = '0_team', nice = 'Team'))
		
	# ### "team" x raster output type (continuous vs thresholded)

	# 	y_name <- '0_team_thresholded'
	# 	match_on <- 'raster'

	# 	y <- rast_fields$team_code
	# 	y[rast_fields$raster_name == 'M1'] <- 'Mc'
	# 	y[rast_fields$raster_name == 'M2'] <- 'Mt'
	# 	if (species_focal == 'Priona') {
	# 		y[rast_fields$raster_name %in% c('N1', 'N2')] <- 'Nc'
	# 		y[rast_fields$raster_name %in% c('N3', 'N4', 'N3a', 'N4a', 'N3b', 'N4b')] <- 'Nt'
	# 	} else if (species_focal == 'Zamia') {
	# 		y[rast_fields$raster_name %in% c('N1', 'N2', 'N3', 'N1a', 'N2a', 'N3a', 'N1b', 'N2b', 'N3b')] <- 'Nc'
	# 		y[rast_fields$raster_name %in% c('N4', 'N5', 'N6', 'N4a', 'N5a', 'N6a', 'N4b', 'N5b', 'N6b')] <- 'Nt'
	# 	}
		
	# 	y_match <- match_single_y(match_on, y, attributes)
	# 	attributes[ , (y_name) := y_match]
	# 	nice_names <- rbind(nice_names, data.table(computer = '0_team_thresholded', nice = 'Team x Continuous/Thresholded'))
		
	### ODMAP *mean* score

		y_name <- '5_standards_mean_score'
		match_on <- 'team'

		y <- as.numeric(team_fields$odmap_mean)
		
		y_match <- match_single_y(match_on, y, attributes)
		attributes[ , (y_name) := y_match]
		nice_names <- rbind(nice_names, data.table(computer = '5_standards_mean_score', nice = 'SDM Standards'))
		
	# ### ODMAP *minimum* of mean score
	# # Not doing bc few teams >0

	# 	y_name <- '0_standards_minimum_score'
	# 	match_on <- 'team'

	# 	y <- as.numeric(team_fields$odmap_mean)

	# 	y_match <- match_single_y(match_on = match_on, y = y, attributes = attributes)
	# 	attributes[ , (y_name) := y_match]

	### number of occurrences

		y_name <- '1_number_of_occurrences'
		nice <- 'Occurrences: Number of occurrences'
		match_on <- 'team'

		y <- as.numeric(team_fields$num_occurrences_minimum)

		y_match <- match_single_y(match_on = match_on, y = y, attributes = attributes)
		attributes[ , (y_name) := y_match]
		nice_names <- rbind(nice_names, data.table(computer = '1_number_of_occurrences', nice = 'Number of Occurrences'))

	### number of predictors

		y_name <- '1_number_of_predictors'
		match_on <- 'team'

		y <- as.numeric(team_fields$predictors_climate_nonclimate_num_total)

		y_match <- match_single_y(match_on = match_on, y = y, attributes = attributes)
		attributes[ , (y_name) := y_match]
		nice_names <- rbind(nice_names, data.table(computer = '1_number_of_predictors', nice = 'Number of Predictors'))
		
	### climate: number of predictors

		y_name <- '1_number_of_climate_predictors'
		match_on <- 'team'

		y <- as.numeric(team_fields$predictors_climate_num)

		y_match <- match_single_y(match_on = match_on, y = y, attributes = attributes)
		attributes[ , (y_name) := y_match]
		nice_names <- rbind(nice_names, data.table(computer = '1_number_of_climate_predictors', nice = 'Number of Climate Predictors'))
		
	### non-climate: number of predictors

		y_name <- '1_number_of_non_climate_predictors'
		match_on <- 'team'

		y <- as.numeric(team_fields$predictors_nonclimate_num)

		y_match <- match_single_y(match_on = match_on, y = y, attributes = attributes)
		attributes[ , (y_name) := y_match]
		nice_names <- rbind(nice_names, data.table(computer = '1_number_of_non_climate_predictors', nice = 'Number of Non-Climate Predictors'))

	### climate predictors

		prepend <- '1'
		match_on <- 'team'

		field_names <- c(paste0('predictors_climate_bio', 1:19), 'predictors_climate_num_other')
		attributes <- match_field_by_field(match_on = match_on, field_names = field_names, attributes = attributes, prepend = prepend)
		nice_names <- rbind(nice_names, data.table(computer = c(paste0('1_predictors_climate_bio', 1:19), '1_predictors_climate_num_other'), nice = c(paste0('BIOCLIM ', prefix(1:19, 2)), 'Number of Other Climate Predictors')))

	### non-climate predictors

		prepend <- '1'
		match_on <- 'team'

		field_names <- c('predictors_nonclimate_elevation', 'predictors_nonclimate_slope', 'predictors_nonclimate_aspect', 'predictors_nonclimate_terrain_ruggedness_position', 'predictors_nonclimate_forest_cover', 'predictors_nonclimate_land_cover', 'predictors_nonclimate_soil', 'predictors_nonclimate_human_impact', 'predictors_nonclimate_npp_ndvi', 'predictors_nonclimate_other')

		attributes <- match_field_by_field(match_on = match_on, field_names = field_names, attributes = attributes, prepend = prepend)
		nice_names <- rbind(nice_names, data.table(computer = paste0(prepend, '_', field_names), nice = c('Elevation', 'Slope', 'Aspect', 'Terrain Ruggedness/Position', 'Forest Cover', 'Land Cover', 'Soil', 'Human Impact', 'NPP/NDVI', 'Other Non-Climate Predictors')))

	### source of climate predictors

		y_name <- '1_climate_predictor_source'
		match_on <- 'team'

		y <- team_fields$predictors_climate_source
		y_match <- match_single_y(match_on = match_on, y = y, attributes = attributes)
		attributes[ , (y_name) := y_match]
		nice_names <- rbind(nice_names, data.table(computer = '1_climate_predictor_source', nice = 'Source of Climate Predictors'))

	### spatial resolution: cell size
		
		y_name <- '1_spatial_resolution_km2'
		match_on <- 'team'

		y <- as.numeric(team_fields$res_km2)

		y_match <- match_single_y(match_on = match_on, y = y, attributes = attributes)
		attributes[ , (y_name) := y_match]
		nice_names <- rbind(nice_names, data.table(computer = '1_spatial_resolution_km2', nice = 'Spatial Resolution (km²)'))

	### collinearity: method

		prepend <- '2'
		y_name <- 'collinearity_method'
		match_on <- 'team'

		field_names <- c('collinearity_pca', 'collinearity_correlation', 'collinearity_vif', 'collinearity_other_method')
		attributes <- match_field_by_field(match_on = match_on, field_names = field_names, attributes = attributes, prepend = prepend)
		nice_names <- rbind(nice_names, data.table(computer = paste0(prepend, '_', field_names), nice = c('Collinearity: PCA', 'Collinearity: Correlation', 'Collinearity: VIF', 'Collinearity: Other Method')))

	### modeling_software

		prepend <- '3'
		y_name <- 'sdm_software'
		match_on <- 'team'

		field_names <- c('modeling_software_enmeval', 'modeling_software_enmtools', 'modeling_software_wallace', 'modeling_software_biomod2', 'modeling_software_sabinansdm', 'modeling_software_sdm', 'modeling_software_miamaxent', 'modeling_software_enmsdmx', 'modeling_software_flexsdm', 'modeling_software_sdmtune', 'modeling_software_piecemeal')

		attributes <- match_field_by_field(match_on = match_on, field_names = field_names, attributes = attributes, prepend = prepend)
		nice_names <- rbind(nice_names, data.table(computer = paste0(prepend, '_', field_names), nice = c('Modeling Software: ENMeval', 'Modeling Software: ENMTools', 'Modeling Software: Wallace', 'Modeling Software: biomod2', 'Modeling Software: sabinansdm', 'Modeling Software: sdm', 'Modeling Software: MIAmaxent', 'Modeling Software: enmSdmX', 'Modeling Software: flexsdm', 'Modeling Software: SDMtune', 'Modeling Software: Piecemeal')))

	### modeling_software used by developers

		prepend <- '3'
		y_name <- 'sdm_software_used_by_developers'
		match_on <- 'raster'

		y <- rep(0, nrow(rast_fields))
		y[rast_fields$team_code %in% c('E', 'B', 'C', 'H', 'D')] <- 1
		
		y_match <- match_single_y(match_on = match_on, y = y, attributes = attributes)
		attributes[ , (y_name) := y_match]
		nice_names <- rbind(nice_names, data.table(computer = y_name, nice = 'Modeling Software Used by Developers'))

	### algorithm: number of algorithms used in ensemble (including 0)

		y_name <- '3_number_of_ensemble_algorithms'
		match_on <- 'raster'

		y <- as.numeric(rast_fields$algo_ensemble_number_of_models)

		y_match <- match_single_y(match_on = match_on, y = y, attributes = attributes)
		attributes[ , (y_name) := y]
		nice_names <- rbind(nice_names, data.table(computer = y_name, nice = 'Number of Algorithms in Ensemble'))

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
		nice_names <- rbind(nice_names, data.table(computer = y_name, nice = 'SDM Algorithm'))

	### bias correction: method

		prepend <- '2'
		match_on <- 'team'

		field_names <- c('bias_correction_spatial_thinning', 'bias_correction_environmental_thinning', 'bias_correction_target_background', 'bias_correction_nonrandom_background')

		attributes <- match_field_by_field(match_on = match_on, field_names = field_names, attributes = attributes, prepend = prepend)
		nice_names <- rbind(nice_names, data.table(computer = paste0(prepend, '_', field_names), nice = c('Bias Correction: Spatial Thinning', 'Bias Correction: Environ. Thinning', 'Bias Correction: Target Background', 'Bias Correction: Non-random BG')))

	### non-presence type

		y_name <- '2_non_presence_type'
		match_on <- 'team'

		field_names <- c('nonpres_type_background', 'nonpres_type_pseudoabsence')

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
		nice_names <- rbind(nice_names, data.table(computer = y_name, nice = 'Non-presence Type'))

	### calibration region boundary
	
		prepend <- '2'
		match_on <- 'team'

		# non-presences: type
		field_names <- c('boundary_rectangle', 'boundary_natural', 'boundary_convex_hull', 'boundary_range_map', 'boundary_political', 'boundary_buffer_around_occurrences')

		attributes <- match_field_by_field(match_on = match_on, field_names = field_names, attributes = attributes, prepend = prepend)
		nice_names <- rbind(nice_names, data.table(computer = paste0(prepend, '_', field_names), nice = c('Calibration Region: Rectangle', 'Calibration Region: Natural', 'Calibration Region: Convex Hull', 'Calibration Region: Range Map', 'Calibration Region: Political', 'Calibration Region: Buffer')))

	### calibration region extent
		
		y_name <- '2_calibration_extent_km2'
		match_on <- 'team'

		y <- as.numeric(team_fields$extent_calibration_sans_water_km2)
	
		y_match <- match_single_y(match_on = match_on, y = y, attributes = attributes)
		attributes[ , (y_name) := y_match]
		nice_names <- rbind(nice_names, data.table(computer = y_name, nice = 'Calibration Region Extent'))

	### AUC
		
		y_name <- '4_auc'
		match_on <- 'raster'

		y <- as.numeric(rast_fields$eval_metric_auc_roc_value)

		y_match <- match_single_y(match_on = match_on, y = y, attributes = attributes)
		attributes[ , (y_name) := y_match]
		nice_names <- rbind(nice_names, data.table(computer = y_name, nice = 'AUC'))

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
		nice_names <- rbind(nice_names, data.table(computer = y_name, nice = 'Contin./Binary Thresh./Multi-thresh.'))

	### time period

		y_name <- '4_time_period'
		match_on <- 'team'
		time_period <- 'late'

		y <- team_fields$`4_time_period`

		y_match <- match_single_y(match_on = match_on, y = y, attributes = attributes)
		attributes[ , (y_name) := y_match]
		nice_names <- rbind(nice_names, data.table(computer = y_name, nice = 'Time Period'))

	# ### late-century time period
	# # present and mid-century time period are redundant with climate data source, so not doing them
	# # NB not using this bc highly correlated with climate data source.

	# 	y_name <- '4_late_century_time_period'
	# 	match_on <- 'team'
	# 	time_period <- 'late'

	# 	y <- team_fields$future_scenario_latecentury_year

	# 	y_match <- match_single_y(match_on = match_on, y = y, attributes = attributes)
	# 	attributes[ , (y_name) := y_match]
	# 	nice_names <- rbind(nice_names, data.table(computer = y_name, nice = 'Late-Century Time Period'))

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
		nice_names <- rbind(nice_names, data.table(computer = y_name, nice = 'Present/Future Scenario'))

	### extrapolation: individual methods

		prepend <- '4'
		y_name <- 'extrapolation_method'
		match_on <- 'raster'
		
		field_names <- c('extrapolation_clamping_masking_clipping', 'extrapolation_exdet', 'extrapolation_mess', 'extrapolation_shape', 'extrapolation_area_of_applicability', 'extrapolation_response_curve_inspection', 'extrapolation_kissmig')

		y_match <- match_field_by_field(match_on = match_on, field_names = field_names, attributes = attributes, prepend = prepend)
		attributes <- y_match
		nice_names <- rbind(nice_names, data.table(computer = paste0(prepend, '_', field_names), nice = c('Extrap.: Clamp/Mask/Clip', 'Extrap.: ExDet', 'Extrap.: MESS', 'Extrap.: Shape', 'Extrap.: Area of Applicability', 'Extrap.: Response Curves Inspect', 'Extrap.: KISSMig')))

	### taxonomy: accounted for subspecies
		
		if (species_focal == 'Priona') {

			y_name <- '1_modeled_only_mainland'
			match_on <- 'team'

			y <- as.numeric(team_fields$taxonomy_mainland_only)

			y_match <- match_single_y(match_on = match_on, y = y, attributes = attributes)
			attributes[ , (y_name) := y_match]
			nice_names <- rbind(nice_names, data.table(computer = y_name, nice = 'Modeled Only Mainland'))

		}

	### save
	########

	saveRDS(attributes, paste0(this_out_dir, '/workflow_attributes.rds'))
	saveRDS(nice_names, paste0(this_out_dir, '/workflow_attributes_nice_names.rds'))

	### multivariate random forest
	predictors <- names(attributes)[grepl('^[0-9]', names(attributes))]
	# predictors <- predictors[predictors %notin% '0_team']
	forest_data <- cbind(scores, attributes[ , ..predictors])

	# convert character columns to factors
	char_cols <- names(forest_data)[sapply(forest_data, is.character)]
	forest_data[ , (char_cols) := lapply(.SD, as.factor), .SDcols = char_cols]

	options(rf.cores = 1) # deterministic execution to ensure seed pertains

	for (na_opt in c('na.impute', 'na.omit', 'median')) {

		this_data <- forest_data
		if (na_opt == 'na.impute') {
			na.action <- 'na.impute'
		} else if (na_opt == 'na.omit') {
			na.action <- 'na.omit'
		} else if (na_opt == 'median') {
			
			# replace NAs with median of each numeric column
			for (col in names(this_data)) {
				if (is.numeric(this_data[[col]])) {
					this_data[[col]][is.na(this_data[[col]])] <- median(this_data[[col]], na.rm = TRUE)
				} else if (is.factor(this_data[[col]])) {
					# replace NAs with the most frequent level
					most_freq <- names(sort(table(this_data[[col]]), decreasing = TRUE))[1]
					this_data[[col]][is.na(this_data[[col]])] <- most_freq
				}
			}

			na.action <- 'na.omit'
		
		}

		mrf <- rfsrc(
			Multivar(PC1, PC2) ~ .,
			data = this_data, 
			mtry = ceiling(sqrt(ncol(this_data) - 2)), # good for n ~ p
			ntree = 10000,
			nodesize = ceiling(nrow(this_data) / 10), # size of each "end" cluster
			nimpute = 3, # makes OOB optimistic but better for cases with small n... only for na.action = 'na.impute'
			na.action = na.action,
			importance = TRUE,
			seed = 1
		)

		var_imp <- vimp(mrf, importance = 'permute', nrep = 10000, seed = 1)

		# PC1
		this_vi <- var_imp$regrOutput$PC1$importance
		this_vi[this_vi < 0] <- 0
		attribute <- names(this_vi)
		nice_name <- nice_names$nice[match(attribute, nice_names$computer)]
		prop_var_explained <- pca$sdev[1]^2 / sum(pca$sdev^2)
		weighted <- this_vi * prop_var_explained
		proportion <- weighted / sum(weighted)
		stage <- substr(attribute, 1, 1)
		vi_pc1 <- data.table(PC = 1, attribute = attribute, nice_name = nice_name, raw = this_vi, weighted = weighted, proportion = proportion, stage = stage)

		# PC2
		this_vi <- var_imp$regrOutput$PC2$importance
		this_vi[this_vi < 0] <- 0
		attribute <- names(this_vi)
		nice_name <- nice_names$nice[match(attribute, nice_names$computer)]
		prop_var_explained <- pca$sdev[2]^2 / sum(pca$sdev^2)
		weighted <- this_vi * prop_var_explained
		proportion <- weighted / sum(weighted)
		stage <- substr(attribute, 1, 1)
		vi_pc2 <- data.table(PC = 2, attribute = attribute, nice_name = nice_name, raw = this_vi, weighted = weighted, proportion = proportion, stage = stage)

		# PC1 + PC2
		raw <- vi_pc1$raw + vi_pc2$raw
		weighted <- vi_pc1$weighted + vi_pc2$weighted
		proportion <- weighted / sum(weighted)
		stage <- substr(attribute, 1, 1)
		vi_pc1_pc2 <- data.table(PC = '1 & 2', attribute = attribute, nice_name = nice_name, raw = raw, weighted = weighted, proportion = proportion, stage = stage)

		order <- order(vi_pc1_pc2$proportion, decreasing = TRUE)
		vi_pc1 <- vi_pc1[order]
		vi_pc2 <- vi_pc2[order]
		vi_pc1_pc2 <- vi_pc1_pc2[order]

		### proportion of total variance explained
		##########################################

		# PC variance
		pc_variance <- pca$sdev^2

		# OOB predictions
		oob_pred_1 <- mrf$regrOutput$PC1$predicted.oob
		oob_pred_2 <- mrf$regrOutput$PC2$predicted.oob

		# SSEs (obs - OOB prediction)^2
		sse_pc1 <- sum((mrf$yvar$PC1 - oob_pred_1)^2)
		sse_pc2 <- sum((mrf$yvar$PC2 - oob_pred_2)^2)

		# total SS
		ss_pc1 <- sum((this_data$PC1 - mean(this_data$PC1))^2)
		ss_pc2 <- sum((this_data$PC2 - mean(this_data$PC2))^2)

		# calculate individual pseudo-r2
		r2_pc1 <- 1 - (sse_pc1 / ss_pc1)
		r2_pc2 <- 1 - (sse_pc2 / ss_pc2)

		r2 <- c(PC1 = r2_pc1, PC2 = r2_pc2)

		# eigenvalue-weighted variance explained by model
		pc_1_2_variance_explained <- sum(pc_variance[1:2] * r2)    

		# proportion of the 2D PC space explained
		prop_pcs_1_2_explained <- pc_1_2_variance_explained / sum(pc_variance[1:2])

		### poropoton of explained variance explained by each predictor
		###############################################################

		vimp_pc1 <- mrf$regrOutput$PC1$importance
		vimp_pc2 <- mrf$regrOutput$PC2$importance

		# total variance explained by each predictor
		n_obs <- length(mrf$yvar$PC1)
		prop_total_var_pc1 <- vimp_pc1 / (ss_pc1 / n_obs)
		prop_total_var_pc2 <- vimp_pc2 / (ss_pc2 / n_obs)

		# eigenvalue-weighted proportion of variance explained
		predictor_absolute_variance <- (prop_total_var_pc1 * pc_variance[1]) + (prop_total_var_pc2 * pc_variance[2])

		prop_of_explained_variance <- predictor_absolute_variance / pc_1_2_variance_explained

		prop_of_explained_variance <- sort(prop_of_explained_variance, decreasing = TRUE)	

		vi <- list(
			PC1 = vi_pc1,
			PC2 = vi_pc2,
			PC1_and_PC2 = vi_pc1_pc2,
			R2 = list(
				r2 = r2,
				prop_pcs_1_2_explained = prop_pcs_1_2_explained,
				of_var_explained_prop_explained_by_each_pred_direct_effects = prop_of_explained_variance,
				of_var_explained_prop_explained_by_direct_effects = sum(prop_of_explained_variance)
			)
		)

		saveRDS(vi, paste0(this_out_dir, '/MRF Variable Importance - ', na_opt, '.rds'))

		say(na_opt, level = 2)
		say('r2 = ', r2)
		say('prop_pcs_1_2_explained = ', prop_pcs_1_2_explained)
		say('of_var_explained_prop_explained_by_direct_effects = ', sum(prop_of_explained_variance))

	} # next AN optoion

say('####################################')
say('### graph of variable importance ###')
say('####################################')

	for (na_opt in c('na.impute', 'na.omit', 'median')) {

		vis <- readRDS(paste0(this_out_dir, '/MRF Variable Importance - ', na_opt, '.rds'))

		# graph variable importance
		var_imps <- list()
		for (i in 1:3) {

			if (i %in% 1:2) {
				title <- paste0(letters[i], ') PC ', i)
			} else {
				title <- 'c) PCs 1 & 2'
			}

			var_imps[[i]] <- ggplot(vis[[i]], aes(x = reorder(nice_name, proportion), y = proportion, fill = stage)) +
				geom_bar(stat = 'identity', color = 'gray30', linewidth = 0.2) +
				scale_fill_manual(
					values = c('0' = 'white', '1' = '#60ADF1', '2' = '#F9CB40', '3' = '#11EFC9', '4' = '#F56096', '5' = 'gray40'),
					labels = c('0' = 'Team', '1' = 'Inputs', '2' = 'Stage setting', '3' = 'Algorithm/software', '4' = 'Post-processing', '5' = 'Standards'),
					name = NULL
				) +
				labs(x = NULL, y = 'Relative importance') +
				theme_bw() +
				coord_flip() +
				ggtitle(title) +
				theme(
					legend.position = c(0.99, 0.01),
					legend.justification = c(1, 0),
					axis.text = element_text(size = 8)
				)

		}

		var_imp <- var_imps[[1]] + var_imps[[2]] + var_imps[[3]]
		ggsave(paste0(this_out_dir, '/MRF Variable Importance - ', na_opt, '.png'), var_imp, width = 16, height = 8, dpi = 600, bg = 'white')

	}

say('###################################################################################################')
say('### plot of variable importance and selected biplots for main text for Prionailurus bengalensis ###')
say('###################################################################################################')

	if (species_focal != 'Priona') stop('Composite figure only for Priona.')

	### formatting
	##############

		point_size <- 2.4
		plot_title_size <- 12

	### variable importance
	#######################

		vis <- readRDS(paste0(this_out_dir, '/MRF Variable Importance - na.impute.rds'))
		vi <- vis[[3]]

		title <- 'a) Variable importance'

		var_imp <- ggplot(vi, aes(x = reorder(nice_name, proportion), y = proportion, fill = stage)) +
			geom_bar(stat = 'identity', color = 'gray30', linewidth = 0.2) +
			scale_fill_manual(
				values = c('0' = 'white', '1' = '#60ADF1', '2' = '#F9CB40', '3' = '#11EFC9', '4' = '#F56096', '5' = 'gray40'),
				labels = c('0' = 'Team', '1' = 'Inputs', '2' = 'Stage setting', '3' = 'Algorithm/software', '4' = 'Post-processing', '5' = 'Standards'),
				name = 'Variable type'
			) +
			labs(x = NULL, y = 'Relative importance') +
			theme_bw() +
			coord_flip() +
			ggtitle(title) +
			theme(
				plot.title = element_text(size = plot_title_size),
				legend.position = c(0.99, 0.01),
				legend.justification = c(1, 0),
				legend.title = element_text(size = 13),
				legend.text = element_text(size = 11),
				axis.text = element_text(size = 9)
			)

	### biplot setup
	################

		biplots <- list()

		rast_fields <- load_rast_fields(species_focal = species_focal)
		team_fields <- load_team_fields(species_focal = species_focal)

		### PCA
		preds <- load_predictions(species_focal = species_focal, period = 'all', scale = TRUE, subset_teams = TRUE)
		preds_trans <- t(preds)

		pca <- prcomp(preds_trans)
		scores <- pca$x[ , 1:2]
		scores <- as.data.frame(scores)

		scores$raster_period <- rownames(scores)
		scores$raster <- scrub_period(scores$raster_period)
		nc <- nchar(scores$raster_period)
		starts <- regexpr(scores$raster_period, pattern = '_') + 1
		scores$period <- substr(scores$raster_period, starts, nc)
		scores$period <- capIt(scores$period)
		scores$team <- substr(scores$raster_period, 1, 1)

		### generic biplot to be filled in
		var1 <- sprintf('%.1f', 100 * pca$sdev[1]^2 / sum(pca$sdev^2))
		var2 <- sprintf('%.1f', 100 * pca$sdev[2]^2 / sum(pca$sdev^2))

		x_lab <- paste0('PC 1 (', var1, '%)')
		y_lab <- paste0('PC 2 (', var2, '%)')

		base_biplot <- ggplot() +
			coord_cartesian(clip = 'off') +
			xlab(x_lab) + ylab(y_lab)

	### calibration region extent: Priona
	#####################################

		title <- paste0(letters[length(biplots) + 2], ') Calibration region extent')

		vi <- vis[[3]][vis[[3]]$attribute == '2_calibration_extent_km2', ]$proportion
		vi <- roundTo(vi, 0.001)
		vi <- sprintf('%.3f', vi)

		fields <- load_team_fields(species_focal = species_focal)
		scores$calibration_extent_log10 <- log10(as.numeric(fields$extent_calibration_sans_water_km2[match(scores$team, fields$team_code)]))

		cluster <- 'calibration_extent_log10'
		biplots[[length(biplots) + 1]] <- base_biplot +
			geom_point(data = scores, aes(x = PC1, y = PC2), pch = 1, alpha = 0.7, size = point_size, color = 'gray') +
			geom_point(data = scores[!is.na(scores$calibration_extent_log10), ], aes(x = PC1, y = PC2, fill = .data[[cluster]]), pch = 21, alpha = 0.7, size = point_size) +
			scale_fill_gradient(low = 'darkblue', high = 'yellow', na.value = 'gray90') +
			# annotate(
			# 	'text', x = Inf, y = -Inf, label = paste0('VI: ', vi), 
			# 	parse = FALSE, hjust = 1, vjust = -0.5, size = 7
			# ) +
			ggtitle(title)

	### teams
	#########

		field_names <- '0_team'
		title <- paste0(letters[length(biplots) + 2], ') Team')

		vi <- vis[[3]][vis[[3]]$attribute %in% field_names, ]$proportion
		vi <- roundTo(vi, 0.001)
		vi <- sprintf('%.3f', vi)

		# cluster
		polys <- data.table()
		unique_clusters <- unique(scores$team)
		for (i in seq_along(unique_clusters)) {
		
			cluster <- unique_clusters[i]
			pts <- scores[scores$team == cluster, c('PC1', 'PC2')]
			hull_indices <- chull(pts)
			hull_pts <- pts[hull_indices, ]
			hull_pts <- rbind(hull_pts, hull_pts[1, , drop = FALSE])
			hull_pts$team <- cluster
			polys <- rbind(polys, hull_pts)
		
		}
		polys[ , team := factor(team, levels = unique_clusters)]

		biplots[[length(biplots) + 1]] <- base_biplot +
			geom_point(
				data = scores, aes(x = PC1, y = PC2, color = team, fill = team),
				size = point_size,
				pch = 16
			) +
			geom_polygon(data = polys, aes(x = PC1, y = PC2, fill = team), alpha = 0.4, color = 'gray40') +
			# annotate(
			# 	'text', x = Inf, y = -Inf, label = paste0('VI: ', vi), 
			# 	parse = FALSE, hjust = 1, vjust = -0.5, size = 7
			# ) +
			ggtitle(title)

	### AUC
	#######

		title <- paste0(letters[length(biplots) + 2], ') AUC')

		vi <- vis[[3]][vis[[3]]$attribute == '4_auc', ]$proportion
		vi <- roundTo(vi, 0.001)
		vi <- sprintf('%.3f', vi)

		fields <- load_rast_fields(species_focal = species_focal)
		scores$auc <- NA_real_

		fields$raster_period <- apply(fields[ , c('raster_name', 'time_period')], 1, paste, collapse = '_')
		index <- match(fields$raster_period, scores$raster_period)
		scores$auc[index] <- fields$eval_metric_auc_roc_value
		scores$auc <- as.numeric(scores$auc)

		cluster <- 'auc'
		biplots[[length(biplots) + 1]] <- base_biplot +
			geom_point(data = scores, aes(x = PC1, y = PC2), pch = 1, alpha = 0.7, size = point_size, color = 'gray') +
			geom_point(data = scores[!is.na(scores$auc), ], aes(x = PC1, y = PC2, fill = .data[[cluster]]), pch = 21, alpha = 0.7, size = point_size) +
			scale_fill_gradient(low = 'darkblue', high = 'yellow', na.value = 'gray90') +
			# annotate(
			# 	'text', x = Inf, y = -Inf, label = paste0('VI: ', vi), 
			# 	parse = FALSE, hjust = 1, vjust = -0.5, size = 7
			# ) +
			ggtitle(title)

	### BIOCLIM 02
	##############

		title <- paste0(letters[length(biplots) + 2], ') Use of BIOCLIM 02')

		vi <- vis[[3]][vis[[3]]$attribute == '1_predictors_climate_bio2', ]$proportion
		vi <- roundTo(vi, 0.001)
		vi <- sprintf('%.3f', vi)

		fields <- load_team_fields(species_focal = species_focal)
		scores$bioclim_02 <- fields$predictors_climate_bio2[match(scores$team, fields$team_code)]
		scores$bioclim_02 <- factor(scores$bioclim_02)

		# cluster
		cluster <- 'bioclim_02'
		polys <- data.table()
		unique_clusters <- unique(scores[[cluster]])
		for (i in seq_along(unique_clusters)) {
		
			cluster_name <- unique_clusters[i]
			select <- which(scores[[cluster]] == cluster_name)
			pts <- scores[select, c('PC1', 'PC2')]
			hull_indices <- chull(pts)
			hull_pts <- pts[hull_indices, ]
			hull_pts <- rbind(hull_pts, hull_pts[1, , drop = FALSE])
			hull_pts$cluster <- cluster_name
			polys <- rbind(polys, hull_pts)
		
		}
		polys[ , cluster := factor(cluster, levels = rev(unique_clusters))]

		biplots[[length(biplots) + 1]] <- base_biplot +
			geom_polygon(data = polys, aes(x = PC1, y = PC2, fill = cluster), alpha = 0.4, color = 'gray40') +
			geom_point(data = scores, aes(x = PC1, y = PC2, fill = .data[[cluster]], shape = .data[[cluster]]), alpha = 0.7, size = point_size) +
			scale_shape_manual(values = c(21, 22)) +
			# annotate(
			# 	'text', x = Inf, y = -Inf, label = paste0('VI: ', vi), 
			# 	parse = FALSE, hjust = 1, vjust = -0.5, size = 7
			# ) +
			ggtitle(title)

	### workflow quality (standards)
	################################

		title <- paste0(letters[length(biplots) + 2], ') Workflow quality')

		vi <- vis[[3]][vis[[3]]$attribute == '0_standards_mean_score', ]$proportion
		vi <- roundTo(vi, 0.001)
		vi <- sprintf('%.3f', vi)

		### add MEAN ODMAP score and MINIMUM SCORE ACROSS CATEGORIES to fields
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

		fields <- load_team_fields(species_focal = species_focal)
		fields$odmap_mean <- odmap_means[match(fields$team_code, team_codes)]
		fields$odmap_min <- odmap_mins[match(fields$team_code, team_codes)]

		index <- match(scores$team, fields$team)
		scores$odmap_mean <- fields$odmap_mean[index]

		cluster <- 'odmap_mean'
		biplots[[length(biplots) + 1]] <- base_biplot +
			geom_point(data = scores, aes(x = PC1, y = PC2, fill = .data[[cluster]]), pch = 21, alpha = 0.7, size = point_size) +
			scale_fill_viridis_c(option = 'magma') +
			# annotate(
			# 	'text', x = Inf, y = -Inf, label = paste0('VI: ', vi), 
			# 	parse = FALSE, hjust = 1, vjust = -0.5, size = 7
			# ) +
			ggtitle(title)

	### composite figure
	####################

		for (i in seq_along(biplots)) {

			biplots[[i]] <- biplots[[i]] +
				coord_fixed() +
				theme_minimal() +
				theme(
					legend.position = 'none',
					plot.title = element_text(size = plot_title_size),
					axis.title = element_text(size = 10),
					axis.text = element_text(size = 8)
				)

		}

		biplot_panels <- plot_grid(plotlist = biplots, ncol = 1, align = 'hv', axis = 'tblr')
		# composite <- plot_grid(var_imp, biplot_panels, ncol = 2, rel_widths = c(0.5, 0.5), align = 'hv', axis = 'tblr')
		composite <- plot_grid(var_imp, biplot_panels, ncol = 2, rel_widths = c(0.6, 0.4))

		ggsave(paste0(this_out_dir, '/Composite Figure - Variable Importance & Select Biplots.png'), composite, width = 8, height = 10, dpi = 600, bg = 'white')

say('###################################################################')
say('### selected biplots for main text for Prionailurus bengalensis ###')
say('###################################################################')

	### formatting
	##############

		point_size <- 7

	### biplot setup
	################

		biplots <- list()

		rast_fields <- load_rast_fields(species_focal = 'Priona')
		team_fields <- load_team_fields(species_focal = 'Priona')

		### PCA
		preds <- load_predictions(species_focal = 'Priona', period = 'all', scale = TRUE, subset_teams = TRUE)
		preds_trans <- t(preds)

		pca <- prcomp(preds_trans)
		scores <- pca$x[ , 1:2]
		scores <- as.data.frame(scores)

		scores$raster_period <- rownames(scores)
		scores$raster <- scrub_period(scores$raster_period)
		nc <- nchar(scores$raster_period)
		starts <- regexpr(scores$raster_period, pattern = '_') + 1
		scores$period <- substr(scores$raster_period, starts, nc)
		scores$period <- capIt(scores$period)
		scores$team <- substr(scores$raster_period, 1, 1)

		### generic biplot to be filled in
		var1 <- sprintf('%.1f', 100 * pca$sdev[1]^2 / sum(pca$sdev^2))
		var2 <- sprintf('%.1f', 100 * pca$sdev[2]^2 / sum(pca$sdev^2))

		x_lab <- paste0('PC 1 (', var1, '%)')
		y_lab <- paste0('PC 2 (', var2, '%)')

		base_biplot <- ggplot() +
			coord_cartesian(clip = 'off') +
			xlab(x_lab) + ylab(y_lab)

	### calibration region extent: Priona
	#####################################

		title <- 'Calibration region extent'

		# vi <- vis[[3]][vis[[3]]$attribute == '2_calibration_extent_km2', ]$proportion
		# vi <- roundTo(vi, 0.001)
		# vi <- sprintf('%.3f', vi)

		fields <- load_team_fields(species_focal = 'Priona')
		scores$calibration_extent_log10 <- log10(as.numeric(fields$extent_calibration_sans_water_km2[match(scores$team, fields$team_code)]))

		cluster <- 'calibration_extent_log10'
		biplots[[length(biplots) + 1]] <- base_biplot +
			geom_point(data = scores, aes(x = PC1, y = PC2), pch = 1, alpha = 0.7, size = point_size, color = 'gray') +
			geom_point(data = scores[!is.na(scores$calibration_extent_log10), ], aes(x = PC1, y = PC2, fill = .data[[cluster]]), pch = 21, alpha = 0.7, size = point_size) +
			scale_fill_gradient(low = 'darkblue', high = 'yellow', na.value = 'gray90') +
			# annotate(
			# 	'text', x = Inf, y = -Inf, label = paste0('VI: ', vi), 
			# 	parse = FALSE, hjust = 1, vjust = -0.5, size = 7
			# ) +
			ggtitle(title)

		names(biplots)[length(biplots)] <- 'calibration_extent'

	### teams
	#########

		field_names <- '0_team'
		title <- 'Team'

		# vi <- vis[[3]][vis[[3]]$attribute %in% field_names, ]$proportion
		# vi <- roundTo(vi, 0.001)
		# vi <- sprintf('%.3f', vi)

		# cluster
		polys <- data.table()
		unique_clusters <- unique(scores$team)
		for (i in seq_along(unique_clusters)) {
		
			cluster <- unique_clusters[i]
			pts <- scores[scores$team == cluster, c('PC1', 'PC2')]
			hull_indices <- chull(pts)
			hull_pts <- pts[hull_indices, ]
			hull_pts <- rbind(hull_pts, hull_pts[1, , drop = FALSE])
			hull_pts$team <- cluster
			polys <- rbind(polys, hull_pts)
		
		}
		polys[ , team := factor(team, levels = unique_clusters)]

		biplots[[length(biplots) + 1]] <- base_biplot +
			geom_point(
				data = scores, aes(x = PC1, y = PC2, color = team, fill = team),
				size = point_size,
				pch = 16
			) +
			geom_polygon(data = polys, aes(x = PC1, y = PC2, fill = team), alpha = 0.4, color = 'gray40') +
			# annotate(
			# 	'text', x = Inf, y = -Inf, label = paste0('VI: ', vi), 
			# 	parse = FALSE, hjust = 1, vjust = -0.5, size = 7
			# ) +
			ggtitle(title)

		names(biplots)[length(biplots)] <- 'team'

	### AUC
	#######

		title <- 'AUC'

		# vi <- vis[[3]][vis[[3]]$attribute == '4_auc', ]$proportion
		# vi <- roundTo(vi, 0.001)
		# vi <- sprintf('%.3f', vi)

		fields <- load_rast_fields(species_focal = 'Priona')
		scores$auc <- NA_real_

		fields$raster_period <- apply(fields[ , c('raster_name', 'time_period')], 1, paste, collapse = '_')
		index <- match(fields$raster_period, scores$raster_period)
		scores$auc[index] <- fields$eval_metric_auc_roc_value
		scores$auc <- as.numeric(scores$auc)

		cluster <- 'auc'
		biplots[[length(biplots) + 1]] <- base_biplot +
			geom_point(data = scores, aes(x = PC1, y = PC2), pch = 1, alpha = 0.7, size = point_size, color = 'gray') +
			geom_point(data = scores[!is.na(scores$auc), ], aes(x = PC1, y = PC2, fill = .data[[cluster]]), pch = 21, alpha = 0.7, size = point_size) +
			scale_fill_gradient(low = 'darkblue', high = 'yellow', na.value = 'gray90') +
			# annotate(
			# 	'text', x = Inf, y = -Inf, label = paste0('VI: ', vi), 
			# 	parse = FALSE, hjust = 1, vjust = -0.5, size = 7
			# ) +
			ggtitle(title)

		names(biplots)[length(biplots)] <- 'auc'

	### BIOCLIM 02
	##############

		title <- 'Use of BIOCLIM 02'

		# vi <- vis[[3]][vis[[3]]$attribute == '1_predictors_climate_bio2', ]$proportion
		# vi <- roundTo(vi, 0.001)
		# vi <- sprintf('%.3f', vi)

		fields <- load_team_fields(species_focal = 'Priona')
		scores$bioclim_02 <- fields$predictors_climate_bio2[match(scores$team, fields$team_code)]
		scores$bioclim_02 <- factor(scores$bioclim_02)

		# cluster
		cluster <- 'bioclim_02'
		polys <- data.table()
		unique_clusters <- unique(scores[[cluster]])
		for (i in seq_along(unique_clusters)) {
		
			cluster_name <- unique_clusters[i]
			select <- which(scores[[cluster]] == cluster_name)
			pts <- scores[select, c('PC1', 'PC2')]
			hull_indices <- chull(pts)
			hull_pts <- pts[hull_indices, ]
			hull_pts <- rbind(hull_pts, hull_pts[1, , drop = FALSE])
			hull_pts$cluster <- cluster_name
			polys <- rbind(polys, hull_pts)
		
		}
		polys[ , cluster := factor(cluster, levels = rev(unique_clusters))]

		biplots[[length(biplots) + 1]] <- base_biplot +
			geom_polygon(data = polys, aes(x = PC1, y = PC2, fill = cluster), alpha = 0.4, color = 'gray40') +
			geom_point(data = scores, aes(x = PC1, y = PC2, fill = .data[[cluster]], shape = .data[[cluster]]), alpha = 0.7, size = point_size) +
			scale_shape_manual(values = c(21, 22)) +
			# annotate(
			# 	'text', x = Inf, y = -Inf, label = paste0('VI: ', vi), 
			# 	parse = FALSE, hjust = 1, vjust = -0.5, size = 7
			# ) +
			ggtitle(title)

		names(biplots)[length(biplots)] <- 'bioclim_02'

	### workflow quality (standards)
	################################

		title <- 'Workflow quality'

		# vi <- vis[[3]][vis[[3]]$attribute == '0_standards_mean_score', ]$proportion
		# vi <- roundTo(vi, 0.001)
		# vi <- sprintf('%.3f', vi)

		### add MEAN ODMAP score and MINIMUM SCORE ACROSS CATEGORIES to fields
		odmap <- readRDS('./Outputs Shared Anonymized/ODMAP Scoring Anonymized.rds')

		y <- odmap[['means']]

		y <- y[grepl(y$species, pattern = 'Priona')]
		criteria <- c(paste0(1, LETTERS[1:5]), paste0(2, LETTERS[1:3]), paste0(3, LETTERS[1:4]), paste0(4, LETTERS[1:3]))
		odmap_scores <- y[ , ..criteria]
		
		odmap_means <- rowMeans(odmap_scores)
		odmap_mins <- apply(odmap_scores, 1, max) # taking max bc 1 = gold, 0 = deficient

		odmap_means <- 4 - odmap_means # reverse ranks so 4 = gold, 0 = deficient
		odmap_mins <- 4 - odmap_mins # reverse ranks so 4 = gold, 0 = deficient

		team_codes <- odmap$means$team_code[odmap$means$species == 'Prionailurus bengalensis']
		names(odmap_means) <- team_codes
		names(odmap_mins) <- team_codes

		fields <- load_team_fields(species_focal = 'Priona')
		fields$odmap_mean <- odmap_means[match(fields$team_code, team_codes)]
		fields$odmap_min <- odmap_mins[match(fields$team_code, team_codes)]

		index <- match(scores$team, fields$team)
		scores$odmap_mean <- fields$odmap_mean[index]

		cluster <- 'odmap_mean'
		biplots[[length(biplots) + 1]] <- base_biplot +
			geom_point(data = scores, aes(x = PC1, y = PC2, fill = .data[[cluster]]), pch = 21, alpha = 0.7, size = point_size) +
			scale_fill_viridis_c(option = 'magma') +
			# annotate(
			# 	'text', x = Inf, y = -Inf, label = paste0('VI: ', vi), 
			# 	parse = FALSE, hjust = 1, vjust = -0.5, size = 7
			# ) +
			ggtitle(title)

		names(biplots)[length(biplots)] <- 'standards'

		for (i in seq_along(biplots)) {

			biplots[[i]] <- biplots[[i]] + theme(legend.position = 'none') + coord_fixed()

			ggsave(paste0('./Outputs Prionailurus bengalensis/Multivariate RF on Differences in PC Space/Biplot for Priona - ', capIt(names(biplots)[i]), '.svg'), biplots[[i]], width = 6, height = 3.5, dpi = 600, bg = 'white')

		}


say('########################################################')
say('### selected biplots for main text for Zamia prasina ###')
say('########################################################')

	### formatting
	##############

		point_size <- 7

	### biplot setup
	################

		biplots <- list()

		rast_fields <- load_rast_fields(species_focal = 'Zamia')
		team_fields <- load_team_fields(species_focal = 'Zamia')

		### PCA
		preds <- load_predictions(species_focal = 'Zamia', period = 'all', scale = TRUE, subset_teams = TRUE)
		preds_trans <- t(preds)

		pca <- prcomp(preds_trans)
		scores <- pca$x[ , 1:2]
		scores <- as.data.frame(scores)

		scores$raster_period <- rownames(scores)
		scores$raster <- scrub_period(scores$raster_period)
		nc <- nchar(scores$raster_period)
		starts <- regexpr(scores$raster_period, pattern = '_') + 1
		scores$period <- substr(scores$raster_period, starts, nc)
		scores$period <- capIt(scores$period)
		scores$team <- substr(scores$raster_period, 1, 1)

		### generic biplot to be filled in
		var1 <- sprintf('%.1f', 100 * pca$sdev[1]^2 / sum(pca$sdev^2))
		var2 <- sprintf('%.1f', 100 * pca$sdev[2]^2 / sum(pca$sdev^2))

		x_lab <- paste0('PC 1 (', var1, '%)')
		y_lab <- paste0('PC 2 (', var2, '%)')

		base_biplot <- ggplot() +
			coord_cartesian(clip = 'off') +
			xlab(x_lab) + ylab(y_lab)

	### calibration region extent: Zamia
	#####################################

		title <- 'Calibration region extent'

		# vi <- vis[[3]][vis[[3]]$attribute == '2_calibration_extent_km2', ]$proportion
		# vi <- roundTo(vi, 0.001)
		# vi <- sprintf('%.3f', vi)

		fields <- load_team_fields(species_focal = 'Zamia')
		scores$calibration_extent_log10 <- log10(as.numeric(fields$extent_calibration_sans_water_km2[match(scores$team, fields$team_code)]))

		cluster <- 'calibration_extent_log10'
		biplots[[length(biplots) + 1]] <- base_biplot +
			geom_point(data = scores, aes(x = PC1, y = PC2), pch = 1, alpha = 0.7, size = point_size, color = 'gray') +
			geom_point(data = scores[!is.na(scores$calibration_extent_log10), ], aes(x = PC1, y = PC2, fill = .data[[cluster]]), pch = 21, alpha = 0.7, size = point_size) +
			scale_fill_gradient(low = 'darkblue', high = 'yellow', na.value = 'gray90') +
			# annotate(
			# 	'text', x = Inf, y = -Inf, label = paste0('VI: ', vi), 
			# 	parse = FALSE, hjust = 1, vjust = -0.5, size = 7
			# ) +
			ggtitle(title)

		names(biplots)[length(biplots)] <- 'calibration_extent'

	### spatial resoluion (km2)
	###########################

		title <- 'Spatial resolution (km²)'

		# vi <- vis[[3]][vis[[3]]$attribute == '2_calibration_extent_km2', ]$proportion
		# vi <- roundTo(vi, 0.001)
		# vi <- sprintf('%.3f', vi)

		fields <- load_team_fields(species_focal = 'Zamia')
		scores$spatial_resolution_km2_log10 <- log10(as.numeric(fields$res_km2[match(scores$team, fields$team_code)]))

		cluster <- 'spatial_resolution_km2_log10'
		biplots[[length(biplots) + 1]] <- base_biplot +
			geom_point(data = scores, aes(x = PC1, y = PC2), pch = 1, alpha = 0.7, size = point_size, color = 'gray') +
			geom_point(data = scores[!is.na(scores$spatial_resolution_km2_log10), ], aes(x = PC1, y = PC2, fill = .data[[cluster]]), pch = 21, alpha = 0.7, size = point_size) +
			scale_fill_gradient(low = 'darkblue', high = 'yellow', na.value = 'gray90') +
			# annotate(
			# 	'text', x = Inf, y = -Inf, label = paste0('VI: ', vi), 
			# 	parse = FALSE, hjust = 1, vjust = -0.5, size = 7
			# ) +
			ggtitle(title)

		names(biplots)[length(biplots)] <- 'spatial_resolution'

	### teams
	#########

		field_names <- '0_team'
		title <- 'Team'

		# vi <- vis[[3]][vis[[3]]$attribute %in% field_names, ]$proportion
		# vi <- roundTo(vi, 0.001)
		# vi <- sprintf('%.3f', vi)

		# cluster
		polys <- data.table()
		unique_clusters <- unique(scores$team)
		for (i in seq_along(unique_clusters)) {
		
			cluster <- unique_clusters[i]
			pts <- scores[scores$team == cluster, c('PC1', 'PC2')]
			hull_indices <- chull(pts)
			hull_pts <- pts[hull_indices, ]
			hull_pts <- rbind(hull_pts, hull_pts[1, , drop = FALSE])
			hull_pts$team <- cluster
			polys <- rbind(polys, hull_pts)
		
		}
		polys[ , team := factor(team, levels = unique_clusters)]

		biplots[[length(biplots) + 1]] <- base_biplot +
			geom_point(
				data = scores, aes(x = PC1, y = PC2, color = team, fill = team),
				size = point_size,
				pch = 16
			) +
			geom_polygon(data = polys, aes(x = PC1, y = PC2, fill = team), alpha = 0.4, color = 'gray40') +
			# annotate(
			# 	'text', x = Inf, y = -Inf, label = paste0('VI: ', vi), 
			# 	parse = FALSE, hjust = 1, vjust = -0.5, size = 7
			# ) +
			ggtitle(title)

		names(biplots)[length(biplots)] <- 'team'

	### AUC
	#######

		title <- 'AUC'

		# vi <- vis[[3]][vis[[3]]$attribute == '4_auc', ]$proportion
		# vi <- roundTo(vi, 0.001)
		# vi <- sprintf('%.3f', vi)

		fields <- load_rast_fields(species_focal = 'Zamia')
		scores$auc <- NA_real_

		fields$raster_period <- apply(fields[ , c('raster_name', 'time_period')], 1, paste, collapse = '_')
		index <- match(fields$raster_period, scores$raster_period)
		scores$auc[index] <- fields$eval_metric_auc_roc_value
		scores$auc <- as.numeric(scores$auc)

		cluster <- 'auc'
		biplots[[length(biplots) + 1]] <- base_biplot +
			geom_point(data = scores, aes(x = PC1, y = PC2), pch = 1, alpha = 0.7, size = point_size, color = 'gray') +
			geom_point(data = scores[!is.na(scores$auc), ], aes(x = PC1, y = PC2, fill = .data[[cluster]]), pch = 21, alpha = 0.7, size = point_size) +
			scale_fill_gradient(low = 'darkblue', high = 'yellow', na.value = 'gray90') +
			# annotate(
			# 	'text', x = Inf, y = -Inf, label = paste0('VI: ', vi), 
			# 	parse = FALSE, hjust = 1, vjust = -0.5, size = 7
			# ) +
			ggtitle(title)

		names(biplots)[length(biplots)] <- 'auc'

	### number of occurrences
	#########################

		title <- 'Number of Occurrences'

		# vi <- vis[[3]][vis[[3]]$attribute == '4_auc', ]$proportion
		# vi <- roundTo(vi, 0.001)
		# vi <- sprintf('%.3f', vi)

		fields <- load_team_fields(species_focal = 'Zamia')
		scores$num_occurrences_log10 <- log10(as.numeric(fields$num_occurrences_minimum[match(scores$team, fields$team_code)]))

		cluster <- 'num_occurrences_log10'
		biplots[[length(biplots) + 1]] <- base_biplot +
			geom_point(data = scores, aes(x = PC1, y = PC2), pch = 1, alpha = 0.7, size = point_size, color = 'gray') +
			geom_point(data = scores[!is.na(scores$num_occurrences_log10), ], aes(x = PC1, y = PC2, fill = .data[[cluster]]), pch = 21, alpha = 0.7, size = point_size) +
			scale_fill_gradient(low = 'green4', high = 'white', na.value = 'gray90') +
			# annotate(
			# 	'text', x = Inf, y = -Inf, label = paste0('VI: ', vi), 
			# 	parse = FALSE, hjust = 1, vjust = -0.5, size = 7
			# ) +
			ggtitle(title)

		names(biplots)[length(biplots)] <- 'num_occurrences_log10'

	### BIOCLIM 02
	##############

		title <- 'Use of BIOCLIM 16'

		# vi <- vis[[3]][vis[[3]]$attribute == '1_predictors_climate_bio2', ]$proportion
		# vi <- roundTo(vi, 0.001)
		# vi <- sprintf('%.3f', vi)

		fields <- load_team_fields(species_focal = 'Zamia')
		scores$bioclim_16 <- fields$predictors_climate_bio16[match(scores$team, fields$team_code)]
		scores$bioclim_16 <- factor(scores$bioclim_16)

		# cluster
		cluster <- 'bioclim_16'
		polys <- data.table()
		unique_clusters <- unique(scores[[cluster]])
		for (i in seq_along(unique_clusters)) {
		
			cluster_name <- unique_clusters[i]
			select <- which(scores[[cluster]] == cluster_name)
			pts <- scores[select, c('PC1', 'PC2')]
			hull_indices <- chull(pts)
			hull_pts <- pts[hull_indices, ]
			hull_pts <- rbind(hull_pts, hull_pts[1, , drop = FALSE])
			hull_pts$cluster <- cluster_name
			polys <- rbind(polys, hull_pts)
		
		}
		polys[ , cluster := factor(cluster, levels = rev(unique_clusters))]

		biplots[[length(biplots) + 1]] <- base_biplot +
			geom_polygon(data = polys, aes(x = PC1, y = PC2, fill = cluster), alpha = 0.4, color = 'gray40') +
			geom_point(data = scores, aes(x = PC1, y = PC2, fill = .data[[cluster]], shape = .data[[cluster]]), alpha = 0.7, size = point_size) +
			scale_shape_manual(values = c(21, 22)) +
			# annotate(
			# 	'text', x = Inf, y = -Inf, label = paste0('VI: ', vi), 
			# 	parse = FALSE, hjust = 1, vjust = -0.5, size = 7
			# ) +
			ggtitle(title)

		names(biplots)[length(biplots)] <- 'bioclim_16'

	### bias correction: used target background
	###########################################

		title <- 'Bias Correction with Target Background'

		# vi <- vis[[3]][vis[[3]]$attribute == '1_predictors_climate_bio2', ]$proportion
		# vi <- roundTo(vi, 0.001)
		# vi <- sprintf('%.3f', vi)

		fields <- load_team_fields(species_focal = 'Zamia')
		scores$bias_correction_target_background <- fields$bias_correction_target_background[match(scores$team, fields$team_code)]
		scores$bias_correction_target_background <- factor(scores$bias_correction_target_background)

		# cluster
		cluster <- 'bias_correction_target_background'
		polys <- data.table()
		unique_clusters <- unique(scores[[cluster]])
		for (i in seq_along(unique_clusters)) {
		
			cluster_name <- unique_clusters[i]
			select <- which(scores[[cluster]] == cluster_name)
			pts <- scores[select, c('PC1', 'PC2')]
			hull_indices <- chull(pts)
			hull_pts <- pts[hull_indices, ]
			hull_pts <- rbind(hull_pts, hull_pts[1, , drop = FALSE])
			hull_pts$cluster <- cluster_name
			polys <- rbind(polys, hull_pts)
		
		}
		polys[ , cluster := factor(cluster, levels = rev(unique_clusters))]
		polys[ , cluster := factor(cluster, levels = c(NA, '0', '1'), exclude = NULL)]
		

		biplots[[length(biplots) + 1]] <- base_biplot +
			geom_polygon(data = polys, aes(x = PC1, y = PC2, fill = cluster), alpha = 0.4, color = 'gray40') +
			geom_point(data = scores, aes(x = PC1, y = PC2, fill = .data[[cluster]], shape = .data[[cluster]]), alpha = 0.7, size = point_size) +
			scale_shape_manual(values = c(21, 22, 1)) +
			# annotate(
			# 	'text', x = Inf, y = -Inf, label = paste0('VI: ', vi), 
			# 	parse = FALSE, hjust = 1, vjust = -0.5, size = 7
			# ) +
			ggtitle(title)

		names(biplots)[length(biplots)] <- 'bias_correction_target_background'

	### workflow quality (standards)
	################################

		title <- 'Workflow quality'

		# vi <- vis[[3]][vis[[3]]$attribute == '0_standards_mean_score', ]$proportion
		# vi <- roundTo(vi, 0.001)
		# vi <- sprintf('%.3f', vi)

		### add MEAN ODMAP score and MINIMUM SCORE ACROSS CATEGORIES to fields
		odmap <- readRDS('./Outputs Shared Anonymized/ODMAP Scoring Anonymized.rds')

		y <- odmap[['means']]

		y <- y[grepl(y$species, pattern = 'Zamia')]
		criteria <- c(paste0(1, LETTERS[1:5]), paste0(2, LETTERS[1:3]), paste0(3, LETTERS[1:4]), paste0(4, LETTERS[1:3]))
		odmap_scores <- y[ , ..criteria]
		
		odmap_means <- rowMeans(odmap_scores)
		odmap_mins <- apply(odmap_scores, 1, max) # taking max bc 1 = gold, 0 = deficient

		odmap_means <- 4 - odmap_means # reverse ranks so 4 = gold, 0 = deficient
		odmap_mins <- 4 - odmap_mins # reverse ranks so 4 = gold, 0 = deficient

		team_codes <- odmap$means$team_code[odmap$means$species == 'Prionailurus bengalensis']
		names(odmap_means) <- team_codes
		names(odmap_mins) <- team_codes

		fields <- load_team_fields(species_focal = 'Zamia')
		fields$odmap_mean <- odmap_means[match(fields$team_code, team_codes)]
		fields$odmap_min <- odmap_mins[match(fields$team_code, team_codes)]

		index <- match(scores$team, fields$team)
		scores$odmap_mean <- fields$odmap_mean[index]

		cluster <- 'odmap_mean'
		biplots[[length(biplots) + 1]] <- base_biplot +
			geom_point(data = scores, aes(x = PC1, y = PC2, fill = .data[[cluster]]), pch = 21, alpha = 0.7, size = point_size) +
			scale_fill_viridis_c(option = 'magma') +
			# annotate(
			# 	'text', x = Inf, y = -Inf, label = paste0('VI: ', vi), 
			# 	parse = FALSE, hjust = 1, vjust = -0.5, size = 7
			# ) +
			ggtitle(title)

		names(biplots)[length(biplots)] <- 'standards'

	### climate data source
	#######################

		title <- 'Climate Data Source'

		# vi <- vis[[3]][vis[[3]]$attribute == '1_predictors_climate_bio2', ]$proportion
		# vi <- roundTo(vi, 0.001)
		# vi <- sprintf('%.3f', vi)

		fields <- load_team_fields(species_focal = 'Zamia')
		scores$predictors_climate_source <- fields$predictors_climate_source[match(scores$team, fields$team_code)]
		scores$predictors_climate_source <- factor(scores$predictors_climate_source)

		# cluster
		cluster <- 'predictors_climate_source'
		polys <- data.table()
		unique_clusters <- unique(scores[[cluster]])
		for (i in seq_along(unique_clusters)) {
		
			cluster_name <- unique_clusters[i]
			select <- which(scores[[cluster]] == cluster_name)
			pts <- scores[select, c('PC1', 'PC2')]
			hull_indices <- chull(pts)
			hull_pts <- pts[hull_indices, ]
			hull_pts <- rbind(hull_pts, hull_pts[1, , drop = FALSE])
			hull_pts$cluster <- cluster_name
			polys <- rbind(polys, hull_pts)
		
		}
		polys[ , cluster := factor(cluster, levels = rev(unique_clusters))]

		biplots[[length(biplots) + 1]] <- base_biplot +
			geom_polygon(data = polys, aes(x = PC1, y = PC2, fill = cluster), alpha = 0.4, color = 'gray40') +
			geom_point(data = scores, aes(x = PC1, y = PC2, fill = .data[[cluster]], shape = .data[[cluster]]), alpha = 0.7, size = point_size) +
			scale_shape_manual(values = c(21, 22)) +
			# annotate(
			# 	'text', x = Inf, y = -Inf, label = paste0('VI: ', vi), 
			# 	parse = FALSE, hjust = 1, vjust = -0.5, size = 7
			# ) +
			ggtitle(title)

		names(biplots)[length(biplots)] <- 'climate_data_source'

	### save

		for (i in seq_along(biplots)) {

			biplots[[i]] <- biplots[[i]] + theme(legend.position = 'none') + coord_fixed()

			ggsave(paste0('./Outputs Zamia prasina/Multivariate RF on Differences in PC Space/Biplot for Zamia - ', capIt(names(biplots)[i]), '.svg'), biplots[[i]], width = 6, height = 3.5, dpi = 600, bg = 'white')

		}


say('################################################################################################')
say('### plot of variable importance and selected biplots for main text for BOTH SPECIES together ###')
say('################################################################################################')

	### formatting
	##############

		point_size <- 2.4
		plot_title_size <- 12

	### variable importance
	#######################

		vis_priona <- readRDS('./Outputs Prionailurus bengalensis/Multivariate RF on Differences in PC Space/MRF Variable Importance - na.impute.rds')
		vis_zamia <- readRDS('./Outputs Zamia prasina/Multivariate RF on Differences in PC Space/MRF Variable Importance - na.impute.rds')
		
		vi_priona <- vis_priona[[3]]
		vi_zamia <- vis_zamia[[3]]

		title_priona <- expression('a) ' ~ italic('Prionailurus bengalensis'))
		title_zamia <- expression('b) ' ~ italic('Zamia prasina'))

		var_imp_priona <- ggplot(vi_priona, aes(x = reorder(nice_name, proportion), y = proportion, fill = stage)) +
			geom_bar(stat = 'identity', color = 'gray30', linewidth = 0.2) +
			scale_fill_manual(
				values = c('0' = 'white', '1' = '#60ADF1', '2' = '#F9CB40', '3' = '#11EFC9', '4' = '#F56096', '5' = 'gray40'),
				labels = c('0' = 'Team', '1' = 'Inputs', '2' = 'Stage setting', '3' = 'Algorithm/software', '4' = 'Post-processing', '5' = 'Standards'),
				name = 'Variable type'
			) +
			labs(x = NULL, y = 'Relative importance') +
			theme_bw() +
			coord_flip() +
			ggtitle(title_priona) +
			theme(
				plot.title = element_text(size = plot_title_size, hjust = 1),
				legend.position = c(0.96, 0.01),
				legend.justification = c(1, 0),
				legend.title = element_text(size = 11),
				legend.text = element_text(size = 9),
				axis.text = element_text(size = 9),
				legend.box = 'solid',
				legend.box.background = element_rect(color = 'lightgray', linewidth = 0.5),
				panel.grid.major.y = element_blank(),
				panel.grid.minor.y = element_blank()
			)

		var_imp_zamia <- ggplot(vi_zamia, aes(x = reorder(nice_name, proportion), y = proportion, fill = stage)) +
			geom_bar(stat = 'identity', color = 'gray30', linewidth = 0.2) +
			scale_fill_manual(
				values = c('0' = 'white', '1' = '#60ADF1', '2' = '#F9CB40', '3' = '#11EFC9', '4' = '#F56096', '5' = 'gray40'),
				labels = c('0' = 'Team', '1' = 'Inputs', '2' = 'Stage setting', '3' = 'Algorithm/software', '4' = 'Post-processing', '5' = 'Standards'),
				name = 'Variable type'
			) +
			labs(x = NULL, y = 'Relative importance') +
			theme_bw() +
			coord_flip() +
			ggtitle(title_zamia) +
			theme(
				plot.title = element_text(size = plot_title_size, hjust = 0),
				legend.position = c(0.96, 0.01),
				legend.justification = c(1, 0),
				legend.title = element_text(size = 11),
				legend.text = element_text(size = 9),
				axis.text = element_text(size = 9),
				legend.box = 'solid',
				legend.box.background = element_rect(color = 'lightgray', linewidth = 0.5),
				panel.grid.major.y = element_blank(),
				panel.grid.minor.y = element_blank()
			)
			
		var_imp <- plot_grid(var_imp_priona, var_imp_zamia, ncol = 2, align = 'hv', axis = 'tblr')
		ggsave('./Outputs Shared Anonymized/MRF Variable Importance for Both Species.svg', var_imp, width = 8, height = 10, dpi = 600, bg = 'white')
		ggsave('./Outputs Shared Anonymized/MRF Variable Importance for Both Species.png', var_imp, width = 8, height = 10, dpi = 600, bg = 'white')


say('DONE', level = 1)