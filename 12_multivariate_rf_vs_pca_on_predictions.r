### SDM METHODOLOGICAL PLURALITY
### Anna Thonis, Nikki Calavari, Uzma Ashraf, Toni Lyn Morelli, and Adam B. Smith*
### * adam.smith@mobot.org | Missouri Botanical Garden | 2025-10
###
### Estimate importance of workflow attributes on distances between rasters in PC space using multivariate RF, and graph results.
###
### source('C:/Kaji/Research Group/ENMs - Plurality of Modeling Workflows (Anna Thonis)/enm_methods_plurality/12_multivariate_rf_vs_pca_on_predictions.r')
###
### CONTENTS
### multivariate RF on PC scores in 1st two PC axes ###
###
#############
### setup ###
#############


	rm(list = ls())
	source('C:/Kaji/Research Group/ENMs - Plurality of Modeling Workflows (Anna Thonis)/enm_methods_plurality/00_shared_constants_and_functions.r')

	# drop_na <- TRUE # drop NAs when calculating multivariate RFs
	# drop_na <- FALSE # impute NAs

	this_out_dir <- paste0(out_dir, '/Multivariate RF on Differences in PC Space')
	dirCreate(this_out_dir)

	library(randomForestSRC)
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

# say('#######################################################')
# say('### multivariate RF on PC scores in 1st two PC axes ###')
# say('#######################################################')

# 	rast_fields <- load_rast_fields(species_focal = species_focal)
# 	team_fields <- load_team_fields(species_focal = species_focal)

# 	### add MEAN ODMAP score ACROSS CATEGORIES to fields
# 	####################################################

# 	odmap <- readRDS('./Outputs Shared Anonymized/ODMAP Scoring Anonymized.rds')

# 	y <- odmap[['means']]
# 	y <- y[grepl(y$species, pattern = species_full)]
# 	criteria <- c(paste0(1, LETTERS[1:5]), paste0(2, LETTERS[1:3]), paste0(3, LETTERS[1:4]), paste0(4, LETTERS[1:3]))
# 	odmap_scores <- y[ , ..criteria]
	
# 	odmap_means <- rowMeans(odmap_scores)
# 	odmap_means <- 4 - odmap_means # reverse ranks so 4 = gold, 0 = deficient

# 	team_codes <- odmap$means$team_code[odmap$means$species == species_full]
# 	names(odmap_means) <- team_codes

# 	team_fields$odmap_mean <- odmap_means[match(team_fields$team_code, team_codes)]

# 	### PCA on predictions
# 	######################

# 	preds <- load_predictions(species_focal = species_focal, period = 'all', scale = TRUE, subset_teams = TRUE)
# 	preds_trans <- t(preds)

# 	pca <- prcomp(preds_trans)
# 	scores <- pca$x[ , 1:2]
# 	scores <- as.data.table(scores)

# 	### compile workflow attribute data
# 	###################################

# 	attributes <- rast_fields[ , c('team_code', 'raster_name', 'time_period')]
# 	rast_names_with_period <- paste(attributes$raster_name, attributes$time_period, sep = '_')
	
# 	names(attributes)[names(attributes) == 'time_period'] <- '4_time_period'
# 	nice_names <- data.table(computer = '4_time_period', nice = c('Time Period'))

# 	### "team"

# 		y_name <- '0_team'
# 		match_on <- 'team'

# 		y <- team_fields$team_code
		
# 		y_match <- match_single_y(match_on, y, attributes)
# 		attributes[ , (y_name) := y_match]
# 		nice_names <- rbind(nice_names, data.table(computer = '0_team', nice = 'Team'))
		
# 	# ### "team" x raster output type (continuous vs thresholded)

# 	# 	y_name <- '0_team_thresholded'
# 	# 	match_on <- 'raster'

# 	# 	y <- rast_fields$team_code
# 	# 	y[rast_fields$raster_name == 'M1'] <- 'Mc'
# 	# 	y[rast_fields$raster_name == 'M2'] <- 'Mt'
# 	# 	if (species_focal == 'Priona') {
# 	# 		y[rast_fields$raster_name %in% c('N1', 'N2')] <- 'Nc'
# 	# 		y[rast_fields$raster_name %in% c('N3', 'N4', 'N3a', 'N4a', 'N3b', 'N4b')] <- 'Nt'
# 	# 	} else if (species_focal == 'Zamia') {
# 	# 		y[rast_fields$raster_name %in% c('N1', 'N2', 'N3', 'N1a', 'N2a', 'N3a', 'N1b', 'N2b', 'N3b')] <- 'Nc'
# 	# 		y[rast_fields$raster_name %in% c('N4', 'N5', 'N6', 'N4a', 'N5a', 'N6a', 'N4b', 'N5b', 'N6b')] <- 'Nt'
# 	# 	}
		
# 	# 	y_match <- match_single_y(match_on, y, attributes)
# 	# 	attributes[ , (y_name) := y_match]
# 	# 	nice_names <- rbind(nice_names, data.table(computer = '0_team_thresholded', nice = 'Team x Continuous/Thresholded'))
		
# 	### ODMAP *mean* score

# 		y_name <- '5_standards_mean_score'
# 		match_on <- 'team'

# 		y <- as.numeric(team_fields$odmap_mean)
		
# 		y_match <- match_single_y(match_on, y, attributes)
# 		attributes[ , (y_name) := y_match]
# 		nice_names <- rbind(nice_names, data.table(computer = '5_standards_mean_score', nice = 'SDM Standards'))
		
# 	# ### ODMAP *minimum* of mean score
# 	# # Not doing bc few teams >0

# 	# 	y_name <- '0_standards_minimum_score'
# 	# 	match_on <- 'team'

# 	# 	y <- as.numeric(team_fields$odmap_mean)

# 	# 	y_match <- match_single_y(match_on = match_on, y = y, attributes = attributes)
# 	# 	attributes[ , (y_name) := y_match]

# 	### number of occurrences

# 		y_name <- '1_number_of_occurrences'
# 		nice <- 'Occurrences: Number of occurrences'
# 		match_on <- 'team'

# 		y <- as.numeric(team_fields$num_occurrences_minimum)

# 		y_match <- match_single_y(match_on = match_on, y = y, attributes = attributes)
# 		attributes[ , (y_name) := y_match]
# 		nice_names <- rbind(nice_names, data.table(computer = '1_number_of_occurrences', nice = 'Number of Occurrences'))

# 	### number of predictors

# 		y_name <- '1_number_of_predictors'
# 		match_on <- 'team'

# 		y <- as.numeric(team_fields$predictors_climate_nonclimate_num_total)

# 		y_match <- match_single_y(match_on = match_on, y = y, attributes = attributes)
# 		attributes[ , (y_name) := y_match]
# 		nice_names <- rbind(nice_names, data.table(computer = '1_number_of_predictors', nice = 'Number of Predictors'))
		
# 	### climate: number of predictors

# 		y_name <- '1_number_of_climate_predictors'
# 		match_on <- 'team'

# 		y <- as.numeric(team_fields$predictors_climate_num)

# 		y_match <- match_single_y(match_on = match_on, y = y, attributes = attributes)
# 		attributes[ , (y_name) := y_match]
# 		nice_names <- rbind(nice_names, data.table(computer = '1_number_of_climate_predictors', nice = 'Number of Climate Predictors'))
		
# 	### non-climate: number of predictors

# 		y_name <- '1_number_of_non_climate_predictors'
# 		match_on <- 'team'

# 		y <- as.numeric(team_fields$predictors_nonclimate_num)

# 		y_match <- match_single_y(match_on = match_on, y = y, attributes = attributes)
# 		attributes[ , (y_name) := y_match]
# 		nice_names <- rbind(nice_names, data.table(computer = '1_number_of_non_climate_predictors', nice = 'Number of Non-Climate Predictors'))

# 	### climate predictors

# 		prepend <- '1'
# 		match_on <- 'team'

# 		field_names <- c(paste0('predictors_climate_bio', 1:19), 'predictors_climate_num_other')
# 		attributes <- match_field_by_field(match_on = match_on, field_names = field_names, attributes = attributes, prepend = prepend)
# 		nice_names <- rbind(nice_names, data.table(computer = c(paste0('1_predictors_climate_bio', 1:19), '1_predictors_climate_num_other'), nice = c(paste0('BIOCLIM ', prefix(1:19, 2)), 'Number of Other Climate Predictors')))

# 	### non-climate predictors

# 		prepend <- '1'
# 		match_on <- 'team'

# 		field_names <- c('predictors_nonclimate_elevation', 'predictors_nonclimate_slope', 'predictors_nonclimate_aspect', 'predictors_nonclimate_terrain_ruggedness_position', 'predictors_nonclimate_forest_cover', 'predictors_nonclimate_land_cover', 'predictors_nonclimate_soil', 'predictors_nonclimate_human_impact', 'predictors_nonclimate_npp_ndvi', 'predictors_nonclimate_other')

# 		attributes <- match_field_by_field(match_on = match_on, field_names = field_names, attributes = attributes, prepend = prepend)
# 		nice_names <- rbind(nice_names, data.table(computer = paste0(prepend, '_', field_names), nice = c('Elevation', 'Slope', 'Aspect', 'Terrain Ruggedness/Position', 'Forest Cover', 'Land Cover', 'Soil', 'Human Impact', 'NPP/NDVI', 'Other Non-Climate Predictors')))

# 	### source of climate predictors

# 		y_name <- '1_climate_predictor_source'
# 		match_on <- 'team'

# 		y <- team_fields$predictors_climate_source
# 		y_match <- match_single_y(match_on = match_on, y = y, attributes = attributes)
# 		attributes[ , (y_name) := y_match]
# 		nice_names <- rbind(nice_names, data.table(computer = '1_climate_predictor_source', nice = 'Source of Climate Predictors'))

# 	### spatial resolution: cell size
		
# 		y_name <- '1_spatial_resolution_km2'
# 		match_on <- 'team'

# 		y <- as.numeric(team_fields$res_km2)

# 		y_match <- match_single_y(match_on = match_on, y = y, attributes = attributes)
# 		attributes[ , (y_name) := y_match]
# 		nice_names <- rbind(nice_names, data.table(computer = '1_spatial_resolution_km2', nice = 'Spatial Resolution (km²)'))

# 	### collinearity: method

# 		prepend <- '2'
# 		y_name <- 'collinearity_method'
# 		match_on <- 'team'

# 		field_names <- c('collinearity_pca', 'collinearity_correlation', 'collinearity_vif', 'collinearity_other_method')
# 		attributes <- match_field_by_field(match_on = match_on, field_names = field_names, attributes = attributes, prepend = prepend)
# 		nice_names <- rbind(nice_names, data.table(computer = paste0(prepend, '_', field_names), nice = c('Collinearity: PCA', 'Collinearity: Correlation', 'Collinearity: VIF', 'Collinearity: Other Method')))

# 	### modeling_software

# 		prepend <- '3'
# 		y_name <- 'sdm_software'
# 		match_on <- 'team'

# 		field_names <- c('modeling_software_enmeval', 'modeling_software_enmtools', 'modeling_software_wallace', 'modeling_software_biomod2', 'modeling_software_sabinansdm', 'modeling_software_sdm', 'modeling_software_miamaxent', 'modeling_software_enmsdmx', 'modeling_software_flexsdm', 'modeling_software_sdmtune', 'modeling_software_piecemeal')

# 		attributes <- match_field_by_field(match_on = match_on, field_names = field_names, attributes = attributes, prepend = prepend)
# 		nice_names <- rbind(nice_names, data.table(computer = paste0(prepend, '_', field_names), nice = c('Modeling Software: ENMeval', 'Modeling Software: ENMTools', 'Modeling Software: Wallace', 'Modeling Software: biomod2', 'Modeling Software: sabinansdm', 'Modeling Software: sdm', 'Modeling Software: MIAmaxent', 'Modeling Software: enmSdmX', 'Modeling Software: flexsdm', 'Modeling Software: SDMtune', 'Modeling Software: Piecemeal')))

# 	### modeling_software used by developers

# 		prepend <- '3'
# 		y_name <- 'sdm_software_used_by_developers'
# 		match_on <- 'raster'

# 		y <- rep(0, nrow(rast_fields))
# 		y[rast_fields$team_code %in% c('E', 'B', 'C', 'H', 'D')] <- 1
		
# 		y_match <- match_single_y(match_on = match_on, y = y, attributes = attributes)
# 		attributes[ , (y_name) := y_match]
# 		nice_names <- rbind(nice_names, data.table(computer = y_name, nice = 'Modeling Software Used by Developers'))

# 	### algorithm: number of algorithms used in ensemble (including 0)

# 		y_name <- '3_number_of_ensemble_algorithms'
# 		match_on <- 'raster'

# 		y <- as.numeric(rast_fields$algo_ensemble_number_of_models)

# 		y_match <- match_single_y(match_on = match_on, y = y, attributes = attributes)
# 		attributes[ , (y_name) := y]
# 		nice_names <- rbind(nice_names, data.table(computer = y_name, nice = 'Number of Algorithms in Ensemble'))

# 	### algorithm: identity

# 		y_name <- '3_sdm_algorithm'
# 		match_on <- 'raster'
# 		field_names <- c('algo_ensemble', 'algo_maxent', 'algo_maxnet', 'algo_glm', 'algo_gam', 'algo_rf')

# 		y <- rast_fields[ , ..field_names]
# 		y <- y[ , lapply(.SD, as.numeric)]
# 		y <- apply(y, 1, function(row) {
# 			cols_with_1 <- field_names[row == 1]
# 			if (length(cols_with_1) == 0) return(NA)
# 			paste(cols_with_1, collapse = ", ")
# 		})
# 		y <- replace_y_NAs(y = y, field_names = field_names)

# 		y_match <- match_single_y(match_on = match_on, y = y, attributes = attributes)
# 		attributes[ , (y_name) := y_match]
# 		nice_names <- rbind(nice_names, data.table(computer = y_name, nice = 'SDM Algorithm'))

# 	### bias correction: method

# 		prepend <- '2'
# 		match_on <- 'team'

# 		field_names <- c('bias_correction_spatial_thinning', 'bias_correction_environmental_thinning', 'bias_correction_target_background', 'bias_correction_nonrandom_background')

# 		attributes <- match_field_by_field(match_on = match_on, field_names = field_names, attributes = attributes, prepend = prepend)
# 		nice_names <- rbind(nice_names, data.table(computer = paste0(prepend, '_', field_names), nice = c('Bias Correction: Spatial Thinning', 'Bias Correction: Environ. Thinning', 'Bias Correction: Target Background', 'Bias Correction: Non-random BG')))

# 	### non-presence type

# 		y_name <- '2_non_presence_type'
# 		match_on <- 'team'

# 		field_names <- c('nonpres_type_background', 'nonpres_type_pseudoabsence', 'nonpres_type_target_background')

# 		y <- team_fields[ , ..field_names]
# 		y <- y[ , lapply(.SD, as.numeric)]
# 		y <- apply(y, 1, function(row) {
# 			cols_with_1 <- field_names[row == 1]
# 			if (length(cols_with_1) == 0) return(NA)
# 			paste(cols_with_1, collapse = ", ")
# 		})
# 		y <- replace_y_NAs(y = y, field_names = field_names)

# 		y_match <- match_single_y(match_on = match_on, y = y, attributes = attributes)
# 		attributes[ , (y_name) := y_match]
# 		nice_names <- rbind(nice_names, data.table(computer = y_name, nice = 'Non-presence Type'))

# 	### calibration region boundary
	
# 		prepend <- '2'
# 		match_on <- 'team'

# 		# non-presences: type
# 		field_names <- c('boundary_rectangle', 'boundary_natural', 'boundary_convex_hull', 'boundary_range_map', 'boundary_political', 'boundary_buffer_around_occurrences')

# 		attributes <- match_field_by_field(match_on = match_on, field_names = field_names, attributes = attributes, prepend = prepend)
# 		nice_names <- rbind(nice_names, data.table(computer = paste0(prepend, '_', field_names), nice = c('Calibration Region: Rectangle', 'Calibration Region: Natural', 'Calibration Region: Convex Hull', 'Calibration Region: Range Map', 'Calibration Region: Political', 'Calibration Region: Buffer')))

# 	### calibration region extent
		
# 		y_name <- '2_calibration_extent_km2'
# 		match_on <- 'team'

# 		y <- as.numeric(team_fields$extent_calibration_sans_water_km2)
	
# 		y_match <- match_single_y(match_on = match_on, y = y, attributes = attributes)
# 		attributes[ , (y_name) := y_match]
# 		nice_names <- rbind(nice_names, data.table(computer = y_name, nice = 'Calibration Region Extent'))

# 	### AUC
		
# 		y_name <- '4_auc'
# 		match_on <- 'raster'

# 		y <- as.numeric(rast_fields$eval_metric_auc_roc_value)

# 		y_match <- match_single_y(match_on = match_on, y = y, attributes = attributes)
# 		attributes[ , (y_name) := y_match]
# 		nice_names <- rbind(nice_names, data.table(computer = y_name, nice = 'AUC'))

# 	### thresholded predictions

# 		y_name <- '4_continuous_vs_thresholded'
# 		match_on <- 'raster'

# 		field_names <- c('prediction_type_continuous', 'prediction_type_binary_threshold', 'prediction_type_multi_threshold')

# 		y <- rast_fields[ , ..field_names]
# 		y <- y[ , lapply(.SD, as.numeric)]
# 		y <- apply(y, 1, function(row) {
# 			cols_with_1 <- field_names[row == 1]
# 			if (length(cols_with_1) == 0) return(NA)
# 			paste(cols_with_1, collapse = ", ")
# 		})
# 		y <- replace_y_NAs(y = y, field_names = field_names)

# 		y_match <- match_single_y(match_on = match_on, y = y, attributes = attributes)
# 		attributes[ , (y_name) := y_match]
# 		nice_names <- rbind(nice_names, data.table(computer = y_name, nice = 'Contin./Binary Thresh./Multi-thresh.'))

# 	### time period

# 		y_name <- '4_time_period'
# 		match_on <- 'team'
# 		time_period <- 'late'

# 		y <- team_fields$`4_time_period`

# 		y_match <- match_single_y(match_on = match_on, y = y, attributes = attributes)
# 		attributes[ , (y_name) := y_match]
# 		nice_names <- rbind(nice_names, data.table(computer = y_name, nice = 'Time Period'))

# 	# ### late-century time period
# 	# # present and mid-century time period are redundant with climate data source, so not doing them
# 	# # NB not using this bc highly correlated with climate data source.

# 	# 	y_name <- '4_late_century_time_period'
# 	# 	match_on <- 'team'
# 	# 	time_period <- 'late'

# 	# 	y <- team_fields$future_scenario_latecentury_year

# 	# 	y_match <- match_single_y(match_on = match_on, y = y, attributes = attributes)
# 	# 	attributes[ , (y_name) := y_match]
# 	# 	nice_names <- rbind(nice_names, data.table(computer = y_name, nice = 'Late-Century Time Period'))

# 	### future: emission scenario
		
# 		y_name <- '4_emissions_scenario'
# 		match_on <- 'raster'
		
# 		field_names <- c('future_scenario_ensemble', 'future_scenario_ssp126', 'future_scenario_ssp245', 'future_scenario_ssp370', 'future_scenario_ssp585', 'future_scenario_rcp45', 'future_scenario_rcp85')

# 		y <- rast_fields[ , ..field_names]
# 		y <- y[ , lapply(.SD, as.numeric)]
# 		y <- apply(y, 1, function(row) {
# 			cols_with_1 <- field_names[row == 1]
# 			if (length(cols_with_1) == 0) return(NA)
# 			paste(cols_with_1, collapse = ", ")
# 		})
# 		y <- replace_y_NAs(y = y, field_names = field_names)
# 		y[rast_fields$time_period == 'present'] <- 'present'

# 		y_match <- match_single_y(match_on = match_on, y = y, attributes = attributes)
# 		attributes[ , (y_name) := y_match]
# 		nice_names <- rbind(nice_names, data.table(computer = y_name, nice = 'Present/Future Scenario'))

# 	### extrapolation: individual methods

# 		prepend <- '4'
# 		y_name <- 'extrapolation_method'
# 		match_on <- 'raster'
		
# 		field_names <- c('extrapolation_clamping_masking_clipping', 'extrapolation_exdet', 'extrapolation_mess', 'extrapolation_shape', 'extrapolation_area_of_applicability', 'extrapolation_response_curve_inspection', 'extrapolation_kissmig')

# 		y_match <- match_field_by_field(match_on = match_on, field_names = field_names, attributes = attributes, prepend = prepend)
# 		attributes <- y_match
# 		nice_names <- rbind(nice_names, data.table(computer = paste0(prepend, '_', field_names), nice = c('Extrap.: Clamp/Mask/Clip', 'Extrap.: ExDet', 'Extrap.: MESS', 'Extrap.: Shape', 'Extrap.: Area of Applicability', 'Extrap.: Response Curves Inspect', 'Extrap.: KISSMig')))

# 	### taxonomy: accounted for subspecies
		
# 		if (species_focal == 'Priona') {

# 			y_name <- '1_modeled_only_mainland'
# 			match_on <- 'team'

# 			y <- as.numeric(team_fields$taxonomy_mainland_only)

# 			y_match <- match_single_y(match_on = match_on, y = y, attributes = attributes)
# 			attributes[ , (y_name) := y_match]
# 			nice_names <- rbind(nice_names, data.table(computer = y_name, nice = 'Modeled Only Mainland'))

# 		}

# 	### save
# 	########

# 	saveRDS(attributes, paste0(this_out_dir, '/workflow_attributes.rds'))
# 	saveRDS(nice_names, paste0(this_out_dir, '/workflow_attributes_nice_names.rds'))

# 	### multivariate random forest
# 	predictors <- names(attributes)[grepl('^[0-9]', names(attributes))]
# 	# predictors <- predictors[predictors %notin% '0_team']
# 	forest_data <- cbind(scores, attributes[ , ..predictors])

# 	# convert character columns to factors
# 	char_cols <- names(forest_data)[sapply(forest_data, is.character)]
# 	forest_data[ , (char_cols) := lapply(.SD, as.factor), .SDcols = char_cols]

# 	options(rf.cores = 1) # deterministic execution to ensure seed pertains

# 	for (na_opt in c('na.impute', 'na.omit', 'median')) {

# 		this_data <- forest_data
# 		if (na_opt == 'na.impute') {
# 			na.action <- 'na.impute'
# 		} else if (na_opt == 'na.omit') {
# 			na.action <- 'na.omit'
# 		} else if (na_opt == 'median') {
			
# 			# replace NAs with median of each numeric column
# 			for (col in names(this_data)) {
# 				if (is.numeric(this_data[[col]])) {
# 					this_data[[col]][is.na(this_data[[col]])] <- median(this_data[[col]], na.rm = TRUE)
# 				} else if (is.factor(this_data[[col]])) {
# 					# replace NAs with the most frequent level
# 					most_freq <- names(sort(table(this_data[[col]]), decreasing = TRUE))[1]
# 					this_data[[col]][is.na(this_data[[col]])] <- most_freq
# 				}
# 			}

# 			na.action <- 'na.omit'
		
# 		}

# 		mrf <- rfsrc(
# 			Multivar(PC1, PC2) ~ .,
# 			data = this_data, 
# 			mtry = ceiling(sqrt(ncol(this_data) - 2)), # good for n ~ p
# 			ntree = 10000,
# 			nodesize = ceiling(nrow(this_data) / 10), # size of each "end" cluster
# 			nimpute = 3, # makes OOB optimistic but better for cases with small n... only for na.action = 'na.impute'
# 			na.action = na.action,
# 			importance = TRUE,
# 			seed = 1
# 		)

# 		var_imp <- vimp(mrf, importance = 'permute', nrep = 10000, seed = 1)

# 		# PC1
# 		this_vi <- var_imp$regrOutput$PC1$importance
# 		this_vi[this_vi < 0] <- 0
# 		attribute <- names(this_vi)
# 		nice_name <- nice_names$nice[match(attribute, nice_names$computer)]
# 		prop_var_explained <- pca$sdev[1]^2 / sum(pca$sdev^2)
# 		weighted <- this_vi * prop_var_explained
# 		proportion <- weighted / sum(weighted)
# 		stage <- substr(attribute, 1, 1)
# 		vi_pc1 <- data.table(PC = 1, attribute = attribute, nice_name = nice_name, raw = this_vi, weighted = weighted, proportion = proportion, stage = stage)

# 		# PC2
# 		this_vi <- var_imp$regrOutput$PC2$importance
# 		this_vi[this_vi < 0] <- 0
# 		attribute <- names(this_vi)
# 		nice_name <- nice_names$nice[match(attribute, nice_names$computer)]
# 		prop_var_explained <- pca$sdev[2]^2 / sum(pca$sdev^2)
# 		weighted <- this_vi * prop_var_explained
# 		proportion <- weighted / sum(weighted)
# 		stage <- substr(attribute, 1, 1)
# 		vi_pc2 <- data.table(PC = 2, attribute = attribute, nice_name = nice_name, raw = this_vi, weighted = weighted, proportion = proportion, stage = stage)

# 		# PC1 + PC2
# 		raw <- vi_pc1$raw + vi_pc2$raw
# 		weighted <- vi_pc1$weighted + vi_pc2$weighted
# 		proportion <- weighted / sum(weighted)
# 		stage <- substr(attribute, 1, 1)
# 		vi_pc1_pc2 <- data.table(PC = '1 & 2', attribute = attribute, nice_name = nice_name, raw = raw, weighted = weighted, proportion = proportion, stage = stage)

# 		order <- order(vi_pc1_pc2$proportion, decreasing = TRUE)
# 		vi_pc1 <- vi_pc1[order]
# 		vi_pc2 <- vi_pc2[order]
# 		vi_pc1_pc2 <- vi_pc1_pc2[order]

# 		vi <- list(vi_pc1, vi_pc2, vi_pc1_pc2)

# 		names(vi) <- c('PC1', 'PC2', 'PC1 & PC2')
# 		saveRDS(vi, paste0(this_out_dir, '/MRF Variable Importance - ', na_opt, '.rds'))

# 	} # next AN optoion

# say('####################################')
# say('### graph of variable importance ###')
# say('####################################')

# 	for (na_opt in c('na.impute', 'na.omit', 'median')) {

# 		vis <- readRDS(paste0(this_out_dir, '/MRF Variable Importance - ', na_opt, '.rds'))

# 		# graph variable importance
# 		var_imps <- list()
# 		for (i in 1:3) {

# 			if (i %in% 1:2) {
# 				title <- paste0(letters[i], ') PC ', i)
# 			} else {
# 				title <- 'c) PCs 1 & 2'
# 			}

# 			var_imps[[i]] <- ggplot(vis[[i]], aes(x = reorder(nice_name, proportion), y = proportion, fill = stage)) +
# 				geom_bar(stat = 'identity', color = 'gray30', linewidth = 0.2) +
# 				scale_fill_manual(
# 					values = c('0' = 'white', '1' = '#60ADF1', '2' = '#F9CB40', '3' = '#11EFC9', '4' = '#F56096', '5' = 'gray40'),
# 					labels = c('0' = 'Team', '1' = 'Inputs', '2' = 'Stage setting', '3' = 'Algorithm/software', '4' = 'Post-processing', '5' = 'Standards'),
# 					name = NULL
# 				) +
# 				labs(x = NULL, y = 'Relative importance') +
# 				theme_bw() +
# 				coord_flip() +
# 				ggtitle(title) +
# 				theme(
# 					legend.position = c(0.99, 0.01),
# 					legend.justification = c(1, 0),
# 					axis.text = element_text(size = 8)
# 				)

# 		}

# 		var_imp <- var_imps[[1]] + var_imps[[2]] + var_imps[[3]]
# 		ggsave(paste0(this_out_dir, '/MRF Variable Importance - ', na_opt, '.png'), var_imp, width = 16, height = 8, dpi = 600, bg = 'white')

# 	}

# say('##################################################################################################')
# say('### make PCA biplots with rasters coded by select workflow attributes for figures in main text ###')
# say('##################################################################################################')

# 	# Make individual plots of rasters in PC space and annotate with selected, individual workflow attributes. Good for presentation.

# 	rast_fields <- load_rast_fields(species_focal = species_focal)
# 	team_fields <- load_team_fields(species_focal = species_focal)

# 	### PCA
# 	preds <- load_predictions(species_focal = species_focal, period = 'all', scale = TRUE, subset_teams = TRUE)
# 	preds_trans <- t(preds)

# 	pca <- prcomp(preds_trans)
# 	scores <- pca$x[ , 1:2]
# 	scores <- as.data.frame(scores)

# 	scores$raster_period <- rownames(scores)
# 	scores$raster <- scrub_period(scores$raster_period)
# 	nc <- nchar(scores$raster_period)
# 	starts <- regexpr(scores$raster_period, pattern = '_') + 1
# 	scores$period <- substr(scores$raster_period, starts, nc)
# 	scores$period <- capIt(scores$period)
# 	scores$team <- substr(scores$raster_period, 1, 1)

# 	### generic biplot to be filled in
# 	var1 <- sprintf('%.1f', 100 * pca$sdev[1]^2 / sum(pca$sdev^2))
# 	var2 <- sprintf('%.1f', 100 * pca$sdev[2]^2 / sum(pca$sdev^2))

# 	x_lab <- paste0('PC 1 (', var1, '%)')
# 	y_lab <- paste0('PC 2 (', var2, '%)')

# 	base_biplot <- ggplot() +
# 		coord_cartesian(clip = 'off') +
# 		xlab(x_lab) + ylab(y_lab) +
# 		coord_fixed() +
# 		theme_minimal()

# 	# ### teams
# 	# #########

# 	# 	p <- tests$permanova_p[tests$nice == 'Team']
# 	# 	p <- roundTo(p, 0.001)

# 	# 	# cluster
# 	# 	polys <- data.table()
# 	# 	unique_clusters <- unique(scores$team)
# 	# 	for (i in seq_along(unique_clusters)) {
		
# 	# 		cluster <- unique_clusters[i]
# 	# 		pts <- scores[scores$team == cluster, c('PC1', 'PC2')]
# 	# 		hull_indices <- chull(pts)
# 	# 		hull_pts <- pts[hull_indices, ]
# 	# 		hull_pts <- rbind(hull_pts, hull_pts[1, , drop = FALSE])
# 	# 		hull_pts$team <- cluster
# 	# 		polys <- rbind(polys, hull_pts)
		
# 	# 	}

# 	# 	biplot <- base_biplot +
# 	# 		geom_point(
# 	# 			data = scores, aes(x = PC1, y = PC2, color = team),
# 	# 			size = 5,
# 	# 			pch = 1
# 	# 		) +
# 	# 		geom_polygon(data = polys, aes(x = PC1, y = PC2, fill = team), alpha = 0.4, color = 'gray30') +
# 	# 		annotate(
# 	# 			'text', x = -Inf, y = -Inf, label = paste0('italic(P) == ', sprintf('%.5f', p)),
# 	# 			parse = TRUE, hjust = -0.3, vjust = -1, size = 7, fontface = 'bold'
# 	# 		) +
# 	# 		coord_fixed() +
# 	# 		theme_minimal() +
# 	# 		theme(
# 	# 			legend.position = 'none',
# 	# 			axis.title = element_text(size = 20),
# 	# 			axis.text = element_text(size = 14)
# 	# 		)

# 	# 	ggsave(biplot, filename = paste0(this_out_dir, '/Biplot - Team.png'), width = 8, height = 5, dpi = 600, bg = 'white')

# 	# ### thresholding
# 	# ################

# 	# 	p <- tests$permanova_p[tests$nice == 'Predictions: Continuous/thresholded']
# 	# 	if (p >= 0.001) p <- roundTo(p, 0.001, ceiling)

# 	# 	rast_fields <- load_rast_fields(species_focal = species_focal)
# 	# 	scores$thresholded <- 'Continuous'

# 	# 	rast_fields$raster_period <- apply(rast_fields[ , c('raster_name', 'time_period')], 1, paste, collapse = '_')
# 	# 	index <- match(rast_fields$raster_period, scores$raster_period)
# 	# 	this_index <- index[rast_fields$prediction_type_binary_threshold == 1]
# 	# 	scores$thresholded[this_index] <- 'Binary Threshold'

# 	# 	this_index <- index[rast_fields$prediction_type_multi_threshold == 1]
# 	# 	scores$thresholded[this_index] <- 'Multiple Thresholds'

# 	# 	# cluster
# 	# 	cluster <- 'thresholded'
# 	# 	polys <- data.table()
# 	# 	unique_clusters <- unique(scores[[cluster]])
# 	# 	for (i in seq_along(unique_clusters)) {
		
# 	# 		cluster_name <- unique_clusters[i]
# 	# 		pts <- scores[scores[ , cluster, drop = TRUE] == cluster_name, c('PC1', 'PC2')]
# 	# 		hull_indices <- chull(pts)
# 	# 		hull_pts <- pts[hull_indices, ]
# 	# 		hull_pts <- rbind(hull_pts, hull_pts[1, , drop = FALSE])
# 	# 		hull_pts$cluster <- cluster_name
# 	# 		polys <- rbind(polys, hull_pts)
		
# 	# 	}

# 	# 	biplot <- base_biplot +
# 	# 		geom_point(
# 	# 			data = scores, aes(x = PC1, y = PC2, color = thresholded, shape = thresholded), 
# 	# 			size = 5
# 	# 		) +
# 	# 		scale_shape_manual(values = c(0, 1, 2)) +
# 	# 		geom_polygon(data = polys, aes(x = PC1, y = PC2, fill = cluster), alpha = 0.4, color = 'gray30') +
# 	# 		annotate(
# 	# 			'text', x = -Inf, y = -Inf, label = paste0('italic(P) == ', sprintf('%.6f', p)),
# 	# 			parse = TRUE, hjust = -0.3, vjust = -1, size = 7, fontface = 'bold'
# 	# 		) +
# 	# 		coord_fixed() +
# 	# 		theme_minimal() +
# 	# 		theme(
# 	# 			legend.position = 'none',
# 	# 			axis.title = element_text(size = 20),
# 	# 			axis.text = element_text(size = 14)
# 	# 		)

# 	# 	ggsave(biplot, filename = paste0(this_out_dir, '/Biplot - Thresholding.png'), width = 8, height = 5, dpi = 600, bg = 'white')

# 	# ### teams + thresholded/continuous
# 	# ##################################

# 	# 	p <- tests$permanova_p[tests$nice == 'Team × Continuous/Thresholded']
# 	# 	if (p > 0.001) p <- roundTo(p, 0.001)

# 	# 	# cluster
# 	# 	polys <- data.table()
# 	# 	scores$team_threshold <- paste(scores$team, scores$thresholded)
# 	# 	unique_clusters <- unique(scores$team_threshold)
# 	# 	for (i in seq_along(unique_clusters)) {
		
# 	# 		cluster <- unique_clusters[i]
# 	# 		pts <- scores[scores$team_threshold == cluster, c('PC1', 'PC2')]
# 	# 		hull_indices <- chull(pts)
# 	# 		hull_pts <- pts[hull_indices, ]
# 	# 		hull_pts <- rbind(hull_pts, hull_pts[1, , drop = FALSE])
# 	# 		hull_pts$team_threshold <- cluster
# 	# 		polys <- rbind(polys, hull_pts)
		
# 	# 	}

# 	# 	biplot <- base_biplot +
# 	# 		geom_point(
# 	# 			data = scores, aes(x = PC1, y = PC2, color = team_threshold),
# 	# 			size = 5,
# 	# 			pch = 1
# 	# 		) +
# 	# 		geom_polygon(data = polys, aes(x = PC1, y = PC2, fill = team_threshold), alpha = 0.4, color = 'gray30') +
# 	# 		annotate(
# 	# 			'text', x = -Inf, y = -Inf, label = paste0('italic(P) == ', sprintf('%.5f', p)),
# 	# 			parse = TRUE, hjust = -0.3, vjust = -1, size = 7, fontface = 'bold'
# 	# 		) +
# 	# 		coord_fixed() +
# 	# 		theme_minimal() +
# 	# 		theme(
# 	# 			legend.position = 'none',
# 	# 			axis.title = element_text(size = 20),
# 	# 			axis.text = element_text(size = 14)
# 	# 		)

# 	# 	ggsave(biplot, filename = paste0(this_out_dir, '/Biplot - Team × Continuous-Thresholded.png'), width = 8, height = 5, dpi = 600, bg = 'white')

# 	# ### time period
# 	# ###############

# 	# 	p <- tests$permanova_p[tests$nice == 'Projection: Time period']
# 	# 	if (p >= 0.001) p <- roundTo(p, 0.001)

# 	# 	# cluster
# 	# 	polys <- data.table()
# 	# 	unique_clusters <- unique(scores$period)
# 	# 	for (i in seq_along(unique_clusters)) {
		
# 	# 		cluster <- unique_clusters[i]
# 	# 		pts <- scores[scores$period == cluster, c('PC1', 'PC2')]
# 	# 		hull_indices <- chull(pts)
# 	# 		hull_pts <- pts[hull_indices, ]
# 	# 		hull_pts <- rbind(hull_pts, hull_pts[1, , drop = FALSE])
# 	# 		hull_pts$period <- cluster
# 	# 		polys <- rbind(polys, hull_pts)
		
# 	# 	}

# 	# 	biplot <- base_biplot +
# 	# 		geom_point(
# 	# 			data = scores, aes(x = PC1, y = PC2, fill = period, shape = period), 
# 	# 			size = 5
# 	# 		) +
# 	# 		scale_shape_manual(values = c(21, 22, 23)) +
# 	# 		geom_polygon(data = polys, aes(x = PC1, y = PC2, fill = period), alpha = 0.4, color = 'gray30') +
# 	# 		annotate(
# 	# 			'text', x = -Inf, y = -Inf, label = paste0('italic(P) == ', sprintf('%.5f', p)),
# 	# 			parse = TRUE, hjust = -0.3, vjust = -1, size = 7, fontface = 'bold'
# 	# 		) +
# 	# 		coord_fixed() +
# 	# 		theme_minimal() +
# 	# 		theme(
# 	# 			legend.position = 'none',
# 	# 			axis.title = element_text(size = 20),
# 	# 			axis.text = element_text(size = 14)
# 	# 		)

# 	# 	ggsave(biplot, filename = paste0(this_out_dir, '/Biplot - Time Period.png'), width = 8, height = 5, dpi = 600, bg = 'white')

# 	### climate data source
# 	#######################

# 		vi <- vis[[3]][vis[[3]]$attribute == '1_climate_predictor_source', ]$proportion
# 		vi <- roundTo(vi, 0.001)
# 		vi <- sprintf('%.3f', vi)

# 		fields <- load_team_fields(species_focal = species_focal)
# 		index <- match(scores$team, fields$team)
# 		scores$predictors_climate_source <- fields$predictors_climate_source[index]

# 		# cluster
# 		cluster <- 'predictors_climate_source'
# 		polys <- data.table()
# 		unique_clusters <- unique(scores[[cluster]])
# 		for (i in seq_along(unique_clusters)) {
		
# 			cluster_name <- unique_clusters[i]
# 			pts <- scores[scores[ , cluster, drop = TRUE] == cluster_name, c('PC1', 'PC2')]
# 			hull_indices <- chull(pts)
# 			hull_pts <- pts[hull_indices, ]
# 			hull_pts <- rbind(hull_pts, hull_pts[1, , drop = FALSE])
# 			hull_pts$cluster <- cluster_name
# 			polys <- rbind(polys, hull_pts)
		
# 		}

# 		biplot <- base_biplot +
# 			geom_polygon(data = polys, aes(x = PC1, y = PC2, fill = cluster), alpha = 0.4, color = 'black') +
# 			geom_point(
# 				data = scores, aes(x = PC1, y = PC2, fill = predictors_climate_source, shape = predictors_climate_source), 
# 				size = 5
# 			) +
# 			scale_shape_manual(values = c(21, 22)) +
# 			annotate(
# 				'text', x = Inf, y = -Inf, label = paste0('VI: ', vi), 
# 				parse = FALSE, hjust = 1, vjust = -0.5, size = 7
# 			) +
# 			coord_fixed() +
# 			theme_minimal() +
# 			theme(
# 				legend.position = 'none',
# 				axis.title = element_text(size = 20),
# 				axis.text = element_text(size = 14)
# 			)

# 		ggsave(biplot, filename = paste0(this_out_dir, '/Biplot - Climate Data Source.png'), width = 8, height = 5, dpi = 600, bg = 'white')

# 	# ### number of predictors
# 	# ########################

# 	# 	vi_1 <- vis$PC1[vis$PC1$variable == '3_number_of_predictors', ]
# 	# 	vi_2 <- vis$PC2[vis$PC2$variable == '3_number_of_predictors', ]


# 	# 	p <- tests$permanova_p[tests$nice == 'Predictors: Total number']
# 	# 	if (p > 0.001) p <- roundTo(p, 0.001)

# 	# 	fields <- load_team_fields(species_focal = species_focal)
# 	# 	index <- match(scores$team, fields$team)
# 	# 	scores$n_predictors <- fields$predictors_climate_nonclimate_num_total[index]

# 	# 	cluster <- 'n_predictors'
# 	# 	biplot <- base_biplot +
# 	# 		geom_point(data = scores, aes(x = PC1, y = PC2, fill = .data[[cluster]]), pch = 21, alpha = 0.7, size = 5) +
# 	# 		scale_fill_viridis_c(option = 'magma', trans = 'log2') +
# 	# 		annotate(
# 	# 			'text', x = -Inf, y = -Inf, label = paste0('italic(P) == ', sprintf('%.5f', p)),
# 	# 			parse = TRUE, hjust = -0.3, vjust = -1, size = 7, fontface = 'bold'
# 	# 		) +
# 	# 		coord_fixed() +
# 	# 		theme_minimal() +
# 	# 		theme(
# 	# 			legend.position = 'none',
# 	# 			axis.title = element_text(size = 20),
# 	# 			axis.text = element_text(size = 14)
# 	# 		)

# 	# 	ggsave(biplot, filename = paste0(this_out_dir, '/Biplot - Number of Predictors.png'), width = 8, height = 5, dpi = 600, bg = 'white')

# 	### number of occurrences
# 	#########################

# 		vi <- vis[[3]][vis[[3]]$attribute == '1_number_of_occurrences', ]$proportion
# 		vi <- roundTo(vi, 0.001)
# 		vi <- sprintf('%.3f', vi)

# 		fields <- load_team_fields(species_focal = species_focal)
# 		index <- match(scores$team, fields$team)
# 		scores$n_occurrences <- fields$num_occurrences_minimum[index]

# 		cluster <- 'n_occurrences'
# 		biplot <- base_biplot +
# 			geom_point(data = scores, aes(x = PC1, y = PC2, fill = .data[[cluster]]), pch = 21, alpha = 0.7, size = 5) +
# 			scale_fill_viridis_c(option = 'magma', trans = 'log2') +
# 			annotate(
# 				'text', x = Inf, y = -Inf, label = paste0('VI: ', vi), 
# 				parse = FALSE, hjust = 1, vjust = -0.5, size = 7
# 			) +
# 			coord_fixed() +
# 			theme_minimal() +
# 			theme(
# 				legend.position = 'none',
# 				axis.title = element_text(size = 20),
# 				axis.text = element_text(size = 14)
# 			)

# 		ggsave(biplot, filename = paste0(this_out_dir, '/Biplot - Number of Occurrences.png'), width = 8, height = 5, dpi = 600, bg = 'white')

# 	### spatial resolution (km2)
# 	############################

# 		vi <- vis[[3]][vis[[3]]$attribute == '1_spatial_resolution_km2', ]$proportion
# 		vi <- roundTo(vi, 0.001)
# 		vi <- sprintf('%.3f', vi)

# 		fields <- load_team_fields(species_focal = species_focal)
# 		index <- match(scores$team, fields$team)
# 		scores$res_km2 <- fields$res_km2[index]

# 		cluster <- 'res_km2'
# 		biplot <- base_biplot +
# 			geom_point(data = scores, aes(x = PC1, y = PC2, fill = .data[[cluster]]), pch = 21, alpha = 0.7, size = 5) +
# 			scale_fill_viridis_c(option = 'magma', trans = 'log2') +
# 			annotate(
# 				'text', x = Inf, y = -Inf, label = paste0('VI: ', vi), 
# 				parse = FALSE, hjust = 1, vjust = -0.5, size = 7
# 			) +
# 			coord_fixed() +
# 			theme_minimal() +
# 			theme(
# 				legend.position = 'none',
# 				axis.title = element_text(size = 20),
# 				axis.text = element_text(size = 14)
# 			)

# 		ggsave(biplot, filename = paste0(this_out_dir, '/Biplot - Spatial Resolution.png'), width = 8, height = 5, dpi = 600, bg = 'white')

# 	### AUC
# 	#######

# 		vi <- vis[[3]][vis[[3]]$attribute == '4_auc', ]$proportion
# 		vi <- roundTo(vi, 0.001)
# 		vi <- sprintf('%.3f', vi)

# 		fields <- load_rast_fields(species_focal = species_focal)
# 		scores$auc <- NA_real_

# 		fields$raster_period <- apply(fields[ , c('raster_name', 'time_period')], 1, paste, collapse = '_')
# 		index <- match(fields$raster_period, scores$raster_period)
# 		scores$auc[index] <- fields$eval_metric_auc_roc_value
# 		scores$auc <- as.numeric(scores$auc)

# 		cluster <- 'auc'
# 		biplot <- base_biplot +
# 			geom_point(data = scores, aes(x = PC1, y = PC2), pch = 1, alpha = 0.7, size = 5, color = 'gray') +
# 			geom_point(data = scores[!is.na(scores$auc), ], aes(x = PC1, y = PC2, fill = .data[[cluster]]), pch = 21, alpha = 0.7, size = 5) +
# 			scale_fill_gradient(low = 'darkblue', high = 'yellow', na.value = 'gray90') +
# 			annotate(
# 				'text', x = Inf, y = -Inf, label = paste0('VI: ', vi), 
# 				parse = FALSE, hjust = 1, vjust = -0.5, size = 7
# 			) +
# 			coord_fixed() +
# 			theme_minimal() +
# 			theme(
# 				legend.position = 'none',
# 				axis.title = element_text(size = 20),
# 				axis.text = element_text(size = 14)
# 			)

# 		ggsave(biplot, filename = paste0(this_out_dir, '/Biplot - AUC.png'), width = 8, height = 5, dpi = 600, bg = 'white')

# 	### calibration region extent
# 	#############################

# 		vi <- vis[[3]][vis[[3]]$attribute == '2_calibration_extent_km2', ]$proportion
# 		vi <- roundTo(vi, 0.001)
# 		vi <- sprintf('%.3f', vi)

# 		fields <- load_team_fields(species_focal = species_focal)
# 		scores$calibration_extent_log10 <- log10(as.numeric(fields$extent_calibration_sans_water_km2[match(scores$team, fields$team_code)]))

# 		cluster <- 'calibration_extent_log10'
# 		biplot <- base_biplot +
# 			geom_point(data = scores, aes(x = PC1, y = PC2), pch = 1, alpha = 0.7, size = 5, color = 'gray') +
# 			geom_point(data = scores[!is.na(scores$calibration_extent_log10), ], aes(x = PC1, y = PC2, fill = .data[[cluster]]), pch = 21, alpha = 0.7, size = 5) +
# 			scale_fill_gradient(low = 'darkblue', high = 'yellow', na.value = 'gray90') +
# 			annotate(
# 				'text', x = Inf, y = -Inf, label = paste0('VI: ', vi), 
# 				parse = FALSE, hjust = 1, vjust = -0.5, size = 7
# 			) +
# 			coord_fixed() +
# 			theme_minimal() +
# 			theme(
# 				legend.position = 'none',
# 				axis.title = element_text(size = 20),
# 				axis.text = element_text(size = 14)
# 			)

# 		ggsave(biplot, filename = paste0(this_out_dir, '/Biplot - Calibration Extent.png'), width = 8, height = 5, dpi = 600, bg = 'white')

# 	### modeled mainland only or mainland + insular
# 	###############################################

# 		if (species_focal == 'Priona') {

# 			vi <- vis[[3]][vis[[3]]$attribute == '1_modeled_only_mainland', ]$proportion
# 			vi <- roundTo(vi, 0.001)
# 			vi <- sprintf('%.3f', vi)

# 			fields <- load_team_fields(species_focal = species_focal)
# 			index <- match(scores$team, fields$team)
# 			scores$modeled_subspecies <- fields$taxonomy_mainland_only[index]
# 			scores$modeled_subspecies <- ifelse(scores$modeled_subspecies == 1, 'Mainland Only', 'Mainland + Insular')

# 			# cluster
# 			cluster <- 'modeled_subspecies'
# 			polys <- data.table()
# 			unique_clusters <- unique(scores[[cluster]])
# 			for (i in seq_along(unique_clusters)) {
			
# 				cluster_name <- unique_clusters[i]
# 				pts <- scores[scores[ , cluster, drop = TRUE] == cluster_name, c('PC1', 'PC2')]
# 				hull_indices <- chull(pts)
# 				hull_pts <- pts[hull_indices, ]
# 				hull_pts <- rbind(hull_pts, hull_pts[1, , drop = FALSE])
# 				hull_pts$cluster <- cluster_name
# 				polys <- rbind(polys, hull_pts)
			
# 			}

# 			biplot <- base_biplot +
# 				geom_polygon(data = polys, aes(x = PC1, y = PC2, fill = cluster), alpha = 0.4, color = 'black') +
# 				geom_point(data = scores, aes(x = PC1, y = PC2, fill = .data[[cluster]]), pch = 21, alpha = 0.7, size = 5) +
# 				annotate(
# 					'text', x = Inf, y = -Inf, label = paste0('VI: ', vi), 
# 					parse = FALSE, hjust = 1, vjust = -0.5, size = 7
# 				) +
# 				coord_fixed() +
# 				theme_minimal() +
# 				theme(
# 					legend.position = 'none',
# 					axis.title = element_text(size = 20),
# 					axis.text = element_text(size = 14)
# 				)

# 			ggsave(biplot, filename = paste0(this_out_dir, '/Biplot - Taxonomy.png'), width = 8, height = 5, dpi = 600, bg = 'white')

# 		}

# 	### BIOCLIM 02
# 	##############

# 		vi <- vis[[3]][vis[[3]]$attribute == '1_predictors_climate_bio2', ]$proportion
# 		vi <- roundTo(vi, 0.001)
# 		vi <- sprintf('%.3f', vi)

# 		fields <- load_team_fields(species_focal = species_focal)
# 		index <- match(scores$team, fields$team)
# 		scores$modeled_subspecies <- fields$predictors_climate_bio2[index]
# 		scores$modeled_subspecies <- ifelse(scores$modeled_subspecies == 1, 'Yes', 'No')

# 		# cluster
# 		cluster <- 'modeled_subspecies'
# 		polys <- data.table()
# 		unique_clusters <- unique(scores[[cluster]])
# 		for (i in seq_along(unique_clusters)) {
		
# 			cluster_name <- unique_clusters[i]
# 			pts <- scores[scores[ , cluster, drop = TRUE] == cluster_name, c('PC1', 'PC2')]
# 			hull_indices <- chull(pts)
# 			hull_pts <- pts[hull_indices, ]
# 			hull_pts <- rbind(hull_pts, hull_pts[1, , drop = FALSE])
# 			hull_pts$cluster <- cluster_name
# 			polys <- rbind(polys, hull_pts)
		
# 		}

# 		biplot <- base_biplot +
# 			geom_polygon(data = polys, aes(x = PC1, y = PC2, fill = cluster), alpha = 0.4, color = 'black') +
# 			geom_point(data = scores, aes(x = PC1, y = PC2, fill = .data[[cluster]]), alpha = 0.7, size = 5) +
# 			scale_shape_manual(values = c(21, 22)) +
# 			annotate(
# 				'text', x = Inf, y = -Inf, label = paste0('VI: ', vi), 
# 				parse = FALSE, hjust = 1, vjust = -0.5, size = 7
# 			) +
# 			coord_fixed() +
# 			theme_minimal() +
# 			theme(
# 				legend.position = 'none',
# 				axis.title = element_text(size = 20),
# 				axis.text = element_text(size = 14)
# 			)

# 		ggsave(biplot, filename = paste0(this_out_dir, '/Biplot - BIOCLIM 02.png'), width = 8, height = 5, dpi = 600, bg = 'white')

# 	# ### SDM algorithm
# 	# #################

# 	# 	p <- tests$permanova_p[tests$nice == 'Algorithm']
# 	# 	if (p > 0.001) p <- roundTo(p, 0.001)

# 	# 	fields <- load_rast_fields(species_focal = species_focal)
# 	# 	scores$algorithm <- NA_character_

# 	# 	fields$raster_period <- apply(fields[ , c('raster_name', 'time_period')], 1, paste, collapse = '_')
# 	# 	index <- match(fields$raster_period, scores$raster_period)
		
# 	# 	this_index <- index[rast_fields$algo_ensemble == 1]
# 	# 	scores$algorithm[this_index] <- 'Ensemble'

# 	# 	this_index <- index[rast_fields$algo_maxent == 1]
# 	# 	scores$algorithm[this_index] <- 'MaxEnt'

# 	# 	this_index <- index[rast_fields$algo_maxnet == 1]
# 	# 	scores$algorithm[this_index] <- 'MaxNet'

# 	# 	this_index <- index[rast_fields$algo_gam == 1]
# 	# 	scores$algorithm[this_index] <- 'GAM'

# 	# 	this_index <- index[rast_fields$algo_rf == 1]
# 	# 	scores$algorithm[this_index] <- 'RF'

# 	# 	if (species_focal == 'Zamia') {
# 	# 		this_index <- index[rast_fields$algo_sre == 1]
# 	# 		scores$algorithm[this_index] <- 'SRE'
# 	# 	}

# 	# 	# cluster
# 	# 	cluster <- 'algorithm'
# 	# 	polys <- data.table()
# 	# 	unique_clusters <- unique(scores[[cluster]])
# 	# 	for (i in seq_along(unique_clusters)) {
		
# 	# 		cluster_name <- unique_clusters[i]
# 	# 		pts <- scores[scores[ , cluster, drop = TRUE] == cluster_name, c('PC1', 'PC2')]
# 	# 		hull_indices <- chull(pts)
# 	# 		hull_pts <- pts[hull_indices, ]
# 	# 		hull_pts <- rbind(hull_pts, hull_pts[1, , drop = FALSE])
# 	# 		hull_pts$cluster <- cluster_name
# 	# 		polys <- rbind(polys, hull_pts)
		
# 	# 	}

# 	# 	biplot <- base_biplot +
# 	# 		geom_point(data = scores, aes(x = PC1, y = PC2, fill = .data[[cluster]], shape = .data[[cluster]]), alpha = 0.6, size = 5) +
# 	# 		scale_shape_manual(values = c(21, 22, 24, 25, 23, 24, 25)) +
# 	# 		geom_polygon(data = polys, aes(x = PC1, y = PC2, fill = cluster), alpha = 0.4, color = 'black') +
# 	# 		annotate(
# 	# 			'text', x = -Inf, y = -Inf, label = paste0('italic(P) == ', sprintf('%.5f', p)),
# 	# 			parse = TRUE, hjust = -0.3, vjust = -1, size = 7, fontface = 'bold'
# 	# 		) +
# 	# 		coord_fixed() +
# 	# 		theme_minimal() +
# 	# 		theme(
# 	# 			legend.position = 'none',
# 	# 			axis.title = element_text(size = 20),
# 	# 			axis.text = element_text(size = 14)
# 	# 		)

# 	# 	ggsave(biplot, filename = paste0(this_out_dir, '/Biplot - SDM Algorithm.png'), width = 8, height = 5, dpi = 600, bg = 'white')

# 	### non-presence type
# 	#####################

# 		vi <- vis[[3]][vis[[3]]$attribute == '2_non_presence_type', ]$proportion
# 		vi <- roundTo(vi, 0.001)
# 		vi <- sprintf('%.3f', vi)

# 		fields <- load_team_fields(species_focal = species_focal)
# 		scores$nonpres_type <- NA_character_

# 		for (i in 1:nrow(scores)) {

# 			team <- scores$team[i]
# 			y <- if (fields$nonpres_type_background[fields$team == team] == 1) {
# 				'Background Sites'
# 			} else if (fields$nonpres_type_pseudoabsence[fields$team == team] == 1) {
# 				'Pseudoabsences'
# 			} else if (fields$nonpres_type_target_background[fields$team == team] == 1) {
# 				'Target Background'
# 			} else if (fields$nonpres_type_unclear_no_response[fields$team == team] == 1) {
# 				'Unclear/No Response'
# 			}
# 			scores$nonpres_type[i] <- y

# 		}

# 		# cluster
# 		cluster <- 'nonpres_type'
# 		polys <- data.table()
# 		unique_clusters <- unique(scores[[cluster]])
# 		for (i in seq_along(unique_clusters)) {
		
# 			cluster_name <- unique_clusters[i]
# 			pts <- scores[scores[ , cluster, drop = TRUE] == cluster_name, c('PC1', 'PC2')]
# 			hull_indices <- chull(pts)
# 			hull_pts <- pts[hull_indices, ]
# 			hull_pts <- rbind(hull_pts, hull_pts[1, , drop = FALSE])
# 			hull_pts$cluster <- cluster_name
# 			polys <- rbind(polys, hull_pts)
		
# 		}

# 		biplot <- base_biplot +
# 			geom_polygon(data = polys, aes(x = PC1, y = PC2, fill = cluster), alpha = 0.4, color = 'black') +
# 			geom_point(data = scores, aes(x = PC1, y = PC2, fill = .data[[cluster]], shape = .data[[cluster]]), alpha = 0.7, size = 5) +
# 			scale_shape_manual(values = c(21, 22, 24, 25)) +
# 			annotate(
# 				'text', x = Inf, y = -Inf, label = paste0('VI: ', vi), 
# 				parse = FALSE, hjust = 1, vjust = -0.5, size = 7
# 			) +
# 			coord_fixed() +
# 			theme_minimal() +
# 			theme(
# 				legend.position = 'none',
# 				axis.title = element_text(size = 20),
# 				axis.text = element_text(size = 14)
# 			)

# 		ggsave(biplot, filename = paste0(this_out_dir, '/Biplot - Non-presence Type.png'), width = 8, height = 5, dpi = 600, bg = 'white')

# 	### bias correction
# 	###################

# 		field_names <- c('bias_correction_spatial_thinning', 'bias_correction_environmental_thinning', 'bias_correction_target_background', 'bias_correction_nonrandom_background')
# 		field_names <- paste0('2_', field_names)

# 		vi <- vis[[3]][vis[[3]]$attribute %in% field_names, ]$proportion
# 		vi <- sum(vi)
# 		vi <- roundTo(vi, 0.001)
# 		vi <- sprintf('%.3f', vi)

# 		fields <- load_team_fields(species_focal = species_focal)
# 		scores$bias_correction <- NA_character_

# 		for (i in 1:nrow(scores)) {

# 			team <- scores$team[i]
# 			if (fields$bias_correction_spatial_thinning[fields$team == team] == 1) {
# 				y <- 'Spatial thinning'
# 			} else if (fields$bias_correction_environmental_thinning[fields$team == team] == 1) {
# 				y <- 'Environmental thinning'
# 			} else if (fields$bias_correction_target_background[fields$team == team] == 1) {
# 				y <- 'Target background'
# 			} else if (fields$bias_correction_nonrandom_background[fields$team == team] == 1) {
# 				y <- 'Non-random background'
# 			} else {
# 				y <- 'Unknown/unclear'
# 			}
# 			scores$bias_correction[i] <- y

# 		}

# 		# Create shape mapping for unique software values
# 		unique_software <- unique(scores$bias_correction[!is.na(scores$bias_correction)])
# 		shape_values <- c(21, 24, 24, 22, 5, 25, 0, 1, 2, 3, 23, 6, 4)
# 		software_names <- c('Spatial thinning', 'Environmental thinning', 'Target background', 'Non-random background', 'Unknown/unclear')
# 		shapes <- setNames(shape_values[match(unique_software, software_names)], unique_software)

# 		# cluster
# 		cluster <- 'bias_correction'
# 		cluster_format <- 'bias_correction_shape'
# 		polys <- data.table()
# 		unique_clusters <- unique(scores[[cluster]])
# 		for (i in seq_along(unique_clusters)) {
		
# 			cluster_name <- unique_clusters[i]
# 			pts <- scores[scores[ , cluster, drop = TRUE] == cluster_name, c('PC1', 'PC2')]
# 			hull_indices <- chull(pts)
# 			hull_pts <- pts[hull_indices, ]
# 			hull_pts <- rbind(hull_pts, hull_pts[1, , drop = FALSE])
# 			hull_pts$cluster <- cluster_name
# 			polys <- rbind(polys, hull_pts)
		
# 		}

# 		biplot <- base_biplot +
# 			scale_fill_brewer(palette = 'Set3') +
# 			geom_polygon(data = polys, aes(x = PC1, y = PC2, fill = cluster), alpha = 0.5, color = 'black') +
# 			geom_point(data = scores, aes(x = PC1, y = PC2, fill = .data[[cluster]], shape = .data[[cluster]]), size = 5) +
# 			scale_shape_manual(values = shapes) +
# 			annotate(
# 				'text', x = Inf, y = -Inf, label = paste0('VI: ', vi), 
# 				parse = FALSE, hjust = 1, vjust = -0.5, size = 7
# 			) +
# 			coord_fixed() +
# 			theme_minimal() +
# 			theme(
# 				legend.position = 'none',
# 				axis.title = element_text(size = 20),
# 				axis.text = element_text(size = 14)
# 			)

# 		ggsave(biplot, filename = paste0(this_out_dir, '/Biplot - Bias Correction.png'), width = 8, height = 5, dpi = 600, bg = 'white')

# 	### bias correction: target background
# 	######################################

# 		field_names <- '2_bias_correction_target_background'

# 		vi <- vis[[3]][vis[[3]]$attribute %in% field_names, ]$proportion
# 		vi <- roundTo(vi, 0.001)
# 		vi <- sprintf('%.3f', vi)

# 		fields <- load_team_fields(species_focal = species_focal)
# 		scores$bias_correction_target_bg <- NA_character_

# 		for (i in 1:nrow(scores)) {

# 			team <- scores$team[i]
# 			if (fields$bias_correction_target_background[fields$team == team] == 1) {
# 				y <- 'Target background'
# 			} else {
# 				y <- 'Other/unknown/unclear'
# 			}
# 			scores$bias_correction_target_bg[i] <- y

# 		}

# 		# shape mapping
# 		uniques <- unique(scores$bias_correction_target_bg[!is.na(scores$bias_correction_target_bg)])
# 		shape_values <- c(21, 24)
# 		names <- c('Target background', 'Other/unknown/unclear')
# 		shapes <- setNames(shape_values[match(uniques, names)], uniques)

# 		# cluster
# 		cluster <- 'bias_correction_target_bg'
# 		cluster_format <- 'bias_correction_target_bg_shape'
# 		polys <- data.table()
# 		unique_clusters <- unique(scores[[cluster]])
# 		for (i in seq_along(unique_clusters)) {
		
# 			cluster_name <- unique_clusters[i]
# 			pts <- scores[scores[ , cluster, drop = TRUE] == cluster_name, c('PC1', 'PC2')]
# 			hull_indices <- chull(pts)
# 			hull_pts <- pts[hull_indices, ]
# 			hull_pts <- rbind(hull_pts, hull_pts[1, , drop = FALSE])
# 			hull_pts$cluster <- cluster_name
# 			polys <- rbind(polys, hull_pts)
		
# 		}

# 		biplot <- base_biplot +
# 			scale_fill_brewer(palette = 'Set3') +
# 			geom_polygon(data = polys, aes(x = PC1, y = PC2, fill = cluster), alpha = 0.5, color = 'black') +
# 			geom_point(data = scores, aes(x = PC1, y = PC2, fill = .data[[cluster]], shape = .data[[cluster]]), size = 5) +
# 			scale_shape_manual(values = shapes) +
# 			annotate(
# 				'text', x = Inf, y = -Inf, label = paste0('VI: ', vi), 
# 				parse = FALSE, hjust = 1, vjust = -0.5, size = 7
# 			) +
# 			coord_fixed() +
# 			theme_minimal() +
# 			theme(
# 				legend.position = 'none',
# 				axis.title = element_text(size = 20),
# 				axis.text = element_text(size = 14)
# 			)

# 		ggsave(biplot, filename = paste0(this_out_dir, '/Biplot - Bias Correction - Target BG.png'), width = 8, height = 5, dpi = 600, bg = 'white')

# 	# ### software
# 	# ############

# 	# 	p <- tests$permanova_p[tests$nice == 'Software']
# 	# 	if (p > 0.001) p <- roundTo(p, 0.001)

# 	# 	fields <- load_team_fields(species_focal = species_focal)
# 	# 	scores$software <- NA_character_

# 	# 	for (i in 1:nrow(scores)) {

# 	# 		team <- scores$team[i]
# 	# 		if (fields$modeling_software_enmeval[fields$team == team] == 1) {
# 	# 			y <- 'ENMeval'
# 	# 		} else if (fields$modeling_software_enmtools[fields$team == team] == 1) {
# 	# 			y <- 'ENMTools'
# 	# 		} else if (fields$modeling_software_wallace[fields$team == team] == 1) {
# 	# 			y <- 'Wallace'
# 	# 		} else if (fields$modeling_software_biomod2[fields$team == team] == 1) {
# 	# 			y <- 'BIOMOD2'
# 	# 		} else if (fields$modeling_software_sabinansdm[fields$team == team] == 1) {
# 	# 			y <- 'sabinaNSDM'
# 	# 		} else if (fields$modeling_software_sdm[fields$team == team] == 1) {
# 	# 			y <- 'sdm'
# 	# 		} else if (fields$modeling_software_miamaxent[fields$team == team] == 1) {
# 	# 			y <- 'MIAmaxent'
# 	# 		} else if (fields$modeling_software_enmsdmx[fields$team == team] == 1) {
# 	# 			y <- 'enmSdmX'
# 	# 		} else if (fields$modeling_software_flexsdm[fields$team == team] == 1) {
# 	# 			y <- 'flexsdm'
# 	# 		} else if (fields$modeling_software_sdmtune[fields$team == team] == 1) {
# 	# 			y <- 'SDMTune'
# 	# 		} else if (fields$modeling_software_piecemeal[fields$team == team] == 1) {
# 	# 			y <- 'piecemeal'
# 	# 		} else if (fields$modeling_software_other[fields$team == team] == 1) {
# 	# 			y <- 'Other'
# 	# 		} else if (fields$modeling_software_unclear_no_response[fields$team == team] == 1) {
# 	# 			y <- 'Unclear/no response'
# 	# 		} else {
# 	# 			y <- NA_character_
# 	# 		}
# 	# 		scores$software[i] <- y

# 	# 	}

# 	# 	# Create shape mapping for unique software values
# 	# 	unique_software <- unique(scores$software[!is.na(scores$software)])
# 	# 	shape_values <- c(21, 24, 24, 22, 5, 25, 0, 1, 2, 3, 23, 6, 4)
# 	# 	software_names <- c('ENMeval', 'ENMTools', 'Wallace', 'BIOMOD2', 'sabinaNSDM', 'sdm', 
# 	# 						'MIAmaxent', 'enmSdmX', 'flexsdm', 'SDMTune', 'piecemeal', 'Other', 'Unclear/no response')
# 	# 	shapes <- setNames(shape_values[match(unique_software, software_names)], unique_software)

# 	# 	# cluster
# 	# 	cluster <- 'software'
# 	# 	cluster_format <- 'software_shape'
# 	# 	polys <- data.table()
# 	# 	unique_clusters <- unique(scores[[cluster]])
# 	# 	for (i in seq_along(unique_clusters)) {
		
# 	# 		cluster_name <- unique_clusters[i]
# 	# 		pts <- scores[scores[ , cluster, drop = TRUE] == cluster_name, c('PC1', 'PC2')]
# 	# 		hull_indices <- chull(pts)
# 	# 		hull_pts <- pts[hull_indices, ]
# 	# 		hull_pts <- rbind(hull_pts, hull_pts[1, , drop = FALSE])
# 	# 		hull_pts$cluster <- cluster_name
# 	# 		polys <- rbind(polys, hull_pts)
		
# 	# 	}

# 	# 	biplot <- base_biplot +
# 	# 		geom_point(data = scores, aes(x = PC1, y = PC2, fill = .data[[cluster]], shape = .data[[cluster]]), size = 5) +
# 	# 		scale_shape_manual(values = shapes) +
# 	# 		scale_fill_brewer(palette = 'Set3') +
# 	# 		geom_polygon(data = polys, aes(x = PC1, y = PC2, fill = cluster), alpha = 0.5, color = 'black') +
# 	# 		annotate(
# 	# 			'text', x = -Inf, y = -Inf, label = paste0('italic(P) == ', sprintf('%.5f', p)),
# 	# 			parse = TRUE, hjust = -0.3, vjust = -1, size = 7, fontface = 'bold'
# 	# 		) +
# 	# 		coord_fixed() +
# 	# 		theme_minimal() +
# 	# 		theme(
# 	# 			legend.position = 'none',
# 	# 			axis.title = element_text(size = 20),
# 	# 			axis.text = element_text(size = 14)
# 	# 		)

# 	# 	ggsave(biplot, filename = paste0(this_out_dir, '/Biplot - Software.png'), width = 8, height = 5, dpi = 600, bg = 'white')

# 	# ### software by developers
# 	# ##########################

# 	# 	p <- tests$permanova_p[tests$nice == 'Software use by developers']
# 	# 	if (p >= 0.001) p <- roundTo(p, 0.001)

# 	# 	fields <- load_rast_fields(species_focal = species_focal)
# 	# 	y <- rast_fields$team_code
# 	# 	y[y %in% c('E', 'B', 'C', 'H', 'D')] <- 'Yes'
# 	# 	y[y != 'Yes'] <- 'No'

# 	# 	scores$software_by_developers <- y

# 	# 	# cluster
# 	# 	cluster <- 'software_by_developers'
# 	# 	polys <- data.table()
# 	# 	unique_clusters <- unique(scores[[cluster]])
# 	# 	for (i in seq_along(unique_clusters)) {
		
# 	# 		cluster_name <- unique_clusters[i]
# 	# 		pts <- scores[scores[ , cluster, drop = TRUE] == cluster_name, c('PC1', 'PC2')]
# 	# 		hull_indices <- chull(pts)
# 	# 		hull_pts <- pts[hull_indices, ]
# 	# 		hull_pts <- rbind(hull_pts, hull_pts[1, , drop = FALSE])
# 	# 		hull_pts$cluster <- cluster_name
# 	# 		polys <- rbind(polys, hull_pts)
		
# 	# 	}

# 	# 	biplot <- base_biplot +
# 	# 		geom_point(
# 	# 			data = scores, aes(x = PC1, y = PC2, fill = software_by_developers, shape = software_by_developers), 
# 	# 			size = 5
# 	# 		) +
# 	# 		scale_shape_manual(values = c(21, 22)) +
# 	# 		geom_polygon(data = polys, aes(x = PC1, y = PC2, fill = cluster), alpha = 0.4, color = 'black') +
# 	# 		annotate(
# 	# 			'text', x = -Inf, y = -Inf, label = paste0('italic(P) == ', sprintf('%.5f', p)),
# 	# 			parse = TRUE, hjust = -0.3, vjust = -1, size = 7, fontface = 'bold'
# 	# 		) +
# 	# 		coord_fixed() +
# 	# 		theme_minimal() +
# 	# 		theme(
# 	# 			legend.position = 'none',
# 	# 			axis.title = element_text(size = 20),
# 	# 			axis.text = element_text(size = 14)
# 	# 		)

# 	# 	ggsave(biplot, filename = paste0(this_out_dir, '/Biplot - Software Used by Developers.png'), width = 8, height = 5, dpi = 600, bg = 'white')

# 	### workflow quality (standards)
# 	################################

# 		vi <- vis[[3]][vis[[3]]$attribute == '0_standards_mean_score', ]$proportion
# 		vi <- roundTo(vi, 0.001)
# 		vi <- sprintf('%.3f', vi)

# 		### add MEAN ODMAP score and MINIMUM SCORE ACROSS CATEGORIES to fields
# 		odmap <- readRDS('./Outputs Shared Anonymized/ODMAP Scoring Anonymized.rds')

# 		y <- odmap[['means']]

# 		y <- y[grepl(y$species, pattern = species_full)]
# 		criteria <- c(paste0(1, LETTERS[1:5]), paste0(2, LETTERS[1:3]), paste0(3, LETTERS[1:4]), paste0(4, LETTERS[1:3]))
# 		odmap_scores <- y[ , ..criteria]
		
# 		odmap_means <- rowMeans(odmap_scores)
# 		odmap_mins <- apply(odmap_scores, 1, max) # taking max bc 1 = gold, 0 = deficient

# 		odmap_means <- 4 - odmap_means # reverse ranks so 4 = gold, 0 = deficient
# 		odmap_mins <- 4 - odmap_mins # reverse ranks so 4 = gold, 0 = deficient

# 		team_codes <- odmap$means$team_code[odmap$means$species == species_full]
# 		names(odmap_means) <- team_codes
# 		names(odmap_mins) <- team_codes

# 		fields <- load_team_fields(species_focal = species_focal)
# 		fields$odmap_mean <- odmap_means[match(fields$team_code, team_codes)]
# 		fields$odmap_min <- odmap_mins[match(fields$team_code, team_codes)]

# 		index <- match(scores$team, fields$team)
# 		scores$odmap_mean <- fields$odmap_mean[index]

# 		cluster <- 'odmap_mean'
# 		biplot <- base_biplot +
# 			geom_point(data = scores, aes(x = PC1, y = PC2, fill = .data[[cluster]]), pch = 21, alpha = 0.7, size = 5) +
# 			scale_fill_viridis_c(option = 'magma') +
# 			annotate(
# 				'text', x = Inf, y = -Inf, label = paste0('VI: ', vi), 
# 				parse = FALSE, hjust = 1, vjust = -0.5, size = 7
# 			) +
# 			coord_fixed() +
# 			theme_minimal() +
# 			theme(
# 				legend.position = 'none',
# 				axis.title = element_text(size = 20),
# 				axis.text = element_text(size = 14)
# 			)

# 		ggsave(biplot, filename = paste0(this_out_dir, '/Biplot - Workflow Quality (Mean Rank).png'), width = 8, height = 5, dpi = 600, bg = 'white')

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

	### non-presence type
	#####################

		title <- paste0(letters[length(biplots) + 2], ') Non-presence type')

		vi <- vis[[3]][vis[[3]]$attribute == '2_non_presence_type', ]$proportion
		vi <- roundTo(vi, 0.001)
		vi <- sprintf('%.3f', vi)

		fields <- load_team_fields(species_focal = species_focal)
		scores$nonpres_type <- NA_character_

		for (i in 1:nrow(scores)) {

			team <- scores$team[i]
			y <- if (fields$nonpres_type_background[fields$team == team] == 1) {
				'Background Sites'
			} else if (fields$nonpres_type_pseudoabsence[fields$team == team] == 1) {
				'Pseudoabsences'
			} else if (fields$nonpres_type_target_background[fields$team == team] == 1) {
				'Target Background'
			} else if (fields$nonpres_type_unclear_no_response[fields$team == team] == 1) {
				'Unclear/No Response'
			}
			scores$nonpres_type[i] <- y

		}

		# cluster
		cluster <- 'nonpres_type'
		polys <- data.table()
		unique_clusters <- unique(scores[[cluster]])
		for (i in seq_along(unique_clusters)) {
		
			cluster_name <- unique_clusters[i]
			pts <- scores[scores[ , cluster, drop = TRUE] == cluster_name, c('PC1', 'PC2')]
			hull_indices <- chull(pts)
			hull_pts <- pts[hull_indices, ]
			hull_pts <- rbind(hull_pts, hull_pts[1, , drop = FALSE])
			hull_pts$cluster <- cluster_name
			polys <- rbind(polys, hull_pts)
		
		}

		biplots[[length(biplots) + 1]] <- base_biplot +
			geom_polygon(data = polys, aes(x = PC1, y = PC2, fill = cluster), alpha = 0.4, color = 'black') +
			geom_point(data = scores, aes(x = PC1, y = PC2, fill = .data[[cluster]], shape = .data[[cluster]]), alpha = 0.7, size = point_size) +
			scale_shape_manual(values = c(21, 22, 24, 25)) +
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

		biplots[[length(biplots) + 1]] <- base_biplot +
			geom_point(
				data = scores, aes(x = PC1, y = PC2, color = team, fill = team),
				size = point_size,
				pch = 16
			) +
			geom_polygon(data = polys, aes(x = PC1, y = PC2, fill = team), alpha = 0.4, color = 'gray30') +
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


say('DONE', level = 1)
