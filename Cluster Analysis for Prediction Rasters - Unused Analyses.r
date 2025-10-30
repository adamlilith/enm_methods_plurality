### SDM METHODOLOGICAL PLURALITY
### Anna Thonis, Nikki Calavari, Uzma Ashraf, Toni Lyn Morelli, and Adam B. Smith*
### * adam.smith@mobot.org | Missouri Botanical Garden | 2025-02
###
### This code applies and plots a regression tree of SDM predictions, one per focal_species, based on decisions each team made.
###
### source('C:/Kaji/Research Group/ENMs - Plurality of Modeling Workflows (Anna Thonis)/Code/abcxyz.r')
###
### CONTENTS ###

# say('############################################')
# say('### PCA on predictions: biplots and maps ###')
# say('############################################')

# 	### Create biplot of PC predictions with vector map of teams overlain. Create maps of PC predictions.

# 	# modeling decisions: fields to use in analysis
# 	fields <- load_fields(focal_species)
# 	teams <- fields$team

# 	# predictions
# 	wide <- load_predictions(focal_species, period = 'present')
# 	spatial <- vect(wide, geom = c('longitude', 'latitude'), crs = getCRS('WGS84'))

# 	wide <- wide[ , ..teams]

# 	# PCA
# 	pca <- prcomp(wide, center = TRUE, scale. = TRUE)

# 	spatial$pc1 <- pca$x[ , 'PC1']
# 	spatial$pc2 <- pca$x[ , 'PC2']

# 	# map of PC scores... what does it mean?
# 	extent <- buffer(spatial, 50000)
# 	extent <- ext(extent)
# 	extent <- as.vector(extent)

# 	# download world country boundaries as a spatial vector
# 	world <- ne_countries(scale = 'medium', returnclass = 'sv')

# 	map_pc1 <- ggplot() +
# 		layer_spatial(world, fill = 'gray80') +
# 		layer_spatial(spatial, aes(color = pc1), size = 0.85) +
# 		scale_color_viridis(name = 'PC1', option = 'magma') +
# 		layer_spatial(world, fill = NA, color = 'gray20') +
# 		xlim(extent[1], extent[2]) + ylim(extent[3], extent[4]) +
# 		ggtitle('C) PC1')

# 	map_pc2 <- ggplot() +
# 		layer_spatial(world, fill = 'gray80') +
# 		layer_spatial(spatial, aes(color = pc2), size = 0.85) +
# 		scale_color_viridis(name = 'PC2', option = 'magma') +
# 		layer_spatial(world, fill = NA, color = 'gray20') +
# 		xlim(extent[1], extent[2]) + ylim(extent[3], extent[4]) +
# 		ggtitle('D) PC2')

# 	maps <- plot_grid(map_pc1, map_pc2, align = 'h', ncol = 2)

# 	# PC axis vs PC axis
# 	scores <- as.data.table(pca$x)
# 	scores[ , mean := rowMeans(wide)]
# 	scores[ , sd := apply(wide, 1, sd)]
# 	xy <- crds(spatial)
# 	colnames(xy) <- c('longitude', 'latitude')
# 	scores <- cbind(scores, xy)

# 	loadings <- as.data.frame(pca$rotation)
# 	scale_factor <- max(abs(scores[ , 1:3])) / max(abs(loadings[ , 1:3]))

# 	loadings_scaled <- loadings
# 	loadings_scaled[ , 1:3] <- loadings_scaled[ , 1:3] * scale_factor
# 	loadings_scaled$team <- rownames(loadings_scaled)
# 	if (anonymize) loadings_scaled$team <- anonymize_teams(loadings_scaled$team, focal_species = focal_species)

# 	pc1_lab <- paste0('PC1 (', round(100 * pca$sdev[1]^2 / sum(pca$sdev^2), 2), '%)')
# 	pc2_lab <- paste0('PC2 (', round(100 * pca$sdev[2]^2 / sum(pca$sdev^2), 2), '%)')
# 	pc3_lab <- paste0('PC3 (', round(100 * pca$sdev[3]^2 / sum(pca$sdev^2), 2), '%)')

# 	scores <- scores[sample(nrow(scores))]
# 	library(viridis) # for magma palette

# 	# pc_1_2 <- ggplot(scores, aes(x = PC1, y = PC2, color = mean)) +
# 	pc_1_2 <- ggplot(scores, aes(x = PC1, y = PC2, color = longitude)) +
# 		geom_point(alpha = 0.05, pch = 16) +
# 		# scale_color_viridis(option = 'magma', name = 'Mean\npredicted\nvalue') +
# 		scale_color_viridis(option = 'magma', name = 'Longitude') +
# 		geom_segment(data = loadings_scaled,
# 			aes(x = 0, y = 0, xend = PC1, yend = PC2),
# 			arrow = arrow(length = unit(0.2, 'cm')),
# 			color = 'gray30'
# 		) +
# 		geom_text(data = loadings_scaled,
# 			aes(x = PC1, y = PC2, label = team),
# 			color = 'black',
# 			hjust = 1.1, vjust = 1.1
# 		) +
# 		xlab(pc1_lab) + ylab(pc2_lab) +
# 		ggtitle('A) PC1 vs. PC2')

# 	# pc_1_3 <- ggplot(scores, aes(x = PC1, y = PC3, color = mean)) +
# 	pc_1_3 <- ggplot(scores, aes(x = PC1, y = PC3, color = longitude)) +
# 		geom_point(alpha = 0.05, pch = 16) +
# 		scale_color_viridis(option = 'magma', name = 'Longitude') +
# 		geom_segment(data = loadings_scaled,
# 			aes(x = 0, y = 0, xend = PC1, yend = PC3),
# 			arrow = arrow(length = unit(0.2, 'cm')),
# 			color = 'gray30'
# 		) +
# 		geom_text(data = loadings_scaled,
# 			aes(x = PC1, y = PC3, label = team),
# 			color = 'black',
# 			hjust = 1.1, vjust = 1.1
# 		) +
# 		xlab(pc1_lab) + ylab(pc3_lab) +
# 		ggtitle('B) PC1 vs. PC3')

# 	pcs <- plot_grid(pc_1_2, pc_1_3, align = 'h', ncol = 2)

# 	combo <- plot_grid(pcs, maps, nrow = 2)
# 	ggsave(combo, filename = paste0('./Analysis/', focal_species, ' PCA Maps & Biplots on Present-day Predictions', ifelse(anonymize, ' Anonymized', ''), '.png'), width = 16, height = 16, dpi = 600, bg = 'white')


# # # say('#####################################################')
# # # say('### CART analysis of SDM predictions (unfinished) ###')
# # # say('#####################################################')

# # # 	# modeling decisions: fields to use in analysis
# # # 	fields <- load_fields(focal_species)

# # # 		### load each time period's predictions and cbind() them all
# # # 		wide_present <- load_predictions(focal_species, period = 'present', subset_teams = TRUE, retain_coords = TRUE)
# # # 		wide_mid <- load_predictions(focal_species, period = 'mid', subset_teams = TRUE, retain_coords = TRUE)
# # # 		wide_late <- load_predictions(focal_species, period = 'late', subset_teams = TRUE, retain_coords = TRUE)

# # # 		coords_present <- wide_present[ , c('longitude', 'latitude')]
# # # 		coords_mid <- wide_mid[ , c('longitude', 'latitude')]
# # # 		coords_late <- wide_late[ , c('longitude', 'latitude')]

# # # 		# find the set of coordinates in common across all sets
# # # 		coords_common <- Reduce(
# # # 			function(x, y) merge(x, y, by = c('longitude', 'latitude')),
# # # 			list(coords_present, coords_mid, coords_late)
# # # 		)

# # # 		wide_present <- merge(coords_common, wide_present, by = c('longitude', 'latitude'))
# # # 		wide_mid <- merge(coords_common, wide_mid, by = c('longitude', 'latitude'))
# # # 		wide_late <- merge(coords_common, wide_late, by = c('longitude', 'latitude'))

# # # 		removes <- c('longitude', 'latitude')
# # # 		wide_present <- wide_present[ , !names(wide_present) %in% removes, with = FALSE]
# # # 		wide_mid <- wide_mid[ , !names(wide_mid) %in% removes, with = FALSE]
# # # 		wide_late <- wide_late[ , !names(wide_late) %in% removes, with = FALSE]

# # # 		names(wide_present) <- paste(names(wide_present), 'present')
# # # 		names(wide_mid) <- paste(names(wide_mid), 'mid')
# # # 		names(wide_late) <- paste(names(wide_late), 'late')

# # # 		wides <- cbind(wide_present, wide_mid, wide_late)
# # # 		long <- wide_to_long(wides)
# # # 		long[ , team := factor(team)]

# # # 		long[ , team_base := team]
# # # 		long[ , team_base := gsub(long$team_base, pattern = ' present', replacement = '')]
# # # 		long[ , team_base := gsub(long$team_base, pattern = ' mid', replacement = '')]
# # # 		long[ , team_base := gsub(long$team_base, pattern = ' late', replacement = '')]
# # # 		long[ , team_base := gsub(long$team_base, pattern = ' 1', replacement = '')]
# # # 		long[ , team_base := gsub(long$team_base, pattern = ' 2', replacement = '')]
# # # 		long[ , team_base := gsub(long$team_base, pattern = ' 3', replacement = '')]
# # # 		long[ , team_base := gsub(long$team_base, pattern = ' 4', replacement = '')]

# # # 		long[ , time_period := NA_character_]
# # # 		long$time_period[grepl(long$team, pattern = ' present')] <- 'Present'
# # # 		long$time_period[grepl(long$team, pattern = ' mid')] <- 'Mid-century'
# # # 		long$time_period[grepl(long$team, pattern = ' late')] <- 'Late century'
# # # 		long[ , time_period := factor(time_period)]

# # # 	### match workflow attributes to new columns

# # # 		# inserts the predictor variable into the "long" data.table
# # # 		match_y <- function(y, long, resp) {

# # # 			if (is.character(y)) y[is.na(y)] <- 'U/NR'

# # # 			y <- y[match(long$team_base, names(y))]
# # # 			long[ , DUMMY := y]
# # # 			if (is.character(y)) long[ , DUMMY := factor(DUMMY)]
# # # 			names(long)[names(long) == 'DUMMY'] <- resp
# # # 			long

# # # 		}

# # # 		# PCA cluster

# # # 			y <- rep(NA_integer_, nrow(long))

# # # 			clusts <- readRDS(paste0('./Analysis/', tolower(focal_species), '_clusters_of_teams_present.rds'))

# # # 		# prediction type

# # # 			resp <- 'prediction_type'

# # # 			y1 <- as.numeric(fields$prediction_type_continuous)
# # # 			y2 <- as.numeric(fields$prediction_type_thresholded)
# # # 			y <- rep(NA_character_, nrow(fields))
# # # 			y[y1 == 1] <- 'Continuous'
# # # 			y[y2 == 1] <- 'Thresholded'

# # # 			names(y) <- fields$team
# # # 			long <- match_y(y = y, long = long, resp = resp)

# # # 		# extrapolation

# # # 			resp <- 'managed_extrapolation'

# # # 			field_names <- c('extrapolation_clamping_masking_clipping', 'extrapolation_exdet', 'extrapolation_mess', 'extrapolation_shape', 'extrapolation_area_of_applicability', 'extrapolation_response_curve_inspection', 'extrapolation_kissmig')

# # # 			y <- fields[ , ..field_names]
# # # 			y <- y[ , lapply(.SD, as.numeric)]
# # # 			y <- rowSums(y)
# # # 			y <- as.numeric(y > 0)
# # # 			y[y == 1] <- 'Managed'
# # # 			y[y == 0] <- 'Not managed'

# # # 			names(y) <- fields$team
# # # 			long <- match_y(y = y, long = long, resp = resp)

# # # 		# number of occurrences

# # # 			resp <- 'number_of_occurrences'

# # # 			y <- fields$num_occurrences_minimum

# # # 			names(y) <- fields$team
# # # 			long <- match_y(y = y, long = long, resp = resp)

# # # 		# collinearity

# # # 			resp <- 'managed_collinearity'

# # # 			field_names <- c('collinearity_pca', 'collinearity_correlation', 'collinearity_vif', 'collinearity_other_method')

# # # 			y <- fields[ , ..field_names]
# # # 			y <- y[ , lapply(.SD, as.numeric)]
# # # 			y <- rowSums(y)
# # # 			y <- as.numeric(y > 0)
# # # 			y[y == 0] <- 'Not managed'
# # # 			y[y == 1] <- 'Managed'

# # # 			names(y) <- fields$team
# # # 			long <- match_y(y = y, long = long, resp = resp)

# # # 		# number of climate predictors

# # # 			resp <- 'predictors_climate_num_total'

# # # 			y <- fields$predictors_climate_num_total

# # # 			names(y) <- fields$team
# # # 			long <- match_y(y = y, long = long, resp = resp)

# # # 		# number of non-climate predictors

# # # 			resp <- 'predictors_nonclimate_num'

# # # 			y <- fields$predictors_nonclimate_num

# # # 			names(y) <- fields$team
# # # 			long <- match_y(y = y, long = long, resp = resp)

# # # 		# total number of predictors

# # # 			resp <- 'predictors_climate_nonclimate_num_total'

# # # 			y <- fields$predictors_climate_nonclimate_num_total

# # # 			names(y) <- fields$team
# # # 			long <- match_y(y = y, long = long, resp = resp)

# # # 		# climate data source

# # # 			resp <- 'climate_data_source'

# # # 			y1 <- as.numeric(fields$predictors_climate_source_worldclim)
# # # 			y2 <- as.numeric(fields$predictors_climate_source_chelsa)
# # # 			y <- rep(NA_character_, nrow(fields))
# # # 			y[y1 == 1] <- 'WorldClim'
# # # 			y[y2 == 1] <- 'CHELSA'

# # # 			names(y) <- fields$team
# # # 			long <- match_y(y = y, long = long, resp = resp)

# # # 		# modeling software: ENMeval

# # # 			resp <- 'enmeval_wallace'
			
# # # 			enmeval <- as.numeric(fields$modeling_software_enmeval)
# # # 			wallace <- as.numeric(fields$modeling_software_wallace)

# # # 			y <- rep('No', nrow(fields))
# # # 			y[enmeval == 1] <- 'Yes'
# # # 			y[wallace == 1] <- 'Yes'

# # # 			names(y) <- fields$team
# # # 			long <- match_y(y = y, long = long, resp = resp)

# # # 		# modeling software: BIOMOD2

# # # 			resp <- 'biomod2'
			
# # # 			y <- as.numeric(fields$modeling_software_biomod2)
# # # 			y[y == 1] <- 'Yes'
# # # 			y[y == 0] <- 'No'
# # # 			names(y) <- fields$team
# # # 			long <- match_y(y = y, long = long, resp = resp)

# # # 		# model ensemble

# # # 			resp <- 'algo_ensemble'

# # # 			y <- fields$algo_ensemble
# # # 			y[y == 0] <- 'No'
# # # 			y[y == 1] <- 'Yes'

# # # 			names(y) <- fields$team
# # # 			long <- match_y(y = y, long = long, resp = resp)

# # # 		# number of models in ensemble

# # # 			resp <- 'algo_ensemble_num_models'

# # # 			y <- fields$algo_ensemble_number_of_models

# # # 			names(y) <- fields$team
# # # 			long <- match_y(y = y, long = long, resp = resp)

# # # 		# used MaxEnt/MaxNet

# # # 			resp <- 'algo_maxent_maxnet'
# # # 			y <- rep(NA_character_, nrow(fields))
# # # 			y[as.numeric(fields$algo_maxent) == 0 & as.numeric(fields$algo_maxnet) == 0] <- 'No'
# # # 			y[fields$algo_maxent == 1] <- 'Yes'
# # # 			y[fields$algo_maxnet == 1] <- 'Yes'

# # # 			names(y) <- fields$team
# # # 			long <- match_y(y = y, long = long, resp = resp)

# # # 		# bias correction

# # # 			resp <- 'bias_correction'

# # # 			spatial <- fields$bias_correction_spatial_thinning
# # # 			env <- fields$bias_correction_environmental_thinning
# # # 			target <- fields$bias_correction_target_background
# # # 			nonrand <- fields$bias_correction_nonrandom_background
# # # 			none <- fields$bias_correction_none
# # # 			unclear <- fields$bias_correction_unclear_no_response
			
# # # 			y <- rep(NA_character_, nrow(fields))

# # # 			# only doing software used by 5 or more teams
# # # 			for (i in seq_along(y)) {
			
# # # 				if (spatial[i] == 1) {
# # # 					y[i] <- 'Spatial thinning'
# # # 				} else if (env[i] == 1) {
# # # 					y[i] <- 'Env. thinning'
# # # 				} else if (target[i]  == 1) {
# # # 					y[i] <- 'Target BG'
# # # 				} else if (nonrand[i]  == 1) {
# # # 					y[i] <- 'Non-random BG'
# # # 				} else if (none[i]  == 1) {
# # # 					y[i] <- 'None (explicit)'
# # # 				}

# # # 			}
		
# # # 			names(y) <- fields$team
# # # 			long <- match_y(y = y, long = long, resp = resp)

# # # 		# type of non-presences

# # # 			resp <- 'nonpres_type'

# # # 			y1 <- as.numeric(fields$nonpres_type_background)
# # # 			y2 <- as.numeric(fields$nonpres_type_pseudoabsence)
# # # 			y <- rep(NA_character_, nrow(fields))
# # # 			y[y1 == 1] <- 'Background'
# # # 			y[y2 == 1] <- 'Pseudoabsence'

# # # 			names(y) <- fields$team
# # # 			long <- match_y(y = y, long = long, resp = resp)
			
# # # 		# type of calibration region

# # # 			resp <- 'calibration_region'

# # # 			y <- rep(NA, nrow(fields))
# # # 			y[fields$boundary_rectangle == 1] <- 'Bounding box'
# # # 			y[fields$boundary_natural == 1] <- 'Ecoregions'
# # # 			y[fields$boundary_convex_hull == 1] <- 'Convex hull'
# # # 			y[fields$boundary_range_map == 1] <- 'Range map + buffer'
# # # 			y[fields$boundary_political == 1] <- 'Political'
# # # 			y[fields$boundary_buffer_around_occurrences == 1] <- 'Buffer a/r occs.'
# # # 			y[fields$boundary_unclear_not_reported == 1] <- 'U/NR'

# # # 			names(y) <- fields$team
# # # 			long <- match_y(y = y, long = long, resp = resp)

# # # 		# calibration extent

# # # 			resp <- 'calibration_region_extent'

# # # 			y <- fields$extent_calibration_sans_water_km2
# # # 			y <- as.numeric(y)
			
# # # 			names(y) <- fields$team
# # # 			long <- match_y(y = y, long = long, resp = resp)

# # # 		# spatial resolution

# # # 			resp <- 'spatial_resolution'

# # # # 			y <- fields$res_mean_cell_size_km2
# # # 			y <- fields$res_arcmin
			
# # # 			names(y) <- fields$team
# # # 			long <- match_y(y = y, long = long, resp = resp)

# # # long <- long[sample(nrow(long), round(0.05 * nrow(long)))]

# # # library(rpart)
# # # library(rpart.plot)

# # # 	form <- prediction ~
# # # 		prediction_type +
# # # 		time_period +
# # # 		number_of_occurrences +
# # # 		managed_extrapolation
# # # 		# managed_collinearity +
# # # 		# predictors_climate_num_total +
# # # 		# predictors_nonclimate_num +
# # # 		# predictors_climate_nonclimate_num_total +
# # # 		# climate_data_source +
# # # 		# enmeval_wallace +
# # # 		# biomod2 +
# # # 		# algo_ensemble +
# # # 		# algo_ensemble_num_models +
# # # 		# algo_maxent_maxnet +
# # # 		# calibration_region_extent +
# # # 		# spatial_resolution

# # # 	model <- rpart(form, data = long)

# # # 	# model <- tree(prediction ~ prediction_type + time_period + number_of_occurrences, data = long)
# # # 	# plot(model)
# # # 	# text(model, all = TRUE, cex = 1)

# # # 	pretty_names <- c(
# # # 		'prediction_type' = 'Prediction type',
# # # 		'time_period' = 'Time period',
# # # 		'number_of_occurrences' = 'Sample size',
# # # 		'managed_extrapolation' = 'Managed extrapolation',
# # # 		'managed_collinearity' = 'Managed collinearity'
# # # 		# 'predictors_climate_num_total' = 'Number of climate predictors',
# # # 		# 'predictors_nonclimate_num' = 'Number of non-climate predictors',
# # # 		# 'predictors_climate_nonclimate_num_total' = 'Number of predictors',
# # # 		# 'climate_data_source' = 'Climate data source',
# # # 		# 'enmeval_wallace' = 'ENMeval/Wallace',
# # # 		# 'biomod2' = 'BIOMOD2',
# # # 		# 'algo_ensemble' = 'SDM ensemble',
# # # 		# 'algo_ensemble_num_models' = 'Number of ensemble models',
# # # 		# 'algo_maxent_maxnet' = 'Used MaxEnt/Net',
# # # 		# 'calibration_region_extent' = 'Calibration extent',
# # # 		# 'spatial_resolution' = 'Spatial resolution'
# # # 	)

# # # 	terms_obj <- terms(model)
# # # 	attr(terms_obj, 'term.labels') <- unname(pretty_names[attr(terms_obj, 'term.labels')])
# # # 	model$terms <- terms_obj

# # # 	model$frame$var <- ifelse(
# # # 		model$frame$var %in% names(pretty_names),
# # # 		pretty_names[model$frame$var],
# # # 		model$frame$var
# # # 	)

# # # 	rpart.plot(model, box.palette = 'Blues', tweak = 1.2, fallen.leaves = TRUE)
