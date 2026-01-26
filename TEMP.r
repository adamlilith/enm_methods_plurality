say('###########################################################################')
say('### test for associations between team clusters and workflow attributes ###')
say('###########################################################################')

	# Implements a *single* Kruskal-Wallis or contingency table analysis (with Fisher's exact test and simulate P values)-- according to covariate type--with cluster ID as the predictor.
	#
	# Count number of each unique value in y
	# fields		data.table with workflow attributes
	# nice      	Nice name for association (e.g., 'Occurrences: Number' or 'Occurrences: Source)
	# period		'present, 'mid', 'late': time period of predictions
	# results		data.table with results so far
	# y				predictor vector (factor or numeric)
	# test			type of test: 'contingency' (table) for categorical y, or 'kw' (Kruskal-Wallis) for numeric y
	analyze_association <- function(fields, nice, period, results, y, test) {

		# remove missing values from fields and from response
		clusts <- fields$cluster

		missing <- is.na(y)
		if (any(missing)) {
		
			clusts <- clusts[!missing]
			y <- y[!missing]

		}

		if (test == 'contingency') {
			
			x_table <- table(clusts, y)

			y_sufficient <- (sum(y) > 1 & sum(1 - y) > 1) | length(unique(y)) > 2

			### Fisher's exact test with simulate P value for non-2x2 tables: categorical vs categorical

			if (y_sufficient) {

				x_table <- table(clusts, y)
				# chi <- chisq.test(x_table)
				fisher <- fisher.test(x_table, simulate.p.value = TRUE, B = 100000)

				results <- rbind(
					results,
					data.table(
						nice = nice,
						focal_species = focal_species,
						period = period,
						test = 'Contingency',
						kw_chi_sq = NA_real_,
						p = fisher$p.value,
						sig = ifelse(fisher$p.value <= 0.05, '*', '.')
					)
				)

			} else {
			
				results <- rbind(
					results,
					data.table(
						nice = nice,
						focal_species = focal_species,
						period = period,
						test = NA,
						kw_chi_sq = NA_real_,
						p = NA,
						sig = 'Insufficient number of categories'
					)
				)
			
			}

		} else if (test == 'kw') {
			
			### Kruskal-Wallis (numeric vs categorical): categorical vs numeric (non-normal)

			data <- data.table(y = y, cluster = clusts)
			kw <- kruskal.test(y ~ cluster, data = data)

			results <- rbind(
				results,
				data.table(
					nice = nice,
					focal_species = focal_species,
					period = period,
					test = 'KW',
					kw_chi_sq = kw$statistic,
					p = kw$p.value,
					sig = ifelse(kw$p.value <= 0.05, '*', '.')
				)
			)

		}
		results

	}

	# Test for possible associations between all sets of workflow attributes and cluster identity to see what (if anything) explains cluster identity
	#
	# fields 	data.table with workflow attributed
	# period	'present' (present day), 'future' (mid- or later-century), or 'difference' (future - present)
	#
	# Returns a data.table with tests and their p-values
	analyze_associations <- function(fields, period) {

		results <- data.table()

		### thresholded predictions

			y <- as.numeric(fields$prediction_type_thresholded)
			
			test <- 'contingency'

			nice <- 'Prediction Type (thresholded/continuous)'
			results <- analyze_association(fields = fields, period = period, nice = nice, results = results, y = y, test = test)

		### number of occurrences

			y <- as.numeric(fields$num_occurrences_minimum)
			test <- 'kw'

			nice <- 'Occurrences: Number of occurrences'
			results <- analyze_association(fields = fields, period = period, nice = nice, results = results, y = y, test = test)

		### collinearity: managed at all

			field_names <- c('collinearity_pca', 'collinearity_correlation', 'collinearity_vif', 'collinearity_other_method')
			y <- fields[ , ..field_names]
			y <- y[ , lapply(.SD, as.numeric)]

			y <- rowSums(y)
			y <- as.numeric(y > 0)
			test <- 'contingency'

			nice <- 'Collinearity: Explicitly Managed'
			results <- analyze_association(fields = fields, period = period, nice = nice, results = results, y = y, test = test)

		### collinearity: method

			field_names <- c('collinearity_pca', 'collinearity_correlation', 'collinearity_vif', 'collinearity_other_method')
			for (i in seq_along(field_names)) {

				field_name <- field_names[i]
				y <- fields[[field_name]]
				y <- as.numeric(y)

				test <- 'contingency'

				nice <- if (grepl(field_name, pattern = 'pca')) {
					'Collinearity: Used PC axes as predictors'
				} else if (grepl(field_name, pattern = 'correlation')) {
					'Collinearity: Screened using correlation'
				} else if (grepl(field_name, pattern = 'vif')) {
					'Collinearity: Screened using VIF'
				} else if (grepl(field_name, pattern = 'other_method')) {
					'Collinearity: Screened using other method'
				}

				results <- analyze_association(fields = fields, period = period, nice = nice, results = results, y = y, test = test)

			}

		# ### BIOCLIM predictors: identity

		# 	for (bio in 1:19) {

		# 		field_name <- paste0('predictors_climate_bio', bio)
		# 		y <- fields[[field_name]]
		# 		y <- as.numeric(y)

		# 		test <- 'contingency'

		# 		nice <- paste('Predictors: Used BIO', bio)
		# 		results <- analyze_association(fields = fields, period = period, nice = nice, results = results, y = y, test = test)

		# 	}

		# ### BIOCLIM predictors: number

		# 	field_name <- 'predictors_climate_num_bioclim'
		# 	y <- fields[[field_name]]
		# 	y <- as.numeric(y)

		# 	test <- 'kw'

		# 	nice <- 'Predictors: Number of BIOCLIM predictors'
		# 	results <- analyze_association(fields = fields, period = period, nice = nice, results = results, y = y, test = test)

		### climate predictors: number

			y <- fields$predictors_climate_num_total
			y <- as.numeric(y)

			test <- 'kw'

			nice <- 'Predictors: Total number of climate predictors'
			results <- analyze_association(fields = fields, period = period, nice = nice, results = results, y = y, test = test)

		### non-climate predictors: identity

			# field_names <- c('predictors_nonclimate_elevation', 'predictors_nonclimate_aspect', 'predictors_nonclimate_forest_cover', 'predictors_nonclimate_soil', 'predictors_nonclimate_npp_ndvi', 'predictors_nonclimate_terrain_ruggedness_position', 'predictors_nonclimate_land_cover', 'predictors_nonclimate_human_impact', 'predictors_nonclimate_other')

			# for (pred in seq_along(field_names)) {

			# 	field_name <- field_names[pred]
			# 	y <- fields[[field_name]]
			# 	y <- as.numeric(y)

			# 	test <- 'contingency'

			# 	nice <- if (grepl(field_name, pattern = 'elevation')) {
			# 		'Predictors: Used elevation'
			# 	} else if (grepl(field_name, pattern = 'aspect')) {
			# 		'Predictors: Used aspect'
			# 	} else if (grepl(field_name, pattern = 'forest_cover')) {
			# 		'Predictors: Used forest cover'
			# 	} else if (grepl(field_name, pattern = 'soil')) {
			# 		'Predictors: Used soil'
			# 	} else if (grepl(field_name, pattern = 'npp_ndvi')) {
			# 		'Predictors: Used NPP/NDVI'
			# 	} else if (grepl(field_name, pattern = 'terrain_ruggedness_position')) {
			# 		'Predictors: Used TRI/TPI'
			# 	} else if (grepl(field_name, pattern = 'land_cover')) {
			# 		'Predictors: Used LULC'
			# 	} else if (grepl(field_name, pattern = 'human_impact')) {
			# 		'Predictors: Used human impact predictor(s)'
			# 	} else if (grepl(field_name, pattern = 'other')) {
			# 		'Predictors: Used other non-climate predictor(s)'
			# 	}

			# 	results <- analyze_association(fields = fields, period = period, nice = nice, results = results, y = y, test = test)

			# }

		### non-climate: number of predictors

			y <- fields$predictors_nonclimate_num
			y <- as.numeric(y)

			test <- 'kw'

			nice <- 'Predictors: Number of non-climate predictors'
			results <- analyze_association(fields = fields, period = period, nice = nice, results = results, y = y, test = test)

		### source of climate predictors

			y <- fields$predictors_climate_source_worldclim
			y <- as.numeric(y)

			test <- 'contingency'

			nice <- 'Predictors: Climate data source'
			results <- analyze_association(fields = fields, period = period, nice = nice, results = results, y = y, test = test)

		### number of predictors

			y <- fields$predictors_climate_nonclimate_num_total
			y <- as.numeric(y)

			test <- 'kw'

			nice <- 'Predictors: Total number'
			results <- analyze_association(fields = fields, period = period, nice = nice, results = results, y = y, test = test)

		### modeling_software

			set_names <- c('modeling_software_enmeval', 'modeling_software_enmtools', 'modeling_software_wallace', 'modeling_software_biomod2', 'modeling_software_sabinansdm', 'modeling_software_sdm', 'modeling_software_miamaxent', 'modeling_software_enmsdmx', 'modeling_software_flexsdm', 'modeling_software_sdmtune', 'modeling_software_piecemeal')

			test <- 'contingency'

			for (i in seq_along(set_names)) {

				field_name <- set_names[i]
				y <- fields[[field_name]]
				y <- as.numeric(y)

				nice <- if (grepl(field_name, pattern = '_enmeval')) {
					'Software: Used ENMeval'
				} else if (grepl(field_name, pattern = '_enmtools')) {
					'Software: Used ENMTools'
				} else if (grepl(field_name, pattern = '_wallace')) {
					'Software: Used Wallace'
				} else if (grepl(field_name, pattern = '_biomod2')) {
					'Software: Used BIOMOD2'
				} else if (grepl(field_name, pattern = '_sabinansdm')) {
					'Software: Used SabinaNSDM'
				} else if (grepl(field_name, pattern = '_sdmtune')) {
					'Software: Used SDMtune'
				} else if (grepl(field_name, pattern = '_sdm')) {
					'Software: Used sdm'
				} else if (grepl(field_name, pattern = '_miamaxent')) {
					'Software: Used MIAmaxent'
				} else if (grepl(field_name, pattern = '_enmsdmx')) {
					'Software: Used enmSdmX'
				} else if (grepl(field_name, pattern = '_flexsdm')) {
					'Software: Used flexsdm'
				} else if (grepl(field_name, pattern = '_piecemeal')) {
					'Software: Used piecemeal R packages'
				}

				results <- analyze_association(fields = fields, period = period, nice = nice, results = results, y = y, test = test)

			}

		### algorithm: number

			y <- fields$algo_num
			y <- as.numeric(y)

			test <- 'kw'
			nice <- 'Algorithm: Number of algorithms'

			results <- analyze_association(fields = fields, period = period, nice = nice, results = results, y = y, test = test)

		### algorithm: used ensemble
		
			y <- fields$algo_ensemble
			y <- as.numeric(y)

			test <- 'contingency'
			nice <- 'Algorithm: Used ensemble'

			results <- analyze_association(fields = fields, period = period, nice = nice, results = results, y = y, test = test)

		### algorithm: number of algorithms used in ensemble (including 0)
		
			y <- fields$algo_ensemble_number_of_models
			y <- as.numeric(y)

			test <- 'kw'
			nice <- 'Algorithm: Number of algorithms in ensemble (inc. 0)'

			results <- analyze_association(fields = fields, period = period, nice = nice, results = results, y = y, test = test)

		### algorithm: identity
		
			field_names <- c('algo_maxent', 'algo_maxnet', 'algo_brt_gbm', 'algo_glm', 'algo_gam', 'algo_rf', 'algo_xgboost')

			for (i in seq_along(field_names)) {

				field_name <- field_names[i]
				y <- fields[[field_name]]
				y <- as.numeric(y)

				test <- 'contingency'

				nice <- if (grepl(field_name, pattern = 'maxent')) {
					'Algorithms: Used MaxEnt'
				} else if (grepl(field_name, pattern = 'maxnet')) {
					'Algorithms: Used MaxNet'
				} else if (grepl(field_name, pattern = 'brt_gbm')) {
					'Algorithms: Used BRTs/GBMs'
				} else if (grepl(field_name, pattern = 'glm')) {
					'Algorithms: Used GLM'
				} else if (grepl(field_name, pattern = 'gam')) {
					'Algorithms: Used GAM'
				} else if (grepl(field_name, pattern = 'rf')) {
					'Algorithms: Used RFs'
				} else if (grepl(field_name, pattern = 'mars')) {
					'Algorithms: Used MARS'
				} else if (grepl(field_name, pattern = 'fda')) {
					'Algorithms: Used FDA'
				} else if (grepl(field_name, pattern = 'xgboost')) {
					'Algorithms: Used XGBoost'
				}

				results <- analyze_association(fields = fields, period = period, nice = nice, results = results, y = y, test = test)

			}

		### bias correction: did any
			
			field_names <- c('bias_correction_spatial_thinning', 'bias_correction_environmental_thinning', 'bias_correction_target_background', 'bias_correction_nonrandom_background')
			
			y <- fields[ , ..field_names]
			y <- y[ , lapply(.SD, as.numeric)]
			y <- rowSums(y)
			y <- as.numeric(y > 0)

			test <- 'contingency'

			nice <- 'Bias correction: Implemented'
			results <- analyze_association(fields = fields, period = period, nice = nice, results = results, y = y, test = test)

		### bias correction: method

			field_names <- c('bias_correction_spatial_thinning', 'bias_correction_environmental_thinning', 'bias_correction_target_background', 'bias_correction_nonrandom_background')

			for (i in seq_along(field_names)) {

				field_name <- field_names[i]
				y <- fields[[field_name]]
				y <- as.numeric(y)

				test <- 'contingency'

				nice <- if (grepl(field_name, pattern = 'spatial_thinning')) {
					'Bias correction: Spatial thinning'
				} else if (grepl(field_name, pattern = 'environmental_thinning')) {
					'Bias correction: Environmental thinning'
				} else if (grepl(field_name, pattern = 'target_background')) {
					'Bias correction: Target background'
				} else if (grepl(field_name, pattern = 'nonrandom_background')) {
					'Bias correction: Non-random background'
				}

				results <- analyze_association(fields = fields, period = period, nice = nice, results = results, y = y, test = test)

			}
			
		# ### occurrence data: number of sources

		# 	field_names <- c('occurrence_data_gbif', 'occurrence_data_idigbio', 'occurrence_data_vertnet', 'occurrence_data_inaturalist', 'occurrence_data_publications')
		# 	y <- fields[ , ..field_names]
		# 	y <- y[ , lapply(.SD, as.numeric)]			
		# 	y <- rowSums(y)

		# 	test <- 'kw'
		# 	nice <- 'Occurrence data: Number of sources'

		# 	results <- analyze_association(fields = fields, period = period, nice = nice, results = results, y = y, test = test)

		# ### occurrence data: source
			
		# 	field_names <- c('occurrence_data_gbif', 'occurrence_data_idigbio', 'occurrence_data_vertnet', 'occurrence_data_inaturalist', 'occurrence_data_publications')

		# 	for (i in seq_along(field_names)) {

		# 		field_name <- field_names[i]
		# 		y <- fields[[field_name]]
		# 		y <- as.numeric(y)

		# 		test <- 'contingency'

		# 		nice <- if (grepl(field_name, pattern = 'gbif')) {
		# 			'Occurrence data: From GBIF'
		# 		} else if (grepl(field_name, pattern = 'idigbio')) {
		# 			'Occurrence data: From iDigBio'
		# 		} else if (grepl(field_name, pattern = 'vertnet')) {
		# 			'Occurrence data: From VertNet'
		# 		} else if (grepl(field_name, pattern = 'inaturalist')) {
		# 			'Occurrence data: From iNaturalist'
		# 		} else if (grepl(field_name, pattern = 'publications')) {
		# 			'Occurrence data: From publications'
		# 		}

		# 		results <- analyze_association(fields = fields, period = period, nice = nice, results = results, y = y, test = test)

		# 	}

		### non-presence type
		
			# non-presences: type
			field_names <- c('nonpres_type_background', 'nonpres_type_pseudoabsence', 'nonpres_type_target_background')

			y <- rep(NA_character_, nrow(fields))

			for (i in seq_along(field_names)) {

				field_name <- field_names[i]
				y <- fields[[field_name]]
				y <- as.numeric(y)

				test <- 'contingency'

				nice <- if (grepl(field_name, pattern = 'nonpres_type_background')) {
					'Non-presence type: Background sites'
				} else if (grepl(field_name, pattern = 'pseudoabsence')) {
					'Non-presence type: Pseudoabsences'
				} else if (grepl(field_name, pattern = 'target_background')) {
					'Non-presence type: Target background'
				}

				results <- analyze_association(fields = fields, period = period, nice = nice, results = results, y = y, test = test)

			}
			
		### calibration region boundary
		
			# non-presences: type
			field_names <- c('boundary_rectangle', 'boundary_natural', 'boundary_convex_hull', 'boundary_range_map', 'boundary_political', 'boundary_buffer_around_occurrences')

			for (i in seq_along(field_names)) {

				field_name <- field_names[i]
				y <- fields[[field_name]]
				y <- as.numeric(y)

				test <- 'contingency'

				nice <- if (grepl(field_name, pattern = 'rectangle')) {
					'Calibration region: Rectangle'
				} else if (grepl(field_name, pattern = 'natural')) {
					'Calibration region: Natural (e.g., ecoregions)'
				} else if (grepl(field_name, pattern = 'convex_hull')) {
					'Calibration region: Convex hull (possibly with buffer)'
				} else if (grepl(field_name, pattern = 'range_map')) {
					'Calibration region: Range map (possibly with buffer)'
				} else if (grepl(field_name, pattern = 'political')) {
					'Calibration region: Political'
				} else if (grepl(field_name, pattern = 'buffer_around_occurrences')) {
					'Calibration region: Buffer around occurrences'
				}

				results <- analyze_association(fields = fields, period = period, nice = nice, results = results, y = y, test = test)

			}

		### calibration region extent
			
			y <- as.numeric(fields$extent_calibration_sans_water_km2)

			test <- 'kw'
			nice <- 'Calibration extent: Area'

			results <- analyze_association(fields = fields, period = period, nice = nice, results = results, y = y, test = test)
		
		### spatial resolution: cell size
			
			y <- as.numeric(fields$res_mean_cell_size_km2)

			test <- 'kw'
			nice <- 'Spatial resolution: Cell size (km2)'

			results <- analyze_association(fields = fields, period = period, nice = nice, results = results, y = y, test = test)
		
		### taxonomy: accounted for subspecies
			
			y <- as.numeric(fields$taxonomy_accounted_for_subspecies)

			test <- 'contingency'
			nice <- 'Accounted for subspecies'

			results <- analyze_association(fields = fields, period = period, nice = nice, results = results, y = y, test = test)
		
		### ODMAP *mean* score

			y <- as.numeric(fields$odmap_mean)
			
			test <- 'kw'
			nice <- 'ODMAP: Mean score'

			results <- analyze_association(fields = fields, period = period, nice = nice, results = results, y = y, test = test)

		### ODMAP *minimum* of mean score

			y <- as.numeric(fields$odmap_min)
			
			test <- 'kw'
			nice <- 'ODMAP: Minimum score across criteria'

			results <- analyze_association(fields = fields, period = period, nice = nice, results = results, y = y, test = test)

		if (period != 'present') {
		
			### extrapolation: individual methods

				field_names <- c('extrapolation_clamping_masking_clipping', 'extrapolation_exdet', 'extrapolation_mess', 'extrapolation_shape', 'extrapolation_area_of_applicability', 'extrapolation_response_curve_inspection')

				for (i in seq_along(field_names)) {

					field_name <- field_names[i]
					y <- fields[[field_name]]
					y <- as.numeric(y)

					test <- 'contingency'

					nice <- if (grepl(field_name, pattern = 'clamping_masking_clipping')) {
						'Extrapolation: Clamping/masking/clipping'
					} else if (grepl(field_name, pattern = 'exdet')) {
						'Extrapolation: Exdet'
					} else if (grepl(field_name, pattern = 'mess')) {
						'Extrapolation: MESS'
					} else if (grepl(field_name, pattern = 'shape')) {
						'Extrapolation: SHAPE'
					} else if (grepl(field_name, pattern = 'area_of_applicability')) {
						'Extrapolation: Area of Applicability'
					} else if (grepl(field_name, pattern = 'response_curve_inspection')) {
						'Extrapolation: Response Curve Inspection'
					} else {
						NA
					}

					results <- analyze_association(fields = fields, period = period, nice = nice, results = results, y = y, test = test)

				}

			### extrapolation: any method

				field_names <- c('extrapolation_clamping_masking_clipping', 'extrapolation_exdet', 'extrapolation_mess', 'extrapolation_shape', 'extrapolation_area_of_applicability', 'extrapolation_response_curve_inspection')
				
				y <- fields[ , ..field_names]
				y <- y[ , lapply(.SD, as.numeric)]
				y <- rowSums(y)
				y <- as.numeric(y > 0)
			
				test <- 'contingency'
				nice <- 'Managed Extrapolation (any method)'

				results <- analyze_association(fields = fields, period = period, nice = nice, results = results, y = y, test = test)

			### dynamic dispersal

				y <- as.numeric(fields$future_dispersal_dynamic)
			
				test <- 'contingency'
				nice <- 'Post-processing: Dispersal simulated'

				results <- analyze_association(fields = fields, period = period, nice = nice, results = results, y = y, test = test)

			### future: CMIP

				y <- as.numeric(fields$future_scenario_cmip)
				y[y == 'NA'] <- NA
				y <- as.numeric(y == 'RCP')
			
				test <- 'contingency'
				nice <- 'Future: CMIP'

				results <- analyze_association(fields = fields, period = period, nice = nice, results = results, y = y, test = test)

			### future: mid-century year

				if (period == 'mid') {

					y <- fields$future_scenario_midcentury_year
					y[y == 'NA'] <- NA
					y[y == '2041-2060'] <- '0'
					y[y == '2041-2070'] <- '1'
					y <- as.numeric(y)

					test <- 'contingency'
					nice <- 'Future: Mid-century period'

					results <- analyze_association(fields = fields, period = period, nice = nice, results = results, y = y, test = test)

				}

			### future: late-century year

				if (period == 'late') {

					y <- fields$future_scenario_latecentury_year
					y[y == 'NA'] <- NA
					y[y == '2061-2080'] <- '0'
					y[y == '2071-2100'] <- '1'
					y[y == '2081-2100'] <- '2'
					y <- as.numeric(y)

					test <- 'contingency'
					nice <- 'Future: Mid-century period'

					results <- analyze_association(fields = fields, period = period, nice = nice, results = results, y = y, test = test)

				}

		}

		results
		
	} # EOF

	fields <- load_fields(focal_species)

	### add clusters to fields
	clusters_present <- readRDS(paste0('./Analysis/', focal_species, '_clusters_of_teams_present.rds'))
	clusters_mid <- readRDS(paste0('./Analysis/', focal_species, '_clusters_of_teams_mid.rds'))
	clusters_late <- readRDS(paste0('./Analysis/', focal_species, '_clusters_of_teams_late.rds'))

	fields_present <- fields_mid <- fields_late <- fields

	# present
	fields_present[ , cluster := clusters_present[match(fields_present$team, names(clusters_present))]]

	# mid-century... we duplicate the team's row in the table for each duplicate raster
	team_versions_mid <- fread('./Analysis/team_versions_mid.csv')

	bases <- unique(team_versions_mid$base)
	for (i in seq_along(bases)) {
	
		base <- bases[i]
		row_index <- which(fields_mid$team == base)
		team <- fields_mid$team[row_index]
		n_versions <- sum(team_versions_mid$base == base)

		if (n_versions > 1) {
		
			this_row <- fields_mid[row_index]
			fields_mid$team[row_index] <- paste0(team, ' 1')

			for (j in 2:n_versions) {
				this_row_copy <- this_row
				this_row_copy$team <- paste(this_row_copy$team, j)
				fields_mid <- rbind(fields_mid, this_row_copy)
			}
		
		}	
	
	}

	fields_mid[ , cluster := clusters_mid[match(fields_mid$team, names(clusters_mid))]]
	# fields_mid <- fields_mid[!is.na(cluster)]

	# late century... we duplicate the team's row in the table for each duplicate raster
	team_versions_late <- fread('./Analysis/team_versions_late.csv')

	bases <- unique(team_versions_late$base)
	for (i in seq_along(bases)) {
	
		base <- bases[i]
		row_index <- which(fields_late$team == base)
		team <- fields_late$team[row_index]
		n_versions <- sum(team_versions_late$base == base)

		if (n_versions > 1) {
		
			this_row <- fields_late[row_index]
			fields_late$team[row_index] <- paste0(team, ' 1')

			for (j in 2:n_versions) {
				this_row_copy <- this_row
				this_row_copy$team <- paste(this_row_copy$team, j)
				fields_late <- rbind(fields_late, this_row_copy)
			}
		
		}	
	
	}

	fields_late[ , cluster := clusters_late[match(fields_late$team, names(clusters_late))]]
	fields_late <- fields_late[!is.na(cluster)]

	fields_present$cluster <- as.factor(fields_present$cluster)
	fields_mid$cluster <- as.factor(fields_mid$cluster)
	fields_late$cluster <- as.factor(fields_late$cluster)

	### add MEAN ODMAP score to fields
	odmap <- readRDS('./Analysis/Summary of Assessment of SDM Workflows by SDM Standards.rds')

	y <- odmap$means
	y <- y[grepl(y$species, pattern = focal_species)]
	criteria <- c(paste0(1, LETTERS[1:5]), paste0(2, LETTERS[1:3]), paste0(3, LETTERS[1:4]), paste0(4, LETTERS[1:3]))
	odmap_scores <- y[ , ..criteria]
	odmap_means <- rowMeans(odmap_scores)

	odmap_teams <- y$first_author
	odmap_teams[odmap_teams == 'JIMENEZ-VALVERDE'] <- 'Jiménez-Valverde'
	odmap_teams <- tolower(odmap_teams)
	names(odmap_means) <- odmap_teams

	teams <- fields_present$team
	names(odmap_means) <- teams[match(odmap_teams, tolower(teams))]

	fields_present$odmap_mean <- odmap_means[match(fields_present$team, names(odmap_means))]

	fields_mid[ , odmap_mean := NA_real_]
	for (i in seq_along(odmap_means)) {
		odmap_team <- names(odmap_means)[i]
		odmap_team <- tolower(odmap_team)
		index <- which(grepl(tolower(fields_mid$team), pattern = odmap_team))
		fields_mid$odmap_mean[index] <- odmap_means[i]
	}

	fields_late[ , odmap_mean := NA_real_]
	for (i in seq_along(odmap_means)) {
		odmap_team <- names(odmap_means)[i]
		odmap_team <- tolower(odmap_team)
		index <- which(grepl(tolower(fields_late$team), pattern = odmap_team))
		fields_late$odmap_mean[index] <- odmap_means[i]
	}

	### add MINIMUM of MEAN ODMAP score to fields
	y <- odmap$means
	y <- y[grepl(y$species, pattern = focal_species)]
	criteria <- c(paste0(1, LETTERS[1:5]), paste0(2, LETTERS[1:3]), paste0(3, LETTERS[1:4]), paste0(4, LETTERS[1:3]))
	odmap_scores <- y[ , ..criteria]
	odmap_mins <- apply(odmap_scores, 1, min)

	odmap_teams <- y$first_author
	odmap_teams[odmap_teams == 'JIMENEZ-VALVERDE'] <- 'Jiménez-Valverde'
	odmap_teams <- tolower(odmap_teams)
	names(odmap_mins) <- odmap_teams

	teams <- fields_present$team
	names(odmap_mins) <- teams[match(odmap_teams, tolower(teams))]

	fields_present$odmap_min <- odmap_mins[match(fields_present$team, names(odmap_mins))]

	fields_mid[ , odmap_min := NA_real_]
	for (i in seq_along(odmap_mins)) {
		odmap_team <- names(odmap_mins)[i]
		odmap_team <- tolower(odmap_team)
		index <- which(grepl(tolower(fields_mid$team), pattern = odmap_team))
		fields_mid$odmap_min[index] <- odmap_mins[i]
	}

	fields_late[ , odmap_min := NA_real_]
	for (i in seq_along(odmap_mins)) {
		odmap_team <- names(odmap_mins)[i]
		odmap_team <- tolower(odmap_team)
		index <- which(grepl(tolower(fields_late$team), pattern = odmap_team))
		fields_late$odmap_min[index] <- odmap_mins[i]
	}

	results_present <- analyze_associations(fields = fields_present, period = 'present')
	results_mid <- analyze_associations(fields = fields_mid, period = 'mid')
	results_late <- analyze_associations(fields = fields_late, period = 'late')
	
	results <- rbind(
		results_present,
		results_mid,
		results_late
	)
	write.csv(results, './Analysis/Associations between Team Clusters and Workflow Attributes.csv', row.names = FALSE)
