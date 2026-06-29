### SDM METHODOLOGICAL PLURALITY
### Anna Thonis, Nikki Calavari, Uzma Ashraf, Toni Lyn Morelli, and Adam B. Smith*
### * adam.smith@mobot.org | Missouri Botanical Garden | 2025-10
###
### Measure associations between clusters of teams and individual workflow attributes.
###
### source('C:/Kaji/Research Group/ENMs - Plurality of Modeling Workflows (Anna Thonis)/enm_methods_plurality/06_univariate_workflow_attributes_vs_raster_clusters.r')
###
### CONTENTS ###
### setup ###
### test for associations between unsupervised clusters of rasters and workflow attributes ###
### reshape results associating workflow attributes with clusters into table for display ###
### univariate Mantel and PERMANOVA tests between distances between rasters in PCA space and individual workflow attributes ###
### make PCA biplots with rasters coded by select workflow attributes for figures in main text ###

#############
### setup ###
#############

	rm(list = ls())
	source('C:/Kaji/Research Group/ENMs - Plurality of Modeling Workflows (Anna Thonis)/enm_methods_plurality/00_shared_constants_and_functions.r')

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

	# Match a workflow attribute to a table with attributes for each raster
	#
	# match_on		"team" or "raster": Match y to the table scoring teams or the table scoring rasters
	# y				Numeric or character values for each team or raster
	# clusters		Integer vector with team clusters. Names are rasters.
	#
	# NB We assume access to the table with scoring by team or by rasters (team_fields, raster_fields)
	match_y_to_cluster <- function(match_on, y, clusters) {

		### indices matching TEAM fields to cluster
		if (match_on == 'team') {

			cluster_rast_names <- names(clusters)
			cluster_team_names <- substr(cluster_rast_names, 1, 1)
			matches_team <- match(cluster_team_names, team_fields$team_code)

			y_match <- y[matches_team]

		} else if (match_on == 'raster') {

			### indices matching RASTER fields to cluster
			cluster_rast_names <- names(clusters)
			field_rast_names <- paste0(rast_fields$raster_name, '_', rast_fields$time_period)
			matches_rast <- match(cluster_rast_names, field_rast_names)

			y_match <- y[matches_rast]

		} else {
			stop('Bad match.')
		}
		y_match

	}

	# Implement Kruskal-Wallis test for numerical covariates of cluster identity
	#
	# nice			Nicely-worded characters string describing covariate
	# y				Value of numerical covariate from either workbook sheet with scoring by team or raster
	# clusters		Named vector of integers giving cluster assignments
	# match_on		'team' ==> match y to clusters based on scoring by team (ie, the covariate is from the 
	#				worksheet that scores by team); 'raster' ==> match y to clusters based on scoring by raster
	#				(ie, the covariate is from the worksheet that scores by raster)
	# NB Output has to match same form as contingency_test()!
	kw_test <- function(step, nice, y, clusters, match_on) {

		y_match <- match_y_to_cluster(match_on = match_on, y = y, clusters = clusters)
		data <- data.table(cluster_rasts = names(clusters), y = y_match, cluster = clusters, raster = names(clusters))

		n_na <- sum(is.na(y_match))
		if (n_na > 0) data <- data[complete.cases(data)]

		kw <- kruskal.test(y ~ cluster, data = data)

		results <- rbind(
			results,
			data.table(
				species = species_full,
				k = k,
				step = step,
				nice = nice,
				test = 'Kruskal-Wallis',
				n_na = n_na,
				time_period = 'all',
				kw_chi_sq = kw$statistic,
				p_value = kw$p.value,
				significant = ifelse(kw$p.value <= 0.05, '*', '-'),
				categories = NA_character_
			)
		)
		results

	}

	# Implement contingency_test test for categorical covariates of cluster identity
	#
	# step			Character string with name of modeling step... used to re-order rows of results
	# nice			Nicely-worded characters string describing covariate
	# y				Value(s) of categorical covariate from either workbook sheet with scoring by team or raster
	# clusters		Named vector of integers giving cluster assignments
	# match_on		'team' ==> match y to clusters based on scoring by team (ie, the covariate is from the 
	#				worksheet that scores by team); 'raster' ==> match y to clusters based on scoring by raster
	#				(ie, the covariate is from the worksheet that scores by raster)
	# time_period	'all' (all time periods), 'present', 'future' (mid- and late century), 'mid' or 'late'
	# NB Output has to match same form as kw_test()!
	contingency_test <- function(step, nice, y, clusters, match_on, time_period = 'all') {

		y_match <- match_y_to_cluster(match_on = match_on, y = y, clusters = clusters)

		if (time_period != 'all') {
			if (time_period == 'present') {
				keeps <- rast_fields$time_period == 'present'
			} else if (time_period == 'future') {
				keeps <- rast_fields$time_period %in% c('mid', 'late')
			} else if (time_period == 'mid') {
				keeps <- rast_fields$time_period %in% 'mid'
			} else if (time_period == 'late') {
				keeps <- rast_fields$time_period %in% 'late'
			} else {
				stop('Bad `time_period`.')
			}
			y_match <- y_match[keeps]
			clusters <- clusters[keeps]
		}

		n_na <- sum(is.na(y_match))
		y_tally <- c(table(y_match))
		y_sufficient <- length(unique(y)) > 2 || (min(y_tally) > 1 & max(y_tally) - 1 > 1)

		### Fisher's exact test with simulate P value for non-2x2 tables: categorical vs categorical
		if (y_sufficient) {

			x_table <- table(clusters, y_match)
			categories <- colnames(x_table)
			categories <- paste(categories, collapse = ' | ')

			# chi <- chisq.test(x_table)
			fisher <- fisher.test(x_table, simulate.p.value = TRUE, B = 100000)

			results <- rbind(
				results,
				data.table(
					species = species_full,
					k = k,
					step = step,
					nice = nice,
					test = 'contingency',
					n_na = n_na,
					time_period = time_period,
					kw_chi_sq = NA_real_,
					p_value = fisher$p.value,
					significant = ifelse(fisher$p.value <= 0.05, '*', '-'),
					categories = categories
				)
			)

		} else {
		
			results <- rbind(
				results,
				data.table(
					species = species_full,
					k = k,
					step = step,
					nice = nice,
					test = 'contingency',
					n_na = n_na,
					time_period = time_period,
					kw_chi_sq = NA_real_,
					p_value = NA_real_,
					significant = NA_real_,
					categories = NA_character_
				)
			)
		
		}
		results

	} # EOF

	# Make PCA biplot with rasters coded by attribute
	#
	# biplot		ggplot2 object created in 03_cluster_analysis_of_teams_hierarchical.r
	# match_on		'team' ==> match y to clusters based on scoring by team (ie, the covariate is from the 
	#				worksheet that scores by team); 'raster' ==> match y to clusters based on scoring by raster
	#				(ie, the covariate is from the worksheet that scores by raster)
	# y				Numeric or character vector of values to plot, one per raster
	# y_type		'numeric' or 'factor'
	# trans			Transformation for values of y, in ggplot2 format (e.g., "log10", "log2")
	# nice			Name for plot title
	# legend		Title for legend
	# clusters		Numeric vector of cluster number assignment, one per value in y
	# scores		PC1 and PC2 scores for each raster
	# time_period	'all' (all time periods), 'present', 'future' (mid- and late century), 'mid' or 'late'	
	# show_legend   FALSE: do not show legend
	#
	# Returns a ggplot2 object, displays a biplot with coding for each raster by values in y
	add_attribute_to_biplot_with_clusters <- function(biplot, match_on, y, y_type, trans, nice, legend, clusters, scores, time_period = 'all', show_legend = TRUE) {

		y_match <- match_y_to_cluster(match_on = match_on, y = y, clusters = clusters)

		df <- cbind(scores, data.table(cluster = clusters, raster = names(clusters), y = y_match))

		if (time_period != 'all') {
			if (time_period == 'present') {
				keeps <- grepl(df$raster, pattern = '_present')
			} else if (time_period == 'future') {
				keeps <- grepl(df$raster, pattern = '_mid|_late')
			} else if (time_period == 'mid') {
				keeps <- grepl(df$raster, pattern = '_mid')
			} else if (time_period == 'late') {
				keeps <- grepl(df$raster, pattern = '_late')
			} else {
				stop('Bad `time_period`.')
			}
			df <- df[keeps, ]
		}
		
		# plot limits (before removing NAs)
		mult <- 0.05
		xlim <- range(df[[1]])
		ylim <- range(df[[2]])
		xrange <- diff(xlim)
		yrange <- diff(ylim)
		xlim[1] <- xlim[1] - mult * xrange
		xlim[2] <- xlim[2] + mult * xrange
		ylim[1] <- ylim[1] - 2 * mult * yrange # extend to fit legend
		ylim[2] <- ylim[2] + mult * yrange

		df <- df[complete.cases(df), ]

		legend_position <- if (show_legend) {
			c(0.98, 0.02)
		} else {
			'none'
		}

		graph <- biplot +
			coord_cartesian(xlim = xlim, ylim = ylim) +
			theme(
				legend.background = element_rect(fill = alpha('white', 0.5)),
				legend.position = legend_position,
				legend.justification = c('right', 'bottom'),
				plot.title = element_text(size = 8, face = 'bold'),
				axis.title = element_text(size = 6),
				axis.text = element_text(size = 5),
				legend.title = element_text(size = 6),
				legend.text = element_text(size = 6),
				legend.key.size = unit(0.2, 'cm'),
				legend.spacing = unit(0.03, 'cm'),
				legend.box.spacing = unit(0.1, 'cm'),
				legend.margin = margin(1.3, 1.3, 1.3, 1.3),

				axis.title.x = element_blank(),  # remove x-axis label
				axis.title.y = element_blank(),  # remove y-axis label
				axis.text.x  = element_blank(),  # remove x-axis numbers
				axis.text.y  = element_blank(),  # remove y-axis numbers
				axis.ticks.x = element_blank(),  # optional: remove x-axis ticks
				axis.ticks.y = element_blank()   # optional: remove y-axis ticks

			) +
			ggtitle(nice)

			# display p value
			p_value <- results$p_value[nrow(results)]
			sig <- p_value <= 0.05

			if (p_value < 0.001) {
				p_value <- '<0.001'
			} else {
				p_value <- paste0('= ', sprintf('%.3f', p_value))
			}

			graph <- graph +
				annotate(
					'text',
					x = -Inf,
					y = Inf,
					label = paste0('p ', p_value),
					hjust = -0.1,
					vjust = 1.5,
					size = 3,
					color = ifelse(sig, 'red', 'black'),
					fontface = ifelse(sig, 'bold', 'plain')
				)

			if (y_type == 'numeric' & !is.na(trans)) {
			
				graph <- graph +
					geom_point(
						data = df,
						mapping = aes(x = PC1, y = PC2, fill = y),
						pch = 21, size = 2
					) +
					scale_fill_viridis(
						name = legend,
						option = 'magma',
						trans = trans,
						labels = scales::comma
					)

			} else if (y_type == 'numeric' & is.na(trans)) {
			
				graph <- graph +
					geom_point(
						data = df,
						mapping = aes(x = PC1, y = PC2, fill = y),
						pch = 21, size = 2
					) +
					scale_fill_viridis(
						name = legend,
						option = 'magma'
					)

			} else if (y_type == 'factor') {
				
				n_unique <- length(unique(df$y))

				if (n_unique == 2) {
					cols <- c('#1b9e77', '#d95f02')
					pch <- c(21, 22, 23, 24, 25)[1:n_unique]
				} else if (n_unique < 9) {
					cols <- RColorBrewer::brewer.pal(n = length(unique(y)), name = 'Set2')
					pch <- c(21, 22, 23, 24, 25, 15:18)[1:n_unique]
				} else {
					cols <- rainbow(n_unique)
					pch <- c(21, 22, 23, 24, 25, 15:18, 1:14)[1:n_unique]
				}

				graph <- graph +		
					geom_point(
						data = df,
						mapping = aes(x = PC1, y = PC2, fill = y, shape = y),
						size = 2, alpha = 0.6
					) +
					scale_fill_manual(
						name = legend,
						values = cols
					) +
					scale_shape_manual(
						name = legend,
						values = pch
					)

					if (n_unique > 5) {

						graph <- graph +
							scale_color_manual(
								name = legend,
								values = cols
							)
					
					} 
			
			}

		graph

	}

# say('##############################################################################################')
# say('### test for associations between unsupervised clusters of rasters and workflow attributes ###')
# say('##############################################################################################')

# 	### MAIN
# 	########

# 	rast_fields <- load_rast_fields(species_focal = species_focal)
# 	team_fields <- load_team_fields(species_focal = species_focal)

# 	### cluster
# 	preds <- load_predictions(species_focal = species_focal, period = 'all', scale = TRUE, subset_teams = TRUE)
# 	preds_trans <- t(preds)

# 	pca <- prcomp(preds_trans)
# 	scores <- pca$x[ , 1:2]
# 	scores <- as.data.frame(scores)

# 	dists <- dist(scores)
# 	clust <- hclust(dists, method = 'complete')

# 	### PCA biplots with polygons for each size of cluster
# 	biplots <- readRDS(paste0(out_dir, '/Cluster Analysis of Rasters/Biplot and Dendrogram of All Rasters Hierarchical Complete Polygon Plots.rds'))

# 	### analyze associations between workflow attributes and clusters created by predictions
# 	########################################################################################
# 	results <- data.table()

# 	for (k in get_ks()) {
# 	# for (k in c(2, 5, 12)) {

# 		say('k ', k)

# 		panels <- list() # list of biplots (ggplot2 objects) with one panel for each attribute
# 		biplot <- biplots[[paste0('k', k)]]
# 		clusters <- cutree(clust, k = k)

# 		# ### indices matching TEAM fields to cluster
# 		# cluster_rast_names <- names(clusters)
# 		# cluster_team_names <- substr(cluster_rast_names, 1, 1)
# 		# matches_team <- match(cluster_team_names, team_fields$team_code)

# 		# ### indices matching RASTER fields to cluster
# 		# cluster_rast_names <- names(clusters)
# 		# field_rast_names <- paste0(rast_fields$raster_name, '_', rast_fields$time_period)
# 		# matches_rast <- match(cluster_rast_names, field_rast_names)

# 		### add MEAN ODMAP score and MINIMUM SCORE ACROSS CATEGORIES to fields
# 		######################################################################

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

# 		team_fields$odmap_mean <- odmap_means[match(team_fields$team_code, team_codes)]
# 		team_fields$odmap_min <- odmap_mins[match(team_fields$team_code, team_codes)]

# 		### evaluate individual workflow attributes
# 		###########################################

# 		### "team"

# 			step <- '0 Quality/Team'
# 			nice <- 'Team'
# 			match_on <- 'team'

# 			y <- team_fields$team_code

# 			results <- contingency_test(step = step, nice = nice, y = y, clusters = clusters, match_on = match_on)

# 			# plot
# 			y_type <- 'factor'
# 			legend <- 'Team'
# 			trans <- NA

# 			panels[[length(panels) + 1]] <- add_attribute_to_biplot_with_clusters(biplot = biplot, match_on = match_on, y = y, y_type = y_type, trans = trans, nice = nice, legend = legend, clusters = clusters, scores = scores, show_legend = FALSE)

# 		### "team x thresholding"

# 			step <- '0 Quality/Team'
# 			nice <- 'Team × Continuous/Thresholding'
# 			match_on <- 'raster'

# 			y <- rast_fields$team_code
# 			y[rast_fields$raster_name == 'M1'] <- 'Mc'
# 			y[rast_fields$raster_name == 'M2'] <- 'Mt'
# 			if (species_focal == 'Priona') {
# 				y[rast_fields$raster_name %in% c('N1', 'N2')] <- 'Nc'
# 				y[rast_fields$raster_name %in% c('N3', 'N4', 'N3a', 'N4a', 'N3b', 'N4b')] <- 'Nt'
# 			} else if (species_focal == 'Zamia') {
# 				y[rast_fields$raster_name %in% c('N1', 'N2', 'N3', 'N1a', 'N2a', 'N3a', 'N1b', 'N2b', 'N3b')] <- 'Nc'
# 				y[rast_fields$raster_name %in% c('N4', 'N5', 'N6', 'N4a', 'N5a', 'N6a', 'N4b', 'N5b', 'N6b')] <- 'Nt'
# 			}

# 			results <- contingency_test(step = step, nice = nice, y = y, clusters = clusters, match_on = match_on)

# 			# plot
# 			y_type <- 'factor'
# 			legend <- 'Team × Continuous/Thresholding'
# 			trans <- NA

# 			panels[[length(panels) + 1]] <- add_attribute_to_biplot_with_clusters(biplot = biplot, match_on = match_on, y = y, y_type = y_type, trans = trans, nice = nice, legend = legend, clusters = clusters, scores = scores, show_legend = FALSE)

# 		### ODMAP *mean* score

# 			step <- '0 Quality/Team'
# 			nice <- 'SDM standards: Mean rank'
# 			match_on <- 'team'

# 			y <- as.numeric(team_fields$odmap_mean)

# 			results <- kw_test(step = step, nice = nice, y = y, clusters = clusters, match_on = match_on)

# 			# plot
# 			y_type <- 'numeric'
# 			legend <- 'Rank\n(numeric)'
# 			trans <- NA

# 			panels[[length(panels) + 1]] <- add_attribute_to_biplot_with_clusters(biplot = biplot, match_on = match_on, y = y, y_type = y_type, trans = trans, nice = nice, legend = legend, clusters = clusters, scores = scores)

# 		# ### ODMAP *minimum* of mean score
# 		# # Not doing this one bc only one team had >0 minimum score

# 		# 	step <- '0 Quality/Team'
# 		# 	nice <- 'SDM standards: Minimum rank'
# 		# 	match_on <- 'team'

# 		# 	y <- as.numeric(team_fields$odmap_min)
			
# 		# 	results <- kw_test(step = step, nice = nice, y = y, clusters = clusters, match_on = match_on)

# 		# 	# plot
# 		# 	y_type <- 'numeric'
# 		# 	legend <- 'Rank\n(numeric)'
# 		# 	trans <- NA

# 		# 	panels[[length(panels) + 1]] <- add_attribute_to_biplot_with_clusters(biplot = biplot, match_on = match_on, y = y, y_type = y_type, trans = trans, nice = nice, legend = legend, clusters = clusters, scores = scores)

# 		### number of occurrences

# 			step <- '1 Data'
# 			nice <- 'Occurrences: Number of occurrences'
# 			match_on <- 'team'

# 			y <- team_fields$num_occurrences_minimum
# 			results <- kw_test(step = step, nice = nice, y = y, clusters = clusters, match_on = match_on)
			
# 			# plot
# 			y_type <- 'numeric'
# 			legend <- 'Number'
# 			trans <- 'log10'

# 			panels[[length(panels) + 1]] <- add_attribute_to_biplot_with_clusters(biplot = biplot, match_on = match_on, y = y, y_type = y_type, trans = trans, nice = nice, legend = legend, clusters = clusters, scores = scores)

# 		### climate predictors: number

# 			step <- '1 Data'
# 			nice <- 'Predictors: Total number of climate predictors'
# 			match_on <- 'team'

# 			y <- team_fields$predictors_climate_num_total
# 			y <- as.numeric(y)

# 			results <- kw_test(step = step, nice = nice, y = y, clusters = clusters, match_on = match_on)

# 			# plot
# 			y_type <- 'numeric'
# 			legend <- 'Number'
# 			trans <- 'log10'

# 			panels[[length(panels) + 1]] <- add_attribute_to_biplot_with_clusters(biplot = biplot, match_on = match_on, y = y, y_type = y_type, trans = trans, nice = nice, legend = legend, clusters = clusters, scores = scores)

# 		### non-climate: ANY non-climate predictors

# 			step <- '1 Data'
# 			nice <- 'Predictors: Non-climate predictors'
# 			match_on <- 'team'

# 			y <- team_fields$predictors_nonclimate_num > 0
# 			y <- as.numeric(y)
# 			y[y == 1] <- 'Yes'
# 			y[y == 0] <- 'No'

# 			results <- contingency_test(step = step, nice = nice, y = y, clusters = clusters, match_on = match_on)

# 			# plot
# 			y_type <- 'factor'
# 			legend <- 'Any non-climate\npredictor(s)'
# 			trans <- NA

# 			panels[[length(panels) + 1]] <- add_attribute_to_biplot_with_clusters(biplot = biplot, match_on = match_on, y = y, y_type = y_type, trans = trans, nice = nice, legend = legend, clusters = clusters, scores = scores)

# 		### non-climate: number of predictors

# 			step <- '1 Data'
# 			nice <- 'Predictors: Number of non-climate predictors'
# 			match_on <- 'team'

# 			y <- team_fields$predictors_nonclimate_num
# 			y <- as.numeric(y)

# 			results <- kw_test(step = step, nice = nice, y = y, clusters = clusters, match_on = match_on)

# 		### source of climate predictors

# 			step <- '1 Data'
# 			nice <- 'Predictors: Climate data source'
# 			match_on <- 'team'

# 			y <- team_fields$predictors_climate_source

# 			results <- contingency_test(step = step, nice = nice, y = y, clusters = clusters, match_on = match_on)

# 			# plot
# 			y_type <- 'factor'
# 			legend <- 'Source'
# 			trans <- NA

# 			panels[[length(panels) + 1]] <- add_attribute_to_biplot_with_clusters(biplot = biplot, match_on = match_on, y = y, y_type = y_type, trans = trans, nice = nice, legend = legend, clusters = clusters, scores = scores)

# 		### number of predictors

# 			step <- '1 Data'
# 			nice <- 'Predictors: Total number'
# 			match_on <- 'team'

# 			y <- team_fields$predictors_climate_nonclimate_num_total
# 			y <- as.numeric(y)

# 			results <- kw_test(step = step, nice = nice, y = y, clusters = clusters, match_on = match_on)

# 			# plot
# 			y_type <- 'numeric'
# 			legend <- 'Number'
# 			trans <- 'log2'

# 			panels[[length(panels) + 1]] <- add_attribute_to_biplot_with_clusters(biplot = biplot, match_on = match_on, y = y, y_type = y_type, trans = trans, nice = nice, legend = legend, clusters = clusters, scores = scores)

# 		### spatial resolution: cell size *qualitative*
			
# 			step <- '1 Data'
# 			nice <- 'Spatial resolution (arcmin)'
# 			match_on <- 'team'

# 			y <- team_fields$res_arcmin

# 			results <- contingency_test(step = step, nice = nice, y = y, clusters = clusters, match_on = match_on)

# 		### spatial resolution: cell size
			
# 			step <- '1 Data'
# 			nice <- 'Spatial resolution (km2)'
# 			match_on <- 'team'

# 			y <- as.numeric(team_fields$res_km2)

# 			results <- kw_test(step = step, nice = nice, y = y, clusters = clusters, match_on = match_on)

# 			# plot
# 			y_type <- 'numeric'
# 			legend <- 'Area (km2)'
# 			trans <- 'log2'

# 			panels[[length(panels) + 1]] <- add_attribute_to_biplot_with_clusters(biplot = biplot, match_on = match_on, y = y, y_type = y_type, trans = trans, nice = nice, legend = legend, clusters = clusters, scores = scores)

# 		### collinearity: managed at all

# 			step <- '2 Model setup'
# 			nice <- 'Collinearity: Explicitly managed'
# 			match_on <- 'team'

# 			field_names <- c('collinearity_pca', 'collinearity_correlation', 'collinearity_vif', 'collinearity_other_method')
# 			y <- team_fields[ , ..field_names]
# 			y <- y[ , lapply(.SD, as.numeric)]

# 			y <- rowSums(y)
# 			y <- as.numeric(y > 0)
# 			y[y == 1] <- 'Managed'
# 			y[y == '0'] <- 'Not managed'

# 			results <- contingency_test(step = step, nice = nice, y = y, clusters = clusters, match_on = match_on)

# 			# plot
# 			y_type <- 'factor'
# 			legend <- 'Collinearity'
# 			trans <- NA

# 			panels[[length(panels) + 1]] <- add_attribute_to_biplot_with_clusters(biplot = biplot, match_on = match_on, y = y, y_type = y_type, trans = trans, nice = nice, legend = legend, clusters = clusters, scores = scores)

# 		### collinearity: method

# 			step <- '2 Model setup'
# 			nice <- 'Collinearity: Method of management'
# 			match_on <- 'team'

# 			field_names <- c('collinearity_pca', 'collinearity_correlation', 'collinearity_vif', 'collinearity_other_method')
# 			y <- team_fields[ , ..field_names]
# 			y <- y[ , lapply(.SD, as.numeric)]
# 			y <- apply(y, 1, function(row) {
# 				cols_with_1 <- field_names[row == 1]
# 				if (length(cols_with_1) == 0) return(NA)
# 				paste(cols_with_1, collapse = ", ")
# 			})
# 			y <- replace_y_NAs(y = y, field_names = field_names)

# 			results <- contingency_test(step = step, nice = nice, y = y, clusters = clusters, match_on = match_on)

# 			y <- sub(y, pattern = 'collinearity_pca', replacement = 'PCA')
# 			y <- sub(y, pattern = 'collinearity_correlation', replacement = 'Corr.')
# 			y <- sub(y, pattern = 'collinearity_vif', replacement = 'VIF')
# 			y <- sub(y, pattern = 'collinearity_other_method', replacement = 'Other')

# 			# plot
# 			y_type <- 'factor'
# 			legend <- 'Method'
# 			trans <- NA

# 			panels[[length(panels) + 1]] <- add_attribute_to_biplot_with_clusters(biplot = biplot, match_on = match_on, y = y, y_type = y_type, trans = trans, nice = nice, legend = legend, clusters = clusters, scores = scores)

# 		### modeling_software

# 			step <- '3 Modeling software'
# 			nice <- 'Modeling software'

# 			field_names <- c('modeling_software_enmeval', 'modeling_software_enmtools', 'modeling_software_wallace', 'modeling_software_biomod2', 'modeling_software_sabinansdm', 'modeling_software_sdm', 'modeling_software_miamaxent', 'modeling_software_enmsdmx', 'modeling_software_flexsdm', 'modeling_software_sdmtune', 'modeling_software_piecemeal', 'modeling_software_other')

# 			match_on <- 'team'

# 			y <- team_fields[ , ..field_names]
# 			y <- y[ , lapply(.SD, as.numeric)]
# 			y <- apply(y, 1, function(row) {
# 				cols_with_1 <- field_names[row == 1]
# 				if (length(cols_with_1) == 0) return(NA)
# 				paste(cols_with_1, collapse = ", ")
# 			})
# 			y <- replace_y_NAs(y = y, field_names = field_names)

# 			y <- sub(y, pattern = 'modeling_software_enmeval', replacement = 'ENMeval')
# 			y <- sub(y, pattern = 'modeling_software_enmtools', replacement = 'ENMTools')
# 			y <- sub(y, pattern = 'modeling_software_wallace', replacement = 'Wallace')
# 			y <- sub(y, pattern = 'modeling_software_biomod2', replacement = 'BIOMOD2')
# 			y <- sub(y, pattern = 'modeling_software_sabinansdm', replacement = 'sabinaNSDM')
# 			y <- sub(y, pattern = 'modeling_software_sdm', replacement = 'sdm')
# 			y <- sub(y, pattern = 'modeling_software_miamaxent', replacement = 'MIAmaxent')
# 			y <- sub(y, pattern = 'modeling_software_enmsdmx', replacement = 'enmSdmX')
# 			y <- sub(y, pattern = 'modeling_software_flexsdm', replacement = 'flexsdm')
# 			y <- sub(y, pattern = 'modeling_software_sdmtune', replacement = 'SDMtune')
# 			y <- sub(y, pattern = 'modeling_software_other', replacement = 'other')
# 			y <- sub(y, pattern = 'modeling_software_piecemeal', replacement = 'piecemeal')

# 			results <- contingency_test(step = step, nice = nice, y = y, clusters = clusters, match_on = match_on)

# 			# plot
# 			y_type <- 'factor'
# 			legend <- 'Software'
# 			trans <- NA

# 			panels[[length(panels) + 1]] <- add_attribute_to_biplot_with_clusters(biplot = biplot, match_on = match_on, y = y, y_type = y_type, trans = trans, nice = nice, legend = legend, clusters = clusters, scores = scores)

# 		### software_used_by_team_developing_it

# 			step <- '3 Software: Developers of the software'
# 			nice <- 'Software use by developers'
# 			trans <- NA

# 			y <- rast_fields$team_code
# 			y[y %in% c('E', 'B', 'C', 'H', 'D')] <- 'Yes'
# 			y[y != 'Yes'] <- 'No'

# 			results <- contingency_test(step = step, nice = nice, y = y, clusters = clusters, match_on = match_on)

# 			# plot
# 			y_type <- 'factor'
# 			legend <- 'Developer'
# 			trans <- NA

# 			panels[[length(panels) + 1]] <- add_attribute_to_biplot_with_clusters(biplot = biplot, match_on = match_on, y = y, y_type = y_type, trans = trans, nice = nice, legend = legend, clusters = clusters, scores = scores)

# 		### modeling_software: ENVeval / Wallace

# 			step <- '3 Model algorithm'
# 			nice <- 'Software: Used ENMeval/Wallace'
# 			match_on <- 'team'

# 			set_names <- c('modeling_software_enmeval', 'modeling_software_wallace')
# 			y <- team_fields [ , ..set_names]
# 			y <- y[ , lapply(.SD, as.numeric)]
# 			y <- as.numeric(y > 0)
# 			y[y == 1] <- 'ENMeval/Wallace'
# 			y[y == '0'] <- 'Other'

# 			results <- contingency_test(step = step, nice = nice, y = y, clusters = clusters, match_on = match_on)

# 			# plot
# 			y_type <- 'factor'
# 			legend <- 'Software'
# 			trans <- NA

# 			panels[[length(panels) + 1]] <- add_attribute_to_biplot_with_clusters(biplot = biplot, match_on = match_on, y = y, y_type = y_type, trans = trans, nice = nice, legend = legend, clusters = clusters, scores = scores)

# 		### algorithm: used ensemble
		
# 			step <- '3 Model algorithm'
# 			nice <- 'Algorithm: Used ensemble'
# 			match_on <- 'team'

# 			y <- rast_fields$algo_ensemble
# 			y <- as.numeric(y)
# 			y[y == 1] <- 'Yes'
# 			y[y == '0'] <- 'No'

# 			test <- 'contingency_test'

# 			results <- contingency_test(step = step, nice = nice, y = y, clusters = clusters, match_on = match_on)

# 			# plot
# 			y_type <- 'factor'
# 			legend <- 'Ensemble'
# 			trans <- NA

# 			panels[[length(panels) + 1]] <- add_attribute_to_biplot_with_clusters(biplot = biplot, match_on = match_on, y = y, y_type = y_type, trans = trans, nice = nice, legend = legend, clusters = clusters, scores = scores)

# 		### algorithm: number of algorithms used in ensemble (including 0)
			
# 			step <- '3 Model algorithm'
# 			nice <- 'Algorithm: Number of algorithms in ensemble'
# 			match_on <- 'raster'

# 			y <- rast_fields$algo_ensemble_number_of_models
# 			y <- as.numeric(y)

# 			results <- kw_test(step = step, nice = nice, y = y, clusters = clusters, match_on = match_on)

# 			# plot
# 			y_type <- 'numeric'
# 			legend <- 'Algorithms'
# 			trans <- NA

# 			panels[[length(panels) + 1]] <- add_attribute_to_biplot_with_clusters(biplot = biplot, match_on = match_on, y = y, y_type = y_type, trans = trans, nice = nice, legend = legend, clusters = clusters, scores = scores)

# 		### algorithm: identity

# 			step <- '3 Model algorithm'
# 			nice <- 'Algorithm'
# 			match_on <- 'raster'

# 			field_names <- c('algo_ensemble', 'algo_maxent', 'algo_maxnet', 'algo_glm', 'algo_gam', 'algo_rf', 'algo_sre')

# 			y <- rast_fields[ , ..field_names]
# 			y <- y[ , lapply(.SD, as.numeric)]
# 			y <- apply(y, 1, function(row) {
# 				cols_with_1 <- field_names[row == 1]
# 				if (length(cols_with_1) == 0) return(NA)
# 				paste(cols_with_1, collapse = ", ")
# 			})
# 			y <- replace_y_NAs(y = y, field_names = field_names)

# 			y[y == 'algo_ensemble'] <- 'Ensemble'
# 			y[y == 'algo_maxent'] <- 'MaxEnt'
# 			y[y == 'algo_maxnet'] <- 'MaxNet'
# 			y[y == 'algo_glm'] <- 'GLM'
# 			y[y == 'algo_gam'] <- 'GAM'
# 			y[y == 'algo_rf'] <- 'RF'
# 			y[y == 'algo_sre'] <- 'SRE'

# 			results <- contingency_test(step = step, nice = nice, y = y, clusters = clusters, match_on = match_on)

# 			# plot
# 			y_type <- 'factor'
# 			legend <- 'Algorithm'
# 			trans <- NA

# 			panels[[length(panels) + 1]] <- add_attribute_to_biplot_with_clusters(biplot = biplot, match_on = match_on, y = y, y_type = y_type, trans = trans, nice = nice, legend = legend, clusters = clusters, scores = scores)

# 		### algorithm: MaxEnt / MaxNet

# 			step <- '3 Model algorithm'
# 			nice <- 'Algorithm: Used MaxEnt/MaxNet'
# 			matchOn <- 'raster'

# 			set_names <- c('algo_maxent', 'algo_maxnet')
# 			y <- rast_fields[ , ..set_names]
# 			y <- y[ , lapply(.SD, as.numeric)]
# 			y <- rowSums(y)
# 			y[y == 1] <- 'MaxEnt/Net'
# 			y[y == '0'] <- 'Other'

# 			results <- contingency_test(step = step, nice = nice, y = y, clusters = clusters, match_on = match_on)

# 			# plot
# 			y_type <- 'factor'
# 			legend <- 'MaxEnt/Net'
# 			trans <- NA

# 			panels[[length(panels) + 1]] <- add_attribute_to_biplot_with_clusters(biplot = biplot, match_on = match_on, y = y, y_type = y_type, trans = trans, nice = nice, legend = legend, clusters = clusters, scores = scores)

# 		### bias correction: did any
			
# 			step <- '2 Model setup'
# 			nice <- 'Bias correction: Implemented'
# 			match_on <- 'team'

# 			field_names <- c('bias_correction_spatial_thinning', 'bias_correction_environmental_thinning', 'bias_correction_target_background', 'bias_correction_nonrandom_background')
			
# 			y_star <- team_fields[ , ..field_names]
# 			y_star <- y_star[ , lapply(.SD, as.numeric)]
# 			y_star <- rowSums(y_star)
# 			y_star <- y_star > 0
# 			y <- rep(NA, nrow(rast_fields))
# 			y[y_star] <- 'Yes'
# 			y[!y_star] <- 'No'

# 			results <- contingency_test(step = step, nice = nice, y = y, clusters = clusters, match_on = match_on)

# 			# plot
# 			y_type <- 'factor'
# 			legend <- 'Bias\ncorrection'
# 			trans <- NA

# 			panels[[length(panels) + 1]] <- add_attribute_to_biplot_with_clusters(biplot = biplot, match_on = match_on, y = y, y_type = y_type, trans = trans, nice = nice, legend = legend, clusters = clusters, scores = scores)

# 		### bias correction: method

# 			step <- '2 Model setup'
# 			nice <- 'Bias correction: Method'
# 			match_on <- 'team'

# 			field_names <- c('bias_correction_spatial_thinning', 'bias_correction_environmental_thinning', 'bias_correction_target_background', 'bias_correction_nonrandom_background', 'bias_correction_none')

# 			y <- team_fields[ , ..field_names]
# 			y <- y[ , lapply(.SD, as.numeric)]
# 			y <- apply(y, 1, function(row) {
# 				cols_with_1 <- field_names[row == 1]
# 				if (length(cols_with_1) == 0) return(NA)
# 				paste(cols_with_1, collapse = ", ")
# 			})
# 			y <- replace_y_NAs(y = y, field_names = field_names)

# 			y <- sub(y, pattern = 'bias_correction_spatial_thinning', replacement = 'Spatial')
# 			y <- sub(y, pattern = 'bias_correction_environmental_thinning', replacement = 'Env. thin')
# 			y <- sub(y, pattern = 'bias_correction_target_background', replacement = 'Target')
# 			y <- sub(y, pattern = 'bias_correction_nonrandom_background', replacement = 'NR BG')
# 			y <- sub(y, pattern = 'bias_correction_none', replacement = 'None')
# 			y[is.na(y)] <- 'Unknown'

# 			results <- contingency_test(step = step, nice = nice, y = y, clusters = clusters, match_on = match_on)

# 			# plot
# 			y_type <- 'factor'
# 			legend <- 'Method'
# 			trans <- NA

# 			panels[[length(panels) + 1]] <- add_attribute_to_biplot_with_clusters(biplot = biplot, match_on = match_on, y = y, y_type = y_type, trans = trans, nice = nice, legend = legend, clusters = clusters, scores = scores)

# 		### non-presence type

# 			step <- '2 Model setup'
# 			nice <- 'Non-presence type'
# 			match_on <- 'team'

# 			# non-presences: type
# 			field_names <- c('nonpres_type_background', 'nonpres_type_pseudoabsence', 'nonpres_type_target_background')

# 			y <- team_fields[ , ..field_names]
# 			y <- y[ , lapply(.SD, as.numeric)]
# 			y <- apply(y, 1, function(row) {
# 				cols_with_1 <- field_names[row == 1]
# 				if (length(cols_with_1) == 0) return(NA)
# 				paste(cols_with_1, collapse = ", ")
# 			})
# 			y <- replace_y_NAs(y = y, field_names = field_names)

# 			y <- sub(y, pattern = 'nonpres_type_background', replacement = 'Random')
# 			y <- sub(y, pattern = 'nonpres_type_pseudoabsence', replacement = 'PSA')
# 			y <- sub(y, pattern = 'nonpres_type_target_background', replacement = 'Target')
		
# 			results <- contingency_test(step = step, nice = nice, y = y, clusters = clusters, match_on = match_on)

# 			# plot
# 			y_type <- 'factor'
# 			legend <- 'Type'
# 			trans <- NA

# 			panels[[length(panels) + 1]] <- add_attribute_to_biplot_with_clusters(biplot = biplot, match_on = match_on, y = y, y_type = y_type, trans = trans, nice = nice, legend = legend, clusters = clusters, scores = scores)

# 		### calibration region boundary
		
# 			step <- '2 Model setup'
# 			nice <- 'Calibration region: Boundary definition'
# 			match_on <- 'team'

# 			# non-presences: type
# 			field_names <- c('boundary_rectangle', 'boundary_natural', 'boundary_convex_hull', 'boundary_range_map', 'boundary_political', 'boundary_buffer_around_occurrences')

# 			y <- team_fields[ , ..field_names]
# 			y <- y[ , lapply(.SD, as.numeric)]
# 			y <- apply(y, 1, function(row) {
# 				cols_with_1 <- field_names[row == 1]
# 				if (length(cols_with_1) == 0) return(NA)
# 				paste(cols_with_1, collapse = ", ")
# 			})
# 			y <- replace_y_NAs(y = y, field_names = field_names)

# 			y <- sub(y, pattern = 'boundary_rectangle', replacement = 'Rect.')
# 			y <- sub(y, pattern = 'boundary_natural', replacement = 'Natural')
# 			y <- sub(y, pattern = 'boundary_convex_hull', replacement = 'Hull')
# 			y <- sub(y, pattern = 'boundary_range_map', replacement = 'Range')
# 			y <- sub(y, pattern = 'boundary_political', replacement = 'Polit.')
# 			y <- sub(y, pattern = 'boundary_buffer_around_occurrences', replacement = 'Buffer')

# 			results <- contingency_test(step = step, nice = nice, y = y, clusters = clusters, match_on = match_on)

# 			# plot
# 			y_type <- 'factor'
# 			legend <- 'Type'
# 			trans <- NA

# 			panels[[length(panels) + 1]] <- add_attribute_to_biplot_with_clusters(biplot = biplot, match_on = match_on, y = y, y_type = y_type, trans = trans, nice = nice, legend = legend, clusters = clusters, scores = scores)

# 		### calibration region extent
			
# 			step <- '2 Model setup'
# 			nice <- 'Calibration region: Area'
# 			match_on <- 'team'

# 			y <- as.numeric(team_fields$extent_calibration_sans_water_km2)

# 			results <- kw_test(step = step, nice = nice, y = y, clusters = clusters, match_on = match_on)
		
# 			# plot
# 			y_type <- 'numeric'
# 			legend <- 'Area (km2)'
# 			trans <- 'log2'

# 			panels[[length(panels) + 1]] <- add_attribute_to_biplot_with_clusters(biplot = biplot, match_on = match_on, y = y, y_type = y_type, trans = trans, nice = nice, legend = legend, clusters = clusters, scores = scores)

# 		### AUC
			
# 			step <- '4 Model evaluation'
# 			nice <- 'Evaluation: Value of AUC'
# 			match_on <- 'raster'

# 			y <- rast_fields$eval_metric_auc_roc_value
# 			y <- as.numeric(y)

# 			results <- kw_test(step = step, nice = nice, y = y, clusters = clusters, match_on = match_on)

# 			# plot
# 			y_type <- 'numeric'
# 			legend <- 'AUC'
# 			trans <- NA

# 			panels[[length(panels) + 1]] <- add_attribute_to_biplot_with_clusters(biplot = biplot, match_on = match_on, y = y, y_type = y_type, trans = trans, nice = nice, legend = legend, clusters = clusters, scores = scores)

# 		### thresholded predictions

# 			step <- '5 Prediction/projection'
# 			nice <- 'Predictions: Continuous/thresholded'
# 			match_on <- 'raster'

# 			field_names <- c('prediction_type_continuous', 'prediction_type_binary_threshold', 'prediction_type_multi_threshold')

# 			y <- rast_fields[ , ..field_names]
# 			y <- y[ , lapply(.SD, as.numeric)]
# 			y <- apply(y, 1, function(row) {
# 				cols_with_1 <- field_names[row == 1]
# 				if (length(cols_with_1) == 0) return(NA)
# 				paste(cols_with_1, collapse = ", ")
# 			})
# 			y <- replace_y_NAs(y = y, field_names = field_names)

# 			y <- sub(y, pattern = 'prediction_type_continuous', replacement = 'Continuous')
# 			y <- sub(y, pattern = 'prediction_type_binary_threshold', replacement = 'Binary Thresh.')
# 			y <- sub(y, pattern = 'prediction_type_multi_threshold', replacement = 'Multiple Thresh.')

# 			results <- contingency_test(step = step, nice = nice, y = y, clusters = clusters, match_on = match_on)

# 			# plot
# 			y_type <- 'factor'
# 			legend <- 'Type'
# 			trans <- NA

# 			panels[[length(panels) + 1]] <- add_attribute_to_biplot_with_clusters(biplot = biplot, match_on = match_on, y = y, y_type = y_type, trans = trans, nice = nice, legend = legend, clusters = clusters, scores = scores)

# 		### time period

# 			step <- '5 Prediction/projection'
# 			nice <- 'Projection: Time period'
# 			match_on <- 'raster'

# 			y <- rast_fields$time_period
# 			results <- contingency_test(step = step, nice = nice, y = y, clusters = clusters, match_on = match_on)

# 			y_type <- 'factor'
# 			legend <- 'Period'
# 			trans <- NA

# 			panels[[length(panels) + 1]] <- add_attribute_to_biplot_with_clusters(biplot = biplot, match_on = match_on, y = y, y_type = y_type, trans = trans, nice = nice, legend = legend, clusters = clusters, scores = scores)

# 		### late-century time period
# 		# present and mid-century time period are redundant with climate data source, so not doing them

# 			step <- '5 Prediction/projection'
# 			nice <- 'Projection: Late 20th-century time period'
# 			match_on <- 'team'
# 			time_period <- 'late'

# 			y <- team_fields$future_scenario_latecentury_year
# 			results <- contingency_test(step = step, nice = nice, y = y, clusters = clusters, match_on = match_on, time_period = 'late')

# 			y_type <- 'factor'
# 			legend <- 'Period'
# 			trans <- NA

# 			panels[[length(panels) + 1]] <- add_attribute_to_biplot_with_clusters(biplot = biplot, match_on = match_on, y = y, y_type = y_type, trans = trans, nice = nice, legend = legend, clusters = clusters, scores = scores, time_period = time_period)

# 		### future: emission scenario
			
# 			step <- '5 Prediction/projection'
# 			nice <- 'Projection: Future climate scenario'
# 			match_on <- 'team'
# 			time_period <- 'future'

# 			field_names <- c('future_scenario_ensemble', 'future_scenario_ssp126', 'future_scenario_ssp245', 'future_scenario_ssp370', 'future_scenario_ssp585', 'future_scenario_rcp45', 'future_scenario_rcp85')

# 			y <- rast_fields[ , ..field_names]
# 			y <- y[ , lapply(.SD, as.numeric)]
# 			y <- apply(y, 1, function(row) {
# 				cols_with_1 <- field_names[row == 1]
# 				if (length(cols_with_1) == 0) return(NA)
# 				paste(cols_with_1, collapse = ", ")
# 			})
# 			y <- replace_y_NAs(y = y, field_names = field_names)

# 			y <- sub(y, pattern = 'future_scenario_ensemble', replacement = 'Ensemble')
# 			y <- sub(y, pattern = 'future_scenario_ssp126', replacement = 'SSP 126')
# 			y <- sub(y, pattern = 'future_scenario_ssp245', replacement = 'SSP 245')
# 			y <- sub(y, pattern = 'future_scenario_ssp370', replacement = 'SSP 370')
# 			y <- sub(y, pattern = 'future_scenario_ssp585', replacement = 'SSP 585')
# 			y <- sub(y, pattern = 'future_scenario_rcp45', replacement = 'RCP 4.5')
# 			y <- sub(y, pattern = 'future_scenario_rcp85', replacement = 'RCP 8.5')

# 			y[rast_fields$time_period == 'present'] <- 'Present'

# 			results <- contingency_test(step = step, nice = nice, y = y, clusters = clusters, match_on = match_on, time_period = time_period)

# 			# plot
# 			y_type <- 'factor'
# 			legend <- 'Scenario'
# 			trans <- NA

# 			panels[[length(panels) + 1]] <- add_attribute_to_biplot_with_clusters(biplot = biplot, match_on = match_on, y = y, y_type = y_type, trans = trans, nice = nice, legend = legend, clusters = clusters, scores = scores, time_period = time_period)

# 		### extrapolation: individual methods

# 			step <- '5 Prediction/projection'
# 			nice <- 'Extrapolation: Method of management'
# 			match_on <- 'raster'
# 			time_period <- 'future'

# 			field_names <- c('extrapolation_clamping_masking_clipping', 'extrapolation_exdet', 'extrapolation_mess', 'extrapolation_shape', 'extrapolation_area_of_applicability', 'extrapolation_response_curve_inspection', 'extrapolation_kissmig', 'extrapolation_no_measures')

# 			y <- rast_fields[ , ..field_names]
# 			y <- y[ , lapply(.SD, as.numeric)]
# 			y <- apply(y, 1, function(row) {
# 				cols_with_1 <- field_names[row == 1]
# 				if (length(cols_with_1) == 0) return(NA)
# 				paste(cols_with_1, collapse = ", ")
# 			})
# 			y <- replace_y_NAs(y = y, field_names = field_names)

# 			y <- sub(y, pattern = 'extrapolation_clamping_masking_clipping', replacement = 'Mask/clip')
# 			y <- sub(y, pattern = 'extrapolation_exdet', replacement = 'ExDet')
# 			y <- sub(y, pattern = 'extrapolation_mess', replacement = 'MESS')
# 			y <- sub(y, pattern = 'extrapolation_shape', replacement = 'shape')
# 			y <- sub(y, pattern = 'extrapolation_area_of_applicability', replacement = 'AOA')
# 			y <- sub(y, pattern = 'extrapolation_response_curve_inspection', replacement = 'Resp. cur.')
# 			y <- sub(y, pattern = 'extrapolation_kissmig', replacement = 'KISSMig')
# 			y <- sub(y, pattern = 'extrapolation_no_measures', replacement = 'None')

# 			results <- contingency_test(step = step, nice = nice, y = y, clusters = clusters, match_on = match_on, time_period = time_period)

# 			# plot
# 			y_type <- 'factor'
# 			legend <- 'Method'
# 			trans <- NA

# 			panels[[length(panels) + 1]] <- add_attribute_to_biplot_with_clusters(biplot = biplot, match_on = match_on, y = y, y_type = y_type, trans = trans, nice = nice, legend = legend, clusters = clusters, scores = scores)

# 		### extrapolation: any method

# 			step <- '5 Prediction/projection'
# 			nice <- 'Extrapolation: Any method'
# 			match_on <- 'raster'

# 			field_names <- c('extrapolation_clamping_masking_clipping', 'extrapolation_exdet', 'extrapolation_mess', 'extrapolation_shape', 'extrapolation_area_of_applicability', 'extrapolation_response_curve_inspection', 'extrapolation_kissmig')
			
# 			y <- rast_fields[ , ..field_names]
# 			y <- y[ , lapply(.SD, as.numeric)]
# 			y <- rowSums(y)
# 			y <- y > 0
# 			y[y] <- 'Yes'
# 			y[y == 'FALSE'] <- 'No'
		
# 			results <- contingency_test(step = step, nice = nice, y = y, clusters = clusters, match_on = match_on, time_period = time_period)

# 		### taxonomy: accounted for subspecies
			
# 			step <- '1 Data'
# 			if (species_focal == 'Priona') {

# 				nice <- 'Modeled only mainland subspecies'
# 				match_on <- 'team'

# 				y <- as.numeric(team_fields$taxonomy_mainland_only)
# 				y[y == 1] <- 'Yes'
# 				y[y == '0'] <- 'No'

# 				results <- contingency_test(step = step, nice = nice, y = y, clusters = clusters, match_on = match_on)
			
# 				# plot
# 				y[y == 1] <- 'Mainland'
# 				y[y == 0] <- 'Mainland +\ninsular'
# 				y_type <- 'factor'
# 				legend <- 'Taxonomy'
# 				trans <- NA

# 				panels[[length(panels) + 1]] <- add_attribute_to_biplot_with_clusters(biplot = biplot, match_on = match_on, y = y, y_type = y_type, trans = trans, nice = nice, legend = legend, clusters = clusters, scores = scores)

# 			} # if Priona

# 		panels_page_1 <- plot_grid(plotlist = panels[1:18], nrow = 6, ncol = 3)
# 		panels_page_2 <- plot_grid(plotlist = panels[19:length(panels)], nrow = 5, ncol = 3)
# 		# panels_page_3 <- plot_grid(plotlist = panels[25:length(panels)], nrow = 4, ncol = 3)

# 		ggsave(panels_page_1, filename = paste0(out_dir, '/Cluster Analysis of Rasters/Workflow Attributes by Cluster k ', k, ' Page 1 .png'), width = 8.5, height = 11, bg = 'white')

# 		ggsave(panels_page_2, filename = paste0(out_dir, '/Cluster Analysis of Rasters/Workflow Attributes by Cluster k ', k, ' Page 2.png'), width = 8.5, height = 8, bg = 'white')
		
# 		# ggsave(panels_page_3, filename = paste0(out_dir, '/Cluster Analysis of Rasters/Workflow Attributes Page 3 by Cluster k ', k, '.png'), width = 8.5, height = 8, bg = 'white')

# 	} # next number of clusters
# 	fwrite(results, paste0(out_dir, '/Cluster Analysis of Rasters/Associations between Team Clusters and Workflow Attributes.csv'))

# say('############################################################################################')
# say('### reshape results associating workflow attributes with clusters into table for display ###')
# say('############################################################################################')

# 	# Reformat output of cluster-workflow attribute analysis so that we can print it neatly. Left few columns have information on decisions, right side has one column per number of clusters (k = 2, 3, 4, ...), with cell values indicating significance.

# 	results <- fread(paste0(out_dir, '/Cluster Analysis of Rasters/Associations between Team Clusters and Workflow Attributes.csv'))
# 	results <- results[order(step, nice)]

# 	ks <- get_ks()
# 	if (exists('full_reshape')) rm(full_reshape)
# 	for (this_k in ks) {
		
# 		this_reshape <- results[results$k == this_k]
# 		this_reshape[ , c('k', 'n_na', 'time_period', 'kw_chi_sq', 'p_value', 'categories') := NULL]
# 		this_reshape[ , DUMMY := significant]
# 		this_reshape[ , significant := NULL]
# 		names(this_reshape)[ncol(this_reshape)] <- paste0('k = ', this_k)
# 		if (exists('full_reshape')) this_reshape[ , c('species', 'step', 'nice', 'test') := NULL]

# 		if (exists('full_reshape')) {
# 			full_reshape <- cbind(full_reshape, this_reshape)
# 		} else {
# 			full_reshape <- this_reshape
# 		}

# 	}

# 	fwrite(full_reshape, paste0(out_dir, '/Cluster Analysis of Rasters/Associations between Team Clusters and Workflow Attributes Reshaped.csv'))

# say('###############################################################################################################################')
# say('### univariate Mantel and PERMANOVA tests between distances between rasters in PCA space and individual workflow attributes ###')
# say('###############################################################################################################################')

# 	# Implement a Mantel test between each workflow attribute and distances in PCA space. Use Gower distance for calculating distance matrix of each attribute.

# 	# nperm <- 99 # number of Mantel permutations
# 	nperm <- 99999 # number of Mantel permutations

# 	### MAIN
# 	########

# 	this_out_dir <- paste0(out_dir, '/Raster Distances ~ Decision Distances')
# 	dirCreate(this_out_dir)

# 	rast_fields <- load_rast_fields(species_focal = species_focal)
# 	team_fields <- load_team_fields(species_focal = species_focal)

# 	### cluster
# 	preds <- load_predictions(species_focal = species_focal, period = 'all', scale = TRUE, subset_teams = TRUE)
# 	preds_trans <- t(preds)

# 	pca <- prcomp(preds_trans)
# 	scores <- pca$x[ , 1:2]
# 	scores <- as.data.frame(scores)

# 	rast_dists <- dist(scores)

# 	logp10 <- function(x) log10(x + 1)

# 	### base plot of rasters
# 	var1 <- sprintf('%.1f', 100 * pca$sdev[1]^2 / sum(pca$sdev^2))
# 	var2 <- sprintf('%.1f', 100 * pca$sdev[2]^2 / sum(pca$sdev^2))

# 	x_lab <- paste0('PC 1 (', var1, '%)')
# 	y_lab <- paste0('PC 2 (', var2, '%)')

# 	pcs <- scores
# 	pcs$raster_period <- rownames(pcs)

# 	pcs$raster <- pcs$raster_period
# 	pcs$raster <- sub(pcs$raster, pattern = '_present', replacement = '')
# 	pcs$raster <- sub(pcs$raster, pattern = '_mid', replacement = '')
# 	pcs$raster <- sub(pcs$raster, pattern = '_late', replacement = '')

# 	pcs$period <- NA_character_
# 	pcs$period[grepl(pcs$raster_period, pattern = '_present')] <- 'Present'
# 	pcs$period[grepl(pcs$raster_period, pattern = '_mid')] <- 'Mid'
# 	pcs$period[grepl(pcs$raster_period, pattern = '_late')] <- 'Late'

# 	pcs <- as.data.table(pcs)

# 	base_biplot <- ggplot() +
# 		coord_cartesian(clip = 'off') +
# 		coord_fixed() +
# 		theme_minimal() +
# 		theme(
# 			# legend.position = 'none',
# 			plot.title = element_text(size = 18),
# 			axis.title = element_text(size = 13),
# 			axis.text = element_text(size = 10),
# 			legend.title = element_text(size = 12),
# 			legend.text = element_text(size = 10)
# 		)

# 	# Match workflow attribute scores to each raster, calculate Gower distances, and execute Mantel test
# 	#
# 	# y				Values to match to rasters
# 	# match_on		"team" or "raster"
# 	# rast_dists 	Distances between rasters in PCA space
# 	# nperm			Number of Mantel permutations
# 	# results		Data table with results
# 	# step			Description of attribute
# 	# nice			Nice description of attribute
# 	# trans			If NA, do not transform values. Otherwise, transformation function like log or log10 (the actual function, not a character naming it)
# 	# plot_type		'categorical' or 'numeric'
# 	# display_legend TRUE/FALSE
# 	# legend_title  Title for legend
# 	# results		List with biplots and data.table.
# 	#
# 	# Returns results data.table with results for this attribute added
# 	do_analysis <- function(y, match_on, rast_dists, nperm, results, step, nice, trans, plot_type, display_legend, legend_title) {
	
# 		say(nice)	

# 		### Mantel
# 		##########

# 		if (match_on == 'team') {
# 			names(y) <- team_fields$team_code
# 			scores_teams <- rownames(scores)
# 			scores_teams <- substr(scores_teams, 1, 1)
# 			index <- match(scores_teams, team_fields$team_code)
# 		} else if (match_on == 'raster') {
# 			names(y) <- team_fields$raster_name
# 			scores_rasters <- rownames(scores)
# 			raster_name_time_period <- paste0(rast_fields$raster_name, '_', rast_fields$time_period)
# 			index <- match(scores_rasters, raster_name_time_period)
# 		}
# 		y_match <- y[index]
# 		y_match <- data.table(y_match = y_match)
# 		if (is.character(y_match[[1]])) y_match[[1]] <- factor(y_match[[1]])

# 		if (is.function(trans)) y_match <- trans(y_match)
		
# 		this_pcs <- copy(pcs)
# 		this_rast_dists <- rast_dists
# 		if (anyNA(y_match)) {
# 			bads <- which(is.na(y_match))
# 			y_match <- y_match[-bads]
# 			this_rast_dists <- this_rast_dists[-bads, -bads]
# 			this_rast_dists <- as.dist(this_rast_dists)
# 			this_pcs <- this_pcs[-bads]
# 			n_removed <- length(bads)
# 		} else {
# 			n_removed <- 0
# 		}

# 		att_dists <- cluster::daisy(y_match, metric = 'gower')
# 		mant <- vegan::mantel(this_rast_dists, att_dists, method = 'pearson', permutations = nperm)

# 		### PERMANOVA
# 		#############

# 		y_match <- y_match[[1]]

# 		# dispersion test
# 		dispersion_test <- betadisper(this_rast_dists, y_match)
# 		aov <- anova(dispersion_test)
# 		permanova_df <- paste0(aov$Df, collapse = ', ')
# 		dispersion_f <- aov$`F value`[1]
# 		dispersion_p <- aov$`Pr(>F)`[1]

# 		# test
# 		perm <- adonis2(this_rast_dists ~ y_match, permutations = nperm)
# 		permanova_r2 <- perm$R2[1]
# 		permanova_p <- perm$`Pr(>F)`[1]

# 		mant_perm <- rbind(
# 			results$mant_perm,
# 			data.table(
# 				step = step,
# 				nice = nice,
# 				n_removed = n_removed,
# 				mantel_stat = mant$statistic,
# 				mantel_p = mant$signif,
# 				mantel_sig = ifelse(mant$signif <= 0.05, '*', '-'),
# 				permanova_p = permanova_p,
# 				permanova_sig = ifelse(permanova_p <= 0.05, '*', '-'),
# 				permanova_r2 = permanova_r2,
# 				permanova_df = permanova_df,
# 				permanova_dispersion_f = dispersion_f,
# 				permanova_dispersion_p = dispersion_p,
# 				permanova_dispersion_sig = ifelse(dispersion_p <= 0.05, '*', '-')
# 			)
# 		)

# 		# plot of PC space with rasters, divided into groups by the workflow attribute

# 		if (plot_type == 'categorical') {

# 			if (all(unique(y_match) %in% c(0, 1))) {
# 				y_match[y_match == 1] <- 'Yes'
# 				y_match[y_match == '0'] <- 'No'
# 			}

# 			this_pcs[ , aspect := y_match]

# 			# one polygon for each group
# 			polys <- list()
# 			unique_clusters <- sort(unique(y_match))
# 			for (i in seq_along(unique_clusters)) {

# 				cluster <- unique_clusters[i]
			
# 				pts <- this_pcs[aspect == cluster, c('PC1', 'PC2')]
# 				if (nrow(pts) >= 3) {
# 					hull_indices <- chull(pts)
# 					hull_coords <- pts[hull_indices, ]
# 					hull_coords[ , aspect := cluster]
# 					polys[[i]] <- hull_coords
# 				} else {
# 					pts[ , aspect := cluster]
# 					polys[[i]] <- pts
# 				}

# 				polys[[i]] <- rbind(polys[[i]], polys[[i]][1, ])
			
# 			}
# 			polys <- rbindlist(polys)
			
# 			r2 <- sprintf('%.2f', round(permanova_r2, 2))
# 			perm_p <- sprintf('%.2f', round(permanova_p, 2))
# 			disp_p <- sprintf('%.2f', round(dispersion_p, 2))
# 			stats_r2 <- bquote(italic('r')^2 * ' = ' * .(r2))
# 			stats_perm_p <- bquote(italic('P')[perm] * ' = ' * .(perm_p))
# 			stats_disp_p <- bquote(italic('P')[disp] * ' = ' * .(disp_p))
# 			stats_df <- data.frame(x = -Inf, y = -Inf, label = as.character(as.expression(c(stats_r2, stats_perm_p, stats_disp_p))))

# 			stats_color <- if (permanova_p <= 0.05 & dispersion_p > 0.05) {
# 				'red'
# 			} else if (permanova_p <= 0.05 & dispersion_p <= 0.05) {
# 				'blue'
# 			} else {
# 				'black'
# 			}

# 			biplot <- base_biplot +
# 				geom_polygon(data = polys, aes(x = PC1, y = PC2, fill = aspect), alpha = 0.5) +
# 				geom_shadowtext(
# 					data = this_pcs, aes(x = PC1, y = PC2, label = raster, color = aspect), 
# 					size = 3,
# 					bg.colour = alpha('black', 0.2), bg.r = 0.05,
# 					fontface = 'bold'
# 				) +
# 				guides(color = guide_legend(override.aes = list(label = 'ABC', size = 5))) +
# 				xlab(x_lab) + ylab(y_lab) +
# 				ggtitle(nice) +
# 				guides(
# 					fill = guide_legend(title = legend_title),
# 					color = guide_legend(title = legend_title, override.aes = list(label = 'AB', size = 5))
# 				) +
# 				geom_text(
# 					data = stats_df[1, ],
# 					aes(x = x, y = y, label = label),
# 					hjust = -0.1,
# 					vjust = -3.3,
# 					parse = TRUE,
# 					size = 4.6,
# 					color = stats_color,
# 					inherit.aes = FALSE
# 				) +
# 				geom_text(
# 					data = stats_df[2, ],
# 					aes(x = x, y = y, label = label),
# 					hjust = -0.1,
# 					vjust = -1.9,
# 					parse = TRUE,
# 					size = 4.6,
# 					color = stats_color,
# 					inherit.aes = FALSE
# 				) +
# 				geom_text(
# 					data = stats_df[3, ],
# 					aes(x = x, y = y, label = label),
# 					hjust = -0.1,
# 					vjust = -0.5,
# 					parse = TRUE,
# 					size = 4.6,
# 					color = stats_color,
# 					inherit.aes = FALSE
# 				)

# 		} else if (plot_type == 'numeric') {

# 			y_match <- as.numeric(y_match)
# 			if (is.function(trans)) {
# 				this_pcs[ , aspect := 10^y_match]
# 			} else {
# 				this_pcs[ , aspect := y_match]
# 			}

# 			r2 <- sprintf('%.2f', round(permanova_r2, 2))
# 			perm_p <- sprintf('%.2f', round(permanova_p, 2))
# 			stats_r2 <- bquote(italic('r')^2 * ' = ' * .(r2))
# 			stats_perm_p <- bquote(italic('P')[perm] * ' = ' * .(perm_p))
# 			stats_df <- data.frame(x = -Inf, y = -Inf, label = as.character(as.expression(c(stats_r2, stats_perm_p))))

# 			stats_color <- if (permanova_p <= 0.05) {
# 				'red'
# 			} else {
# 				'black'
# 			}

# 			biplot <- base_biplot +
# 				geom_point(this_pcs, mapping = aes(x = PC1, y = PC2, fill = aspect), pch = 21, alpha = 0.7, size = 4) +
# 				xlab(x_lab) + ylab(y_lab) +
# 				ggtitle(nice) +
# 				guides(fill = guide_colorbar(title = legend_title, barheight = unit(0.08, 'npc'))) +
#  				geom_text(
# 					data = stats_df[1, ],
# 					aes(x = x, y = y, label = label),
# 					hjust = -0.1,
# 					vjust = -1.9,
# 					parse = TRUE,
# 					size = 4.6,
# 					color = stats_color,
# 					inherit.aes = FALSE
# 				) +
# 				geom_text(
# 					data = stats_df[2, ],
# 					aes(x = x, y = y, label = label),
# 					hjust = -0.1,
# 					vjust = -0.5,
# 					parse = TRUE,
# 					size = 4.6,
# 					color = stats_color,
# 					inherit.aes = FALSE
# 				)

# 			if (is.function(trans)) {

# 				biplot <- biplot + scale_fill_viridis_c(
# 					option = 'magma',
# 					trans = 'log10'
# 				)

# 			} else {

# 				biplot <- biplot + scale_fill_viridis_c(
# 					option = 'magma'
# 				)
			
# 			}
							
# 		}

# 		if (!display_legend) {
			
# 			biplot <- biplot + theme(legend.position = 'none') 

# 		} else {
				
# 			biplot <- biplot +
# 				theme(
# 					legend.position = c(0.98, 0.02),
# 					legend.justification = c('right', 'bottom'),
# 					legend.background = element_rect(fill = alpha('white', 0.5), color = alpha('black', 0.3)),
# 					legend.title = element_text(size = 8),
# 					legend.text = element_text(size = 8),
# 					legend.key.height = unit(0.005, 'npc')
# 				)
			
# 		}

# 		biplot <- biplot +
# 			theme(
# 				plot.title = element_text(size = 13),
# 				axis.title = element_text(size = 9)			
# 			)


# 		out <- list(
# 			biplots = c(results$biplots, biplot),
# 			mant_perm = mant_perm
# 		)

# 		names(out$biplots)[length(out$biplots)] <- nice
# 		out

# 	}
	
# 	### analyze associations between workflow attributes and clusters created by predictions
# 	########################################################################################
	
# 	results <- list(
# 		biplots = list(),
# 		mant_perm = data.table()
# 	)

# 		### add MEAN ODMAP score and MINIMUM SCORE ACROSS CATEGORIES to fields
# 		######################################################################

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

# 		team_fields$odmap_mean <- odmap_means[match(team_fields$team_code, team_codes)]
# 		team_fields$odmap_min <- odmap_mins[match(team_fields$team_code, team_codes)]

# 		## evaluate individual workflow attributes
# 		##########################################

# 		### "team"

# 			step <- '0 Quality/Team'
# 			nice <- 'Team'
# 			match_on <- 'team'
# 			trans <- NA
# 			plot_type <- 'categorical'
# 			display_legend <- FALSE
# 			legend_title <- NULL

# 			y <- team_fields$team_code

# 			results <- do_analysis(y = y, match_on = match_on, rast_dists = rast_dists, nperm = nperm, results = results, step = step, nice = nice, trans = trans, plot_type = plot_type, display_legend = display_legend, legend_title = legend_title)

# 		### "team x thresholding"

# 			step <- '0 Quality/Team'
# 			nice <- 'Team × Continuous/Thresholded'
# 			match_on <- 'raster'
# 			trans <- NA
# 			plot_type <- 'categorical'
# 			display_legend <- FALSE
# 			legend_title <- NULL

# 			y <- rast_fields$team_code
# 			y[rast_fields$raster_name == 'M1'] <- 'Mc'
# 			y[rast_fields$raster_name == 'M2'] <- 'Mt'
# 			if (species_focal == 'Priona') {
# 				y[rast_fields$raster_name %in% c('N1', 'N2')] <- 'Nc'
# 				y[rast_fields$raster_name %in% c('N3', 'N4', 'N3a', 'N4a', 'N3b', 'N4b')] <- 'Nt'
# 			} else if (species_focal == 'Zamia') {
# 				y[rast_fields$raster_name %in% c('N1', 'N2', 'N3', 'N1a', 'N2a', 'N3a', 'N1b', 'N2b', 'N3b')] <- 'Nc'
# 				y[rast_fields$raster_name %in% c('N4', 'N5', 'N6', 'N4a', 'N5a', 'N6a', 'N4b', 'N5b', 'N6b')] <- 'Nt'
# 			}

# 			results <- do_analysis(y = y, match_on = match_on, rast_dists = rast_dists, nperm = nperm, results = results, step = step, nice = nice, trans = trans, plot_type = plot_type, display_legend = display_legend, legend_title = legend_title)

# 		### ODMAP *mean* score

# 			step <- '0 Quality/Team'
# 			nice <- 'SDM Standards: Mean rank'
# 			match_on <- 'team'
# 			trans <- NA
# 			plot_type <- 'numeric'
# 			display_legend <- TRUE
# 			legend_title <- 'Rank'

# 			y <- as.numeric(team_fields$odmap_mean)
			
# 			results <- do_analysis(y = y, match_on = match_on, rast_dists = rast_dists, nperm = nperm, results = results, step = step, nice = nice, trans = trans, plot_type = plot_type, display_legend = display_legend, legend_title = legend_title)

# 		# ### ODMAP *minimum* of mean score... not implementing bc only one team has score > 0 for this factor

# 		# 	step <- '0 Quality/Team'
# 		# 	nice <- 'SDM standards: Minimum rank'
# 		# 	match_on <- 'team'
# 		# 	trans <- NA

# 		# 	y <- as.numeric(team_fields$odmap_min)

# 		# 	results <- do_analysis(y = y, match_on = match_on, rast_dists = rast_dists, nperm = nperm, results = results, step = step, nice = nice, trans = trans, plot_type = plot_type, display_legend = display_legend, legend_title = legend_title)


# 		### number of occurrences

# 			step <- '1 Data'
# 			nice <- 'Occurrences: Number of occurrences'
# 			match_on <- 'team'
# 			trans <- log10
# 			plot_type <- 'numeric'
# 			display_legend <- TRUE
# 			legend_title <- 'Occurrences'

# 			y <- as.numeric(team_fields$num_occurrences_minimum)
# 			results <- do_analysis(y = y, match_on = match_on, rast_dists = rast_dists, nperm = nperm, results = results, step = step, nice = nice, trans = trans, plot_type = plot_type, display_legend = display_legend, legend_title = legend_title)

# 		### climate predictors: number

# 			step <- '1 Data'
# 			nice <- 'Predictors: Total number of climate predictors'
# 			match_on <- 'team'
# 			trans <- log10
# 			plot_type <- 'numeric'
# 			display_legend <- TRUE
# 			legend_title <- 'Occurrences'

# 			y <- team_fields$predictors_climate_num_total
# 			y <- as.numeric(y)

# 			results <- do_analysis(y = y, match_on = match_on, rast_dists = rast_dists, nperm = nperm, results = results, step = step, nice = nice, trans = trans, plot_type = plot_type, display_legend = display_legend, legend_title = legend_title)

# 		### non-climate: ANY non-climate predictors

# 			step <- '1 Data'
# 			nice <- 'Predictors: Non-climate predictors'
# 			match_on <- 'team'
# 			trans <- NA
# 			plot_type <- 'categorical'
# 			display_legend <- TRUE
# 			legend_title <- 'Used\nnon-climatic?'

# 			y <- team_fields$predictors_nonclimate_num > 0
# 			y <- as.numeric(y)

# 			results <- do_analysis(y = y, match_on = match_on, rast_dists = rast_dists, nperm = nperm, results = results, step = step, nice = nice, trans = trans, plot_type = plot_type, display_legend = display_legend, legend_title = legend_title)

# 		### non-climate: number of predictors

# 			step <- '1 Data'
# 			nice <- 'Predictors: Number of non-climate predictors'
# 			match_on <- 'team'
# 			trans <- logp10
# 			plot_type <- 'numeric'
# 			display_legend <- TRUE
# 			legend_title <- 'Predictors'

# 			y <- team_fields$predictors_nonclimate_num
# 			y <- as.numeric(y)

# 			results <- do_analysis(y = y, match_on = match_on, rast_dists = rast_dists, nperm = nperm, results = results, step = step, nice = nice, trans = trans, plot_type = plot_type, display_legend = display_legend, legend_title = legend_title)

# 		### source of climate predictors

# 			step <- '1 Data'
# 			nice <- 'Predictors: Climate data source'
# 			match_on <- 'team'
# 			trans <- NA
# 			plot_type <- 'categorical'
# 			display_legend <- TRUE
# 			legend_title <- 'Source'

# 			y <- team_fields$predictors_climate_source

# 			results <- do_analysis(y = y, match_on = match_on, rast_dists = rast_dists, nperm = nperm, results = results, step = step, nice = nice, trans = trans, plot_type = plot_type, display_legend = display_legend, legend_title = legend_title)

# 		### number of predictors

# 			step <- '1 Data'
# 			nice <- 'Predictors: Total number'
# 			match_on <- 'team'
# 			trans <- log10
# 			plot_type <- 'numeric'
# 			display_legend <- TRUE
# 			legend_title <- 'Predictors'


# 			y <- team_fields$predictors_climate_nonclimate_num_total
# 			y <- as.numeric(y)

# 			results <- do_analysis(y = y, match_on = match_on, rast_dists = rast_dists, nperm = nperm, results = results, step = step, nice = nice, trans = trans, plot_type = plot_type, display_legend = display_legend, legend_title = legend_title)

# 		### spatial resolution: cell size *qualitative*
			
# 			step <- '1 Data'
# 			nice <- 'Spatial resolution (arcmin)'
# 			match_on <- 'team'
# 			trans <- NA
# 			plot_type <- 'categorical'
# 			display_legend <- TRUE
# 			legend_title <- 'Resolution'

# 			y <- team_fields$res_arcmin

# 			results <- do_analysis(y = y, match_on = match_on, rast_dists = rast_dists, nperm = nperm, results = results, step = step, nice = nice, trans = trans, plot_type = plot_type, display_legend = display_legend, legend_title = legend_title)

# 		### spatial resolution: cell size
			
# 			step <- '1 Data'
# 			nice <- 'Spatial resolution (km2)'
# 			match_on <- 'team'
# 			trans <- log10
# 			plot_type <- 'numeric'
# 			display_legend <- TRUE
# 			legend_title <- bquote('Resolution (km'^2 * ')')

# 			y <- as.numeric(team_fields$res_km2)

# 			results <- do_analysis(y = y, match_on = match_on, rast_dists = rast_dists, nperm = nperm, results = results, step = step, nice = nice, trans = trans, plot_type = plot_type, display_legend = display_legend, legend_title = legend_title)

# 		### collinearity: managed at all

# 			step <- '2 Model setup'
# 			nice <- 'Collinearity: Explicitly managed'
# 			match_on <- 'team'
# 			trans <- NA
# 			plot_type <- 'categorical'
# 			display_legend <- TRUE
# 			legend_title <- 'Managed?'

# 			field_names <- c('collinearity_pca', 'collinearity_correlation', 'collinearity_vif', 'collinearity_other_method')
# 			y <- team_fields[ , ..field_names]
# 			y <- y[ , lapply(.SD, as.numeric)]

# 			y <- rowSums(y)
# 			y <- as.numeric(y > 0)
# 			y[y == 1] <- 'Managed'
# 			y[y == '0'] <- 'Not managed'

# 			results <- do_analysis(y = y, match_on = match_on, rast_dists = rast_dists, nperm = nperm, results = results, step = step, nice = nice, trans = trans, plot_type = plot_type, display_legend = display_legend, legend_title = legend_title)


# 		### collinearity: method

# 			step <- '2 Model setup'
# 			nice <- 'Collinearity: Method of management'
# 			match_on <- 'team'
# 			trans <- NA
# 			plot_type <- 'categorical'
# 			display_legend <- TRUE
# 			legend_title <- 'Method'

# 			field_names <- c('collinearity_pca', 'collinearity_correlation', 'collinearity_vif', 'collinearity_other_method')
# 			y <- team_fields[ , ..field_names]
# 			y <- y[ , lapply(.SD, as.numeric)]
# 			y <- apply(y, 1, function(row) {
# 				cols_with_1 <- field_names[row == 1]
# 				if (length(cols_with_1) == 0) return(NA)
# 				paste(cols_with_1, collapse = ", ")
# 			})
# 			y <- replace_y_NAs(y = y, field_names = field_names)

# 			y <- sub(y, pattern = 'collinearity_pca', replacement = 'PCA')
# 			y <- sub(y, pattern = 'collinearity_correlation', replacement = 'Corr.')
# 			y <- sub(y, pattern = 'collinearity_vif', replacement = 'VIF')
# 			y <- sub(y, pattern = 'collinearity_other_method', replacement = 'Other')

# 			results <- do_analysis(y = y, match_on = match_on, rast_dists = rast_dists, nperm = nperm, results = results, step = step, nice = nice, trans = trans, plot_type = plot_type, display_legend = display_legend, legend_title = legend_title)

# 		### modeling_software

# 			step <- '3 Modeling software'
# 			nice <- 'Software'
# 			match_on <- 'team'
# 			trans <- NA
# 			plot_type <- 'categorical'
# 			display_legend <- TRUE
# 			legend_title <- 'Software'

# 			field_names <- c('modeling_software_enmeval', 'modeling_software_enmtools', 'modeling_software_wallace', 'modeling_software_biomod2', 'modeling_software_sabinansdm', 'modeling_software_sdm', 'modeling_software_miamaxent', 'modeling_software_enmsdmx', 'modeling_software_flexsdm', 'modeling_software_sdmtune', 'modeling_software_piecemeal', 'modeling_software_other')

# 			y <- team_fields[ , ..field_names]
# 			y <- y[ , lapply(.SD, as.numeric)]
# 			y <- apply(y, 1, function(row) {
# 				cols_with_1 <- field_names[row == 1]
# 				if (length(cols_with_1) == 0) return(NA)
# 				paste(cols_with_1, collapse = ", ")
# 			})
# 			y <- replace_y_NAs(y = y, field_names = field_names)

# 			y <- sub(y, pattern = 'modeling_software_enmeval', replacement = 'ENMeval')
# 			y <- sub(y, pattern = 'modeling_software_enmtools', replacement = 'ENMTools')
# 			y <- sub(y, pattern = 'modeling_software_wallace', replacement = 'Wallace')
# 			y <- sub(y, pattern = 'modeling_software_biomod2', replacement = 'BIOMOD2')
# 			y <- sub(y, pattern = 'modeling_software_sabinansdm', replacement = 'sabinaNSDM')
# 			y <- sub(y, pattern = 'modeling_software_sdm', replacement = 'sdm')
# 			y <- sub(y, pattern = 'modeling_software_miamaxent', replacement = 'MIAmaxent')
# 			y <- sub(y, pattern = 'modeling_software_enmsdmx', replacement = 'enmSdmX')
# 			y <- sub(y, pattern = 'modeling_software_flexsdm', replacement = 'flexsdm')
# 			y <- sub(y, pattern = 'modeling_software_sdmtune', replacement = 'SDMtune')
# 			y <- sub(y, pattern = 'modeling_software_other', replacement = 'other')
# 			y <- sub(y, pattern = 'modeling_software_piecemeal', replacement = 'piecemeal')

# 			results <- do_analysis(y = y, match_on = match_on, rast_dists = rast_dists, nperm = nperm, results = results, step = step, nice = nice, trans = trans, plot_type = plot_type, display_legend = display_legend, legend_title = legend_title)


# 		### software_used_by_team_developing_it

# 			step <- '3 Software: Developers of the software'
# 			nice <- 'Software use by developers'
# 			trans <- NA
# 			match_on <- 'raster'
# 			plot_type <- 'categorical'
# 			display_legend <- TRUE
# 			legend_title <- 'Developer?'

# 			y <- rep('No', nrow(rast_fields))
# 			y[rast_fields$team_code %in% c('E', 'B', 'C', 'H', 'D')] <- 'Yes'

# 			results <- do_analysis(y = y, match_on = match_on, rast_dists = rast_dists, nperm = nperm, results = results, step = step, nice = nice, trans = trans, plot_type = plot_type, display_legend = display_legend, legend_title = legend_title)

# 		# ### modeling_software: ENVeval / Wallace # not using this one bc using Wallace does not necessarily mean same tools as in ENMeval were used

# 		# 	step <- '3 Model algorithm'
# 		# 	nice <- 'Software: Used ENMeval/Wallace'
# 		# 	match_on <- 'team'
# 		# 	trans <- NA
# 		# 	plot_type <- 'categorical'
# 		# 	display_legend <- TRUE
# 		# 	legend_title <- 'ENMeval/Wallace'

# 		# 	set_names <- c('modeling_software_enmeval', 'modeling_software_wallace')
# 		# 	y <- team_fields [ , ..set_names]
# 		# 	y <- y[ , lapply(.SD, as.numeric)]
# 		# 	y <- as.numeric(y > 0)
# 		# 	y[y == 1] <- 'ENMeval/Wallace'
# 		# 	y[y == '0'] <- 'Other'

# 		# 	results <- do_analysis(y = y, match_on = match_on, rast_dists = rast_dists, nperm = nperm, results = results, step = step, nice = nice, trans = trans, plot_type = plot_type, display_legend = display_legend, legend_title = legend_title)

# 		### algorithm: used ensemble
		
# 			step <- '3 Model algorithm'
# 			nice <- 'Algorithm: Used ensemble'
# 			match_on <- 'raster'
# 			trans <- NA
# 			plot_type <- 'categorical'
# 			display_legend <- TRUE
# 			legend_title <- 'Ensemble?'

# 			y_star <- rast_fields$algo_ensemble
# 			y_star <- as.numeric(y_star)
# 			y <- rep(NA, nrow(rast_fields))
# 			y[y_star == 1] <- 'Yes'
# 			y[y_star == 0] <- 'No'

# 			results <- do_analysis(y = y, match_on = match_on, rast_dists = rast_dists, nperm = nperm, results = results, step = step, nice = nice, trans = trans, plot_type = plot_type, display_legend = display_legend, legend_title = legend_title)

# 		### algorithm: number of algorithms used in ensemble (including 0)
			
# 			step <- '3 Model algorithm'
# 			nice <- 'Algorithm: Number of algorithms in ensemble'
# 			match_on <- 'raster'
# 			trans <- NA
# 			plot_type <- 'numeric'
# 			display_legend <- TRUE
# 			legend_title <- 'Algorithms'

# 			y <- rast_fields$algo_ensemble_number_of_models
# 			y <- as.numeric(y)

# 			results <- do_analysis(y = y, match_on = match_on, rast_dists = rast_dists, nperm = nperm, results = results, step = step, nice = nice, trans = trans, plot_type = plot_type, display_legend = display_legend, legend_title = legend_title)


# 		### algorithm: identity

# 			step <- '3 Model algorithm'
# 			nice <- 'Algorithm'
# 			match_on <- 'raster'
# 			trans <- NA
# 			plot_type <- 'categorical'
# 			display_legend <- TRUE
# 			legend_title <- 'Algorithm'

# 			field_names <- c('algo_ensemble', 'algo_maxent', 'algo_maxnet', 'algo_glm', 'algo_gam', 'algo_rf', 'algo_sre')

# 			y <- rast_fields[ , ..field_names]
# 			y <- y[ , lapply(.SD, as.numeric)]
# 			y <- apply(y, 1, function(row) {
# 				cols_with_1 <- field_names[row == 1]
# 				if (length(cols_with_1) == 0) return(NA)
# 				paste(cols_with_1, collapse = ", ")
# 			})
# 			y <- replace_y_NAs(y = y, field_names = field_names)

# 			y[y == 'algo_ensemble'] <- 'Ensemble'
# 			y[y == 'algo_maxent'] <- 'MaxEnt'
# 			y[y == 'algo_maxnet'] <- 'MaxNet'
# 			y[y == 'algo_glm'] <- 'GLM'
# 			y[y == 'algo_gam'] <- 'GAM'
# 			y[y == 'algo_rf'] <- 'RF'
# 			y[y == 'algo_sre'] <- 'SRE'

# 			results <- do_analysis(y = y, match_on = match_on, rast_dists = rast_dists, nperm = nperm, results = results, step = step, nice = nice, trans = trans, plot_type = plot_type, display_legend = display_legend, legend_title = legend_title)

# 		### algorithm: MaxEnt / MaxNet

# 			step <- '3 Model algorithm'
# 			nice <- 'Algorithm: Used MaxEnt/MaxNet'
# 			matchOn <- 'raster'
# 			trans <- NA
# 			plot_type <- 'categorical'
# 			display_legend <- TRUE
# 			legend_title <- 'MaxEnt/MaxNet?'

# 			set_names <- c('algo_maxent', 'algo_maxnet')
# 			y <- rast_fields[ , ..set_names]
# 			y <- y[ , lapply(.SD, as.numeric)]
# 			y <- rowSums(y)
# 			y[y == 1] <- 'MaxEnt/Net'
# 			y[y == '0'] <- 'Other'

# 			results <- do_analysis(y = y, match_on = match_on, rast_dists = rast_dists, nperm = nperm, results = results, step = step, nice = nice, trans = trans, plot_type = plot_type, display_legend = display_legend, legend_title = legend_title)

# 		### bias correction: did any
			
# 			step <- '2 Model setup'
# 			nice <- 'Bias correction: Implemented'
# 			match_on <- 'team'
# 			trans <- NA
# 			plot_type <- 'categorical'
# 			display_legend <- TRUE
# 			legend_title <- 'Implemented?'

# 			field_names <- c('bias_correction_spatial_thinning', 'bias_correction_environmental_thinning', 'bias_correction_target_background', 'bias_correction_nonrandom_background')
			
# 			y_star <- team_fields[ , ..field_names]
# 			y_star <- y_star[ , lapply(.SD, as.numeric)]
# 			y_star <- rowSums(y_star)
# 			y_star <- y_star > 0
# 			y <- rep(NA, nrow(rast_fields))
# 			y[y_star] <- 'Yes'
# 			y[!y_star] <- 'No'

# 			results <- do_analysis(y = y, match_on = match_on, rast_dists = rast_dists, nperm = nperm, results = results, step = step, nice = nice, trans = trans, plot_type = plot_type, display_legend = display_legend, legend_title = legend_title)


# 		### bias correction: method

# 			step <- '2 Model setup'
# 			nice <- 'Bias correction: Method'
# 			match_on <- 'team'
# 			trans <- NA
# 			plot_type <- 'categorical'
# 			display_legend <- TRUE
# 			legend_title <- 'Method'

# 			field_names <- c('bias_correction_spatial_thinning', 'bias_correction_environmental_thinning', 'bias_correction_target_background', 'bias_correction_nonrandom_background', 'bias_correction_none')

# 			y <- team_fields[ , ..field_names]
# 			y <- y[ , lapply(.SD, as.numeric)]
# 			y <- apply(y, 1, function(row) {
# 				cols_with_1 <- field_names[row == 1]
# 				if (length(cols_with_1) == 0) return(NA)
# 				paste(cols_with_1, collapse = ", ")
# 			})
# 			y <- replace_y_NAs(y = y, field_names = field_names)

# 			y <- sub(y, pattern = 'bias_correction_spatial_thinning', replacement = 'Spatial')
# 			y <- sub(y, pattern = 'bias_correction_environmental_thinning', replacement = 'Env. thin')
# 			y <- sub(y, pattern = 'bias_correction_target_background', replacement = 'Target')
# 			y <- sub(y, pattern = 'bias_correction_nonrandom_background', replacement = 'NR BG')
# 			y <- sub(y, pattern = 'bias_correction_none', replacement = 'None')
# 			y[is.na(y)] <- 'Unknown'

# 			results <- do_analysis(y = y, match_on = match_on, rast_dists = rast_dists, nperm = nperm, results = results, step = step, nice = nice, trans = trans, plot_type = plot_type, display_legend = display_legend, legend_title = legend_title)

# 		### non-presence type

# 			step <- '2 Model setup'
# 			nice <- 'Non-presence type'
# 			match_on <- 'team'
# 			trans <- NA
# 			plot_type <- 'categorical'
# 			display_legend <- TRUE
# 			legend_title <- 'Type'

# 			# non-presences: type
# 			field_names <- c('nonpres_type_background', 'nonpres_type_pseudoabsence', 'nonpres_type_target_background')

# 			y <- team_fields[ , ..field_names]
# 			y <- y[ , lapply(.SD, as.numeric)]
# 			y <- apply(y, 1, function(row) {
# 				cols_with_1 <- field_names[row == 1]
# 				if (length(cols_with_1) == 0) return(NA)
# 				paste(cols_with_1, collapse = ", ")
# 			})
# 			y <- replace_y_NAs(y = y, field_names = field_names)

# 			y <- sub(y, pattern = 'nonpres_type_background', replacement = 'Random')
# 			y <- sub(y, pattern = 'nonpres_type_pseudoabsence', replacement = 'PSA')
# 			y <- sub(y, pattern = 'nonpres_type_target_background', replacement = 'Target')
		
# 			results <- do_analysis(y = y, match_on = match_on, rast_dists = rast_dists, nperm = nperm, results = results, step = step, nice = nice, trans = trans, plot_type = plot_type, display_legend = display_legend, legend_title = legend_title)

# 		### calibration region boundary
		
# 			step <- '2 Model setup'
# 			nice <- 'Calibration region: Boundary definition'
# 			match_on <- 'team'
# 			trans <- NA
# 			plot_type <- 'categorical'
# 			display_legend <- TRUE
# 			legend_title <- 'Method'

# 			# non-presences: type
# 			field_names <- c('boundary_rectangle', 'boundary_natural', 'boundary_convex_hull', 'boundary_range_map', 'boundary_political', 'boundary_buffer_around_occurrences')

# 			y <- team_fields[ , ..field_names]
# 			y <- y[ , lapply(.SD, as.numeric)]
# 			y <- apply(y, 1, function(row) {
# 				cols_with_1 <- field_names[row == 1]
# 				if (length(cols_with_1) == 0) return(NA)
# 				paste(cols_with_1, collapse = ", ")
# 			})
# 			y <- replace_y_NAs(y = y, field_names = field_names)

# 			y <- sub(y, pattern = 'boundary_rectangle', replacement = 'Rect.')
# 			y <- sub(y, pattern = 'boundary_natural', replacement = 'Natural')
# 			y <- sub(y, pattern = 'boundary_convex_hull', replacement = 'Hull')
# 			y <- sub(y, pattern = 'boundary_range_map', replacement = 'Range')
# 			y <- sub(y, pattern = 'boundary_political', replacement = 'Polit.')
# 			y <- sub(y, pattern = 'boundary_buffer_around_occurrences', replacement = 'Buffer')

# 			results <- do_analysis(y = y, match_on = match_on, rast_dists = rast_dists, nperm = nperm, results = results, step = step, nice = nice, trans = trans, plot_type = plot_type, display_legend = display_legend, legend_title = legend_title)


# 		### calibration region extent
			
# 			step <- '2 Model setup'
# 			nice <- 'Calibration region: Area'
# 			match_on <- 'team'
# 			trans <- log10
# 			plot_type <- 'numeric'
# 			display_legend <- TRUE
# 			legend_title <- bquote('Area (km'^2 * ')')

# 			y <- as.numeric(team_fields$extent_calibration_sans_water_km2)

# 			results <- do_analysis(y = y, match_on = match_on, rast_dists = rast_dists, nperm = nperm, results = results, step = step, nice = nice, trans = trans, plot_type = plot_type, display_legend = display_legend, legend_title = legend_title)

		
# 		### AUC
			
# 			step <- '4 Model evaluation'
# 			nice <- 'Evaluation: Value of AUC'
# 			match_on <- 'raster'
# 			trans <- NA
# 			plot_type <- 'numeric'
# 			display_legend <- TRUE
# 			legend_title <- 'AUC'

# 			y <- rast_fields$eval_metric_auc_roc_value
# 			y <- as.numeric(y)

# 			results <- do_analysis(y = y, match_on = match_on, rast_dists = rast_dists, nperm = nperm, results = results, step = step, nice = nice, trans = trans, plot_type = plot_type, display_legend = display_legend, legend_title = legend_title)


# 		### thresholded predictions

# 			step <- '5 Prediction/projection'
# 			nice <- 'Predictions: Continuous/thresholded'
# 			match_on <- 'raster'
# 			trans <- NA
# 			plot_type <- 'categorical'
# 			display_legend <- TRUE
# 			legend_title <- 'Output'

# 			field_names <- c('prediction_type_continuous', 'prediction_type_binary_threshold', 'prediction_type_multi_threshold')

# 			y <- rast_fields[ , ..field_names]
# 			y <- y[ , lapply(.SD, as.numeric)]
# 			y <- apply(y, 1, function(row) {
# 				cols_with_1 <- field_names[row == 1]
# 				if (length(cols_with_1) == 0) return(NA)
# 				paste(cols_with_1, collapse = ", ")
# 			})
# 			y <- replace_y_NAs(y = y, field_names = field_names)

# 			y <- sub(y, pattern = 'prediction_type_continuous', replacement = 'Continuous')
# 			y <- sub(y, pattern = 'prediction_type_binary_threshold', replacement = 'Binary Thresh.')
# 			y <- sub(y, pattern = 'prediction_type_multi_threshold', replacement = 'Multiple Thresh.')

# 			results <- do_analysis(y = y, match_on = match_on, rast_dists = rast_dists, nperm = nperm, results = results, step = step, nice = nice, trans = trans, plot_type = plot_type, display_legend = display_legend, legend_title = legend_title)


# 		### time period

# 			step <- '5 Prediction/projection'
# 			nice <- 'Projection: Time period'
# 			match_on <- 'raster'
# 			trans <- NA
# 			plot_type <- 'categorical'
# 			display_legend <- TRUE
# 			legend_title <- 'Period'

# 			y <- rast_fields$time_period

# 			results <- do_analysis(y = y, match_on = match_on, rast_dists = rast_dists, nperm = nperm, results = results, step = step, nice = nice, trans = trans, plot_type = plot_type, display_legend = display_legend, legend_title = legend_title)


# 		### late-century time period
# 		# present and mid-century time period are redundant with climate data source, so not doing them

# 			step <- '5 Prediction/projection'
# 			nice <- 'Projection: Late 20th-century time period'
# 			match_on <- 'team'
# 			time_period <- 'late'
# 			trans <- NA
# 			plot_type <- 'categorical'
# 			display_legend <- TRUE
# 			legend_title <- 'Period'

# 			y <- team_fields$future_scenario_latecentury_year

# 			results <- do_analysis(y = y, match_on = match_on, rast_dists = rast_dists, nperm = nperm, results = results, step = step, nice = nice, trans = trans, plot_type = plot_type, display_legend = display_legend, legend_title = legend_title)


# 		### future: emission scenario
			
# 			step <- '5 Prediction/projection'
# 			nice <- 'Projection: Climate scenario'
# 			match_on <- 'raster'
# 			time_period <- 'future'
# 			trans <- NA
# 			plot_type <- 'categorical'
# 			display_legend <- TRUE
# 			legend_title <- 'Scenario'

# 			field_names <- c('future_scenario_ensemble', 'future_scenario_ssp126', 'future_scenario_ssp245', 'future_scenario_ssp370', 'future_scenario_ssp585', 'future_scenario_rcp45', 'future_scenario_rcp85')

# 			y <- rast_fields[ , ..field_names]
# 			y <- y[ , lapply(.SD, as.numeric)]
# 			y <- apply(y, 1, function(row) {
# 				cols_with_1 <- field_names[row == 1]
# 				if (length(cols_with_1) == 0) return(NA)
# 				paste(cols_with_1, collapse = ", ")
# 			})
# 			y <- replace_y_NAs(y = y, field_names = field_names)

# 			y <- sub(y, pattern = 'future_scenario_ensemble', replacement = 'Ens. future')
# 			y <- sub(y, pattern = 'future_scenario_ssp126', replacement = 'SSP 126')
# 			y <- sub(y, pattern = 'future_scenario_ssp245', replacement = 'SSP 245')
# 			y <- sub(y, pattern = 'future_scenario_ssp370', replacement = 'SSP 370')
# 			y <- sub(y, pattern = 'future_scenario_ssp585', replacement = 'SSP 585')
# 			y <- sub(y, pattern = 'future_scenario_rcp45', replacement = 'RCP 4.5')
# 			y <- sub(y, pattern = 'future_scenario_rcp85', replacement = 'RCP 8.5')

# 			y[rast_fields$time_period == 'present'] <- 'Present'

# 			results <- do_analysis(y = y, match_on = match_on, rast_dists = rast_dists, nperm = nperm, results = results, step = step, nice = nice, trans = trans, plot_type = plot_type, display_legend = display_legend, legend_title = legend_title)

# 		### extrapolation: individual methods

# 			step <- '5 Prediction/projection'
# 			nice <- 'Extrapolation: Method of management'
# 			match_on <- 'raster'
# 			time_period <- 'future'
# 			trans <- NA
# 			plot_type <- 'categorical'
# 			display_legend <- TRUE
# 			legend_title <- 'Method'

# 			field_names <- c('extrapolation_clamping_masking_clipping', 'extrapolation_exdet', 'extrapolation_mess', 'extrapolation_shape', 'extrapolation_area_of_applicability', 'extrapolation_response_curve_inspection', 'extrapolation_kissmig', 'extrapolation_no_measures')

# 			y <- rast_fields[ , ..field_names]
# 			y <- y[ , lapply(.SD, as.numeric)]
# 			y <- apply(y, 1, function(row) {
# 				cols_with_1 <- field_names[row == 1]
# 				if (length(cols_with_1) == 0) return(NA)
# 				paste(cols_with_1, collapse = ", ")
# 			})
# 			y <- replace_y_NAs(y = y, field_names = field_names)

# 			y <- sub(y, pattern = 'extrapolation_clamping_masking_clipping', replacement = 'Mask/clip')
# 			y <- sub(y, pattern = 'extrapolation_exdet', replacement = 'ExDet')
# 			y <- sub(y, pattern = 'extrapolation_mess', replacement = 'MESS')
# 			y <- sub(y, pattern = 'extrapolation_shape', replacement = 'shape')
# 			y <- sub(y, pattern = 'extrapolation_area_of_applicability', replacement = 'AOA')
# 			y <- sub(y, pattern = 'extrapolation_response_curve_inspection', replacement = 'Resp. cur.')
# 			y <- sub(y, pattern = 'extrapolation_kissmig', replacement = 'KISSMig')
# 			y <- sub(y, pattern = 'extrapolation_no_measures', replacement = 'None')

# 			results <- do_analysis(y = y, match_on = match_on, rast_dists = rast_dists, nperm = nperm, results = results, step = step, nice = nice, trans = trans, plot_type = plot_type, display_legend = display_legend, legend_title = legend_title)


# 		### extrapolation: any method

# 			step <- '5 Prediction/projection'
# 			nice <- 'Extrapolation: Any method'
# 			match_on <- 'raster'
# 			trans <- NA
# 			plot_type <- 'categorical'
# 			display_legend <- TRUE
# 			legend_title <- 'Managed?'

# 			field_names <- c('extrapolation_clamping_masking_clipping', 'extrapolation_exdet', 'extrapolation_mess', 'extrapolation_shape', 'extrapolation_area_of_applicability', 'extrapolation_response_curve_inspection', 'extrapolation_kissmig')
			
# 			y <- rast_fields[ , ..field_names]
# 			y <- y[ , lapply(.SD, as.numeric)]
# 			y <- rowSums(y)
# 			y <- y > 0
# 			y[y] <- 'Yes'
# 			y[y == 'FALSE'] <- 'No'
		
# 			results <- do_analysis(y = y, match_on = match_on, rast_dists = rast_dists, nperm = nperm, results = results, step = step, nice = nice, trans = trans, plot_type = plot_type, display_legend = display_legend, legend_title = legend_title)


# 		### taxonomy: accounted for subspecies
			
# 			step <- '1 Data'
# 			if (species_focal == 'Priona') {

# 				nice <- 'Modeled only mainland subspecies'
# 				match_on <- 'team'
# 				trans <- NA
# 				plot_type <- 'categorical'
# 				display_legend <- TRUE
# 				legend_title <- 'Mainland\nOnly?'

# 				y <- as.numeric(team_fields$taxonomy_mainland_only)
# 				y[y == 1] <- 'Yes'
# 				y[y == '0'] <- 'No'

# 				results <- do_analysis(y = y, match_on = match_on, rast_dists = rast_dists, nperm = nperm, results = results, step = step, nice = nice, trans = trans, plot_type = plot_type, display_legend = display_legend, legend_title = legend_title)


# 			} # if Priona

# 	fwrite(results$mant_perm, paste0(out_dir, '/Raster Distances ~ Decision Distances/Univariate Mantel and PERMANOVA on Distance between Rasters and Workflows.csv'))

# 	### compile plots... 8 per "page"
# 	n_plots_per_page <- 8
# 	n_plots <- length(results$biplots)
# 	pages <- ceiling(n_plots / n_plots_per_page)
# 	for (page in 1:pages) {
	
# 		indices <- (n_plots_per_page * (page - 1) + 1):(min(n_plots_per_page * (page - 1) + n_plots_per_page, length(results$biplots)))

# 		biplots <- results$biplots[indices]
# 		biplots <- plot_grid(plotlist = biplots, ncol = 2, align = 'hv')

# 		mod <- length(indices) %% n_plots_per_page
# 		if (mod == 0) {
# 			height <- 11
# 		} else {
# 			height <- 11 * (ceiling(mod / 2) / (n_plots_per_page / 2))
# 		}

# 		ggsave(biplots, filename = paste0(this_out_dir, '/PERMANOVA Plots Page ', page, '.png'), dpi = 600, height = height, width = 8.5, bg = 'white')

# 	}

say('##################################################################################################')
say('### make PCA biplots with rasters coded by select workflow attributes for figures in main text ###')
say('##################################################################################################')

	# Make individual plots of rasters in PC space and annotate with selected, individual workflow attributes. Good for presentation.

	this_out_dir <- paste0(out_dir, '/Raster Distances ~ Decision Distances')
	dirCreate(this_out_dir)

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

	### univariate mantel tests
	tests <- fread(paste0(this_out_dir, '/Univariate Mantel and PERMANOVA on Distance between Rasters and Workflows.csv'))

	### generic biplot to be filled in
	var1 <- sprintf('%.1f', 100 * pca$sdev[1]^2 / sum(pca$sdev^2))
	var2 <- sprintf('%.1f', 100 * pca$sdev[2]^2 / sum(pca$sdev^2))

	x_lab <- paste0('PC 1 (', var1, '%)')
	y_lab <- paste0('PC 2 (', var2, '%)')

	base_biplot <- ggplot() +
		coord_cartesian(clip = 'off') +
		xlab(x_lab) + ylab(y_lab) +
		coord_fixed() +
		theme_minimal()

	### teams
	#########

		p <- tests$permanova_p[tests$nice == 'Team']
		p <- roundTo(p, 0.001)

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

		biplot <- base_biplot +
			geom_point(
				data = scores, aes(x = PC1, y = PC2, color = team),
				size = 5,
				pch = 1
			) +
			geom_polygon(data = polys, aes(x = PC1, y = PC2, fill = team), alpha = 0.4, color = 'gray30') +
			annotate(
				'text', x = -Inf, y = -Inf, label = paste0('italic(P) == ', sprintf('%.5f', p)),
				parse = TRUE, hjust = -0.3, vjust = -1, size = 7, fontface = 'bold'
			) +
			coord_fixed() +
			theme_minimal() +
			theme(
				legend.position = 'none',
				axis.title = element_text(size = 20),
				axis.text = element_text(size = 14)
			)

		ggsave(biplot, filename = paste0(this_out_dir, '/Biplot - Team.png'), width = 8, height = 5, dpi = 600, bg = 'white')

	### thresholding
	################

		p <- tests$permanova_p[tests$nice == 'Predictions: Continuous/thresholded']
		if (p >= 0.001) p <- roundTo(p, 0.001, ceiling)

		rast_fields <- load_rast_fields(species_focal = species_focal)
		scores$thresholded <- 'Continuous'

		rast_fields$raster_period <- apply(rast_fields[ , c('raster_name', 'time_period')], 1, paste, collapse = '_')
		index <- match(rast_fields$raster_period, scores$raster_period)
		this_index <- index[rast_fields$prediction_type_binary_threshold == 1]
		scores$thresholded[this_index] <- 'Binary Threshold'

		this_index <- index[rast_fields$prediction_type_multi_threshold == 1]
		scores$thresholded[this_index] <- 'Multiple Thresholds'

		# cluster
		cluster <- 'thresholded'
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

		biplot <- base_biplot +
			geom_point(
				data = scores, aes(x = PC1, y = PC2, color = thresholded, shape = thresholded), 
				size = 5
			) +
			scale_shape_manual(values = c(0, 1, 2)) +
			geom_polygon(data = polys, aes(x = PC1, y = PC2, fill = cluster), alpha = 0.4, color = 'gray30') +
			annotate(
				'text', x = -Inf, y = -Inf, label = paste0('italic(P) == ', sprintf('%.6f', p)),
				parse = TRUE, hjust = -0.3, vjust = -1, size = 7, fontface = 'bold'
			) +
			coord_fixed() +
			theme_minimal() +
			theme(
				legend.position = 'none',
				axis.title = element_text(size = 20),
				axis.text = element_text(size = 14)
			)

		ggsave(biplot, filename = paste0(this_out_dir, '/Biplot - Thresholding.png'), width = 8, height = 5, dpi = 600, bg = 'white')

	### teams + thresholded/continuous
	##################################

		p <- tests$permanova_p[tests$nice == 'Team × Continuous/Thresholded']
		if (p > 0.001) p <- roundTo(p, 0.001)

		# cluster
		polys <- data.table()
		scores$team_threshold <- paste(scores$team, scores$thresholded)
		unique_clusters <- unique(scores$team_threshold)
		for (i in seq_along(unique_clusters)) {
		
			cluster <- unique_clusters[i]
			pts <- scores[scores$team_threshold == cluster, c('PC1', 'PC2')]
			hull_indices <- chull(pts)
			hull_pts <- pts[hull_indices, ]
			hull_pts <- rbind(hull_pts, hull_pts[1, , drop = FALSE])
			hull_pts$team_threshold <- cluster
			polys <- rbind(polys, hull_pts)
		
		}

		biplot <- base_biplot +
			geom_point(
				data = scores, aes(x = PC1, y = PC2, color = team_threshold),
				size = 5,
				pch = 1
			) +
			geom_polygon(data = polys, aes(x = PC1, y = PC2, fill = team_threshold), alpha = 0.4, color = 'gray30') +
			annotate(
				'text', x = -Inf, y = -Inf, label = paste0('italic(P) == ', sprintf('%.5f', p)),
				parse = TRUE, hjust = -0.3, vjust = -1, size = 7, fontface = 'bold'
			) +
			coord_fixed() +
			theme_minimal() +
			theme(
				legend.position = 'none',
				axis.title = element_text(size = 20),
				axis.text = element_text(size = 14)
			)

		ggsave(biplot, filename = paste0(this_out_dir, '/Biplot - Team × Continuous-Thresholded.png'), width = 8, height = 5, dpi = 600, bg = 'white')

	### time period
	###############

		p <- tests$permanova_p[tests$nice == 'Projection: Time period']
		if (p >= 0.001) p <- roundTo(p, 0.001)

		# cluster
		polys <- data.table()
		unique_clusters <- unique(scores$period)
		for (i in seq_along(unique_clusters)) {
		
			cluster <- unique_clusters[i]
			pts <- scores[scores$period == cluster, c('PC1', 'PC2')]
			hull_indices <- chull(pts)
			hull_pts <- pts[hull_indices, ]
			hull_pts <- rbind(hull_pts, hull_pts[1, , drop = FALSE])
			hull_pts$period <- cluster
			polys <- rbind(polys, hull_pts)
		
		}

		biplot <- base_biplot +
			geom_point(
				data = scores, aes(x = PC1, y = PC2, fill = period, shape = period), 
				size = 5
			) +
			scale_shape_manual(values = c(21, 22, 23)) +
			geom_polygon(data = polys, aes(x = PC1, y = PC2, fill = period), alpha = 0.4, color = 'gray30') +
			annotate(
				'text', x = -Inf, y = -Inf, label = paste0('italic(P) == ', sprintf('%.5f', p)),
				parse = TRUE, hjust = -0.3, vjust = -1, size = 7, fontface = 'bold'
			) +
			coord_fixed() +
			theme_minimal() +
			theme(
				legend.position = 'none',
				axis.title = element_text(size = 20),
				axis.text = element_text(size = 14)
			)

		ggsave(biplot, filename = paste0(this_out_dir, '/Biplot - Time Period.png'), width = 8, height = 5, dpi = 600, bg = 'white')

	### climate data source
	#######################

		p <- tests$permanova_p[tests$nice == 'Predictors: Climate data source']
		if (p >= 0.001) p <- roundTo(p, 0.001)

		fields <- load_team_fields(species_focal = species_focal)
		index <- match(scores$team, fields$team)
		scores$predictors_climate_source <- fields$predictors_climate_source[index]

		# cluster
		cluster <- 'predictors_climate_source'
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

		biplot <- base_biplot +
			geom_point(
				data = scores, aes(x = PC1, y = PC2, fill = predictors_climate_source, shape = predictors_climate_source), 
				size = 5
			) +
			scale_shape_manual(values = c(21, 22)) +
			geom_polygon(data = polys, aes(x = PC1, y = PC2, fill = cluster), alpha = 0.4, color = 'black') +
			annotate(
				'text', x = -Inf, y = -Inf, label = paste0('italic(P) == ', sprintf('%.5f', p)),
				parse = TRUE, hjust = -0.3, vjust = -1, size = 7, fontface = 'bold'
			) +
			coord_fixed() +
			theme_minimal() +
			theme(
				legend.position = 'none',
				axis.title = element_text(size = 20),
				axis.text = element_text(size = 14)
			)

		ggsave(biplot, filename = paste0(this_out_dir, '/Biplot - Climate Data Source.png'), width = 8, height = 5, dpi = 600, bg = 'white')

	### number of predictors
	########################

		p <- tests$permanova_p[tests$nice == 'Predictors: Total number']
		if (p > 0.001) p <- roundTo(p, 0.001)

		fields <- load_team_fields(species_focal = species_focal)
		index <- match(scores$team, fields$team)
		scores$n_predictors <- fields$predictors_climate_nonclimate_num_total[index]

		cluster <- 'n_predictors'
		biplot <- base_biplot +
			geom_point(data = scores, aes(x = PC1, y = PC2, fill = .data[[cluster]]), pch = 21, alpha = 0.7, size = 5) +
			scale_fill_viridis_c(option = 'magma', trans = 'log2') +
			annotate(
				'text', x = -Inf, y = -Inf, label = paste0('italic(P) == ', sprintf('%.5f', p)),
				parse = TRUE, hjust = -0.3, vjust = -1, size = 7, fontface = 'bold'
			) +
			coord_fixed() +
			theme_minimal() +
			theme(
				legend.position = 'none',
				axis.title = element_text(size = 20),
				axis.text = element_text(size = 14)
			)

		ggsave(biplot, filename = paste0(this_out_dir, '/Biplot - Number of Predictors.png'), width = 8, height = 5, dpi = 600, bg = 'white')

	### number of occurrences
	#########################

		p <- tests$permanova_p[tests$nice == 'Occurrences: Number of occurrences']
		if (p > 0.001) p <- roundTo(p, 0.001)

		fields <- load_team_fields(species_focal = species_focal)
		index <- match(scores$team, fields$team)
		scores$n_occurrences <- fields$num_occurrences_minimum[index]

		cluster <- 'n_occurrences'
		biplot <- base_biplot +
			geom_point(data = scores, aes(x = PC1, y = PC2, fill = .data[[cluster]]), pch = 21, alpha = 0.7, size = 5) +
			scale_fill_viridis_c(option = 'magma', trans = 'log2') +
			annotate(
				'text', x = -Inf, y = Inf, label = paste0('italic(P) == ', sprintf('%.5f', p)), 
				parse = TRUE, hjust = -0.3, vjust = -1, size = 7, fontface = 'bold'
			) +
			coord_fixed() +
			theme_minimal() +
			theme(
				legend.position = 'none',
				axis.title = element_text(size = 20),
				axis.text = element_text(size = 14)
			)

		ggsave(biplot, filename = paste0(this_out_dir, '/Biplot - Number of Occurrences.png'), width = 8, height = 5, dpi = 600, bg = 'white')

	### spatial resolution (km2)
	############################

		p <- tests$permanova_p[tests$nice == 'Spatial resolution (km2)']
		if (p > 0.001) p <- roundTo(p, 0.001)

		fields <- load_team_fields(species_focal = species_focal)
		index <- match(scores$team, fields$team)
		scores$res_km2 <- fields$res_km2[index]

		cluster <- 'res_km2'
		biplot <- base_biplot +
			geom_point(data = scores, aes(x = PC1, y = PC2, fill = .data[[cluster]]), pch = 21, alpha = 0.7, size = 5) +
			scale_fill_viridis_c(option = 'magma', trans = 'log2') +
			annotate(
				'text', x = -Inf, y = -Inf, label = paste0('italic(P) == ', sprintf('%.5f', p)),
				parse = TRUE, hjust = -0.3, vjust = -1, size = 7, fontface = 'bold'
			) +
			coord_fixed() +
			theme_minimal() +
			theme(
				legend.position = 'none',
				axis.title = element_text(size = 20),
				axis.text = element_text(size = 14)
			)

		ggsave(biplot, filename = paste0(this_out_dir, '/Biplot - Spatial Resolution.png'), width = 8, height = 5, dpi = 600, bg = 'white')

	### AUC
	#######

		p <- tests$permanova_p[tests$nice == 'Evaluation: Value of AUC']
		if (p > 0.001) p <- roundTo(p, 0.001)

		fields <- load_rast_fields(species_focal = species_focal)
		scores$auc <- NA_real_

		fields$raster_period <- apply(fields[ , c('raster_name', 'time_period')], 1, paste, collapse = '_')
		index <- match(fields$raster_period, scores$raster_period)
		scores$auc[index] <- fields$eval_metric_auc_roc_value
		scores$auc <- as.numeric(scores$auc)

		cluster <- 'auc'
		biplot <- base_biplot +
			geom_point(data = scores[!is.na(scores$auc), ], aes(x = PC1, y = PC2, fill = .data[[cluster]]), pch = 21, alpha = 0.7, size = 5) +
			scale_fill_gradient(low = 'darkblue', high = 'yellow', na.value = 'gray90') +
			annotate(
				'text', x = -Inf, y = -Inf, label = paste0('italic(P) == ', sprintf('%.5f', p)),
				parse = TRUE, hjust = -0.3, vjust = -1, size = 7, fontface = 'bold'
			) +
			coord_fixed() +
			theme_minimal() +
			theme(
				legend.position = 'none',
				axis.title = element_text(size = 20),
				axis.text = element_text(size = 14)
			)

		ggsave(biplot, filename = paste0(this_out_dir, '/Biplot - AUC.png'), width = 8, height = 5, dpi = 600, bg = 'white')

	### modeled mainland only or mainland + insular
	###############################################

		if (species_focal == 'Priona') {

			p <- tests$permanova_p[tests$nice == 'Modeled only mainland subspecies']
			if (p > 0.001) p <- roundTo(p, 0.001)

			fields <- load_team_fields(species_focal = species_focal)
			index <- match(scores$team, fields$team)
			scores$modeled_subspecies <- fields$taxonomy_mainland_only[index]
			scores$modeled_subspecies <- ifelse(scores$modeled_subspecies == 1, 'Mainland Only', 'Mainland + Insular')

			# cluster
			cluster <- 'modeled_subspecies'
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

			biplot <- base_biplot +
				geom_point(data = scores, aes(x = PC1, y = PC2, fill = .data[[cluster]]), pch = 21, alpha = 0.7, size = 5) +
				geom_polygon(data = polys, aes(x = PC1, y = PC2, fill = cluster), alpha = 0.4, color = 'black') +
				annotate(
					'text', x = -Inf, y = -Inf, label = paste0('italic(P) == ', sprintf('%.5f', p)),
					parse = TRUE, hjust = -0.3, vjust = -1, size = 7, fontface = 'bold'
				) +
				coord_fixed() +
				theme_minimal() +
				theme(
					legend.position = 'none',
					axis.title = element_text(size = 20),
					axis.text = element_text(size = 14)
				)

			ggsave(biplot, filename = paste0(this_out_dir, '/Biplot - Taxonomy.png'), width = 8, height = 5, dpi = 600, bg = 'white')

		}

	### SDM algorithm
	#################

		p <- tests$permanova_p[tests$nice == 'Algorithm']
		if (p > 0.001) p <- roundTo(p, 0.001)

		fields <- load_rast_fields(species_focal = species_focal)
		scores$algorithm <- NA_character_

		fields$raster_period <- apply(fields[ , c('raster_name', 'time_period')], 1, paste, collapse = '_')
		index <- match(fields$raster_period, scores$raster_period)
		
		this_index <- index[rast_fields$algo_ensemble == 1]
		scores$algorithm[this_index] <- 'Ensemble'

		this_index <- index[rast_fields$algo_maxent == 1]
		scores$algorithm[this_index] <- 'MaxEnt'

		this_index <- index[rast_fields$algo_maxnet == 1]
		scores$algorithm[this_index] <- 'MaxNet'

		this_index <- index[rast_fields$algo_gam == 1]
		scores$algorithm[this_index] <- 'GAM'

		this_index <- index[rast_fields$algo_rf == 1]
		scores$algorithm[this_index] <- 'RF'

		if (species_focal == 'Zamia') {
			this_index <- index[rast_fields$algo_sre == 1]
			scores$algorithm[this_index] <- 'SRE'
		}

		# cluster
		cluster <- 'algorithm'
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

		biplot <- base_biplot +
			geom_point(data = scores, aes(x = PC1, y = PC2, fill = .data[[cluster]], shape = .data[[cluster]]), alpha = 0.6, size = 5) +
			scale_shape_manual(values = c(21, 22, 24, 25, 23, 24, 25)) +
			geom_polygon(data = polys, aes(x = PC1, y = PC2, fill = cluster), alpha = 0.4, color = 'black') +
			annotate(
				'text', x = -Inf, y = -Inf, label = paste0('italic(P) == ', sprintf('%.5f', p)),
				parse = TRUE, hjust = -0.3, vjust = -1, size = 7, fontface = 'bold'
			) +
			coord_fixed() +
			theme_minimal() +
			theme(
				legend.position = 'none',
				axis.title = element_text(size = 20),
				axis.text = element_text(size = 14)
			)

		ggsave(biplot, filename = paste0(this_out_dir, '/Biplot - SDM Algorithm.png'), width = 8, height = 5, dpi = 600, bg = 'white')

	### non-presence type
	#####################

		p <- tests$permanova_p[tests$nice == 'Non-presence type']
		if (p > 0.001) p <- roundTo(p, 0.001)

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

		biplot <- base_biplot +
			geom_point(data = scores, aes(x = PC1, y = PC2, fill = .data[[cluster]], shape = .data[[cluster]]), alpha = 0.7, size = 5) +
			scale_shape_manual(values = c(21, 22, 24, 25)) +
			geom_polygon(data = polys, aes(x = PC1, y = PC2, fill = cluster), alpha = 0.4, color = 'black') +
			annotate(
				'text', x = -Inf, y = -Inf, label = paste0('italic(P) == ', sprintf('%.5f', p)),
				parse = TRUE, hjust = -0.3, vjust = -1, size = 7, fontface = 'bold'
			) +
			coord_fixed() +
			theme_minimal() +
			theme(
				legend.position = 'none',
				axis.title = element_text(size = 20),
				axis.text = element_text(size = 14)
			)

		ggsave(biplot, filename = paste0(this_out_dir, '/Biplot - Non-presence Type.png'), width = 8, height = 5, dpi = 600, bg = 'white')

	### software
	############

		p <- tests$permanova_p[tests$nice == 'Software']
		if (p > 0.001) p <- roundTo(p, 0.001)

		fields <- load_team_fields(species_focal = species_focal)
		scores$software <- NA_character_

		for (i in 1:nrow(scores)) {

			team <- scores$team[i]
			if (fields$modeling_software_enmeval[fields$team == team] == 1) {
				y <- 'ENMeval'
			} else if (fields$modeling_software_enmtools[fields$team == team] == 1) {
				y <- 'ENMTools'
			} else if (fields$modeling_software_wallace[fields$team == team] == 1) {
				y <- 'Wallace'
			} else if (fields$modeling_software_biomod2[fields$team == team] == 1) {
				y <- 'BIOMOD2'
			} else if (fields$modeling_software_sabinansdm[fields$team == team] == 1) {
				y <- 'sabinaNSDM'
			} else if (fields$modeling_software_sdm[fields$team == team] == 1) {
				y <- 'sdm'
			} else if (fields$modeling_software_miamaxent[fields$team == team] == 1) {
				y <- 'MIAmaxent'
			} else if (fields$modeling_software_enmsdmx[fields$team == team] == 1) {
				y <- 'enmSdmX'
			} else if (fields$modeling_software_flexsdm[fields$team == team] == 1) {
				y <- 'flexsdm'
			} else if (fields$modeling_software_sdmtune[fields$team == team] == 1) {
				y <- 'SDMTune'
			} else if (fields$modeling_software_piecemeal[fields$team == team] == 1) {
				y <- 'piecemeal'
			} else if (fields$modeling_software_other[fields$team == team] == 1) {
				y <- 'Other'
			} else if (fields$modeling_software_unclear_no_response[fields$team == team] == 1) {
				y <- 'Unclear/no response'
			} else {
				y <- NA_character_
			}
			scores$software[i] <- y

		}

		# Create shape mapping for unique software values
		unique_software <- unique(scores$software[!is.na(scores$software)])
		shape_values <- c(21, 24, 24, 22, 5, 25, 0, 1, 2, 3, 23, 6, 4)
		software_names <- c('ENMeval', 'ENMTools', 'Wallace', 'BIOMOD2', 'sabinaNSDM', 'sdm', 
							'MIAmaxent', 'enmSdmX', 'flexsdm', 'SDMTune', 'piecemeal', 'Other', 'Unclear/no response')
		shapes <- setNames(shape_values[match(unique_software, software_names)], unique_software)

		# cluster
		cluster <- 'software'
		cluster_format <- 'software_shape'
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

		biplot <- base_biplot +
			geom_point(data = scores, aes(x = PC1, y = PC2, fill = .data[[cluster]], shape = .data[[cluster]]), size = 5) +
			scale_shape_manual(values = shapes) +
			scale_fill_brewer(palette = 'Set3') +
			geom_polygon(data = polys, aes(x = PC1, y = PC2, fill = cluster), alpha = 0.5, color = 'black') +
			annotate(
				'text', x = -Inf, y = -Inf, label = paste0('italic(P) == ', sprintf('%.5f', p)),
				parse = TRUE, hjust = -0.3, vjust = -1, size = 7, fontface = 'bold'
			) +
			coord_fixed() +
			theme_minimal() +
			theme(
				legend.position = 'none',
				axis.title = element_text(size = 20),
				axis.text = element_text(size = 14)
			)

		ggsave(biplot, filename = paste0(this_out_dir, '/Biplot - Software.png'), width = 8, height = 5, dpi = 600, bg = 'white')

	### software by developers
	##########################

		p <- tests$permanova_p[tests$nice == 'Software use by developers']
		if (p >= 0.001) p <- roundTo(p, 0.001)

		fields <- load_rast_fields(species_focal = species_focal)
		y <- rast_fields$team_code
		y[y %in% c('E', 'B', 'C', 'H', 'D')] <- 'Yes'
		y[y != 'Yes'] <- 'No'

		scores$software_by_developers <- y

		# cluster
		cluster <- 'software_by_developers'
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

		biplot <- base_biplot +
			geom_point(
				data = scores, aes(x = PC1, y = PC2, fill = software_by_developers, shape = software_by_developers), 
				size = 5
			) +
			scale_shape_manual(values = c(21, 22)) +
			geom_polygon(data = polys, aes(x = PC1, y = PC2, fill = cluster), alpha = 0.4, color = 'black') +
			annotate(
				'text', x = -Inf, y = -Inf, label = paste0('italic(P) == ', sprintf('%.5f', p)),
				parse = TRUE, hjust = -0.3, vjust = -1, size = 7, fontface = 'bold'
			) +
			coord_fixed() +
			theme_minimal() +
			theme(
				legend.position = 'none',
				axis.title = element_text(size = 20),
				axis.text = element_text(size = 14)
			)

		ggsave(biplot, filename = paste0(this_out_dir, '/Biplot - Software Used by Developers.png'), width = 8, height = 5, dpi = 600, bg = 'white')

	### workflow quality (standards)
	################################

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

		p <- tests$permanova_p[tests$nice == 'SDM Standards: Mean rank']
		if (p > 0.001) p <- roundTo(p, 0.001)

		index <- match(scores$team, fields$team)
		scores$odmap_mean <- fields$odmap_mean[index]

		cluster <- 'odmap_mean'
		biplot <- base_biplot +
			geom_point(data = scores, aes(x = PC1, y = PC2, fill = .data[[cluster]]), pch = 21, alpha = 0.7, size = 5) +
			scale_fill_viridis_c(option = 'magma') +
			annotate(
				'text', x = -Inf, y = Inf, label = paste0('italic(P) == ', sprintf('%.5f', p)), 
				parse = TRUE, hjust = -0.3, vjust = 1.5, size = 5, fontface = 'bold'
			) +
			coord_fixed() +
			theme_minimal() +
			theme(
				legend.position = 'none',
				axis.title = element_text(size = 20),
				axis.text = element_text(size = 14)
			)

		ggsave(biplot, filename = paste0(this_out_dir, '/Biplot - Workflow Quality (Mean Rank).png'), width = 8, height = 5, dpi = 600, bg = 'white')


say('DONE', level = 1)

