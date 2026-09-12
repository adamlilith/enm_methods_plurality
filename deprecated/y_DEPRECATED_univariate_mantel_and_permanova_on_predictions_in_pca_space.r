
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
# 			field_names <- c('nonpres_type_background', 'nonpres_type_pseudoabsence')

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
# # 			y <- sub(y, pattern = 'nonpres_type_target_background', replacement = 'Target')
		
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

