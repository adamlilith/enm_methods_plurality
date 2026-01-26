### SDM METHODOLOGICAL PLURALITY
### Anna Thonis, Nikki Calavari, Uzma Ashraf, Toni Lyn Morelli, and Adam B. Smith*
### * adam.smith@mobot.org | Missouri Botanical Garden | 2025-10
###
### Make figures of dendrogram and PCA clustering of rasters using hierarchical clustering.
###
### source('C:/Kaji/Research Group/ENMs - Plurality of Modeling Workflows (Anna Thonis)/enm_methods_plurality/03_cluster_analysis_of_teams_hierarchical.r')
###
### CONTENTS ###
### setup ###
### dendrogram and PCA of teams ###
### clustering each set of rasters from a team ###
### PCA on teams with rasters coded by workflow attribute ###
### not used but may be useful ###

#############
### setup ###
#############

	rm(list = ls())
	source('C:/Kaji/Research Group/ENMs - Plurality of Modeling Workflows (Anna Thonis)/enm_methods_plurality/00_shared_constants_and_functions.r')

	dirCreate(paste0('./Outputs ', species_full, '/Cluster Analysis of Rasters'))

say('###################################')
say('### dendrogram and PCA of teams ###')
say('###################################')

	preds <- load_predictions(period = 'all', scale = TRUE, subset_teams = TRUE)
	preds <- t(preds)

	pca <- prcomp(preds)
	scores <- pca$x[ , 1:2]
	scores <- as.data.frame(scores)

	dists <- dist(scores)

	### hierarchical clustering
	###########################

	### dendrogram
	clust <- hclust(dists, method = 'complete')
	
	### dendrogram figure
	#####################

	ddata <- ggdendro::dendro_data(clust, type = 'rectangle')
	seg <- ddata$segments
	labs <- ddata$labels
	
	labs$raster <- labs$label
	labs$raster <- sub(labs$raster, pattern = '_present', replacement = '')
	labs$raster <- sub(labs$raster, pattern = '_mid', replacement = '')
	labs$raster <- sub(labs$raster, pattern = '_late', replacement = '')

	labs$period <- labs$label
	labs$period[grepl(labs$period, pattern = '_present')] <- 'Present'
	labs$period[grepl(labs$period, pattern = '_mid')] <- 'Mid'
	labs$period[grepl(labs$period, pattern = '_late')] <- 'Late'
	
	hjust <- rep(c(1.5, 3), length.out = length(clusters))

	dendro <- ggplot() +
		geom_segment(data = seg, aes(x = x, y = y, xend = xend, yend = yend)) +
		geom_text(data = labs, aes(x = x, y = y, label = raster, color = period), hjust = hjust, size = 2.4, angle = 90, fontface = 'bold') +
		scale_color_manual(values = period_colors) +
		guides(color = guide_legend(override.aes = list(label = 'AB', size = 3))) +
		coord_cartesian(clip = 'off') +
		labs(y = NULL, color = 'Period') +
		ggtitle('a)') +
		theme_minimal() +
		theme(
			plot.title = element_text(size = 18),
			axis.title = element_blank(),
			axis.text = element_blank(),
			axis.ticks = element_blank(),
			panel.grid.major = element_blank(),
			panel.grid.minor = element_blank(),
			legend.position = c(0.095, 0.9),
			legend.justification = c(0.1, 0.9),
			legend.title = element_text(size = 12),
			legend.text = element_text(size = 11)
		)

	### PCA biplot
	##############

	# generic biplot to be filled in
	var1 <- sprintf('%.1f', 100 * pca$sdev[1]^2 / sum(pca$sdev^2))
	var2 <- sprintf('%.1f', 100 * pca$sdev[2]^2 / sum(pca$sdev^2))

	x_lab <- paste0('PC 1 (', var1, '%)')
	y_lab <- paste0('PC 2 (', var2, '%)')

	base_biplot <- ggplot() +
		coord_cartesian(clip = 'off') +
		xlab(x_lab) + ylab(y_lab)

	bases <- list() # stores biplots with polygons, one per cluster, for use later when plotting attributes by cluster
	ks <- get_ks()
	for (k in c(0, ks)) {
		
		# get cluster assignments
		if (k > 0) clusters <- cutree(clust, k = k)

		### plots of PCA scores and team groups
		pcs <- scores
		if (k > 0) pcs$cluster <- as.factor(clusters)
		pcs$raster_period <- rownames(pcs)

		pcs$raster <- pcs$raster_period
		pcs$raster <- sub(pcs$raster, pattern = '_present', replacement = '')
		pcs$raster <- sub(pcs$raster, pattern = '_mid', replacement = '')
		pcs$raster <- sub(pcs$raster, pattern = '_late', replacement = '')

		pcs$period <- NA_character_
		pcs$period[grepl(pcs$raster_period, pattern = '_present')] <- 'Present'
		pcs$period[grepl(pcs$raster_period, pattern = '_mid')] <- 'Mid'
		pcs$period[grepl(pcs$raster_period, pattern = '_late')] <- 'Late'

		biplot <- base_biplot

		# MCP for each cluster
		if (k > 0) {

			polys <- list()
			unique_clusters <- sort(unique(clusters))
			unique_clusters <- unique_clusters[unique_clusters]
			for (i in unique_clusters) {
			
				cluster_teams <- names(clusters)[clusters == i]
				pts <- scores[rownames(scores) %in% cluster_teams, c('PC1', 'PC2')]
				if (nrow(pts) >= 3) {
					hull_indices <- chull(pts)
					hull_coords <- pts[hull_indices, ]
					polys[[i]] <- hull_coords
				} else {
					polys[[i]] <- pts
				}

				polys[[i]] <- rbind(polys[[i]], polys[[i]][1, ])
			
			}

			# add cluster polygons
			for (i in seq_along(unique_clusters)) {

				biplot <- biplot +
					geom_polygon(
						data = polys[[i]],
						aes(x = PC1, y = PC2),
						color = '#a6cee3',
						fill = alpha('#a6cee3', 0.5)
					)

			}

		}

		if (k > 0) bases[[length(bases) + 1]] <- biplot

		# add raster labels
		biplot <- biplot +
			geom_shadowtext(
				data = pcs, aes(x = PC1, y = PC2, label = raster, color = period), 
				size = 4,
				bg.colour = alpha('black', 0.2), bg.r = 0.05,
				fontface = 'bold'
			) +
			labs(color = 'Period') +
			scale_color_manual(values = period_colors) +
			guides(color = guide_legend(override.aes = list(label = 'ABC', size = 5))) +
			ggtitle('b)') +
			coord_fixed() +
			theme_minimal() +
			theme(
				legend.position = 'none',
				plot.title = element_text(size = 18),
				axis.title = element_text(size = 13),
				axis.text = element_text(size = 10),
				legend.title = element_text(size = 12),
				legend.text = element_text(size = 10)
			)

		combo <- plot_grid(dendro, biplot, nrow = 2, rel_heights = c(0.5, 1), align = 'v')

		ggsave(combo, filename = paste0(out_dir, '/Cluster Analysis of Rasters/Biplot and Dendrogram of All Rasters Hierarchical Complete k ', k, '.png'), width = 6, height = 6, dpi = 600, bg = 'white')

	} # next k

	dev.off()

	names(bases) <- paste0('k', ks)
	saveRDS(bases, file = paste0(out_dir, '/Cluster Analysis of Rasters/Biplot and Dendrogram of All Rasters Hierarchical Complete Polygon Plots.rds'))

# # # say('##################################################')
# # # say('### clustering each set of rasters from a team ###')
# # # say('##################################################')

# # # 	# Make a PCA plot of teams and connect rasters from the same team.

# # # 	preds <- load_predictions(period = 'all', scale = TRUE, subset_teams = TRUE)
# # # 	preds <- t(preds)

# # # 	pca <- prcomp(preds)
# # # 	scores <- pca$x[ , 1:2]
# # # 	scores <- as.data.frame(scores)

# # # 	### PCA biplot
# # # 	##############

# # # 		### plots of PCA scores and team groups
# # # 		pcs <- scores
# # # 		pcs$raster_period <- rownames(pcs)

# # # 		pcs$raster <- pcs$raster_period
# # # 		pcs$raster <- sub(pcs$raster, pattern = '_present', replacement = '')
# # # 		pcs$raster <- sub(pcs$raster, pattern = '_mid', replacement = '')
# # # 		pcs$raster <- sub(pcs$raster, pattern = '_late', replacement = '')

# # # 		pcs$period <- NA_character_
# # # 		pcs$period[grepl(pcs$raster_period, pattern = '_present')] <- 'Present'
# # # 		pcs$period[grepl(pcs$raster_period, pattern = '_mid')] <- 'Mid'
# # # 		pcs$period[grepl(pcs$raster_period, pattern = '_late')] <- 'Late'

# # # 		# axis labels
# # # 		var1 <- sprintf('%.1f', 100 * pca$sdev[1]^2 / sum(pca$sdev^2))
# # # 		var2 <- sprintf('%.1f', 100 * pca$sdev[2]^2 / sum(pca$sdev^2))

# # # 		x_lab <- paste0('PC 1 (', var1, '%)')
# # # 		y_lab <- paste0('PC 2 (', var2, '%)')


# # # 		# function to calculate distance each team shifts
# # # 		dist_fx <- function(a, b) {
# # # 			x1 <- a[1, 1]
# # # 			x2 <- b[1, 1]
# # # 			y1 <- a[1, 2]
# # # 			y2 <- b[1, 2]
# # # 			shift <- sqrt(sum((x1 - x2)^2 + (y1 - y2)^2))
# # # 			x_shift <- abs(x1 - x2)
# # # 			y_shift <- abs(y1 - y2)
# # # 			list(shift = shift, x_shift = x_shift, y_shift = y_shift)
# # # 		}

# # # 		# MCP for each team
# # # 		polys <- list()
# # # 		teams <- LETTERS[1:24]
# # # 		scores_teams <- substr(rownames(scores), 1, 1)
# # # 		net_shift <- data.table()
# # # 		for (i in seq_along(teams)) {
		
# # # 			# hull
# # # 			team <- teams[i]
# # # 			index <- which(scores_teams == team)
# # # 			pts <- scores[index, ]
# # # 			hull_indices <- chull(pts)
# # # 			hull_coords <- pts[hull_indices, ]
# # # 			polys[[i]] <- hull_coords
# # # 			polys[[i]] <- rbind(polys[[i]], polys[[i]][1, ])

# # # 			# net shift in time by a team's rasters across time period
# # # 			rast_names <- rownames(scores)[index]
# # # 			present_name <- rast_names[grepl(rast_names, pattern = '_present')]
# # # 			mid_name <- rast_names[grepl(rast_names, pattern = '_mid')]
# # # 			late_name <- rast_names[grepl(rast_names, pattern = '_late')]
			
# # # 			present_scores <- scores[rownames(scores) %in% present_name, , drop = FALSE]
# # # 			mid_scores <- scores[rownames(scores) %in% mid_name, , drop = FALSE]
# # # 			late_scores <- scores[rownames(scores) %in% late_name, , drop = FALSE]

# # # 			if (nrow(present_scores) > 1) present_scores <- rbind(colMeans(present_scores))
# # # 			if (nrow(mid_scores) > 1) mid_scores <- rbind(colMeans(mid_scores))
# # # 			if (nrow(late_scores) > 1) late_scores <- rbind(colMeans(late_scores))

# # # 			dist_present_mid <- dist_fx(present_scores, mid_scores)
# # # 			dist_mid_late <- dist_fx(mid_scores, late_scores)
# # # 			dist_present_late <- dist_fx(present_scores, late_scores)

# # # 			net_shift <- rbind(
# # # 				net_shift,
# # # 				data.table(
# # # 					team = team,
# # # 					present_mid = dist_present_mid$shift,
# # # 					mid_late = dist_mid_late$shift,
# # # 					present_late = dist_present_late$shift,
# # # 					x_shift_present_mid = dist_present_mid$x_shift,
# # # 					x_shift_mid_late = dist_mid_late$x_shift,
# # # 					x_shift_present_late = dist_present_late$x_shift,
# # # 					y_shift_present_mid = dist_present_mid$y_shift,
# # # 					y_shift_mid_late = dist_mid_late$y_shift,
# # # 					y_shift_present_late = dist_present_late$y_shift
# # # 				)
# # # 			)

# # # 		} # next team

# # # 		net_shift <- rbind(
# # # 			net_shift,
# # # 			net_shift[, .(
# # # 				team = 'mean',
# # # 				present_mid = mean(present_mid),
# # # 				mid_late = mean(mid_late),
# # # 				present_late = mean(present_late),
# # # 				x_shift_present_mid = mean(x_shift_present_mid),
# # # 				x_shift_mid_late = mean(x_shift_mid_late),
# # # 				x_shift_present_late = mean(x_shift_present_late),
# # # 				y_shift_present_mid = mean(y_shift_present_mid),
# # # 				y_shift_mid_late = mean(y_shift_mid_late),
# # # 				y_shift_present_late = mean(y_shift_present_late)
# # # 			)]
# # # 		)

# # # 		# base plot
# # # 		biplot <- ggplot() +
# # # 			coord_cartesian(clip = 'off') +
# # # 			xlab(x_lab) + ylab(y_lab)

# # # 		# add cluster polygons
# # # 		# for (i in seq_along(polys)) {

# # # 		# 	biplot <- biplot +
# # # 		# 		geom_polygon(
# # # 		# 			data = polys[[i]],
# # # 		# 			aes(x = PC1, y = PC2),
# # # 		# 			color = 'gold1',
# # # 		# 			fill = alpha('gold1', 0.3)
# # # 		# 		)

# # # 		# }

# # # 		# add raster labels
# # # 		biplot <- biplot +
# # # 			geom_shadowtext(
# # # 				data = pcs, aes(x = PC1, y = PC2, label = raster, color = period), 
# # # 				size = 4,
# # # 				bg.colour = alpha('black', 0.2), bg.r = 0.05,
# # # 				fontface = 'bold'
# # # 			) +
# # # 			labs(color = 'Period') +
# # # 			scale_color_manual(values = period_colors) +
# # # 			guides(color = guide_legend(override.aes = list(label = 'ABC', size = 5))) +
# # # 			ggtitle('Clustering within teams') +
# # # 			coord_fixed() +
# # # 			theme_minimal() +
# # # 			theme(
# # # 				legend.position = 'none',
# # # 				plot.title = element_text(size = 18),
# # # 				axis.title = element_text(size = 15),
# # # 				axis.text = element_text(size = 12),
# # # 				legend.title = element_text(size = 16),
# # # 				legend.text = element_text(size = 14)
# # # 			)
		
# # # 		# # add mean shift arrow
# # # 		# origin <- colMeans(scores)

# # # 		# mean_shifts <- net_shift[team == 'mean']
# # # 		# # x_lims <- layer_scales(biplot)$x$range$range
# # # 		# # y_lims <- layer_scales(biplot)$y$range$range
# # # 		# arrow_start_x <- origin[1] # x_lims[1] + 0.5 * diff(x_lims)
# # # 		# arrow_start_y <- origin[2] # y_lims[1] + 0.5 * diff(y_lims)
		
# # # 		# biplot <- biplot +
# # # 		# 	geom_segment(
# # # 		# 		aes(
# # # 		# 			x = arrow_start_x, 
# # # 		# 			y = arrow_start_y,
# # # 		# 			xend = arrow_start_x + mean_shifts$x_shift_present_late,
# # # 		# 			yend = arrow_start_y + mean_shifts$y_shift_present_late
# # # 		# 		),
# # # 		# 		arrow = arrow(length = unit(0.3, 'cm'), type = 'closed'),
# # # 		# 		color = 'black',
# # # 		# 		linewidth = 2.2
# # # 		# 	) #+
# # # 		# 	# annotate(
# # # 		# 	# 	'text',
# # # 		# 	# 	x = arrow_start_x,
# # # 		# 	# 	y = arrow_start_y - 0.05 * diff(y_lims),
# # # 		# 	# 	label = sprintf('Mean shift\n(Δx = %.2f, Δy = %.2f)', 
# # # 		# 	# 		mean_shifts$x_shift_present_late, 
# # # 		# 	# 		mean_shifts$y_shift_present_late),
# # # 		# 	# 	size = 4,
# # # 		# 	# 	hjust = 0
# # # 		# 	# )


# # # 		ggsave(biplot, filename = paste0(out_dir, '/Cluster Analysis of Rasters/Biplot.png'), width = 8.5, height = 5.5, dpi = 600, bg = 'white')

# # # say('#############################################################')
# # # say('### PCA on teams with rasters coded by workflow attribute ###')
# # # say('#############################################################')

# # # 	# Make a PCA plot of rasters and code each by workflow attribute
# # # 	# user-defined

# # # 	rast_fields <- load_rast_fields(species_focal = species_focal)
# # # 	team_fields <- load_team_fields(species_focal = species_focal)

# # # 	### add MEAN ODMAP score and minimum score across criteria to fields
# # # 	######################################################################

# # # 		odmap <- readRDS('./Outputs Shared Anonymized/ODMAP Scoring Anonymized.rds')

# # # 		y <- odmap[['means']]

# # # 		y <- y[grepl(y$species, pattern = species_full)]
# # # 		criteria <- c(paste0(1, LETTERS[1:5]), paste0(2, LETTERS[1:3]), paste0(3, LETTERS[1:4]), paste0(4, LETTERS[1:3]))
# # # 		odmap_scores <- y[ , ..criteria]
		
# # # 		odmap_means <- rowMeans(odmap_scores)
# # # 		odmap_mins <- apply(odmap_scores, 1, max) # taking max bc 1 = gold, 0 = deficient

# # # 		odmap_means <- 4 - odmap_means # reverse ranks so 4 = gold, 0 = deficient
# # # 		odmap_mins <- 4 - odmap_mins # reverse ranks so 4 = gold, 0 = deficient

# # # 		team_codes <- odmap$means$team_code[odmap$means$species == species_full]
# # # 		names(odmap_means) <- team_codes
# # # 		names(odmap_mins) <- team_codes

# # # 		team_fields$odmap_mean <- odmap_means[match(team_fields$team_code, team_codes)]
# # # 		team_fields$odmap_min <- odmap_mins[match(team_fields$team_code, team_codes)]

# # # 		# match to raster fields
# # # 		index <- match(rast_fields$team_code, team_fields$team_code)
# # # 		rast_fields[ , odmap_mean := team_fields$odmap_mean[index]]
# # # 		rast_fields[ , odmap_min := team_fields$odmap_min[index]]

# # # 	### make base biplot
# # # 	####################

# # # 		preds <- load_predictions(period = 'all', scale = TRUE, subset_teams = TRUE)
# # # 		preds <- t(preds)

# # # 		pca <- prcomp(preds)
# # # 		scores <- pca$x[ , 1:2]
# # # 		scores <- as.data.frame(scores)

# # # 		rast_fields[ , raster_name_time_period := paste0(raster_name, '_', time_period)]
# # # 		index <- match(rast_fields$raster_name_time_period, rownames(scores))
# # # 		rast_fields[ , PC1 := scores$PC1[index]]
# # # 		rast_fields[ , PC2 := scores$PC2[index]]

# # # 	### PCA biplot
# # # 	##############

# # # 		### plots of PCA scores and team groups
# # # 		pcs <- scores
# # # 		pcs$raster_period <- rownames(pcs)

# # # 		pcs$raster <- pcs$raster_period
# # # 		pcs$raster <- sub(pcs$raster, pattern = '_present', replacement = '')
# # # 		pcs$raster <- sub(pcs$raster, pattern = '_mid', replacement = '')
# # # 		pcs$raster <- sub(pcs$raster, pattern = '_late', replacement = '')

# # # 		pcs$period <- NA_character_
# # # 		pcs$period[grepl(pcs$raster_period, pattern = '_present')] <- 'Present'
# # # 		pcs$period[grepl(pcs$raster_period, pattern = '_mid')] <- 'Mid'
# # # 		pcs$period[grepl(pcs$raster_period, pattern = '_late')] <- 'Late'

# # # 		# axis labels
# # # 		var1 <- sprintf('%.1f', 100 * pca$sdev[1]^2 / sum(pca$sdev^2))
# # # 		var2 <- sprintf('%.1f', 100 * pca$sdev[2]^2 / sum(pca$sdev^2))

# # # 		x_lab <- paste0('PC 1 (', var1, '%)')
# # # 		y_lab <- paste0('PC 2 (', var2, '%)')

# # # 		# biplot
# # # 		biplot <- ggplot() +
# # # 			coord_cartesian(clip = 'off') +
# # # 			coord_fixed() +
# # # 			xlab(x_lab) + ylab(y_lab) +
# # # 			theme_minimal() +
# # # 			theme(
# # # 				plot.title = element_text(size = 18),
# # # 				axis.title = element_text(size = 15),
# # # 				axis.text = element_text(size = 12),
# # # 				legend.title = element_text(size = 16),
# # # 				legend.text = element_text(size = 14)
# # # 			)

# # # 		### ODMAP *mean* score
# # # 		######################

# # # 			filename <- 'SDM Standards - Mean Rank'
# # # 			title <- 'SDM Standards: Mean Rank'
# # # 			legend <- 'Rank'
# # # 			y <- 'odmap_mean'

# # # 			this_biplot <- biplot +
# # # 				geom_point(data = rast_fields, aes(x = PC1, y = PC2, fill = .data[[y]]), pch = 21, size = 6) +
# # # 				scale_fill_viridis_c(name = legend, option = 'magma') +
# # # 				ggtitle(title)
		
# # # 			ggsave(this_biplot, filename = paste0(out_dir, '/Cluster Analysis of Rasters/Biplot Coded by ', filename, '.png'), width = 8.5, height = 4.5, dpi = 600, bg = 'white')

# # # 		### ODMAP *min* score
# # # 		#####################

# # # 			filename <- 'SDM Standards - Minimum Rank'
# # # 			title <- 'SDM Standards: Minimum Rank'
# # # 			legend <- 'Rank'
# # # 			y <- 'odmap_min'

# # # 			this_biplot <- biplot +
# # # 				geom_point(data = rast_fields, aes(x = PC1, y = PC2, fill = .data[[y]]), pch = 21, size = 6) +
# # # 				scale_fill_viridis_c(name = legend, option = 'magma') +
# # # 				ggtitle(title)
		
# # # 			ggsave(this_biplot, filename = paste0(out_dir, '/Cluster Analysis of Rasters/Biplot Coded by ', filename, '.png'), width = 8.5, height = 4.5, dpi = 600, bg = 'white')

# # # 		### number of occurrences
# # # 		#########################

# # # 			filename <- 'Number of Occurrences'
# # # 			title <- 'Number of Occurrences'
# # # 			legend <- 'Number'
# # # 			y <- 'num_occurrences_minimum'
# # # 			trans <- 'log10'

# # # 			index <- match(rast_fields$team_code, team_fields$team_code)
# # # 			rast_fields[ , num_occurrences_minimum := team_fields$num_occurrences_minimum[index]]
			
# # # 			this_biplot <- biplot +
# # # 				geom_point(data = rast_fields, aes(x = PC1, y = PC2, fill = .data[[y]]), pch = 21, size = 6) +
# # # 				scale_fill_viridis_c(name = legend, option = 'magma', trans = trans) +
# # # 				ggtitle(title)
		
# # # 			ggsave(this_biplot, filename = paste0(out_dir, '/Cluster Analysis of Rasters/Biplot Coded by ', filename, '.png'), width = 8.5, height = 4.5, dpi = 600, bg = 'white')

# # # ##################################
# # # ### not used but may be useful ###
# # # ##################################

# # # 	preds <- load_predictions(period = 'all', scale = TRUE, subset_teams = TRUE)
# # # 	preds <- t(preds)

# # # 	pca <- prcomp(preds)
# # # 	scores <- pca$x[ , 1:2]
# # # 	scores <- as.data.frame(scores)

# # # 	dists <- dist(scores)

# # # 	### hierarchical clustering
# # # 	###########################

# # # 	### are individual nodes significant?
# # # 	library(pvclust)
# # # 	scores_trans <- as.data.frame(t(scores))
# # # 	clust <- pvclust(scores_trans, method.hclust = 'complete', method.dist = 'euclidean', parallel = 4L, nboot = 1000)

# # # 	### how stable are individual clusters?
# # # 	library(fpc)
# # # 	d  <- dist(scores, method = 'euclidean')
# # # 	hc <- hclust(d, method = 'complete')

# # # 	## Choose a cut, say k = 4
# # # 	k <- 5
# # # 	cl <- cutree(hc, k = k)

# # # 	## Now assess stability of those k clusters under resampling
# # # 	cb <- clusterboot(scores,
# # # 		B = 500,
# # # 		bootmethod = 'boot',
# # # 		clustermethod = hclustCBI,
# # # 		method = 'ward.D2',
# # # 		k = k,
# # # 		showplots = FALSE)

# # # 		cb$bootmean   # stability measure for each of the k clusters


# # # 	cl <- cb$partition  # cluster labels for rows of x

# # # 	plot(scores, col = cl, pch = 19, main = 'Clusters colored by clusterboot()')
# # # 	legend('topright',
# # # 		legend = paste0('Cluster ', 1:k, ' (J = ', round(cb$bootmean, 2), ')'),
# # # 		col    = 1:k, pch = 19, bty = 'n')

say('DONE', level = 1)
