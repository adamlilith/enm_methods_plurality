### SDM METHODOLOGICAL PLURALITY
### Anna Thonis, Nikki Calavari, Uzma Ashraf, Toni Lyn Morelli, and Adam B. Smith*
### * adam.smith@mobot.org | Missouri Botanical Garden | 2025-10
###
### Make figures of dendrogram and PCA clustering of rasters using DBSCAN for clustering.
###
### source('C:/Kaji/Research Group/ENMs - Plurality of Modeling Workflows (Anna Thonis)/enm_methods_plurality/03_cluster_analysis_of_teams_dbscan.r')
###
### CONTENTS ###
### setup ###
### cluster analysis of teams: all periods ###

#############
### setup ###
#############

	rm(list = ls())
	source('C:/Kaji/Research Group/ENMs - Plurality of Modeling Workflows (Anna Thonis)/enm_methods_plurality/00_shared_constants_and_functions.r')

say('##############################################')
say('### cluster analysis of teams: all periods ###')
say('##############################################')

	preds <- load_predictions(species_focal = species_focal, period = 'all', scale = TRUE, subset_teams = TRUE)
	preds <- t(preds)

	pca <- prcomp(preds)
	scores <- pca$x[ , 1:2]
	scores <- as.data.frame(scores)

	### DBSCAN clustering
	#####################

	### grid search of DBSCAN parameter space for optimal eps and minium number of points in a cluster
	eps_grid <- 1:100
	minpts_grid <- 3:100

	results <- expand.grid(eps = eps_grid, minPts = minpts_grid)
	results$mean_sil <- NA
	results$n_clusters <- NA
	results$n_noise <- NA

	dists <- dist(scores)

	for (i in seq_len(nrow(results))) {

		db <- dbscan(scores, eps = results$eps[i], minPts = results$minPts[i])
		
		if (length(unique(db$cluster[db$cluster != 0])) > 1) {
			sil <- silhouette(db$cluster, dists)
			results$mean_sil[i] <- mean(sil[, 'sil_width'])
		} else { # ignore runs with only noise or one cluster
			results$mean_sil[i] <- NA
		}
		
		results$n_clusters[i] <- length(unique(db$cluster[db$cluster != 0]))
		results$n_noise[i] <- sum(db$cluster == 0)
		
	}

	# optimum eps and minPts
	opt <- which.max(results$mean_sil)
	eps_opt <- results$eps[opt]
	minPts_opt <- results$minPts[opt]

	# means silhouette width vs eps for each minPts size
	db_params <- ggplot(results, aes(x = eps, y = mean_sil, color = factor(minPts))) +
		geom_line() +
		geom_point() +
		labs(y = 'Average silhouette width',
			x = 'eps (neighborhood radius)',
			color = 'minPts'
		) +
		theme_minimal()


		# for Priona, optimal number is near any value of k.max used.

		### plot of mean silhouette width by cluster size
		# looking for peak

		# We'll explore a range of cluster numbers:
		k_range <- 2:max_clusts

		clust <- hclust(dists, method = 'complete')

		# Compute the average silhouette width for each k
		sil_means <- sapply(k_range, function(k) {
			cl <- cutree(clust, k = k)
			sil <- silhouette(cl, dists)
			mean(sil[ , 'sil_width'])
		})

		sills <- data.frame(
			k = k_range,
			mean_sil = sil_means
		)

		# get optimal number of clusters assuming quadratic fit
		# fit quadratic regression, find k at maximum silhouette, find max silhouette, subtract 1 SE from the fit, get value of k that is within 1 SE of k that maximizes silhouette (on the lower side)
		m <- lm(mean_sil ~ k + I(k^2), data = sills)
		coeffs <- coefficients(m)
		k_at_max_sil <- -1 * coeffs['k'] / (2 * coeffs['I(k^2)'])
		
		max_sil <- coeffs['(Intercept)'] + coeffs['k'] * k_at_max_sil + coeffs['I(k^2)'] * k_at_max_sil^2

		sigma <- summary(m)$sigma
		max_sil_minus_se <- max_sil - sigma
		
		a <- coeffs['I(k^2)']
		b <- coeffs['k']
		c <- coeffs['(Intercept)'] - max_sil_minus_se
		
		k_opt_lower <- (-1 * b + sqrt(b^2 - 4 * a * c)) / (2 * a)
		k_opt_upper <- (-1 * b - sqrt(b^2 - 4 * a * c)) / (2 * a)
		
		k_opt_lower <- round(k_opt_lower)

		sil_plot <- ggplot(sills, aes(x = k, y = mean_sil)) +
			geom_line(size = 1) +
			geom_point(size = 2) +
			geom_smooth(
				method = 'lm', formula = y ~ x + I(x^2), se = FALSE,
				color = 'red', linetype = 'dashed'
			) +
			geom_vline(
				xintercept = k_at_max_sil,
				linetype = 'dashed', color = 'black', size = 1
			) +
			geom_hline(
				yintercept = max_sil_minus_se,
				linetype = 'dashed', color = 'blue', size = 1
			) +
			geom_vline(xintercept = k_opt_lower,
				linetype = 'dashed', color = 'orchid', size = 1
			) +
			annotate('text', 
				x = k_opt_lower,
				y = min(sills$mean_sil),
				label = paste0('optimum = ', round(k_opt_lower)),
				vjust = -0.5, color = 'orchid2', size = 4) +
			labs(x = 'Number of clusters (k)',
				y = 'Average silhouette width',
				title = 'Silhouette width by number of clusters'
			) +
			theme_minimal(base_size = 14)


		say('RELATIONSHIP BETWEEN MEAN SILHOUETTE SIZE AND CLUSTER NUMBER', level = 1)
		say('Optimal number of clusters using "+/-1SE rule" is ', k_opt_lower, '.')

		ggsave(sil_plot, filename = paste0('./Outputs ', species_full, '/Cluster Analysis of Rasters - Optimal Cluster Number from Silhouette Width vs k.png'), width = 10, height = 10, bg = 'white')
		saveRDS(k_opt_lower, paste0('./Outputs ', species_full, '/Cluster Analysis of Rasters - Optimal Cluster Number from Silhouette Width vs k.rds'))

	### DBSCAN clustering
	#####################

	db <- fpc::dbscan(scores, eps = eps_opt, MinPts = minPts_opt)
	# plot(db, data = scores)
	db$cluster <- db$cluster + 1
	clusters <- db$cluster
	names(clusters) <- rownames(scores)

	### plots of PCA scores and team groups
	pcs <- scores
	pcs$cluster <- as.factor(db$cluster)
	pcs$raster_period <- rownames(pcs)

	pcs$raster <- pcs$raster_period
	pcs$raster <- sub(pcs$raster, pattern = '_present', replacement = '')
	pcs$raster <- sub(pcs$raster, pattern = '_mid', replacement = '')
	pcs$raster <- sub(pcs$raster, pattern = '_late', replacement = '')

	pcs$period <- NA_character_
	pcs$period[grepl(pcs$raster_period, pattern = '_present')] <- 'Present'
	pcs$period[grepl(pcs$raster_period, pattern = '_mid')] <- 'Mid'
	pcs$period[grepl(pcs$raster_period, pattern = '_late')] <- 'Late'

	# axis labels
	var1 <- sprintf('%.1f', 100 * pca$sdev[1]^2 / sum(pca$sdev^2))
	var2 <- sprintf('%.1f', 100 * pca$sdev[2]^2 / sum(pca$sdev^2))

	x_lab <- paste0('PC 1 (', var1, '%)')
	y_lab <- paste0('PC 2 (', var2, '%)')

	# shape of points... hollow circle is for points not in a group
	shapes <- c('1' = 1, setNames(rep(16, length(unique(pcs$cluster)) - 1), setdiff(unique(pcs$cluster), '1')))

	base_biplot <- ggplot() +
		coord_cartesian(clip = 'off') +
		xlab(x_lab) + ylab(y_lab)

	# MCP for each cluster
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
	# skip first cluster bc it's for singletons
	biplot <- base_biplot
	for (i in 2:length(unique_clusters)) {

		biplot <- biplot +
			geom_polygon(
				data = polys[[i]],
				aes(x = PC1, y = PC2),
				# color = 'gray80',
				color = '#a6cee3',
				# fill = alpha('gray', 0.6)
				fill = alpha('#a6cee3', 0.5)
				# fill = alpha(cluster_cols[i], 0.4)
			)

	}

	# add raster labels
	period_values <- c('Present' = '#66c2a5', 'Mid' = '#8da0cb', 'Late' = '#fc8d62')
	biplot <- biplot +
		geom_shadowtext(
			data = pcs, aes(x = PC1, y = PC2, label = raster, color = period), 
			size = 4,
			bg.colour = alpha('black', 0.2), bg.r = 0.05,
			fontface = 'bold'
		) +
		# geom_text(
			# data = pcs,
			# aes(x = PC1, y = PC2, label = raster, color = period),
			# size = 6,
			# fontface = 'bold'
		# ) +
		labs(color = 'Period') +
		scale_color_manual(values = period_values) +
		guides(color = guide_legend(override.aes = list(label = 'ABC', size = 5))) +
		ggtitle('B') +
		coord_fixed() +
		theme_minimal() +
		theme(
			legend.position = 'none',
			plot.title = element_text(size = 18),
			axis.title = element_text(size = 15),
			axis.text = element_text(size = 12),
			legend.title = element_text(size = 16),
			legend.text = element_text(size = 14)
		)


	### dendrogram
	clust <- hclust(dists, method = 'complete')
	
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
		geom_text(data = labs, aes(x = x, y = y, label = raster, color = period), hjust = hjust, size = 3, angle = 90) +
		scale_color_manual(values = period_values) +
		guides(color = guide_legend(override.aes = list(label = 'AB', size = 5))) +
		# scale_y_reverse() +
		coord_cartesian(clip = 'off') +
		labs(y = NULL, color = 'Period') +
		ggtitle('A') +
		theme_minimal() +
		theme(
			plot.title = element_text(size = 18),
			axis.title = element_blank(),
			axis.text = element_blank(),
			axis.ticks = element_blank(),
			panel.grid.major = element_blank(),
			panel.grid.minor = element_blank(),
			legend.position = c(0.1, 0.9),
			legend.justification = c(0.1, 0.9),
			legend.title = element_text(size = 16),
			legend.text = element_text(size = 14)
		)


	combo <- plot_grid(dendro, biplot, nrow = 2)

	# ggsave(combo, filename = paste0(out_dir, '/Cluster Analysis of Rasters - Biplot and Dendrogram of All Rasters.png'), width = 20, height = 10, dpi = 600, bg = 'white')
	ggsave(combo, filename = paste0(out_dir, '/Cluster Analysis of Rasters - Biplot and Dendrogram of All Rasters DBSCAN.png'), width = 8.5, height = 11, dpi = 600, bg = 'white')

say('DONE', level = 1)
