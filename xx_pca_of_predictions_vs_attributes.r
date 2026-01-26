### SDM METHODOLOGICAL PLURALITY
### Anna Thonis, Nikki Calavari, Uzma Ashraf, Toni Lyn Morelli, and Adam B. Smith*
### * adam.smith@mobot.org | Missouri Botanical Garden | 2025-10
###
### Make figures of dendrogram and PCA clustering of rasters using hierarchical clustering.
###
### source('C:/Kaji/Research Group/ENMs - Plurality of Modeling Workflows (Anna Thonis)/enm_methods_plurality08_cluster_analysis_of_teams_hierarchical_with_attributes.r')
###
### CONTENTS ###
### setup ###
### cluster analysis of teams: all periods ###

#############
### setup ###
#############

	rm(list = ls())
	source('C:/Kaji/Research Group/ENMs - Plurality of Modeling Workflows (Anna Thonis)/enm_methods_plurality/00_shared_constants_and_functions.r')

	this_out_dir <- paste0(out_dir, '/Team Distances ~ Decision Distances')

say('#################################')
say('### cluster analysis of teams ###')
say('#################################')

	preds <- load_predictions(period = 'all', scale = TRUE, subset_teams = TRUE)
	preds <- t(preds)

	pca <- prcomp(preds)
	scores <- pca$x[ , 1:2]
	scores <- as.data.frame(scores)

	attributes <- readRDS(paste0(this_out_dir, '/workflow_attributes.rds'))
	attributes <- attributes[, .SD, .SDcols = function(x) !anyNA(x)]

	fit <- vegan::envfit(scores, attributes, permutations = 999)

	dists <- dist(scores)

	### PCA biplot
	##############

		### plots of PCA scores and team groups
		pcs <- scores
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

		# arrows
		mult <- 1000 # multiple r by this amount 
		arrows_df <- as.data.frame(fit$vectors$arrows * mult * fit$vectors$r)
		arrows_df$var <- rownames(arrows_df)

		biplot <- ggplot() +
			geom_shadowtext(
				data = pcs, aes(x = PC1, y = PC2, label = raster, color = period), 
				size = 4,
				bg.colour = alpha('black', 0.2), bg.r = 0.05,
				fontface = 'bold'
			) +
			geom_segment(
				data = arrows_df,
				aes(x = 0, y = 0, xend = PC1, yend = PC2),
				arrow = arrow(length = unit(0.25, 'cm')),
				color = 'red'
			) +
  			geom_text(
				data = arrows_df,
				aes(x = PC1, y = PC2, label = var),
				color = 'red', vjust = -0.5
			) +
			labs(color = 'Period') +
			xlab(x_lab) + ylab(y_lab) +
			coord_cartesian(clip = 'off') +
			scale_color_manual(values = period_colors) +
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

		ggsave(biplot, filename = paste0(this_out_dir, '/Biplot with Workflow Attributes.png'), width = 8.5, height = 8.5, dpi = 600, bg = 'white')

say('DONE', level = 1)
