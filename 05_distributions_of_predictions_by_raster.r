### SDM METHODOLOGICAL PLURALITY
### Anna Thonis, Nikki Calavari, Uzma Ashraf, Toni Lyn Morelli, and Adam B. Smith*
### * adam.smith@mobot.org | Missouri Botanical Garden | 2025-10
###
### Violin plots of predictions by raster/team
###
### source('C:/Kaji/Research Group/ENMs - Plurality of Modeling Workflows (Anna Thonis)/enm_methods_plurality/05_distributions_of_predictions_by_raster.r')
###
### CONTENTS ###
### setup ###
### distributions of predictions by team ###

#############
### setup ###
#############

	rm(list = ls())
	source('C:/Kaji/Research Group/ENMs - Plurality of Modeling Workflows (Anna Thonis)/enm_methods_plurality/00_shared_constants_and_functions.r')

	dirCreate(paste0('./Outputs ', species_full, '/Cluster Analysis of Rasters'))

say('############################################')
say('### distributions of predictions by team ###')
say('############################################')

	### cluster
	preds <- load_predictions(species_focal = species_focal, period = 'all', scale = TRUE, subset_teams = TRUE)
	preds_trans <- t(preds)

	pca <- prcomp(preds_trans)
	scores <- pca$x[ , 1:2]
	scores <- as.data.frame(scores)

	dists <- dist(scores)
	clust <- hclust(dists, method = 'complete')
	periods <- c('present', 'mid', 'late')
	distrib <- list()
	
	ks <- get_ks()
	for (k in ks) {

		for (i in seq_along(periods)) {

			period <- periods[i]
			say(k, ' ', period)

			# cluster
			clusters <- cutree(clust, k = k)
			clusters <- clusters[grepl(names(clusters), pattern = period)]

			# subset to just rasters for this time period
			keeps <- grepl(names(preds), pattern = period)
			this_preds <- preds[ , ..keeps]
			long <- melt(this_preds, measure.vars = names(this_preds), variable.name = 'raster', value.name = 'prediction', id.vars = NULL)
			long <- as.data.table(long)
			long[ , site := rep(1:nrow(this_preds), times = ncol(this_preds))]

			# add cluster number
			long[ , cluster := clusters[raster]]

			# calculate average prediction at each site
			mean_preds <- long[ , .(prediction = mean(prediction)), by = site]
			mean_preds[ , raster := 'Mean']
			long <- rbind(long, mean_preds, fill = TRUE)
			long$cluster[long$raster == 'Mean'] <- 0

			long[ , raster := scrub_period(raster)]
			names(clusters) <- scrub_period(names(clusters))

			# # order factors by mean predictions
			# ordered_teams <- names(clusters)
			# ordered_teams <- c('Mean' = 'Mean', ordered_teams)
			# long$raster <- factor(long$raster, levels = ordered_teams)
			# long[ , cluster := as.factor(cluster)]

			n_rasts <- length(unique(long$raster))

			if (period == 'present') {
				title <- 'A) Present'
				width <- 80
			} else if (period == 'mid') {
				title <- 'B) Mid-century'
				width <- 8
			} else if (period == 'late') {
				title <- 'C) Late century'
				width <- 8
			}

			n_clusters <- length(unique(clusters))
			# cluster_cols <- get_cluster_cols(n_clusters)
			# cluster_cols <- c('gray80', cluster_cols)

			# reorder rasters by cluster (keeping 'Mean' first)
			cluster_order <- long[raster != 'Mean', .(mean_pred = mean(prediction), cluster = unique(cluster)), by = raster]
			cluster_order <- cluster_order[order(cluster, mean_pred)]
			ordered_teams <- c('Mean', cluster_order$raster)
			long$raster <- factor(long$raster, levels = ordered_teams)

			long[ , cluster := factor(cluster)]

			distrib[[i]] <- ggplot(long, aes(x = raster, y = prediction, fill = cluster)) +
				geom_violin(width = width, alpha = 0.6) +
				# scale_fill_manual(values = cluster_cols, name = 'Cluster') +
				scale_x_discrete(expand = expansion(mult = c(0.01, 0.01))) +
				coord_cartesian(xlim = c(0, n_rasts + 0.75)) +
				ylab('Prediction (scaled to [0, 1])') +
				labs(fill = 'Cluster') +
				theme(
					legend.position = 'none',
					axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
					axis.title.x = element_blank()
				) +
				ggtitle(title)

		} # next period

		distribs <- plot_grid(plotlist = distrib, nrow = 3, align = 'v')

		ggsave(distribs, filename = paste0(out_dir, '/Cluster Analysis of Rasters/Distribution of Predictions by Team, k = ', k, '.png'), width = 8.5, height = 11, dpi = 600, bg = 'white')

	} # next k

say('DONE', level = 1)
