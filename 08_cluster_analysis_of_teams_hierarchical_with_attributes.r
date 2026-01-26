### SDM METHODOLOGICAL PLURALITY
### Anna Thonis, Nikki Calavari, Uzma Ashraf, Toni Lyn Morelli, and Adam B. Smith*
### * adam.smith@mobot.org | Missouri Botanical Garden | 2025-10
###
### Make biplot of PCA on team predictions and overlay workflow attributes.
###
### source('C:/Kaji/Research Group/ENMs - Plurality of Modeling Workflows (Anna Thonis)/enm_methods_plurality/08_pca_of_predictions_vs_attributes.r')
###
#############
### setup ###
#############

	rm(list = ls())
	source('C:/Kaji/Research Group/ENMs - Plurality of Modeling Workflows (Anna Thonis)/enm_methods_plurality/00_shared_constants_and_functions.r')

	this_out_dir <- paste0(out_dir, '/Team Distances ~ Decision Distances')

	### PCA-based distances between team predictions
	################################################

	preds <- load_predictions(period = 'all', scale = TRUE, subset_teams = TRUE)
	preds <- t(preds)

	pca <- prcomp(preds)
	scores <- pca$x[ , 1:2]
	scores <- as.data.frame(scores)

	### fit workflow attributes to PC scores
	########################################
	attributes <- readRDS(paste0(this_out_dir, '/workflow_attributes.rds'))

	removes <- c('team_code', 'raster_name')
	attributes <- attributes[, !..removes]

	attributes <- attributes[ , .SD, .SDcols = function(x) !anyNA(x)]
	names(attributes) <- paste0('X', names(attributes))

	# convert binary columns to yes/no
	binary_cols <- names(attributes)[sapply(attributes, function(x) all(x %in% c(0, 1)))]
	attributes[ , (binary_cols) := lapply(.SD, function(x) factor(ifelse(x == 1, 'yes', 'no'))), .SDcols = binary_cols]

	# fit <- vegan::envfit(scores, attributes, permutations = 99999)
	fit <- vegan::envfit(scores, attributes, permutations = 999)
	say('Only permuting 999 times!!!!!!!', level = 1)
	
	# subset to only significant continuous variables (P <= 0.05)
	sig_vars <- fit$vectors$pvals <= 0.05
	if (any(sig_vars)) {
		fit$vectors$arrows <- fit$vectors$arrows[sig_vars, , drop = FALSE]
		fit$vectors$r <- fit$vectors$r[sig_vars]
		fit$vectors$pvals <- fit$vectors$pvals[sig_vars]
	}

	# subset to only significant factor variables (P <= 0.05)
	sig_vars <- fit$factors$pvals <= 0.05

	removes <- names(sig_vars)[!sig_vars]
	for (remove in removes) {
		y <- attributes[[remove]]
		remove_levels <- paste0(remove, unique(y))
		remove_index <- rownames(fit$factors$centroids) %in% remove_levels
		fit$factors$centroids <- fit$factors$centroids[!remove_index, , drop = FALSE]
	}

	fit$factors$r <- fit$factors$r[sig_vars]
	fit$factors$pvals <- fit$factors$pvals[sig_vars]

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

		# arrows for numeric workflow attributes
		mult <- 500 # multiple r by this amount 
		arrows_df <- as.data.frame(fit$vectors$arrows * mult * fit$vectors$r)
		arrows_df$var <- rownames(arrows_df)

		# centroids for factor workflow attributes
		mult <- 30 # multiple r by this amount 
		cents_df <- as.data.frame(fit$factors$centroids)
		cents_df$var <- rownames(fit$factors$centroids)
		rownames(cents_df) <- NULL
		facts <- names(fit$factors$r)
		for (fact in facts) {
		
			levs <- attributes[[fact]]
			levs <- as.character(levs)
			levs <- unique(levs)
			if (all(levs %in% c('yes', 'no'))) levs <- 'yes' # remove "no"
			fact_levs <- paste0(fact, levs)
			index <- which(cents_df$var %in% fact_levs)
			r <- fit$factor$r[names(fit$factor$r) == fact]
			cents_df$PC1[index] <- cents_df$PC1[index] * sqrt(mult * r)
			cents_df$PC2[index] <- cents_df$PC2[index] * sqrt(mult * r)
		
		}

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
			geom_point(
				data = cents_df,
				aes(x = PC1, y = PC2),
				color = 'red'
			) +
  			geom_text(
				data = cents_df,
				aes(x = PC1, y = PC2, label = var),
				color = 'red', vjust = -0.5
			) +
			labs(color = 'Period') +
			xlab(x_lab) + ylab(y_lab) +
			coord_cartesian(clip = 'off') +
			scale_color_manual(values = period_colors) +
			guides(color = guide_legend(override.aes = list(label = 'ABC', size = 5))) +
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
