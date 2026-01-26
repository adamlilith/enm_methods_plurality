### SDM METHODOLOGICAL PLURALITY
### Anna Thonis, Nikki Calavari, Uzma Ashraf, Toni Lyn Morelli, and Adam B. Smith*
### * adam.smith@mobot.org | Missouri Botanical Garden | 2025-10
###
### Make heatmaps of correlations between predictions by each team.
###
### source('C:/Kaji/Research Group/ENMs - Plurality of Modeling Workflows (Anna Thonis)/enm_methods_plurality/04_heatmaps_of_correlations_between_predictions.r')
###
### CONTENTS ###
### setup ###
### heatmaps of correlations between predictions ###

#############
### setup ###
#############

	rm(list = ls())
	source('C:/Kaji/Research Group/ENMs - Plurality of Modeling Workflows (Anna Thonis)/enm_methods_plurality/00_shared_constants_and_functions.r')

	dirCreate(paste0('./Outputs ', species_full, '/Cluster Analysis of Rasters'))

say('####################################################')
say('### heatmaps of correlations between predictions ###')
say('####################################################')

	### Create heatmaps of correlations between predictions made by each team in each time period for one species.
	periods <- c('present', 'mid', 'late')
	min_cor <- Inf
	heats <- list()
	for (i in seq_along(periods)) {

		period <- periods[i]
		
		wide <- load_predictions(period = period, scale = FALSE, subset_teams = TRUE)

		period_nice <- get_nice_period(period)
		title <- paste0(LETTERS[i], ') ', period_nice)

		### correlation heatmap

		cors <- cor(wide, method = 'spearman')

		# clustering by correlation
		if (anyNA(cors)) {
		
			not_na <- which(!is.na(cors[1, ]))
			cors_not_na <- cors[not_na, not_na]

			cor_dist <- as.dist(1 - abs(cors_not_na))
			hc <- hclust(cor_dist)
			order <- hc$order
			rast_names <- colnames(cors_not_na)[order]
			na_rast_names <- colnames(cors)[colnames(cors) %notin% rast_names]
			rast_names <- c(rast_names, na_rast_names)
		
		} else {

			hc <- hclust(as.dist(1 - abs(cors)))
			order <- hc$order
			rast_names <- colnames(cors)[order]

		}

		# reorder the correlation matrix
		cors_ordered <- cors[rast_names, rast_names]
		rast_names <- scrub_period(rast_names)

		# long form for ggplot
		cors_long <- melt(cors_ordered)
		colnames(cors_long) <- c('team_1', 'team_2', 'correl')
		cors_long$team_1 <- scrub_period(cors_long$team_1)
		cors_long$team_2 <- scrub_period(cors_long$team_2)

		# heatmap
		cors_long$team_1 <- factor(cors_long$team_1, levels = rast_names)
		cors_long$team_2 <- factor(cors_long$team_2, levels = rast_names)
		
		axis_text_size <- if (period == 'present') { 8 } else { 7 }

		# color by team name (first letter in raster name)
		team_colors <- setNames(
			rainbow(length(unique(substr(rast_names, 1, 1)))),
			sort(unique(substr(rast_names, 1, 1)))
		)
		
		axis_colors_x <- team_colors[substr(rast_names, 1, 1)]
		axis_colors_y <- team_colors[substr(rast_names, 1, 1)]
		
		heats[[length(heats) + 1]] <- ggplot(cors_long, aes(x = team_1, y = team_2, fill = correl)) +
			geom_tile() +
			theme_minimal() +
			theme(
				legend.title = element_text(size = 10),
				axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = axis_text_size, color = axis_colors_x),
				axis.text.y = element_text(size = axis_text_size, color = axis_colors_y),
				aspect.ratio = 1
			) +
			labs(title = title, x = NULL, y = NULL)


		min_cor <- min(min_cor, cors_long$correl)

	} # next period

	for (i in seq_along(periods)) heats[[i]] <- heats[[i]] +
		scale_fill_viridis_c(option = 'magma', name = 'Spearman\nCorrelation', limits = c(min_cor, 1))

	# Extract legend from first plot
	legend <- get_legend(heats[[1]])
	
	# Remove legends from all plots
	heats_no_legend <- lapply(heats, function(x) x + theme(legend.position = 'none'))
	
	# Combine plots and add legend to the right
	heatmaps <- plot_grid(
		plot_grid(plotlist = heats_no_legend, ncol = 3),
		legend,
		ncol = 2,
		rel_widths = c(1, 0.06)
	)

	ggsave(heatmaps, filename = paste0('./Outputs ', species_full, '/Cluster Analysis of Rasters/Heatmap of Spearman Correlations between Rasters.png'), width = 12, height = 4, dpi = 600, bg = 'white')

say('DONE', level = 1)
