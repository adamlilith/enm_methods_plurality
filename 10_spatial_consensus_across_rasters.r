### SDM METHODOLOGICAL PLURALITY
### Anna Thonis, Nikki Calavari, Uzma Ashraf, Toni Lyn Morelli, and Adam B. Smith*
### * adam.smith@mobot.org | Missouri Botanical Garden | 2025-10
###
### Analysis of spatial consensus across rasters.
###
### source('C:/Kaji/Research Group/ENMs - Plurality of Modeling Workflows (Anna Thonis)/enm_methods_plurality/10_spatial_consensus_across_rasters.r')
###
### CONTENTS
### setup ###
### consensus in ranking sites ###
### consensus in assigning locations to quartiles of suitability ###
### maps illustrating calculation of consensus in assigning locations to quartiles of suitability ###
### consensus in identifying climate change refugia ###
###
#############
### setup ###
#############

	rm(list = ls())
	source('C:/Kaji/Research Group/ENMs - Plurality of Modeling Workflows (Anna Thonis)/enm_methods_plurality/00_shared_constants_and_functions.r')

	this_out_dir <- paste0(out_dir, '/Spatial Consensus')
	dirCreate(this_out_dir)

# say('##################################')
# say('### consensus in ranking sites ###')
# say('##################################')

# 	# Make a plot with mean prediction across rasters at random sites along the x axis and each team's ranked predictions along the y. If all teams agree exactly in rank prediction, we should have a straight line with an upward slope that intersects the axis at y = rank = 1 at the lowest value of the mean prediction across sites and at y = rank = maximum(rank) at the highest value of the mean prediction across sites.

# 	# predictions
# 	preds <- load_predictions(period = 'all', scale = TRUE, subset_teams = TRUE)

# 	preds_rank <- apply(preds, 2, rank)
# 	preds_rank <- as.data.table(preds_rank)
# 	preds_rank[ , pred_mean := rowMeans(preds)]

# 	preds_rank_long <- melt(preds_rank, measure.vars = names(preds), variable.name = 'team', value.name = 'rank', id.vars = c('pred_mean'))
# 	preds_rank_long <- as.data.table(preds_rank_long)

# 	# remove rasters N3 and N4 for Prionailurus bengalensis futures bc these are all zeros
# 	bads <- paste0(c('N3a', 'N4a', 'N3b', 'N4b'), '_mid')
# 	removes <- preds_rank_long$team %in% bads
# 	preds_rank_long <- preds_rank_long[!removes]

# 	bads <- paste0(c('N3a', 'N4a', 'N3b', 'N4b'), '_late')
# 	removes <- preds_rank_long$team %in% bads
# 	preds_rank_long <- preds_rank_long[!removes]

# 	xlim <- range(preds_rank_long$pred_mean)
# 	xlim[1] <- max(0, roundTo(xlim[1], 0.05, floor))
# 	xlim[2] <- min(1, roundTo(xlim[2], 0.05, ceiling))

# 	ylim <- c(1, max(preds_rank_long$rank))

# 	abline <- data.table(
# 		x = range(preds_rank_long$pred_mean),
# 		y = c(1, nrow(preds_rank))
# 	)

# 	slope <- (nrow(preds_rank) - 1) / diff(range(preds_rank_long$pred_mean))

# 	plots <- list()
# 	for (period in c('present', 'mid', 'late')) {

# 		say(period)

# 		# subsample to expedite
# 		n <- nrow(preds_rank_long)
# 		n_sample <- round(0.1 * n)
# 		sampled <- preds_rank_long[sample(n, n_sample)]
		
# 		sampled <- sampled[grepl(sampled$team, pattern = paste0('_', period)), ]
# 		sampled[ , team := sub(team, pattern = paste0('_', period), replacement = '')]

# 		title <- get_nice_period(period)

# 		plots[[length(plots) + 1]] <- ggplot(sampled, aes(x = pred_mean, y = rank, color = team)) +
# 			geom_point(alpha = 0.03, pch = 16) +
# 			geom_smooth(method = 'loess', se = FALSE) +
# 			geom_smooth(aes(x = pred_mean, y = rank), inherit.aes = FALSE, method = 'loess', se = FALSE, color = 'black', linewidth = 2) +
# 			geom_abline(slope = slope, intercept = 0, linetype = 'dashed', color = 'black', linewidth = 1.2) +
# 			# coord_cartesian(xlim = xlim, ylim = ylim) +
# 			xlim(xlim[1], xlim[2]) + ylim(ylim[1], ylim[2]) +
# 			xlab('Mean prediction at site across rasters') + ylab('Rank of prediction at site') +
# 			ggtitle(title) +
# 			labs(color = 'Raster') +
# 			guides(color = guide_legend(ncol = 12)) +
# 			theme(
# 				legend.position = 'bottom',
# 				legend.title = element_text(size = 22),
# 				legend.text = element_text(size = 14),
# 				plot.title = element_text(size = 30),
# 				axis.title = element_text(size = 28),
# 				axis.text = element_text(size = 20)
# 			)
# 	}

# 	combo <- plot_grid(plotlist = plots, nrow = 1, align = 'hv')
# 	ggsave(combo, filename = paste0(this_out_dir, '/Mean Prediction vs Rank of Prediction with Legend.png'), width = 26, height = 10, bg = 'white')

say('####################################################################')
say('### consensus in assigning locations to quartiles of suitability ###')
say('####################################################################')

	# We want to make 1) maps showing the number of rasters that agree randomly located sites shared across rasters are highly suitable, moderately suitable, moderately unsuitable, and poorly suitable. We will divide these classes by classifying each team's predictions into 4 quartiles, then counting, for each site, the number of rasters that agree on the quantile in which the site is ranked.

	# We want to make 2) a matching set of maps that classifies each location into whether it has a number of agreements that is statistically significantly fewer (negative agreement) or more (positive agreement) than expected by chance. Using a binomial distribution with N = total number of rasters in that time period and p = 0.25. Significance of positive agreements is sum(dbinom(agreements:N, size = N, p = 0.25)) and significance of negative agreements is 1 - this number.

	### calculate significance of positive/negative agreement
	# For each row in a data table that has a row for each site and a column for each raster in which the quartile the prediction (row) has been classified into quartiles, calculate the binomial significance of positive and negative agreement.
	#
	# quarts		Data.table with columns like 'A_present', 'B_present', etc with values in {1:4}
	# preds			Data.table with columns like 'A_present', 'B_present', with predicted values (used to get correct dimensions)
	calc_sig_of_agree <- function(quarts, preds) {

		nr <- nrow(preds)
		N <- ncol(preds)
		for (quart in 1:4) {

			quarts[ , DUMMY_n_quarts := rowSums(.SD == quart), .SDcols = names(preds)]

			N <- ncol(preds)
			nr <- nrow(preds)
			
			p_pos_agree <- rep(NA_real_, nr)
			for (i in 1:nr) {
				p_pos_agree[i] <- sum(dbinom(quarts$DUMMY_n_quarts[i]:N, size = N, p = 0.25))
			}
			
			names(quarts)[ncol(quarts)] <- paste0('n_quart_', quart)
			quarts[ , paste0('p_pos_agree_', quart) := p_pos_agree]
			quarts[ , paste0('p_neg_agree_', quart) := 1 - p_pos_agree]

		}
		quarts
	}

	# world vector
	world <- rnaturalearth::ne_countries(scale = 'medium', returnclass = 'sv')
	focal <- vect(paste0(out_dir, '/common_study_region.gpkg'))

	# plot extent
	extent <- ext(focal)
	extent <- as.polygons(extent, crs = focal)
	extent <- buffer(extent, 20000)
	extent <- ext(extent)
	extent <- as.vector(extent)

	# predictions
	preds_xy <- load_predictions(period = 'all', scale = TRUE, subset_teams = FALSE)
	preds_present <- load_predictions(period = 'present', scale = TRUE, subset_teams = TRUE)
	preds_mid <- load_predictions(period = 'mid', scale = TRUE, subset_teams = TRUE)
	preds_late <- load_predictions(period = 'late', scale = TRUE, subset_teams = TRUE)

	# classify each raster's predictions into quartiles
	quartile_fx <- function(x, probs = c(0.25, 0.5, 0.75)) {
	
		out <- rep(NA_integer_, length(x))

		# is raster binary thresholded? if so, assign all 0s to quartile 1 and all 1s to quartile 4
		if (all(x %in% c(0, 1))) {
			out[x == 0] <- 1
			out[x == 1] <- 4
		} else {

			quants <- quantile(x, probs = probs)
			out[x <= quants[1]] <- 1
			out[x > quants[1] & x <= quants[2]] <- 2
			out[x > quants[2] & x <= quants[3]] <- 3
			out[x > quants[3]] <- 4

		}
		out
	
	}

	quarts_present <- apply(preds_present, 2, quartile_fx)
	quarts_mid <- apply(preds_mid, 2, quartile_fx)
	quarts_late <- apply(preds_late, 2, quartile_fx)

	quarts_present <- as.data.table(quarts_present)
	quarts_mid <- as.data.table(quarts_mid)
	quarts_late <- as.data.table(quarts_late)

	# proportion of rasters that have predictions falling into each quartile
	quarts_present[ , quart_1 := rowSums(.SD == 1) / ncol(.SD), .SDcols = names(preds_present)]
	quarts_present[ , quart_2 := rowSums(.SD == 2) / ncol(.SD), .SDcols = names(preds_present)]
	quarts_present[ , quart_3 := rowSums(.SD == 3) / ncol(.SD), .SDcols = names(preds_present)]
	quarts_present[ , quart_4 := rowSums(.SD == 4) / ncol(.SD), .SDcols = names(preds_present)]

	quarts_mid[ , quart_1 := rowSums(.SD == 1) / ncol(.SD), .SDcols = names(preds_mid)]
	quarts_mid[ , quart_2 := rowSums(.SD == 2) / ncol(.SD), .SDcols = names(preds_mid)]
	quarts_mid[ , quart_3 := rowSums(.SD == 3) / ncol(.SD), .SDcols = names(preds_mid)]
	quarts_mid[ , quart_4 := rowSums(.SD == 4) / ncol(.SD), .SDcols = names(preds_mid)]

	quarts_late[ , quart_1 := rowSums(.SD == 1) / ncol(.SD), .SDcols = names(preds_late)]
	quarts_late[ , quart_2 := rowSums(.SD == 2) / ncol(.SD), .SDcols = names(preds_late)]
	quarts_late[ , quart_3 := rowSums(.SD == 3) / ncol(.SD), .SDcols = names(preds_late)]
	quarts_late[ , quart_4 := rowSums(.SD == 4) / ncol(.SD), .SDcols = names(preds_late)]

	### calculate significance of positive/negative agreement
	quarts_present <- calc_sig_of_agree(quarts_present, preds_present)
	quarts_mid <- calc_sig_of_agree(quarts_mid, preds_mid)
	quarts_late <- calc_sig_of_agree(quarts_late, preds_late)

	# assign quartile proportions to same spatial data.table
	# calculate maximum proportion so we can cap the display of colors to a common value
	pred_max <- agree_max <- -Inf
	pred_min <- agree_min <- Inf
	xy <- preds_xy[ , c('longitude', 'latitude')]
	for (period in c('present', 'mid', 'late')) {
		for (quart in 1:4) {

			# min/max for this set of agreement scores	
			yy <- get(paste0('quarts_', period))
			y <- yy[[paste0('quart_', quart)]]
			agree_min <- min(agree_min, min(y))
			agree_max <- max(agree_max, max(y))

			# remember agreement rate into xy
			xy[[paste0('quart_', quart, '_', period)]] <- y
		
			# remember significance
			xy[ , paste0('sig_pos_neg_agree_', quart, '_', period) := 'Random']
			index <- yy[[paste0('p_pos_agree_', quart)]] <= 0.05
			xy[index, paste0('sig_pos_neg_agree_', quart, '_', period) := 'Positive']
			index <- yy[[paste0('p_neg_agree_', quart)]] <= 0.05
			xy[index, paste0('sig_pos_neg_agree_', quart, '_', period) := 'Negative']
		
			# min/max for this set of predictions
			y <- get(paste0('preds_', period))
			y <- rowMeans(y)
			pred_min <- min(y, min(y))
			pred_max <- max(y, max(y))

		}
	}

	# limits for agreement
	agree_lims <- c(agree_min, agree_max)
	pred_lims <- c(pred_min, pred_max)

	xy[ , pred_mean_present := rowMeans(preds_present)]
	xy[ , pred_mean_mid := rowMeans(preds_mid)]
	xy[ , pred_mean_late := rowMeans(preds_late)]

	pred_lims <- range(c(xy$pred_mean_present, xy$pred_mean_mid, xy$pred_mean_late))

	xy <- vect(xy, geom = c('longitude', 'latitude'), crs = getCRS('WGS84'))

	# size of random points
	point_size <- 0.01

	# color to fill basemap
	world_fill <- 'gray80'

	hist_preds_y_max <- hist_counts_agree_y_max <- hist_proportion_agree_y_max <- -Inf
	# hists_count_num_sigs <- hists_proportion_agree <- maps <- maps_sig <- list()
	hists_count_num_sigs <- maps <- maps_sig <- list()
	for (period in c('present', 'mid', 'late')) {

		### binomial distribution lower 5th and upper 95th significance limits
		# used to color histogram based on positive/negative/random agreement
		x <- get(paste0('preds_', period))
		N <- ncol(x)
		n <- 0:N
		sigs <- dbinom(n, size = N, prob = 0.25)
		
		map_cols <- rep('yellow', N)

		# lower limit
		cumsums <- cumsum(sigs)
		cumsums_lte_05 <- cumsums
		cumsums_lte_05[cumsums_lte_05 > 0.05] <- -Inf
		map_cols[!is.infinite(cumsums_lte_05)] <- '#7b3294'
		agree_sig_lim_05 <- (which.max(cumsums_lte_05) - 1) / N

		# upper limit
		cumsums <- cumsum(sigs)
		cumsums_gte_95 <- cumsums
		cumsums_gte_95[cumsums_gte_95 < 0.95] <- Inf
		map_cols[!is.infinite(cumsums_gte_95)] <- '#008837'
		agree_sig_lim_95 <- (which.min(cumsums_gte_95) - 1) / N

		for (quart in 4:1) {

			### map of mean prediction
			if (quart == 4) {

				k <- length(maps) + 1
				letter <- letters[k]
				title <- paste0(letter, ') Mean prediction')

				this_xy <- xy
				this_xy[['y']] <- this_xy[[paste0('pred_mean_', period)]]

				# map of mean predicted values
				maps[[k]] <- maps_sig[[k]] <- ggplot() +
					layer_spatial(world, fill = world_fill) +
					layer_spatial(this_xy, aes(color = y), size = point_size) +
					# scale_color_gradientn(colors = rev(RColorBrewer::brewer.pal(11, 'Spectral')), name = 'Prediction', limits = pred_lims) +
					# scale_color_viridis_c(option = 'cividis', name = 'Prediction', limits = pred_lims) +
					scale_color_viridis_c(option = 'magma', name = 'Prediction', limits = pred_lims) +
					ggtitle(title, subtitle = get_nice_period(period))

				names(maps)[length(maps)] <- paste0('mean_', period)
			
				### histogram of mean predicted values
				this_xy <- as.data.table(this_xy)
				# hists_proportion_agree[[length(hists_proportion_agree) + 1]] <- hists_count_num_sigs[[length(hists_count_num_sigs) + 1]] <- ggplot(this_xy, aes(x = y, y = after_stat(count / sum(count)))) +
				hists_count_num_sigs[[length(hists_count_num_sigs) + 1]] <- ggplot(this_xy, aes(x = y, y = after_stat(count / sum(count)))) +
					geom_histogram(aes(fill = after_stat(x)), bins = 100) +
					# scale_fill_viridis_c(option = 'cividis', name = NULL, limits = pred_lims) +
					scale_fill_viridis_c(option = 'magma', name = NULL, limits = pred_lims) +
					xlab('Mean predicted value') + ylab('Proportion of sites') +
					ggtitle(title, subtitle = get_nice_period(period))

				# get y-axis limit
				y_lims <- layer_scales(hists_count_num_sigs[[length(hists_count_num_sigs)]])$y$range$range
				hist_preds_y_max <- max(hist_preds_y_max, y_lims[2])

				# names(hists_proportion_agree)[length(hists_proportion_agree)] <- paste0('mean_', period)
				names(hists_count_num_sigs)[length(hists_count_num_sigs)] <- paste0('mean_', period)

			}

			k <- length(maps) + 1
			letter <- letters[k]
			suit_nice <- if (quart == 1) {
				'Highly unsuitable'
			} else if (quart == 2) {
				'Moderately unsuitable'
			} else if (quart == 3) {
				'Moderately suitable'
			} else {
				'Highly suitable'
			}
			title <- paste0(letter, ') ', suit_nice)

			### map of proportion agreement
			##############################
			this_xy <- xy
			x <- this_xy[[paste0('quart_', quart, '_', period)]]
			this_xy[['agree']] <- x

			maps[[k]] <- ggplot() +
				layer_spatial(world, fill = world_fill) +
				layer_spatial(this_xy, aes(color = agree), size = point_size) +
				scale_color_gradientn(colors = map_cols, name = 'Neg    Random                Pos', limits = agree_lims) +
				ggtitle(title, subtitle = get_nice_period(period))

			names(maps)[length(maps)] <- paste0(period, '_quart_', quart)

			### map of agreement significance
			#################################

			this_xy <- xy
			x <- this_xy[[paste0('sig_pos_neg_agree_', quart, '_', period)]]
			this_xy[['pos_neg']] <- x

			maps_sig[[k]] <- ggplot() +
				layer_spatial(world, fill = world_fill) +
				layer_spatial(this_xy, aes(color = pos_neg), size = point_size) +
				scale_color_manual(
					name = 'Dis/agreement',
					values = c('Negative' = '#7b3294', 'Random' = 'yellow', 'Positive' = '#008837'),
					labels = c('Negative' = 'Neg', 'Random' = 'Random', 'Positive' = 'Pos')
				) +
				guides(
					color = guide_legend(
						override.aes = list(
							size = 3, pch = 22, color = 'black',
							fill = c('#7b3294', 'yellow', '#008837')
						)
					)
				 ) +
				ggtitle(title, subtitle = get_nice_period(period))

			names(maps_sig)[length(maps_sig)] <- paste0(period, '_quart_', quart)

			### histogram of sites in each agreement class (positive/random/negative)
			#########################################################################

			this_xy <- xy
			this_xy[['agree']] <- this_xy[[paste0('quart_', quart, '_', period)]]
			this_xy <- as.data.table(this_xy)

			n_rasters <- if (period == 'present') {
				ncol(preds_present)
			} else if (period == 'mid') {
				ncol(preds_mid)
			} else if (period == 'late') {
				ncol(preds_late)
			}

			# count occurrences of each agreement value
			agree_counts <- this_xy[, .N, by = agree]
			agree_counts[ , proportion := N / sum(agree_counts$N)]
			agree_counts <- agree_counts[order(agree)]
			
			neg <- sum(agree_counts$proportion[agree_counts$agree <= agree_sig_lim_05 + eps()])
			pos <- sum(agree_counts$proportion[agree_counts$agree >= agree_sig_lim_95 + eps()])
			rand <- sum(agree_counts$proportion[agree_counts$agree > agree_sig_lim_05 + eps() & agree_counts$agree < agree_sig_lim_95 + eps()])
			neg <- sprintf('%.2f', round(neg, 2))
			pos <- sprintf('%.2f', round(pos, 2))
			rand <- sprintf('%.2f', round(rand, 2))
			neg <- paste0('Neg\n(', neg, ')')
			pos <- paste0('Pos\n(', pos, ')')
			rand <- paste0('Rand\n(', rand, ')')
			
			agree_counts[ , agreement := rand]
			agree_counts$agreement[agree_counts$agree <= agree_sig_lim_05 + eps()] <- neg
			agree_counts$agreement[agree_counts$agree >= agree_sig_lim_95 - eps()] <- pos
			agree_counts[ , agreement := factor(agreement, levels = c(neg, rand, pos))]
			
			fills <- c('#7b3294', 'yellow', '#008837')
			names(fills) <- c(neg, rand, pos)

			hists_count_num_sigs[[length(hists_count_num_sigs) + 1]] <- ggplot(agree_counts, aes(x = agree, y = proportion)) +
				# geom_col(aes(fill = agreement), color = 'gray40', linewidth = 0.4) +
				geom_col(aes(fill = agreement)) +
				scale_fill_manual(
					# name = 'Dis/Agreement',
					name = NULL,
					values = fills
				) +
				xlab('Proportion of rasters in agreement') + ylab('Proportion of sites') +
				ggtitle(title, subtitle = get_nice_period(period))

			# get y-axis limit
			y_lims <- layer_scales(hists_count_num_sigs[[length(hists_count_num_sigs)]])$y$range$range
			hist_counts_agree_y_max <- max(hist_counts_agree_y_max, y_lims[2])

			names(hists_count_num_sigs)[length(hists_count_num_sigs)] <- paste0(period, '_quart_', quart)

			# ### histogram of agreement proportion
			# #####################################
			# this_xy <- xy
			# this_xy <- as.data.table(this_xy)
			# this_xy[['agree']] <- this_xy[[paste0('quart_', quart, '_', period)]]
			
			# # create categorical agreement levels for legend
			# this_xy[ , agree_cat := 'Random']
			# this_xy$agree_cat[this_xy$agree <= agree_sig_lim_05 + eps()] <- 'Negative'
			# this_xy$agree_cat[this_xy$agree >= agree_sig_lim_95 - eps()] <- 'Positive'
			# this_xy[ , agree_cat := factor(agree_cat, levels = c('Negative', 'Random', 'Positive'))]
			
			# hists_proportion_agree[[length(hists_proportion_agree) + 1]] <- ggplot(this_xy, aes(x = agree, y = after_stat(count / sum(count)))) +
			# 	geom_histogram(aes(fill = agree_cat), bins = round(0.5 * n_rasters)) +
			# 	scale_fill_manual(
			# 		name = NULL,
			# 		values = c('Negative' = '#7b3294', 'Random' = 'yellow', 'Positive' = '#008837'),
			# 		labels = c('Negative' = 'Negative', 'Random' = 'Random', 'Positive' = 'Positive')
			# 	) +
			# 	xlab('Proportion of rasters in agreement') + ylab('Proportion of sites') +
			# 	ggtitle(title, subtitle = get_nice_period(period)) +
			# 	theme(legend.position = c(0.95, 0.95), legend.justification = c(1, 1))


			# # get y-axis limit
			# y_lims <- layer_scales(hists_proportion_agree[[length(hists_proportion_agree)]])$y$range$range
			# hist_proportion_agree_y_max <- max(hist_proportion_agree_y_max, y_lims[2])

			# names(hists_proportion_agree)[length(hists_proportion_agree)] <- paste0(period, '_quart_', quart)

		} # next period

	} # next quartile

	for (i in seq_along(maps)) {
	
		# map of proportion of rasters agreeing
		maps[[i]] <- maps[[i]] +
			coord_sf(xlim = extent[1:2], ylim = extent[3:4], expand = FALSE) +
			theme(
				legend.position = 'bottom',
				legend.direction = 'horizontal',
				legend.title.position = 'bottom',
				legend.title = element_text(size = 9),
				legend.text = element_text(size = 8),
				legend.key.width = unit(0.9, 'cm'),
				legend.key.height = unit(0.2, 'cm'),
				legend.margin = margin(t = -5, r = 0, b = 0, l = 0),
				legend.box.margin = margin(t = -3, r = 0, b = 0, l = 0),
				plot.title = element_text(size = 11),
				plot.subtitle = element_text(size = 10),
				axis.title = element_blank(),
				axis.text = element_blank(),
				axis.ticks = element_blank(),
				panel.background = element_blank(),
				panel.border = element_rect(color = 'black', fill = NA, linewidth = 0.4),
				plot.margin = unit(c(0, 0.1, 0, 0.1), 'cm')
			)
	
		# map of significance class
		maps_sig[[i]] <- maps_sig[[i]] +
			coord_sf(xlim = extent[1:2], ylim = extent[3:4], expand = FALSE) +
			theme(
				legend.position = 'bottom',
				legend.direction = 'horizontal',
				legend.title.position = 'bottom',
				legend.title = element_text(size = 9),
				legend.text = element_text(size = 8),
				# legend.key.width = unit(0.7, 'cm'),
				legend.key.height = unit(0.2, 'cm'),
				legend.margin = margin(t = -5, r = 0, b = 0, l = 0),
				legend.box.margin = margin(t = -3, r = 0, b = 0, l = 0),
				plot.title = element_text(size = 11),
				plot.subtitle = element_text(size = 10),
				axis.title = element_blank(),
				axis.text = element_blank(),
				axis.ticks = element_blank(),
				panel.background = element_blank(),
				panel.border = element_rect(color = 'black', fill = NA, linewidth = 0.4),
				plot.margin = unit(c(0, 0.1, 0, 0.1), 'cm')
			)
	
		# histogram of number of sites in each significance class
		hists_count_num_sigs[[i]] <- hists_count_num_sigs[[i]] +
			theme(
				legend.position = 'bottom',
				legend.direction = 'horizontal',
				legend.title.position = 'bottom',
				legend.title = element_text(size = 8),
				legend.text = element_text(size = 7),
				legend.margin = margin(t = -5, r = 0, b = 0, l = 0),
				legend.box.margin = margin(t = -3, r = 0, b = 0, l = 0),
				plot.title = element_text(size = 10),
				plot.subtitle = element_text(size = 8),
				axis.title = element_text(size = 8),
				axis.text = element_text(size = 7),
				# panel.background = element_blank(),
				plot.margin = unit(c(0, 0.4, 0.6, 0.1), 'cm')
			)

		if (grepl(names(hists_count_num_sigs)[i], pattern = 'mean_')) {

			hists_count_num_sigs[[i]] <- hists_count_num_sigs[[i]] +
				theme(
					# legend.key.width = unit(0.3, 'cm'),
					legend.key.height = unit(0.2, 'cm')
				)

		} else {

			hists_count_num_sigs[[i]] <- hists_count_num_sigs[[i]] +
				theme(
					legend.key.width = unit(0.3, 'cm'),
					legend.key.height = unit(0.3, 'cm')
				)

		}

		if (grepl(names(hists_count_num_sigs)[i], pattern = 'mean_')) {
			hists_count_num_sigs[[i]] <- hists_count_num_sigs[[i]] + ylim(0, hist_preds_y_max) + coord_cartesian(xlim = pred_lims)
		}
			
		if (grepl(names(hists_count_num_sigs)[i], pattern = '_quart_')) {
			hists_count_num_sigs[[i]] <- hists_count_num_sigs[[i]] + ylim(0, hist_counts_agree_y_max) + coord_cartesian(xlim = c(0, 1))
		}
	
		# # histogram of proportion in agreement
		# hists_proportion_agree[[i]] <- hists_proportion_agree[[i]] +
		# 	theme(
		# 		legend.position = 'bottom',
		# 		legend.direction = 'horizontal',
		# 		legend.title.position = 'bottom',
		# 		legend.title = element_text(size = 8),
		# 		legend.text = element_text(size = 7),
		# 		legend.margin = margin(t = -5, r = 0, b = 0, l = 0),
		# 		legend.box.margin = margin(t = -3, r = 0, b = 0, l = 0),
		# 		legend.key.height = unit(0.2, 'cm'),
		# 		plot.title = element_text(size = 10),
		# 		plot.subtitle = element_text(size = 8),
		# 		axis.title = element_text(size = 8),
		# 		axis.text = element_text(size = 7),
		# 		# panel.background = element_blank(),
		# 		plot.margin = unit(c(0, 0.4, 0.6, 0.1), 'cm')
		# 	)

		# if (grepl(names(hists_proportion_agree)[i], pattern = 'mean_')) {
		# 	hists_proportion_agree[[i]] <- hists_proportion_agree[[i]] + ylim(0, hist_preds_y_max) + coord_cartesian(xlim = pred_lims)
		# }
			
		# if (grepl(names(hists_proportion_agree)[i], pattern = '_quart_')) {
		# 	hists_proportion_agree[[i]] <- hists_proportion_agree[[i]] + ylim(0, hist_counts_agree_y_max) + coord_cartesian(xlim = c(0, 1))
		# }
	
	}

	map_grid <- plot_grid(plotlist = maps, ncol = 5, align = 'vh')
	ggsave(map_grid, filename = paste0(this_out_dir, '/Maps of Agreement - Proportion of Rasters in Agreement - Map.png'), width = 10.2, height = 9, dpi = 600, bg = 'white')

	map_sig_grid <- plot_grid(plotlist = maps_sig, ncol = 5, align = 'vh')
	ggsave(map_sig_grid, filename = paste0(this_out_dir, '/Maps of Agreement - Site Agreement Significance Class - Map.png'), width = 10.2, height = 9, dpi = 600, bg = 'white')

	hist_grid <- plot_grid(plotlist = hists_count_num_sigs, ncol = 5, align = 'vh')
	ggsave(hist_grid, filename = paste0(this_out_dir, '/Maps of Agreement - Site Agreement Significance Class - Histogram.png'), width = 10.2, height = 7.4, dpi = 600, bg = 'white')

	# hist_grid <- plot_grid(plotlist = hists_proportion_agree, ncol = 5, align = 'vh')
	# ggsave(hist_grid, filename = paste0(this_out_dir, '/Maps of Agreement - Proportion of Rasters in Agreement - Histogram.png'), width = 10.2, height = 7.4, dpi = 600, bg = 'white')

# say('#####################################################################################################')
# say('### maps illustrating calculation of consensus in assigning locations to quartiles of suitability ###')
# say('#####################################################################################################')

# 	# Make three maps, two showing two teams' rasters divided into quartiles, and a third illustrating positive and negative agreement.

# 	# world vector
# 	world <- rnaturalearth::ne_countries(scale = 'medium', returnclass = 'sv')
# 	focal <- vect(paste0(out_dir, '/common_study_region.gpkg'))

# 	# plot extent
# 	extent <- ext(focal)
# 	extent <- as.polygons(extent, crs = focal)
# 	extent <- buffer(extent, 20000)
# 	extent <- ext(extent)
# 	extent <- as.vector(extent)

# 	# predictions
# 	preds_xy <- load_predictions(period = 'all', scale = TRUE, subset_teams = FALSE)
# 	preds_present <- load_predictions(period = 'present', scale = TRUE, subset_teams = TRUE)

# 	# classify each raster's predictions into quartiles
# 	# 1 = not highest, 2 = highest
# 	single_quartile_fx <- function(x, prob = 0.75) {
	
# 		out <- rep(NA_integer_, length(x))

# 		# is raster binary thresholded? if so, assign all 0s to quartile 1 and all 1s to quartile 4
# 		if (all(x %in% c(0, 1))) {
# 			out[x == 0] <- 1
# 			out[x == 1] <- 2
# 		} else {

# 			quant <- quantile(x, prob)
# 			out[x <= quant] <- 1
# 			out[x > quant] <- 2

# 		}
# 		out
	
# 	}

# 	quarts <- apply(preds_present, 2, single_quartile_fx)
# 	quarts <- as.data.table(quarts)
# 	quarts[ , D_present := factor(D_present)]
# 	quarts[ , S_present := factor(S_present)]
# 	quarts[ , c('longitude', 'latitude') := preds_xy[ , c('longitude', 'latitude')]]
# 	quarts[ , agree_pos := (D_present == 2 & S_present == 2)]
# 	quarts[ , agree_neg := (D_present == 1 & S_present == 1)]
# 	quarts[ , disagree := D_present != S_present]

# 	quarts[ , agree_pos := factor(agree_pos)]
# 	quarts[ , agree_neg := factor(agree_neg)]
# 	quarts[ , disagree := factor(disagree)]

# 	quarts <- vect(quarts, geom = c('longitude', 'latitude'), crs = getCRS('WGS84'))
	
# 	world_fill <- 'gray90'
# 	disagree_col <- 'gray60'

# 	maps <- list()

# 	maps$team_1 <- ggplot() +
# 		layer_spatial(world, fill = world_fill) +
# 		layer_spatial(quarts, aes(color = D_present), size = 0.1) +
# 		scale_color_manual(
# 			name = NULL,
# 			values = c('1' = '#182E59', '2' = '#FDE847'),
# 			labels = c('1' = 'Not highest', '2' = 'Highest')
# 		) +
# 		ggtitle('Highest suitability: Team D')

# 	maps$team_2 <- ggplot() +
# 		layer_spatial(world, fill = world_fill) +
# 		layer_spatial(quarts, aes(color = S_present), size = 0.1) +
# 		scale_color_manual(
# 			name = NULL,
# 			values = c('1' = '#182E59', '2' = '#FDE847'),
# 			labels = c('1' = 'Not highest', '2' = 'Highest')
# 		) +
# 		ggtitle('Highest suitability: Team S')

# 	maps$agree_pos <- ggplot() +
# 		layer_spatial(world, fill = world_fill) +
# 		layer_spatial(quarts, aes(color = agree_pos), size = 0.1) +
# 		scale_color_manual(
# 			name = NULL,
# 			values = c('TRUE' = '#008837', 'FALSE' = disagree_col),
# 			labels = c('TRUE' = 'Positive'),
# 			breaks = c('TRUE')
# 		) +
# 		ggtitle('Positive agreement') 

# 	maps$agree_neg <- ggplot() +
# 		layer_spatial(world, fill = world_fill) +
# 		layer_spatial(quarts, aes(color = agree_neg), size = 0.1) +
# 		scale_color_manual(
# 			name = NULL,
# 			values = c('TRUE' = '#7b3294', 'FALSE' = disagree_col),
# 			labels = c('TRUE' = 'Negative'),
# 			breaks = c('TRUE')
# 		) +
# 		ggtitle('Negative agreement')

# 	maps$disagree <- ggplot() +
# 		layer_spatial(world, fill = world_fill) +
# 		layer_spatial(quarts, aes(color = disagree), size = 0.1) +
# 		scale_color_manual(
# 			name = NULL,
# 			values = c('TRUE' = 'yellow', 'FALSE' = disagree_col),
# 			labels = c('TRUE' = 'Random', 'FALSE' = 'Agree')
# 		) +
# 		ggtitle('Disagreement')


# 	for (i in seq_along(maps)) {
	
# 		maps[[i]] <- maps[[i]] +
# 			guides(color = guide_legend(override.aes = list(size = 5, pch = 15), reverse = TRUE)) +
# 			coord_sf(xlim = extent[1:2], ylim = extent[3:4], expand = FALSE) +
# 			theme(
# 				legend.position = 'bottom',
# 				legend.direction = 'horizontal',
# 				legend.title.position = 'bottom',
# 				legend.title = element_text(size = 9),
# 				legend.text = element_text(size = 8),
# 				legend.key = element_blank(),
# 				legend.key.width = unit(0.8, 'cm'),
# 				legend.key.height = unit(0.2, 'cm'),
# 				legend.margin = margin(t = -5, r = 0, b = 0, l = 0),
# 				legend.box.margin = margin(t = -3, r = 0, b = 0, l = 0),
# 				plot.title = element_text(size = 11),
# 				plot.subtitle = element_text(size = 10),
# 				axis.title = element_blank(),
# 				axis.text = element_blank(),
# 				axis.ticks = element_blank(),
# 				panel.background = element_blank(),
# 				panel.border = element_rect(color = 'black', fill = NA, linewidth = 0.4),
# 				plot.margin = unit(c(0, 0.1, 0, 0.1), 'cm')
# 			)
	
# 	}

# 	map_grid <- plot_grid(plotlist = maps, nrow = 1, align = 'vh')

# 	ggsave(map_grid, filename = paste0(this_out_dir, '/Maps of Agreement - Examples.png'), width = 10.2, height = 7.6 / 3, dpi = 600, bg = 'white')

# say('#######################################################')
# say('### consensus in identifying climate change refugia ###')
# say('#######################################################')

# 	# We want to make 1) maps showing the degree to which rasters agree that a location current falls into the highest quartile of suitability and remains in the highest quartile (using present-day threshold) into the future.

# 	# world vector
# 	world <- rnaturalearth::ne_countries(scale = 'medium', returnclass = 'sv')
# 	focal <- vect(paste0(out_dir, '/common_study_region.gpkg'))

# 	# plot extent
# 	extent <- ext(focal)
# 	extent <- as.polygons(extent, crs = focal)
# 	extent <- buffer(extent, 20000)
# 	extent <- ext(extent)
# 	extent <- as.vector(extent)

# 	# predictions
# 	preds_xy <- load_predictions(period = 'all', scale = TRUE, subset_teams = FALSE)
# 	preds_present <- load_predictions(period = 'present', scale = TRUE, subset_teams = TRUE)
# 	preds_mid <- load_predictions(period = 'mid', scale = TRUE, subset_teams = TRUE)
# 	preds_late <- load_predictions(period = 'late', scale = TRUE, subset_teams = TRUE)

# 	# classify each raster's predictions into quartiles
# 	quartile_fx <- function(x, probs = c(0.25, 0.5, 0.75)) {
	
# 		out <- rep(NA_integer_, length(x))

# 		# is raster binary thresholded? if so, assign all 0s to quartile 1 and all 1s to quartile 4
# 		if (all(x %in% c(0, 1))) {
# 			out[x == 0] <- 1
# 			out[x == 1] <- 4
# 		} else {

# 			quants <- quantile(x, probs = probs)
# 			out[x <= quants[1]] <- 1
# 			out[x > quants[1] & x <= quants[2]] <- 2
# 			out[x > quants[2] & x <= quants[3]] <- 3
# 			out[x > quants[3]] <- 4

# 		}
# 		out
	
# 	}

# 	quarts_present <- apply(preds_present, 2, quartile_fx)
# 	quarts_present <- as.data.table(quarts_present)

# 		### identify which locations are in highest suitability quartile now and in future
# 		# team_present, team_mid, team_late		Column names of teams in present/mid/late (as they appear in predictions data.table)
# 		# col_names		Names to be given to columns that indicate if location is a refuge
# 		#
# 		# Returns the `refugia` data.table
# 		calc_refugia <- function(team_present, team_mid, team_future, col_names) {

# 			# quartile for sites in present
# 			present_quart <- quarts_present[[team_present]]

# 			# predictions in each time period
# 			present <- preds_present[[team_present]]
# 			mid <- preds_mid[[team_mid]]
# 			late <- preds_late[[team_late]]

# 			# identify sites in highest suitability quartile
# 			threshold <- min(present[present_quart == 4])
# 			highly_suitable_present <- present >= threshold
# 			highly_suitable_mid <- mid >= threshold
# 			highly_suitable_late <- late >= threshold

# 			# calculate agreement (refugia)
# 			refugia_mid <- highly_suitable_present & highly_suitable_mid
# 			refugia_late <- highly_suitable_present & highly_suitable_late

# 			# remember
# 			refugia[ , DUMMY1 := refugia_mid]
# 			refugia[ , DUMMY2 := refugia_late]
# 			names(refugia)[(ncol(refugia) - 1):ncol(refugia)] <- col_names

# 			refugia

# 		}

# 	# flag sites that remain in topmost quartile
# 	refugia <- preds_xy[ , c('longitude', 'latitude')]
# 	for (team in LETTERS[1:24]) {
	
# 		if (team %notin% c('M', 'J', 'K', 'N', 'T', 'V')) { # all teams that submitted just three rasters

# 			team_present <- paste0(team, '_present')
# 			team_mid <- paste0(team, '_mid')
# 			team_late <- paste0(team, '_late')
# 			col_names <- paste0(team, '_', c('mid', 'late'))

# 			refugia <- calc_refugia(team_present = team_present, team_mid = team_mid, team_future = team_future, col_names = col_names)

# 		} else if (team %in% c('J', 'K', 'T', 'V')) { # submitted one present-day rasters and multiple for each future

# 			n_vers <- if (team == 'J') { 4 } else if (team %in% c('K', 'T', 'V')) { 2 }

# 			for (ver in 1:n_vers) {

# 				team_present <- paste0(team, '_present')
# 				team_mid <- paste0(team, ver, '_mid')
# 				team_late <- paste0(team, ver, '_late')
# 				col_names <- paste0(team, '_', c('mid', 'late'), ver)

# 				refugia <- calc_refugia(team_present = team_present, team_mid = team_mid, team_future = team_future, col_names = col_names)

# 			}

# 		} else if (team == 'M') {
		
# 			for (ver in 1:2) { # submitted multiple present and future

# 				team_present <- paste0(team, ver, '_present')
# 				team_mid <- paste0(team, ver, '_mid')
# 				team_late <- paste0(team, ver, '_late')
# 				col_names <- paste0(team, ver, '_', c('mid', 'late'), ver)

# 				refugia <- calc_refugia(team_present = team_present, team_mid = team_mid, team_future = team_future, col_names = col_names)

# 			}
		
# 		} else if (team == 'N') {
	
# 			for (ver in 1:4) {
# 				for (sub in c('a', 'b')) {

# 					team_present <- paste0(team, ver, '_present')
# 					team_mid <- paste0(team, ver, sub, '_mid')
# 					team_late <- paste0(team, ver, sub, '_late')
# 					col_names <- paste0(team, ver, '_', c('mid', 'late'), ver, sub)

# 					refugia <- calc_refugia(team_present = team_present, team_mid = team_mid, team_future = team_future, col_names = col_names)

# 				}

# 			}
		
# 		} # team-specific processing

# 	} # next team

# 	### number of times a place was deemed a refuge in each time period
# 	for (fut in c('mid', 'late')) {
	
# 		cols <- grepl(names(refugia), pattern = paste0('_', fut))
# 		this_refugia <- refugia[ , ..cols]
# 		refugia[ , paste0('refugia_', fut) := rowSums(this_refugia)]
# 		refugia[ , paste0('refugia_prop_', fut) := rowSums(this_refugia) / ncol(this_refugia)]
	
# 	}

# 	# size of random points
# 	point_size <- 0.01

# 	# color to fill basemap
# 	world_fill <- 'gray80'

# 	refugia <- vect(refugia, geom = c('longitude', 'latitude'), crs = getCRS('WGS84'))

# 	### map of proportion of rasters designating a location a refuge
# 	################################################################

# 	maps <- list()
# 	y_max_hists <- -Inf
# 	for (period in c('mid', 'late')) {

# 		this_refugia <- refugia
# 		this_refugia[['y']] <- this_refugia[[paste0('refugia_prop_', period)]]

# 		letter <- if (period == 'mid') { 'b' } else { 'e' }
# 		title <- paste0(letter, ') Agreement in refugia')

# 		maps[[length(maps) + 1]] <- ggplot() +
# 			layer_spatial(world, fill = world_fill) +
# 			layer_spatial(this_refugia, aes(color = y), size = point_size) +
# 			scale_color_viridis_c(option = 'viridis', name = 'Proportion of teams predicting refuge', limits = c(0, 1)) +
# 			ggtitle(title, subtitle = get_nice_period(period)) +
# 			coord_sf(xlim = extent[1:2], ylim = extent[3:4], expand = FALSE) +
# 			theme(
# 				legend.position = 'bottom',
# 				legend.direction = 'horizontal',
# 				legend.title.position = 'bottom',
# 				legend.title = element_text(size = 20, hjust = 0.5),
# 				legend.text = element_text(size = 14),
# 				legend.key = element_blank(),
# 				legend.key.width = unit(2, 'cm'),
# 				legend.key.height = unit(0.4, 'cm'),
# 				legend.margin = margin(t = -5, r = 0, b = 0, l = 0),
# 				legend.box.margin = margin(t = -3, r = 0, b = 0, l = 0),
# 				axis.title = element_blank(),
# 				axis.text = element_blank(),
# 				axis.ticks = element_blank(),
# 				panel.background = element_blank(),
# 				panel.border = element_rect(color = 'black', fill = NA, linewidth = 0.4)
# 			)

# 	}

# 	### histogram of proportion of rasters designating a location a refuge
# 	######################################################################

# 	hists_agree <- list()
# 	for (period in c('mid', 'late') ) {

# 		this_refugia <- refugia
# 		this_refugia[['x']] <- this_refugia[[paste0('refugia_prop_', period)]]
# 		this_refugia <- as.data.table(this_refugia)

# 		letter <- if (period == 'mid') { 'c' } else { 'f' }
# 		title <- paste0(letter, ') Distribution of agreement')
		
# 		hists_agree[[length(hists_agree) + 1]] <- ggplot(this_refugia, aes(x = x, y = after_stat(count / sum(count)))) +
# 			geom_histogram(aes(fill = after_stat(x)), bins = 25, color = 'black') +
# 			scale_fill_viridis_c(option = 'viridis', name = NULL, limits = c(0, 1)) +
# 			xlab('Proportion of teams predicting refuge') +
# 			ylab('Proportion of sites') +
# 			ggtitle(title, subtitle = get_nice_period(period)) +
# 			theme_minimal() +
# 			theme(
# 				legend.position = 'none',
# 				axis.title = element_text(size = 20),
# 				axis.text = element_text(size = 18)
# 			)

# 		y_lims <- layer_scales(hists_agree[[length(hists_agree)]])$y$range$range
# 		y_max_hists <- max(y_max_hists, y_lims)

# 	} # next period

# 	### histogram of proportion of sites each team assesses as a refuge
# 	###################################################################

# 	hists_refugia <- list()
# 	x_max_hists <- y_max_hists <- -Inf
# 	for (period in c('mid', 'late') ) {

# 		this_refugia <- refugia
# 		this_refugia <- as.data.table(this_refugia)
# 		index <- grepl(names(this_refugia), pattern = paste0('_', period)) & names(this_refugia) %notin% c('refugia_mid', 'refugia_prop_mid', 'refugia_late', 'refugia_prop_late')
# 		this_refugia <- this_refugia[ , ..index]

# 		prop_refugia <- data.table(prop_refugia = colSums(this_refugia) / 100000)
# 		n <- nrow(prop_refugia)
# 		n_bins <- round(n / 2)

# 		letter <- if (period == 'mid') { 'a' } else { 'd' }
# 		title <- paste0(letter, ') Refuge extent across teams')
		
# 		# hists_refugia[[length(hists_refugia) + 1]] <- ggplot(prop_refugia, aes(x = prop_refugia, y = after_stat(count / sum(count)))) +
# 		hists_refugia[[length(hists_refugia) + 1]] <- ggplot(prop_refugia, aes(x = prop_refugia)) +
# 			geom_histogram(bins = n_bins, fill = 'steelblue', color = 'black') +
# 			scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
# 			xlab('Proportion of landscape\npredicted to be refuge') +
# 			ylab('Number of teams') +
# 			ggtitle(title, subtitle = get_nice_period(period)) +
# 			theme_minimal() +
# 			theme(
# 				legend.position = 'none',
# 				axis.title = element_text(size = 20),
# 				axis.text = element_text(size = 18)
# 			)

# 		y_lims <- layer_scales(hists_refugia[[length(hists_refugia)]])$y$range$range
# 		y_max_hists <- max(y_max_hists, y_lims)

# 		x_lims <- layer_scales(hists_refugia[[length(hists_refugia)]])$x$range$range
# 		x_max_hists <- max(x_max_hists, x_lims)

# 	} # next period

# 	xlim <- c(0, x_max_hists)
# 	ylim <- c(0, y_max_hists)
# 	for (i in 1:2) {
# 		hists_refugia[[i]] <- hists_refugia[[i]] + coord_cartesian(xlim = xlim, ylim = ylim)
# 	}

# 	plots <- c(hists_refugia[[1]], maps[[1]], hists_agree[[1]], hists_refugia[[2]], maps[[2]], hists_agree[[2]])

# 	for (i in seq_along(plots)) {
# 		plots[[i]] <- plots[[i]] +
# 			theme(
# 				plot.title = element_text(size = 24),
# 				plot.subtitle = element_text(size = 20),
# 				plot.margin = unit(c(0, 0.1, 1.2, 0.1), 'cm')

# 			)
# 	}

# 	grid <- plot_grid(plotlist = plots, ncol = 3, align = 'none')
# 	ggsave(grid, filename = paste0(this_out_dir, '/Climate Change Refugia.png'), width = 18, height = 12, dpi = 600, bg = 'white')


say('DONE!', level = 1, deco = '^')

