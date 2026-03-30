### SDM METHODOLOGICAL PLURALITY
### Anna Thonis, Nikki Calavari, Uzma Ashraf, Toni Lyn Morelli, and Adam B. Smith*
### * adam.smith@mobot.org | Missouri Botanical Garden | 2025-10
###
### Select plots for main text of publication.
###
### source('C:/Kaji/Research Group/ENMs - Plurality of Modeling Workflows (Anna Thonis)/enm_methods_plurality/11_select_plots_for_publication.r')
###
### CONTENTS
### setup ###
### expected null proportions for positive/negative agreement between rasters in spatial consensus analysis ###
### figure of consensus in assigning locations to quartiles of suitability for main text ###
### multi-panel figure of select associations between rasters in PC space and workflow decisions/attributes ###
###
###
#############
### setup ###
#############

	rm(list = ls())

	source('C:/Kaji/Research Group/ENMs - Plurality of Modeling Workflows (Anna Thonis)/enm_methods_plurality/00_shared_constants_and_functions.r')

# say('###############################################################################################################')
# say('### expected null proportions for positive/negative agreement between rasters in spatial consensus analysis ###')
# say('###############################################################################################################')

# 	p <- 0.25 # probability of a site being in a given quantile of suitability

# 	# Priona
# 	# present: 28
# 	# mid and late: 38

# 	# Zamia
# 	# present: 31
# 	# mid and late: 43

# 	# Priona, present-day
# 	for (n in c(28, 38, 31, 43)) {

# 		nulls <- dbinom(0:n, size = n, p = 1 - p)
# 		cum_sum <- cumsum(nulls)
# 		index <- which.max(cum_sum[cum_sum <= 0.05])
# 		neg_null_rate <- cum_sum[index]
# 		neg_null_n <- {0:n}[index]

# 		nulls <- dbinom(n:0, size = n, p = p)
# 		cum_sum <- cumsum(nulls)
# 		index <- which.max(cum_sum[cum_sum <= 0.05])
# 		pos_null_rate <- cum_sum[index]
# 		pos_null_n <- {n:0}[index]

# 		say('For n = ', n, ' with a positive agreement rate of ', p, ' we should expect:', level = 3)
# 		say(
# 			'The null negative agreement rate that is maximal but <= 0.05 is ',
# 			neg_null_rate,
# 			' which requires at least ',
# 			neg_null_n, ' rasters to agree the location is *not* in the given quantile.'
# 		)

# 		say(
# 			'The null positive agreement rate that is maximal but <= 0.05 is ',
# 			pos_null_rate,
# 			' which requires at least ',
# 			pos_null_n, ' rasters to agree the location *is* in the given quantile.'
# 		)

# 	}

# say('############################################################################################')
# say('### figure of consensus in assigning locations to quartiles of suitability for main text ###')
# say('############################################################################################')

# 	# For each species, We want to make 1) one row of present-day maps showing the number of rasters that agree randomly located sites shared across rasters are highly suitable, moderately suitable, moderately unsuitable, and poorly suitable. We will divide these classes by classifying each team's predictions into 4 quartiles, then counting, for each site, the number of rasters that agree on the quantile in which the site is ranked. 2) One row with histograms of the values in the maps from the row above.

# 	say('NB This chunk overrides the universal `focal_species` definition!!!', level = 2, deco = '!')

# 	### calculate significance of positive/negative agreement
# 	# For each row in a data table that has a row for each site and a column for each raster in which the quartile the prediction (row) has been classified into quartiles, calculate the binomial significance of positive and negative agreement.
# 	#
# 	# quarts		Data.table with columns like 'A_present', 'B_present', etc with values in {1:4}
# 	# preds			Data.table with columns like 'A_present', 'B_present', with predicted values (used to get correct dimensions)
# 	calc_sig_of_agree <- function(quarts, preds) {

# 		nr <- nrow(preds)
# 		N <- ncol(preds)
# 		for (quart in 1:4) {

# 			quarts[ , DUMMY_n_quarts := rowSums(.SD == quart), .SDcols = names(preds)]

# 			N <- ncol(preds)
# 			nr <- nrow(preds)
			
# 			p_pos_agree <- rep(NA_real_, nr)
# 			for (i in 1:nr) {
# 				p_pos_agree[i] <- sum(dbinom(quarts$DUMMY_n_quarts[i]:N, size = N, p = 0.25))
# 			}
			
# 			names(quarts)[ncol(quarts)] <- paste0('n_quart_', quart)
# 			quarts[ , paste0('p_pos_agree_', quart) := p_pos_agree]
# 			quarts[ , paste0('p_neg_agree_', quart) := 1 - p_pos_agree]

# 		}
# 		quarts
# 	}

# 	# world vector
# 	world <- rnaturalearth::ne_countries(scale = 'medium', returnclass = 'sv')
# 	for (species_focal in c('Priona', 'Zamia')) {

# 		say(species_focal)

# 		if (species_focal == 'Priona') {
# 			species_full <- 'Prionailurus bengalensis'
# 			k <- 1
# 		} else {
# 			species_full <- 'Zamia prasina'
# 			k <- 9
# 		}

# 		# plot extent
# 		focal <- vect(paste0('./Outputs ', species_full, '/common_study_region.gpkg'))
# 		extent <- ext(focal)
# 		extent <- as.polygons(extent, crs = focal)
# 		extent <- if (species_focal == 'Priona') { buffer(extent, 120000) } else { buffer(extent, 30000) }
# 		extent <- ext(extent)
# 		extent <- as.vector(extent)

# 		# predictions
# 		preds_xy <- load_predictions(species_focal = species_focal, period = 'all', scale = TRUE, subset_teams = FALSE)
# 		preds_present <- load_predictions(species_focal = species_focal, period = 'present', scale = TRUE, subset_teams = TRUE)
# 		preds_mid <- load_predictions(species_focal = species_focal, period = 'mid', scale = TRUE, subset_teams = TRUE)
# 		preds_late <- load_predictions(species_focal = species_focal, period = 'late', scale = TRUE, subset_teams = TRUE)

# 		# classify each raster's predictions into quartiles
# 		quartile_fx <- function(x, probs = c(0.25, 0.5, 0.75)) {
		
# 			out <- rep(NA_integer_, length(x))

# 			# is raster binary thresholded? if so, assign all 0s to quartile 1 and all 1s to quartile 4
# 			if (all(x %in% c(0, 1))) {
# 				out[x == 0] <- 1
# 				out[x == 1] <- 4
# 			} else {

# 				quants <- quantile(x, probs = probs)
# 				out[x <= quants[1]] <- 1
# 				out[x > quants[1] & x <= quants[2]] <- 2
# 				out[x > quants[2] & x <= quants[3]] <- 3
# 				out[x > quants[3]] <- 4

# 			}
# 			out
		
# 		}

# 		quarts_present <- apply(preds_present, 2, quartile_fx)
# 		quarts_mid <- apply(preds_mid, 2, quartile_fx)
# 		quarts_late <- apply(preds_late, 2, quartile_fx)

# 		quarts_present <- as.data.table(quarts_present)
# 		quarts_mid <- as.data.table(quarts_mid)
# 		quarts_late <- as.data.table(quarts_late)

# 		# proportion of rasters that have predictions falling into each quartile
# 		quarts_present[ , quart_1 := rowSums(.SD == 1) / ncol(.SD), .SDcols = names(preds_present)]
# 		quarts_present[ , quart_2 := rowSums(.SD == 2) / ncol(.SD), .SDcols = names(preds_present)]
# 		quarts_present[ , quart_3 := rowSums(.SD == 3) / ncol(.SD), .SDcols = names(preds_present)]
# 		quarts_present[ , quart_4 := rowSums(.SD == 4) / ncol(.SD), .SDcols = names(preds_present)]

# 		quarts_mid[ , quart_1 := rowSums(.SD == 1) / ncol(.SD), .SDcols = names(preds_mid)]
# 		quarts_mid[ , quart_2 := rowSums(.SD == 2) / ncol(.SD), .SDcols = names(preds_mid)]
# 		quarts_mid[ , quart_3 := rowSums(.SD == 3) / ncol(.SD), .SDcols = names(preds_mid)]
# 		quarts_mid[ , quart_4 := rowSums(.SD == 4) / ncol(.SD), .SDcols = names(preds_mid)]

# 		quarts_late[ , quart_1 := rowSums(.SD == 1) / ncol(.SD), .SDcols = names(preds_late)]
# 		quarts_late[ , quart_2 := rowSums(.SD == 2) / ncol(.SD), .SDcols = names(preds_late)]
# 		quarts_late[ , quart_3 := rowSums(.SD == 3) / ncol(.SD), .SDcols = names(preds_late)]
# 		quarts_late[ , quart_4 := rowSums(.SD == 4) / ncol(.SD), .SDcols = names(preds_late)]

# 		### calculate significance of positive/negative agreement
# 		quarts_present <- calc_sig_of_agree(quarts_present, preds_present)
# 		quarts_mid <- calc_sig_of_agree(quarts_mid, preds_mid)
# 		quarts_late <- calc_sig_of_agree(quarts_late, preds_late)

# 		# assign quartile proportions to same spatial data.table
# 		# calculate maximum proportion so we can cap the display of colors to a common value
# 		pred_max <- agree_max <- -Inf
# 		pred_min <- agree_min <- Inf
# 		xy <- preds_xy[ , c('longitude', 'latitude')]
# 		for (period in c('present', 'mid', 'late')) {
# 			for (quart in 1:4) {

# 				# min/max for this set of agreement scores	
# 				yy <- get(paste0('quarts_', period))
# 				y <- yy[[paste0('quart_', quart)]]
# 				agree_min <- min(agree_min, min(y))
# 				agree_max <- max(agree_max, max(y))

# 				# remember agreement rate into xy
# 				xy[[paste0('quart_', quart, '_', period)]] <- y
			
# 				# remember significance
# 				xy[ , paste0('sig_pos_neg_agree_', quart, '_', period) := 'Random']
# 				index <- yy[[paste0('p_pos_agree_', quart)]] <= 0.05
# 				xy[index, paste0('sig_pos_neg_agree_', quart, '_', period) := 'Positive']
# 				index <- yy[[paste0('p_neg_agree_', quart)]] <= 0.05
# 				xy[index, paste0('sig_pos_neg_agree_', quart, '_', period) := 'Negative']
			
# 				# min/max for this set of predictions
# 				y <- get(paste0('preds_', period))
# 				y <- rowMeans(y)
# 				pred_min <- min(y, min(y))
# 				pred_max <- max(y, max(y))

# 			}
# 		}

# 		# limits for agreement
# 		agree_lims <- c(agree_min, agree_max)
# 		pred_lims <- c(pred_min, pred_max)

# 		xy[ , pred_mean_present := rowMeans(preds_present)]
# 		xy[ , pred_mean_mid := rowMeans(preds_mid)]
# 		xy[ , pred_mean_late := rowMeans(preds_late)]

# 		pred_lims <- range(c(xy$pred_mean_present, xy$pred_mean_mid, xy$pred_mean_late))

# 		xy <- vect(xy, geom = c('longitude', 'latitude'), crs = getCRS('WGS84'))

# 		# size of random points
# 		point_size <- 0.01

# 		# color to fill basemap
# 		world_fill <- 'gray80'

# 		hist_preds_y_max <- hist_counts_agree_y_max <- -Inf
# 		hists_count_num_sigs <- maps <- list()

# 		period <- 'present'

# 			### binomial distribution lower 5th and upper 95th significance limits
# 			# used to color histogram based on positive/negative/random agreement
# 			x <- get(paste0('preds_', period))
# 			N <- ncol(x)
# 			n <- 0:N
# 			sigs <- dbinom(n, size = N, prob = 0.25)
			
# 			# map_cols <- rep('#d95f02', N)
# 			map_cols <- rep('yellow', N)

# 			# lower limit
# 			cumsums <- cumsum(sigs)
# 			cumsums_lte_05 <- cumsums
# 			cumsums_lte_05[cumsums_lte_05 > 0.05] <- -Inf
# 			map_cols[!is.infinite(cumsums_lte_05)] <- '#7b3294'
# 			agree_sig_lim_05 <- (which.max(cumsums_lte_05) - 1) / N

# 			# upper limit
# 			cumsums <- cumsum(sigs)
# 			cumsums_gte_95 <- cumsums
# 			cumsums_gte_95[cumsums_gte_95 < 0.95] <- Inf
# 			map_cols[!is.infinite(cumsums_gte_95)] <- '#008837'
# 			agree_sig_lim_95 <- (which.min(cumsums_gte_95) - 1) / N

# 			for (quart in 4:1) {

# 				# ### map of mean prediction
# 				# if (quart == 4) {

# 				# 	k <- k + 1
# 				# 	letter <- letters[k]
# 				# 	title <- paste0(letter, ') Mean prediction')

# 				# 	this_xy <- xy
# 				# 	this_xy[['y']] <- this_xy[[paste0('pred_mean_', period)]]

# 				# 	# map of mean predicted values
# 				# 	maps[[length(maps) + 1]] <- ggplot() +
# 				# 		layer_spatial(world, fill = world_fill) +
# 				# 		layer_spatial(this_xy, aes(color = y), size = point_size) +
# 				# 		scale_color_viridis_c(option = 'magma', name = 'Prediction', limits = pred_lims) +
# 				# 		ggtitle(title)

# 				# 	names(maps)[length(maps)] <- paste0('mean_', period)
				
# 				# 	### histogram of mean predicted values
# 				# 	kk <- k + 5
# 				# 	letter <- letters[kk]
# 				# 	title <- paste0(letter, ') Mean prediction')
# 				# 	this_xy <- as.data.table(this_xy)

# 				# 	hists_count_num_sigs[[length(hists_count_num_sigs) + 1]] <-
# 				# 		ggplot(this_xy, aes(x = y, y = after_stat(count / sum(count)))) +
# 				# 		geom_histogram(aes(fill = after_stat(x)), bins = 100) +
# 				# 		scale_fill_viridis_c(option = 'magma', name = NULL, limits = pred_lims, alpha = 0.8) +
# 				# 		xlab('Mean predicted value') + ylab('Proportion of sites') +
# 				# 		ggtitle(title)

# 				# 	# get y-axis limit
# 				# 	y_lims <- layer_scales(hists_count_num_sigs[[length(hists_count_num_sigs)]])$y$range$range
# 				# 	hist_preds_y_max <- max(hist_preds_y_max, y_lims[2])

# 				# 	names(hists_count_num_sigs)[length(hists_count_num_sigs)] <- paste0('mean_', period)

# 				# }

# 				### map of proportion agreement
# 				##############################
# 				suit_nice <- if (quart == 1) {
# 					'Highly unsuitable'
# 				} else if (quart == 2) {
# 					'Moderately unsuitable'
# 				} else if (quart == 3) {
# 					'Moderately suitable'
# 				} else {
# 					'Highly suitable'
# 				}

# 				this_xy <- xy
# 				x <- this_xy[[paste0('quart_', quart, '_', period)]]
# 				this_xy[['agree']] <- x

# 				k <- k + 1
# 				letter <- letters[k]
# 				title <- paste0(letter, ') ', suit_nice)
# 				maps[[length(maps) + 1]] <- ggplot() +
# 					layer_spatial(world, fill = world_fill) +
# 					layer_spatial(this_xy, aes(color = agree), size = point_size) +
# 					scale_color_gradientn(colors = map_cols, name = 'Neg  Random               Pos  ', limits = agree_lims) +
# 					ggtitle(title)

# 				names(maps)[length(maps)] <- paste0(period, '_quart_', quart)

# 				### histogram of sites in each agreement class (positive/random/negative)
# 				#########################################################################

# 				suit_nice <- if (quart == 1) {
# 					'Highly unsuitable'
# 				} else if (quart == 2) {
# 					'Moderate. unsuit.'
# 				} else if (quart == 3) {
# 					'Moderately suit.'
# 				} else {
# 					'Highly suitable'
# 				}

# 				this_xy <- xy
# 				this_xy[['agree']] <- this_xy[[paste0('quart_', quart, '_', period)]]
# 				this_xy <- as.data.table(this_xy)

# 				kk <- k + 4
# 				letter <- letters[kk]
# 				title <- paste0(letter, ') ', suit_nice)

# 				n_rasters <- if (period == 'present') {
# 					ncol(preds_present)
# 				} else if (period == 'mid') {
# 					ncol(preds_mid)
# 				} else if (period == 'late') {
# 					ncol(preds_late)
# 				}

# 				# count occurrences of each agreement value
# 				agree_counts <- this_xy[, .N, by = agree]
# 				agree_counts[ , proportion := N / sum(agree_counts$N)]
# 				agree_counts <- agree_counts[order(agree)]
				
# 				neg <- sum(agree_counts$proportion[agree_counts$agree <= agree_sig_lim_05 + eps()])
# 				pos <- sum(agree_counts$proportion[agree_counts$agree >= agree_sig_lim_95 + eps()])
# 				rand <- sum(agree_counts$proportion[agree_counts$agree > agree_sig_lim_05 + eps() & agree_counts$agree < agree_sig_lim_95 + eps()])
# 				neg <- sprintf('%.2f', round(neg, 2))
# 				pos <- sprintf('%.2f', round(pos, 2))
# 				rand <- sprintf('%.2f', round(rand, 2))
# 				neg <- paste0('Neg\n(', neg, ')')
# 				pos <- paste0('Pos\n(', pos, ')')
# 				rand <- paste0('Rand\n(', rand, ')')
				
# 				agree_counts[ , agreement := rand]
# 				agree_counts$agreement[agree_counts$agree <= agree_sig_lim_05 + eps()] <- neg
# 				agree_counts$agreement[agree_counts$agree >= agree_sig_lim_95 - eps()] <- pos
# 				agree_counts[ , agreement := factor(agreement, levels = c(neg, rand, pos))]
				
# 				fills <- c('#7b3294', 'yellow', '#008837')
# 				# fills <- c('#7570b3', '#d95f02', '#1b9e77')
# 				# fills <- c('#7570b3', '#fc8d62', '#1b9e77')
# 				names(fills) <- c(neg, rand, pos)

# 				hists_count_num_sigs[[length(hists_count_num_sigs) + 1]] <- ggplot(agree_counts, aes(x = agree, y = proportion)) +
# 					geom_col(aes(fill = agreement)) +
# 					scale_fill_manual(
# 						name = NULL,
# 						values = fills
# 					) +
# 					xlab('Proportion in agreement') + ylab('Proportion of sites') +
# 					ggtitle(title)

# 				# get y-axis limit
# 				y_lims <- layer_scales(hists_count_num_sigs[[length(hists_count_num_sigs)]])$y$range$range
# 				hist_counts_agree_y_max <- max(hist_counts_agree_y_max, y_lims[2])

# 				names(hists_count_num_sigs)[length(hists_count_num_sigs)] <- paste0(period, '_quart_', quart)

# 		} # next quartile

# 		for (i in seq_along(maps)) {
		
# 			# map of proportion of rasters agreeing
# 			maps[[i]] <- maps[[i]] +
# 				coord_sf(xlim = extent[1:2], ylim = extent[3:4], expand = FALSE) +
# 				theme(
# 					legend.position = 'bottom',
# 					legend.direction = 'horizontal',
# 					legend.title.position = 'bottom',
# 					legend.title = element_text(size = 10),
# 					legend.text = element_text(size = 9),
# 					legend.key.width = unit(0.7, 'cm'),
# 					legend.key.height = unit(0.2, 'cm'),
# 					legend.margin = margin(t = -5, r = 0, b = 1, l = 0),
# 					legend.box.margin = margin(t = -3, r = 0, b = 0, l = 0),
# 					plot.title = element_text(size = 12),
# 					axis.title = element_blank(),
# 					axis.text = element_blank(),
# 					axis.ticks = element_blank(),
# 					panel.background = element_blank(),
# 					panel.border = element_rect(color = 'black', fill = NA, linewidth = 0.4),
# 					plot.margin = unit(c(0, 0.1, 0.2, 0.1), 'cm')
# 				)
		
# 			# histogram of number of sites in each significance class
# 			hists_count_num_sigs[[i]] <- hists_count_num_sigs[[i]] +
# 				theme(
# 					legend.position = 'bottom',
# 					legend.direction = 'horizontal',
# 					legend.title.position = 'bottom',
# 					legend.title = element_text(size = 9),
# 					legend.text = element_text(size = 8, margin = margin(l = 1, unit = 'pt')),
# 					legend.key.width = unit(0.6, 'cm'),
# 					legend.margin = margin(t = -5, r = 0, b = 0, l = 0),
# 					legend.box.margin = margin(t = -3, r = 0, b = 0, l = 0),
# 					plot.title = element_text(size = 11),
# 					axis.title = element_text(size = 9),
# 					axis.text = element_text(size = 8),
# 					# panel.background = element_blank(),
# 					plot.margin = unit(c(0.2, 0.5, 0.8, 0.1), 'cm')
# 				)

# 			if (grepl(names(hists_count_num_sigs)[i], pattern = 'mean_')) {

# 				hists_count_num_sigs[[i]] <- hists_count_num_sigs[[i]] +
# 					theme(
# 						# legend.key.width = unit(0.3, 'cm'),
# 						legend.key.height = unit(0.2, 'cm')
# 					)

# 			} else {

# 				hists_count_num_sigs[[i]] <- hists_count_num_sigs[[i]] +
# 					theme(
# 						legend.key.width = unit(0.3, 'cm'),
# 						legend.key.height = unit(0.3, 'cm')
# 					)

# 			}

# 			if (grepl(names(hists_count_num_sigs)[i], pattern = 'mean_')) {
# 				hists_count_num_sigs[[i]] <- hists_count_num_sigs[[i]] + ylim(0, hist_preds_y_max) + coord_cartesian(xlim = pred_lims)
# 			}
				
# 			if (grepl(names(hists_count_num_sigs)[i], pattern = '_quart_')) {
# 				hists_count_num_sigs[[i]] <- hists_count_num_sigs[[i]] + ylim(0, hist_counts_agree_y_max) + coord_cartesian(xlim = c(0, 1))
# 			}
		
# 		}

# 		if (species_focal == 'Priona') {
# 			priona_maps <- plot_grid(plotlist = maps, ncol = 4, align = 'vh')
# 			priona_hists <- plot_grid(plotlist = hists_count_num_sigs, ncol = 4, align = 'vh')
# 		} else if (species_focal == 'Zamia') {
# 			zamia_maps <- plot_grid(plotlist = maps, ncol = 4, align = 'vh')
# 			zamia_hists <- plot_grid(plotlist = hists_count_num_sigs, ncol = 4, align = 'vh')		
# 		}

# 	} # next species

# 	plot_list <- list(priona_maps, priona_hists, zamia_maps, zamia_hists)
# 	map_grid <- plot_grid(plotlist = plot_list, ncol = 1, align = 'vh')
# 	ggsave(map_grid, filename = paste0('./Outputs Shared Anonymized/Maps of Agreement for Main Text Sans Means.png'), width = 8, height = 10, dpi = 600, bg = 'white')

say('###############################################################################################################')
say('### multi-panel figure of select associations between rasters in PC space and workflow decisions/attributes ###')
say('###############################################################################################################')

	# We want to make a multi-panel figure showing associations between rasters in PC space and select workflow attributes. The plot will have 4 x 2 panels, and we will make two such plots, one for Priona and one for Zamia. Each panel will show the outcome of a Mantel or PERMANOVA test, depending on the type of attribute and whether the dispersion test (PERMANOVA) was significant or not.

	say('This chunk overrides the universal definition of `species_focal`', level = 2, deco = '!')

		# replace NAs in response vector
		replace_y_NAs <- function(y, field_names) {
		
			for (i in 2:length(field_names)) {
			
				nas <- paste(rep('NA', i), collapse = ', ')
				these <- which(y == nas)
				if (length(these) > 0) y[these] <- NA
			
			}
			y
		
		}

		# Match workflow attribute scores to each raster, calculate Gower distances, and execute Mantel test
		#
		# y				Values to match to rasters
		# match_on		"team" or "raster"
		# rast_dists 	Distances between rasters in PCA space
		# nperm			Number of Mantel permutations
		# results		Data table with results
		# step			Description of attribute
		# nice			Nice description of attribute, must match values in the results table from the Mantel and PERMANOVA tests
		# title			Nice description of attribute
		# trans			If NA, do not transform values. Otherwise, transformation function like log or log10 (the actual function, not a character naming it)
		# plot_type		'categorical' or 'numeric'
		# display_legend TRUE/FALSE
		# legend_title  Title for legend
		# results		List with biplots and data.table.
		# letter		Letter to attach to front of title
		#
		# Returns results data.table with results for this attribute added
		do_analysis <- function(y, match_on, rast_dists, results, step, nice, title, trans, plot_type, display_legend, legend_title, letter) {
		
			title <- paste0(letter, ') ', title)
			say(title)
			genus <- if (species_focal == 'Priona') {
				if (nchar(title) > 23) { 'Priona.' } else { 'Prionailurus' }
			} else {
				'Zamia'
			}
			title <- bquote(.(title) * ': ' * italic(.(genus)))

			if (match_on == 'team') {
				names(y) <- team_fields$team_code
				scores_teams <- rownames(scores)
				scores_teams <- substr(scores_teams, 1, 1)
				index <- match(scores_teams, team_fields$team_code)
			} else if (match_on == 'raster') {
				names(y) <- team_fields$raster_name
				scores_rasters <- rownames(scores)
				raster_name_time_period <- paste0(rast_fields$raster_name, '_', rast_fields$time_period)
				index <- match(scores_rasters, raster_name_time_period)
			}
			y_match <- y[index]
			y_match <- data.table(y_match = y_match)
			if (is.character(y_match[[1]])) y_match[[1]] <- factor(y_match[[1]])

			if (is.function(trans)) y_match <- trans(y_match)
			
			this_pcs <- copy(pcs)
			this_rast_dists <- rast_dists
			if (anyNA(y_match)) {
				bads <- which(is.na(y_match))
				y_match <- y_match[-bads]
				this_rast_dists <- this_rast_dists[-bads, -bads]
				this_rast_dists <- as.dist(this_rast_dists)
				this_pcs <- this_pcs[-bads]
				n_removed <- length(bads)
			} else {
				n_removed <- 0
			}

				perm_p <- mantel_perm$permanova_p[mantel_perm$nice == nice]
				perm_r2 <- mantel_perm$permanova_r2[mantel_perm$nice == nice]
				disp_p <- mantel_perm$permanova_dispersion_p[mantel_perm$nice == nice]
				mantel_p <- mantel_perm$mantel_p[mantel_perm$nice == nice]
				mantel_r <- mantel_perm$mantel_stat[mantel_perm$nice == nice]

			# statistics
			# if (disp_p > 0.05) { # PERMANOVA valid
				r_r2 <- sprintf('%.2f', round(perm_r2, 2))
				p <- sprintf('%.2f', round(perm_p, 2))
				r_r2 <- bquote(italic('r')^2 * ' = ' * .(r_r2))
				p <- bquote(italic('P')[PERM] * ' = ' * .(p))
				disp_p <- sprintf('%.2f', round(disp_p, 2))
				dispersion_p <- bquote(italic('P')[disp] * ' = ' * .(disp_p))
				# stats_color <- if (perm_p <= 0.05 & disp_p > 0.05) { 'red' } else { 'black' }
				stats_color <- if (perm_p <= 0.05 & disp_p > 0.05) { 'black' } else { 'black' }
			# } else { # PERMANOVA invalid: use Mantel
				# r_r2 <- sprintf('%.2f', round(mantel_r, 2))
				# p <- sprintf('%.2f', round(mantel_p, 2))
				# r_r2 <- bquote(italic('r') * ' = ' * .(r_r2))
				# p <- bquote(italic('P')[Mant] * ' = ' * .(p))
				# stats_color <- if (mantel_p <= 0.05) { 'red' } else { 'black' }
			# }
			
			stats_df <- data.frame(x = -Inf, y = -Inf, label = as.character(as.expression(c(r_r2, p, dispersion_p))))

			# plot of PC space with rasters, divided into groups by the workflow attribute
			if (plot_type == 'categorical') {

				if (all(unique(y_match) %in% c(0, 1))) {
					y_match[y_match == 1] <- 'Yes'
					y_match[y_match == '0'] <- 'No'
				}

				this_pcs[ , aspect := y_match]

				# one polygon for each group
				polys <- list()
				unique_clusters <- sort(unique(y_match[[1]]))
				for (i in seq_along(unique_clusters)) {

					cluster <- unique_clusters[i]
				
					pts <- this_pcs[aspect == cluster, c('PC1', 'PC2')]
					if (nrow(pts) >= 3) {
						hull_indices <- chull(pts)
						hull_coords <- pts[hull_indices, ]
						hull_coords[ , aspect := cluster]
						polys[[i]] <- hull_coords
					} else {
						pts[ , aspect := cluster]
						polys[[i]] <- pts
					}

					polys[[i]] <- rbind(polys[[i]], polys[[i]][1, ])
				
				}
				polys <- rbindlist(polys)

				biplot <- base_biplot +
					# geom_shadowtext(
					# 	data = this_pcs, aes(x = PC1, y = PC2, label = raster, color = aspect), 
					# 	size = 2.4,
					# 	bg.colour = alpha('black', 0.2), bg.r = 0.05,
					# 	fontface = 'bold'
					# ) +
					# guides(color = guide_legend(override.aes = list(label = 'ABC', size = 5))) +
					geom_point(
						data = this_pcs, aes(x = PC1, y = PC2, fill = aspect), 
						size = 2,
						pch = 21, color = 'black', alpha = 0.2
					) +
					geom_polygon(data = polys, aes(x = PC1, y = PC2, fill = aspect, color = aspect), alpha = 0.5) +
					geom_polygon(data = polys, aes(x = PC1, y = PC2, color = aspect), fill = NA) +
					xlab(x_lab) + ylab(y_lab) +
					ggtitle(title) +
					guides(
						fill = guide_legend(title = legend_title),
						color = guide_legend(title = legend_title, override.aes = list(label = 'AB', size = 5))
					)
			} else if (plot_type == 'numeric') {

				y_match <- as.numeric(y_match[[1]])
				if (is.function(trans)) {
					this_pcs[ , aspect := 10^y_match]
				} else {
					this_pcs[ , aspect := y_match]
				}

				biplot <- base_biplot +
					geom_point(this_pcs, mapping = aes(x = PC1, y = PC2, fill = aspect), pch = 21, alpha = 0.7, size = 2) +
					xlab(x_lab) + ylab(y_lab) +
					ggtitle(title) +
					guides(fill = guide_colorbar(title = legend_title, barheight = unit(0.08, 'npc')))

				if (is.function(trans)) {

					biplot <- biplot + scale_fill_viridis_c(
						option = 'magma',
						trans = 'log10'
					)

				} else {

					biplot <- biplot + scale_fill_viridis_c(
						option = 'magma'
					)
				
				}
								
			}

			if (!display_legend) {
				biplot <- biplot + theme(legend.position = 'none') 
			} else {
					
				biplot <- biplot +
					theme(
						legend.position = c(1.03, -0.03),
						legend.justification = c('right', 'bottom'),
						# legend.background = element_rect(fill = alpha('white', 0.5), color = alpha('black', 0.3)),
						legend.background = element_blank(),
						legend.title = element_text(size = 7, angle = 90),
						legend.title.position = 'left',
						legend.spacing.x = unit(0.05, 'cm'),
						legend.spacing.y = unit(0.05, 'cm'),
						legend.text = element_text(size = 6),
						legend.key.height = unit(0.0013, 'npc'),
						legend.key.width = unit(0.2, 'cm')
					)

				
			}

			if (species_focal == 'Priona') {
				y1 <- -40
				y2 <- -60
				y3 <- -80
			} else if (species_focal == 'Zamia') {
				y1 <- -90
				y2 <- -120
				y3 <- -150
			}
			biplot <- biplot +
				geom_text(
					data = stats_df[2, ],
					aes(x = x, y = y1, label = label),
					hjust = 0,
					vjust = 0,
					parse = TRUE,
					size = 3,
					color = stats_color,
					inherit.aes = FALSE
				) +
				geom_text(
					data = stats_df[1, ],
					aes(x = x, y = y2, label = label),
					hjust = 0,
					vjust = 0,
					parse = TRUE,
					size = 3,
					color = stats_color,
					inherit.aes = FALSE
				) +
				geom_text(
					data = stats_df[3, ],
					aes(x = x, y = y3, label = label),
					hjust = 0,
					vjust = 0,
					parse = TRUE,
					size = 3,
					color = stats_color,
					inherit.aes = FALSE
				) +
				theme(
					plot.title = element_text(size = 9),
					axis.title = element_text(size = 6),
					axis.text = element_text(size = 5)
				)

			results <- c(results, biplot)

			names(results)[length(results)] <- paste(species_focal, nice)
			results

		}
		
	results <- list()
	letter_n <- 0

	for (species_focal in c('Priona', 'Zamia')) {

		species_full <- if (species_focal == 'Priona') {
			'Prionailurus bengalensis'
		} else {
			'Zamia prasina'
		}

		mantel_perm <- fread(paste0('./Outputs ', species_full, '/Raster Distances ~ Decision Distances/Univariate Mantel and PERMANOVA on Distance between Rasters and Workflows.csv'))

		rast_fields <- load_rast_fields(species_focal = species_focal)
		team_fields <- load_team_fields(species_focal = species_focal)

		### cluster
		preds <- load_predictions(species_focal = species_focal, period = 'all', scale = TRUE, subset_teams = TRUE)
		preds_trans <- t(preds)

		pca <- prcomp(preds_trans)
		scores <- pca$x[ , 1:2]
		scores <- as.data.frame(scores)

		rast_dists <- dist(scores)

		logp10 <- function(x) log10(x + 1)

		### base plot of rasters
		var1 <- sprintf('%.1f', 100 * pca$sdev[1]^2 / sum(pca$sdev^2))
		var2 <- sprintf('%.1f', 100 * pca$sdev[2]^2 / sum(pca$sdev^2))

		x_lab <- paste0('PC 1 (', var1, '%)')
		y_lab <- paste0('PC 2 (', var2, '%)')

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

		pcs <- as.data.table(pcs)

		base_biplot <- ggplot() +
			coord_cartesian(clip = 'off') +
			coord_fixed() +
			theme_minimal() +
			theme(
				plot.title = element_text(size = 18),
				axis.title = element_text(size = 13),
				axis.text = element_text(size = 10),
				legend.title = element_text(size = 12),
				legend.text = element_text(size = 10)
			)

		### add MEAN ODMAP score and MINIMUM SCORE ACROSS CATEGORIES to fields
		######################################################################

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

			team_fields$odmap_mean <- odmap_means[match(team_fields$team_code, team_codes)]
			team_fields$odmap_min <- odmap_mins[match(team_fields$team_code, team_codes)]

		### analyze associations between workflow attributes and clusters created by predictions
		########################################################################################

			## evaluate individual workflow attributes
			##########################################

			### ODMAP *mean* score

				nice <- 'SDM Standards: Mean rank'
				title <- 'SDM standards rank'
				letter_n <- letter_n + 1
				letter <- letters[letter_n]
				match_on <- 'team'
				trans <- NA
				plot_type <- 'numeric'
				display_legend <- TRUE
				legend_title <- 'Rank'

				y <- as.numeric(team_fields$odmap_mean)
				
				results <- do_analysis(y = y, match_on = match_on, rast_dists = rast_dists, results = results, step = step, nice = nice, title = title, trans = trans, plot_type = plot_type, display_legend = display_legend, legend_title = legend_title, letter = letter)

			### "team x thresholding"

				nice <- 'Team × Continuous/Thresholded'
				title <- 'Team × contin./threshold'
				letter_n <- letter_n + 1
				letter <- letters[letter_n]
				match_on <- 'raster'
				trans <- NA
				plot_type <- 'categorical'
				display_legend <- FALSE
				legend_title <- NULL

				y <- rast_fields$team_code
				y[rast_fields$raster_name == 'M1'] <- 'Mc'
				y[rast_fields$raster_name == 'M2'] <- 'Mt'
				if (species_focal == 'Priona') {
					y[rast_fields$raster_name %in% c('N1', 'N2')] <- 'Nc'
					y[rast_fields$raster_name %in% c('N3', 'N4', 'N3a', 'N4a', 'N3b', 'N4b')] <- 'Nt'
				} else if (species_focal == 'Zamia') {
					y[rast_fields$raster_name %in% c('N1', 'N2', 'N3', 'N1a', 'N2a', 'N3a', 'N1b', 'N2b', 'N3b')] <- 'Nc'
					y[rast_fields$raster_name %in% c('N4', 'N5', 'N6', 'N4a', 'N5a', 'N6a', 'N4b', 'N5b', 'N6b')] <- 'Nt'
				}

				results <- do_analysis(y = y, match_on = match_on, rast_dists = rast_dists, results = results, step = step, nice = nice, title = title, trans = trans, plot_type = plot_type, display_legend = display_legend, legend_title = legend_title, letter = letter)

			# ### source of climate predictors

				# nice <- 'Predictors: Climate data source'
				# title <- 'Climate data source'
				# letter_n <- letter_n + 1
				# letter <- letters[letter_n]
				# match_on <- 'team'
				# trans <- NA
				# plot_type <- 'categorical'
				# display_legend <- FALSE
				# legend_title <- 'Source'

				# y <- team_fields$predictors_climate_source

				# results <- do_analysis(y = y, match_on = match_on, rast_dists = rast_dists, results = results, step = step, nice = nice, title = title, trans = trans, plot_type = plot_type, display_legend = display_legend, legend_title = legend_title, letter = letter)

			### SDM algorithm

				nice <- 'Algorithm'
				title <- 'SDM algorithm'
				letter_n <- letter_n + 1
				letter <- letters[letter_n]
				match_on <- 'raster'
				trans <- NA
				plot_type <- 'categorical'
				display_legend <- FALSE
				legend_title <- 'Algorithm'

				field_names <- c('algo_ensemble', 'algo_maxent', 'algo_maxnet', 'algo_glm', 'algo_gam', 'algo_rf', 'algo_sre')

				y <- rast_fields[ , ..field_names]
				y <- y[ , lapply(.SD, as.numeric)]
				y <- apply(y, 1, function(row) {
					cols_with_1 <- field_names[row == 1]
					if (length(cols_with_1) == 0) return(NA)
					paste(cols_with_1, collapse = ", ")
				})
				y <- replace_y_NAs(y = y, field_names = field_names)

				y[y == 'algo_ensemble'] <- 'Ensemble'
				y[y == 'algo_maxent'] <- 'MaxEnt'
				y[y == 'algo_maxnet'] <- 'MaxNet'
				y[y == 'algo_glm'] <- 'GLM'
				y[y == 'algo_gam'] <- 'GAM'
				y[y == 'algo_rf'] <- 'RF'
				y[y == 'algo_sre'] <- 'SRE'

				results <- do_analysis(y = y, match_on = match_on, rast_dists = rast_dists, results = results, step = step, nice = nice, title = title, trans = trans, plot_type = plot_type, display_legend = display_legend, legend_title = legend_title, letter = letter)

			### number of occurrences

				nice <- 'Occurrences: Number of occurrences'
				title <- 'Number of occurrences'
				letter_n <- letter_n + 1
				letter <- letters[letter_n]
				match_on <- 'team'
				trans <- log10
				plot_type <- 'numeric'
				display_legend <- TRUE
				legend_title <- 'Occurrences'

				y <- as.numeric(team_fields$num_occurrences_minimum)
				results <- do_analysis(y = y, match_on = match_on, rast_dists = rast_dists, results = results, step = step, nice = nice, title = title, trans = trans, plot_type = plot_type, display_legend = display_legend, legend_title = legend_title, letter = letter)

			### spatial resolution: cell size *qualitative*
				
				nice <- 'Spatial resolution (arcmin)'
				title <- 'Spatial resolution'
				letter_n <- letter_n + 1
				letter <- letters[letter_n]
				match_on <- 'team'
				trans <- NA
				plot_type <- 'categorical'
				display_legend <- FALSE
				legend_title <- 'Resolution'

				y <- team_fields$res_arcmin

				results <- do_analysis(y = y, match_on = match_on, rast_dists = rast_dists, results = results, step = step, nice = nice, title = title, trans = trans, plot_type = plot_type, display_legend = display_legend, legend_title = legend_title, letter = letter)

			### modeling_software

				nice <- 'Software'
				title <- 'Software'
				letter_n <- letter_n + 1
				letter <- letters[letter_n]
				match_on <- 'team'
				trans <- NA
				plot_type <- 'categorical'
				display_legend <- FALSE
				legend_title <- 'Software'

				field_names <- c('modeling_software_enmeval', 'modeling_software_enmtools', 'modeling_software_wallace', 'modeling_software_biomod2', 'modeling_software_sabinansdm', 'modeling_software_sdm', 'modeling_software_miamaxent', 'modeling_software_enmsdmx', 'modeling_software_flexsdm', 'modeling_software_sdmtune', 'modeling_software_piecemeal', 'modeling_software_other')

				y <- team_fields[ , ..field_names]
				y <- y[ , lapply(.SD, as.numeric)]
				y <- apply(y, 1, function(row) {
					cols_with_1 <- field_names[row == 1]
					if (length(cols_with_1) == 0) return(NA)
					paste(cols_with_1, collapse = ", ")
				})
				y <- replace_y_NAs(y = y, field_names = field_names)

				y <- sub(y, pattern = 'modeling_software_enmeval', replacement = 'ENMeval')
				y <- sub(y, pattern = 'modeling_software_enmtools', replacement = 'ENMTools')
				y <- sub(y, pattern = 'modeling_software_wallace', replacement = 'Wallace')
				y <- sub(y, pattern = 'modeling_software_biomod2', replacement = 'BIOMOD2')
				y <- sub(y, pattern = 'modeling_software_sabinansdm', replacement = 'sabinaNSDM')
				y <- sub(y, pattern = 'modeling_software_sdm', replacement = 'sdm')
				y <- sub(y, pattern = 'modeling_software_miamaxent', replacement = 'MIAmaxent')
				y <- sub(y, pattern = 'modeling_software_enmsdmx', replacement = 'enmSdmX')
				y <- sub(y, pattern = 'modeling_software_flexsdm', replacement = 'flexsdm')
				y <- sub(y, pattern = 'modeling_software_sdmtune', replacement = 'SDMtune')
				y <- sub(y, pattern = 'modeling_software_other', replacement = 'other')
				y <- sub(y, pattern = 'modeling_software_piecemeal', replacement = 'piecemeal')

				results <- do_analysis(y = y, match_on = match_on, rast_dists = rast_dists, results = results, step = step, nice = nice, title = title, trans = trans, plot_type = plot_type, display_legend = display_legend, legend_title = legend_title, letter = letter)

			### number of predictors

				nice <- 'Predictors: Total number'
				title <- 'Number of predictors'
				letter_n <- letter_n + 1
				letter <- letters[letter_n]
				match_on <- 'team'
				trans <- log10
				plot_type <- 'numeric'
				display_legend <- TRUE
				legend_title <- 'Predictors'


				y <- team_fields$predictors_climate_nonclimate_num_total
				y <- as.numeric(y)

				results <- do_analysis(y = y, match_on = match_on, rast_dists = rast_dists, results = results, step = step, nice = nice, title = title, trans = trans, plot_type = plot_type, display_legend = display_legend, legend_title = legend_title, letter = letter)

			### AUC

				nice <- 'Evaluation: Value of AUC'
				title <- 'AUC'
				letter_n <- letter_n + 1
				letter <- letters[letter_n]
				match_on <- 'raster'
				plot_type <- 'numeric'
				display_legend <- TRUE
				legend_title <- 'AUC'

				y <- rast_fields$eval_metric_auc_roc_value
				y <- as.numeric(y)

				results <- do_analysis(y = y, match_on = match_on, rast_dists = rast_dists, results = results, step = step, nice = nice, title = title, trans = trans, plot_type = plot_type, display_legend = display_legend, legend_title = legend_title, letter = letter)

			### time period

				nice <- 'Projection: Time period'
				title <- 'Time period'
				letter_n <- letter_n + 1
				letter <- letters[letter_n]
				match_on <- 'raster'
				trans <- NA
				plot_type <- 'categorical'
				display_legend <- FALSE
				legend_title <- 'Period'

				y <- rast_fields$time_period

				results <- do_analysis(y = y, match_on = match_on, rast_dists = rast_dists, results = results, step = step, nice = nice, title = title, trans = trans, plot_type = plot_type, display_legend = display_legend, legend_title = legend_title, letter = letter)


	} # next species

	plots <- plot_grid(plotlist = results, ncol = 3, align = 'hv')
	
	ggsave(plots, filename = paste0('./Outputs Shared Anonymized/PCAs of Rasters with Mantel and PERMANOVA Tests.png'), dpi = 600, height = 10.5, width = 7.5, bg = 'white')

say('DONE!', level = 1, deco = '^')

