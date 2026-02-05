### SDM METHODOLOGICAL PLURALITY
### Anna Thonis, Nikki Calavari, Uzma Ashraf, Toni Lyn Morelli, and Adam B. Smith*
### * adam.smith@mobot.org | Missouri Botanical Garden | 2025-10
###
### This code applies and plots a regression tree of SDM predictions, one per species_focal, based on decisions each team made.
###
### source('C:/Kaji/Research Group/ENMs - Plurality of Modeling Workflows (Anna Thonis)/enm_methods_plurality/02_analysis_of_enm_methods_plurality.r')
###
### CONTENTS ###
### setup ###
### analysis-wide settings ###
### custom functions ###
###
### make maps of all rasters for inspection ###
### create random points from which to draw predictions across all team rasters ###
### extract values for predictions at same sites across all rasters ###
###
### PCA on teams: cluster analysis of teams by predictions ###
### cluster constancy ###
### heatmaps of correlations between predictions ###
### distributions of predictions by team ###
### test for associations between team clusters and workflow attributes ###
### illustrations of team-level clusters and workflow attributes ###
### CART analysis of team clusters ###

#############
### setup ###
#############

	rm(list = ls())

	library(cluster) # clustering
	library(cowplot) # combining ggplots
	library(data.table) # fast data frames
	library(distances) # fast distance calculation
	library(enmSdmX) # SDMing and GIS
	library(ggdendro) # plotting dendrograms
	library(ggplot2) # graphics
	library(ggspatial) # spatial graphics
	library(omnibus) # utilities
	library(readxl) # open Excel documents
	library(reshape2) # wide <--> long data frames
	library(rnaturalearth) # GIS data
	library(rnaturalearthdata) # GIS data
	library(rpart) # CART
	library(rpart.plot) # CART plotting
	library(terra) # GIS
	library(tree) # classification and regression trees
	library(viridis) # for magma palette
	
	# library(FactoMineR) # ggplot PCA
	library(factoextra) # ggplot PCA

	drive <- 'C:/Kaji/'

	setwd(paste0(drive, '/Research Group/ENMs - Plurality of Modeling Workflows (Anna Thonis)'))

##############################
### analysis-wide settings ###
##############################

	species_focal <- 'Priona'
	# species_focal <- 'Zamia'

	# spreadsheet that scores attributes for each workflow
	fields_file_name <- './Data/Model_choices_2025_11_06.xlsx'

	# number of random points to keep that have predictions across all teams' rasters
	n_rand_points_to_keep <- 100000

	say('Using workflow attributes file: ', fields_file_name, level = 1, deco = '!')

	min_variance <- 0.85 # for clustering by PC loadings, keep axes that explain at least this much variance
	max_height <- 0.5 # in dendrogram of teams, if groups of teams are more different than this, then define them as different clusters

	cluster_cols_present <- c('1' = '#f8766d', '2' = '#00b0f6', '3' = '#bcbd4a', '4' = '#e76bf3', '5' = '#FFD966')
	cluster_cols_mid <- c('1' = '#00b0f6', '2' = '#f8766d', '3' = '#e76bf3', '4' = '#00bf7d', '5' = '#FFD966')
	cluster_cols_late <- c('1' = '#00b0f6', '2' = '#f8766d', '3' = '#e76bf3', '4' = '#00bf7d', '5' = '#FFD966')

	species_full <- if (species_focal == 'Priona') {
		'Prionailurus bengalensis'
	} else {
		'Zamia prasina'
	}

	say('Analyzing ', species_full, level = 1, deco = '!')

	dirCreate(paste0('./Outputs ', species_full))

########################
### custom functions ###
########################

	# get_nice_period(): Convert period ('present', 'mid', and 'late') to nice version for folder names
	# load_team_codes(): loads table with team codes
	# load_rast_fields(): loads workbook with attributes of each team's rasters
	# load_team_fields(): loads workbook with attributes of each team
	# load_predictions(): loads SDM predictions
	# wide_to_long(): converts a wide data.table (from load_predictions()) and converts to long format
	# fill_terrestrial_NAs(): fills terrestrial NA cells in a projection raster with 0s based on appropriate land/sea mask and resolution

	### Convert period ('present', 'mid', and 'late') to nice version for folder names
	get_nice_period <- function(period) {
		if (period == 'present') {
			'Present'
		} else if (period == 'mid') {
			'Mid-20th Century'
		} else if (period == 'late') {
			'Late 20th Century'
		} else {
			stop('Bad period name.')
		}
	}

	### loads table with team codes
	load_team_codes <- function() fread('./Outputs Shared/team_info.csv')

	# function to load data on workflow choices
	# species_focal		Either 'Priona' or 'Zamia'
	#
	# returns data.table of modeling choices
	load_rast_fields <- function(species_focal) {

		if (species_focal == 'Priona') {
			fields <- read_xlsx(fields_file_name, sheet = 'Scoring by Raster CAT')
		} else if (species_focal == 'Zamia') {
			fields <- read_xlsx(fields_file_name, sheet = 'Scoring by Raster CYCAD')
		}
		fields <- as.data.table(fields)
		fields
	}

	load_team_fields <- function(species_focal) {

		if (species_focal == 'Priona') {
			fields <- read_xlsx(fields_file_name, sheet = 'Scoring by Team CAT')
		} else if (species_focal == 'Zamia') {
			fields <- read_xlsx(fields_file_name, sheet = 'Scoring by Team CYCAD')
		}
		fields <- as.data.table(fields)
		fields
	}

	### load predictions at sites shared by all rasters
	# period		'all' (all periods), 'present', 'mid', or 'late'
	# scaled		TRUE ==> scale predictions to [0, 1], FALSE ==> return raw scores
	# subset_teams  TRUE ==> return data frame with just columns with predictions, FALSE ==> return all columns in extraction frame (e.g., coordinates)
	# discardNAs	TRUE ==> remove rows in which there is at least one NA
	load_predictions <- function(period = 'all', scale = TRUE, subset_teams = TRUE) {

		out <- readRDS(paste0('./Outputs ', species_full, '/Extractions to Random Sites.rds'))
		if (subset_teams) out[ , c('longitude', 'latitude') := NULL]
		# if (discardNAs) out <- out[complete.cases(out)]
		if (period != 'all') {
			col_indices <- which(grepl(names(out), pattern = paste0('_', period)))
			out <- out[ , ..col_indices]
		}

		if (scale) {
			
			fields <- load_rast_fields(species_focal)

			min_max <- readRDS(paste0('./Outputs ', species_full, '/Extractions to Random Sites Minium & Maximum Values across Rasters.rds'))
			min_max$base_raster <- substr(min_max$raster, 1, 1)
			team_info <- fread('./Outputs Shared/team_info.csv')
			codes <- team_info$code

			column_base_codes <- substr(names(out), 1, 1)
			column_base_codes_subs <- substr(names(out), 1, 2)
			column_base_codes_subs <- sub(column_base_codes_subs, pattern = '_', replacement = '')

			# get min/max value for each team from continuous rasters
			# NB We may need to match rasters across time periods
			# For example, a team submits one present-day raster, then two for each future. Here, it's OK to standardize all using the same values.
			# Counter-example: A team submits two present-day rasters using two different SDM algorithms. The present and future rasters should be scaled independently of one another by the algorithm that was used.
			# To do this, we use the "standardize_group" field in the "Scoring by Raster" sheets
			for (i in seq_along(codes)) {

				this_team_code <- codes[i]
				stand_groups <- fields$standardize_group[fields$team_code == this_team_code]
				stand_groups <- unique(stand_groups)

				min_max_team <- min_max[base_raster == this_team_code]

				# just one group of rasters by the team we need to standardize
				if (length(stand_groups) == 1) {
				

					min_val <- min(min_max_team$min, na.rm = TRUE)
					max_val <- max(min_max_team$max, na.rm = TRUE)

					cols <- which(column_base_codes == this_team_code)
					for (col in cols) {
						out[[col]] <- (out[[col]] - min_val) / (max_val - min_val)
					}
				
				} else { # more than one group to standardize

					for (group in seq_along(stand_groups)) {
						
						stand_group <- stand_groups[group]
						rasts_in_group <- fields$raster_code[fields$team_code == this_team_code & fields$standardize_group == stand_group]
						rasts_in_group <- paste0(this_team_code, rasts_in_group)

						this_min_max_team <- min_max_team[raster %in% rasts_in_group]

						min_val <- min(this_min_max_team$min, na.rm = TRUE)
						max_val <- max(this_min_max_team$max, na.rm = TRUE)

						cols <- which(column_base_codes_subs %in% rasts_in_group)
						for (col in cols) {
							out[[col]] <- (out[[col]] - min_val) / (max_val - min_val)
						}

					}
				
				}


			}
	
		}
		out

	}

	# coerce wide-format prediction data.table (one column per team) to long format (one column for predictions, one column for team)
	wide_to_long <- function(wide) {

		long <- melt(
			wide,
			measure.vars = colnames(wide),
			variable.name = 'team',
			value.name = 'prediction'
		)
		long$team <- as.character(long$team)
		long <- as.data.table(long)
		long

	}

	# replace NA cells in projection raster with 0s based on land/sea mask using appropriate data source and resolution
	# r 			projection raster
	# team_code		team code (e.g., "A", "B", etc.)			
	# team_info 	output of load_team_fields()
	fill_terrestrial_NAs <- function(r, team_code, team_info) {

		climate_source <- team_info$predictors_climate_source[team_info$team_code == team_code]
		resol <- team_info$res[team_info$team_code == team_code]

		r_name <- names(r)

		extent <- ext(r)
		extent <- as.polygons(extent, crs = getCRS(r))
		extent <- buffer(extent, 80000)

		if (climate_source == 'CHELSA') {
			zeros <- rast('./Data/Land Mask CHELSA/landseamask.tif')
		} else if (climate_source == 'WorldClim') {
			zeros <- rast('./Data/Land Mask WorldClim/worldclim_21_mask.tif')
		}
		
		zeros <- crop(zeros, extent)

		if (resol == '2.5 arcmin') {
			zeros <- aggregate(zeros, fact = 5, fun = 'mean')
		} else if (resol == '5 arcmin') {
			zeros <- aggregate(zeros, fact = 10, fun = 'mean')
		} else if (resol == '10 arcmin') {
			zeros <- aggregate(zeros, fact = 20, fun = 'mean')
		} else if (resol == '0.5 deg') {
			zeros <- aggregate(zeros, fact = 60, fun = 'mean')
		} else if (resol == '1000×1000 m') {
			zeros <- resample(zeros, r, method = 'mode')
		} else if (resol != '30 arcsec') {
			stop('Do not know how to proceed with this resolution.')
		}

		# combine prediction raster and 0s raster
		zeros <- resample(zeros, r, method = 'mode')
		r_zeros <- c(r, zeros)
		r <- sum(r_zeros, na.rm = TRUE)		
		names(r) <- r_name
		r

	}

say('###############################################')
say('### make maps of all rasters for inspection ###')
say('###############################################')

	# make PDf containing all maps from present day for each team.
	files <- listFiles(paste0('./Submissions/Rasters ', species_full, ' Present'))
	pdf(paste0('./Outputs ', species_full, '/Maps ', species_full, ' Present.pdf'), width = 10, height = 10)
	for (i in seq_along(files)) {

		file <- files[i]
		r <- rast(file)
		name <- basename(file)
		say(name)

		plot(r, main = name)

	}
	dev.off()

	# make PDf containing all maps for mid-century for each team.
	files <- listFiles(paste0('./Submissions/Rasters ', species_full, ' Mid-20th Century'))
	pdf(paste0('./Outputs ', species_full, '/Maps ', species_full, ' Mid-20th Century.pdf'), width = 10, height = 10)
	for (i in seq_along(files)) {

		file <- files[i]
		r <- rast(file)
		name <- basename(file)
		say(name)

		plot(r, main = name)

	}
	dev.off()

	# make PDf containing all maps for  late century for each team.
	files <- listFiles(paste0('./Submissions/Rasters ', species_full, ' Late 20th Century'))
	pdf(paste0('./Outputs ', species_full, '/Maps ', species_full, ' Late 20th Century.pdf'), width = 10, height = 10)
	for (i in seq_along(files)) {

		file <- files[i]
		r <- rast(file)
		name <- basename(file)
		say(name)

		plot(r, main = name)

	}
	dev.off()

say('###################################################################################')
say('### create random points from which to draw predictions across all team rasters ###')
say('###################################################################################')

	# Here we create a set of random points from which to draw predictions across all teams' rasters. Since each set of rasters has a different extent, we'll fine the smallest region that encompasses  all of the rasters, then within this extent place a large number of random sites. We do this by converting each raster to 1/NA values, converting to a polygon, then taking the intersection of the polygons across all species.

	# NB for Zamia, the region that encompasses non-NA cells across all rasters is exceedingly small. To ameliorate this, we'll take the liberty of replacing terrestrial NA cells with a value of 0. We will use the appropriate data set (CHELSA or WorldClim) and resolution to fill in terrestrial cells. We will not expand the extent of rasters.

	team_info <- load_team_fields(species_focal = species_focal)

	### get common extent
	# Convert each rasters to a polygon
	say('Collating projection extents...')
	polys <- list()
	periods <- c('present', 'mid', 'late')
	count_polys <- 1 # counter for list item storing polygons
	for (count_period in seq_along(periods)) {

		period <- periods[count_period]
		period_nice <- get_nice_period(period)

		files <- listFiles(paste0('./Submissions/Rasters ', species_full, ' ', period_nice, ' Anonymized'))

		for (count_file in seq_along(files)) {

			file <- files[count_file]
			r <- rast(file)
			name <- basename(file)
			say(name, ' ', period)

			# fill terrestrial NA cells with 0s
			if (species_focal == 'Zamia') {
				team_code <- sub(name, pattern = '.tif', replacement = '')
				team_code <- substr(team_code, 1, 1)
				r <- fill_terrestrial_NAs(r, team_code = team_code, team_info = team_info)
			}

			r[!is.na(r)] <- 1
			v <- as.polygons(r)
			polys[[count_polys]] <- v

			count_polys <- count_polys + 1

		} # next file

	} # next period

	# find area in common among all polygons
	say('Intersecting...')
	common <- polys[[1]]
	for (i in 2:length(polys)) {
	
		if (!same.crs(common, polys[[i]])) polys[[i]] <- project(polys[[i]], common)
		poly <- crop(polys[[i]], common)
		common <- intersect(common, poly)
	
	}

	common_buff <- buffer(common, 10000)
	common_buff <- aggregate(common_buff)
	n <- round(2 * n_rand_points_to_keep)
	rands <- spatSample(common_buff, n)

	writeVector(common, paste0('./Outputs ', species_full, '/common_study_region.gpkg'), overwrite = TRUE)
	writeVector(rands, paste0('./Outputs ', species_full, '/common_study_region_random_points.gpkg'), overwrite = TRUE)

say('#######################################################################')
say('### extract values for predictions at same sites across all rasters ###')
say('#######################################################################')

	# This chunk extracts values from prediction rasters for the present and two future time periods for each team's rasters at a pre-defined set of random points. It also collates the minimum and maximum values of predictions across each raster for standardization of predictions in some subsequent analyses.

	team_info <- load_team_fields(species_focal = species_focal)

	### random coordinates across each study region
	coords_vect <- vect(paste0('./Outputs ', species_full, '/common_study_region_random_points.gpkg'))
	extracts <- crds(coords_vect)
	extracts <- as.data.table(extracts)

	periods <- c('present', 'mid', 'late')
	min_max <- data.table()
	for (period in periods) {

		period_nice <- get_nice_period(period)

		files <- listFiles(paste0('./Submissions/Rasters ', species_full, ' ', period_nice, ' Anonymized'))
		for (i in seq_along(files)) {

			file <- files[i]

			say(period, ' ', basename(file))

			# raster file name
			r <- rast(file)
			name <- basename(file)

			# fill terrestrial NA cells with 0s
			if (species_focal == 'Zamia') {
				team_code <- sub(name, pattern = '.tif', replacement = '')
				team_code <- substr(team_code, 1, 1)
				r <- fill_terrestrial_NAs(r, team_code = team_code, team_info = team_info)
			}

			# extract values to random points
			stats <- minmax(r)
			stats <- t(stats)
			stats <- as.data.table(stats)
			stats$raster <- names(r)
			stats$period  <- period

			# thresholded?
			unis <- unique(r)
			if (nrow(unis) == 2) {
				stats$binary_threshold <- TRUE
			} else {
				stats$binary_threshold <- FALSE
			}

			min_max <- rbind(min_max, stats)

			extraction <- extract(r, coords_vect, ID = FALSE)
			names(extraction) <- paste0(names(extraction), '_', period)
			extracts <- cbind(extracts, extraction)

		} # next file

	} # next period

	extracts <- extracts[complete.cases(extracts)]
	if (nrow(extracts) > n_rand_points_to_keep) {
		extracts <- extracts[sample(nrow(extracts), n_rand_points_to_keep)]
	} else {
		stop('Cannot ensure ', n_rand_points_to_keep, ' random points have predictions for all rasters.')
	}

	colnames(extracts)[1:2] <- c('longitude', 'latitude')

	saveRDS(extracts, paste0('./Outputs ', species_full, '/Extractions to Random Sites.rds'))
	saveRDS(min_max, paste0('./Outputs ', species_full, '/Extractions to Random Sites Minium & Maximum Values across Rasters.rds'))
print(NON)
# # # say('##########################################################################')
# # # say('### cluster analysis of teams based on predictions at shared locations ###')
# # # say('##########################################################################')

# # # 	n_sites_for_clustering <- 100000
# # # 	preds <- load_predictions(species_focal = species_focal, period = 'all', scale = TRUE, subset_teams = TRUE)

# # # 	preds <- preds[ , !grepl('N4|N5|N6', names(preds)), with = FALSE]

# # # 	preds <- preds[1:n_sites_for_clustering] # for development
# # # 	preds <- t(preds)

# # # 	clust <- fanny(x = preds, diss = FALSE, k = 3, metric = 'euclidean', stand = FALSE)

# # # 	dists <- dist(preds)
# # # 	clust <- hclust(dists)
# # # 	plot(clust)

# # # 	library(factoextra)
# # # 	library(fpc)
# # # 	library(dbscan)

# # # 	preds <- load_predictions(species_focal = species_focal, period = 'all', scale = TRUE, subset_teams = TRUE)
# # # preds <- preds[1:n_sites_for_clustering] # for development
# # # 	preds <- t(preds)

# # # 	# dists <- dist(preds)

# # # 	db <- fpc::dbscan(preds, eps = 60, MinPts = 3, method = 'hybrid')
# # # 	dbscan::kNNdistplot(preds, k = 3)
# # # 	abline(h = 70, lty = 'dashed')
# # # 	abline(h = 65, lty = 'dotted')
# # # 	abline(h = 60, lty = 'dashed')
# # # 	abline(h = 55, lty = 'dashed')
# # # 	abline(h = 50, lty = 'dashed')

# # # 	db <- fpc::dbscan(preds, eps = 60, MinPts = 3, method = 'hybrid')
# # # 	print(db)

# # # 	fviz_cluster(db, data = preds, stand = FALSE, ellipse = TRUE, show.clust.cent = FALSE, geom = 'point', palette = 'jco', ggtheme = theme_classic())

# # # 	library(NbClust)
# # # 	library(clValid)


# # # 	preds <- load_predictions(species_focal = species_focal, period = 'present', scale = TRUE, subset_teams = TRUE)
# # # preds <- preds[1:20000] # for development
# # # 	preds <- t(preds)


# # # 	clust_methods <- c('hierarchical', 'diana', 'agnes', 'kmeans', 'pam', 'som', 'sota')
# # # 	validation  <- c('internal', 'stability')
# # # 	method <- 'complete' # for hierarchical

# # # 	Sys.time()
# # # 	clusts <- clValid(preds, nClust = 2:3, clMethod = clust_methods, validation = validation, method = method, verbose = TRUE)
# # # 	Sys.time()

# # # 	summary(clusts)
# # # 	optimalScores(clusts)


# # # 	clust <- eclust(preds, FUNcluster = 'hclust', hc_metric = 'euclidean', hc_method = 'ward.D2', method = 'silhouette', verbose = TRUE)


# # # print(NONNONNON)


say('##############################################################')
say('### PCA on teams: cluster analysis of teams by predictions ###')
say('##############################################################')

	### Create a dendrogram of predictions and PCA biplot on predictions for a single species for each time period. Teams are clustered by hierarchical partitioning of PC loadings. Several of the subsequent script chunks rely on output from this analysis, so we should run this one first.

	# user-defined
	title_text_size <- 18 # for plots

	# number of sites to use for clustering
	n_sites_for_clustering <- 10000 # development
	# n_sites_for_clustering <- 1000000 # real analysis

	team_codes <- load_team_codes()

	### construct PCA on all time periods' and teams' scaled predictions

		wides <- load_predictions(species_focal = species_focal, period = 'all', scale = TRUE, subset_teams = TRUE)
		wides <- wides[1:n_sites_for_clustering]

		# # remove thresholded mid-century and late century rasters from team N which tend to overly skew the clusters
		# removes <- 'N4a_mid|N4b_mid|N5a_mid|N5b_mid|N6a_mid|N6b_mid|N4a_late|N4b_late|N5a_late|N5b_late|N6a_late|N6b_late'
		# wides <- wides[ , !grepl(removes, names(wides)), with = FALSE]

		# reduce dimensionality
		trans <- t(wides)
		pca <- prcomp(trans)

		# fviz_pca_ind(pca)

		# dists <- distances(trans, normalize = NULL, weights = NULL)
		# dists <- distance_matrix(dists)

		# clust <- hclust(dists, method = 'complete')
		# clust$labels <- names(wides)[clust$order]

		# from ?hopkins::hopkins on clustertend::hopkins(): "The value returned is: 1 - Hopkins statistic." For hopkins::hopkins(): "Calculated values 0-0.3 indicate regularly-spaced data. Values around 0.5 indicate random data. Values 0.7-1 indicate clustered data."
		sink(paste0('./Outputs ', species_full, '/hopkins_statistic.txt'), split = TRUE)
		say('Hopkins statistic from clustertend::hopkins():')
		say('0-0.3 ==> clustered data; ~0.5 ==> random; 0.7-1 ==> uniform data.')
		say('Observed:')
		print(clustertend::hopkins(trans))
		say(date())
		sink()

		# x_lim <- range(pca$x[ , 'PC1'])
		# y_lim <- range(pca$x[ , 'PC2'])

	### evaluate optimal number of clusters

	periods <- c('present', 'mid', 'late')
	biplot <- biplots_sans_teams <- dendro_pca <- list()
	for (count_period in seq_along(periods)) {

		period <- periods[count_period]

		### dendrogram
		##############

		title <- if (period == 'present') {
			'A) Present'
		} else if (period == 'mid') {
			'B) Mid-20th century'
		} else if (period == 'late') {
			'C) Late 20th century'
		}

		wide <- load_predictions(species_focal = species_focal, period = period, scale = TRUE)
		# wide <- wide[ , !grepl(removes, names(wide)), with = FALSE]
		teams_period <- names(wide)
		scores <- pca$x[rownames(pca$x) %in% teams_period, ]

		# keep only first set of PCs that together explain at least X% of variance
		variances <- cumsum(pca$sdev^2) / sum(pca$sdev^2)
		keeps <- which(variances <= min_variance)
		keeps <- c(keeps, tail(keeps, 1) + 1)
		scores <- as.data.frame(scores)
		scores <- scores[ , keeps]
		rownames(scores) <- gsub(rownames(scores), pattern = paste0('_', period), replacement = '')

		teams_scores <- rownames(scores)

		# optimal number of clusters using gap statistic
		# gaps <- clusGap(scores, FUNcluster = kmeans, B = 1000, K.max = ceiling(sqrt(nrow(scores))))
		# gaps <- clusGap(scores, FUNcluster = kmeans, B = 1000, K.max = ceiling(sqrt(nrow(scores))))
		# gaps <- gaps$Tab[ , 'gap']		
		# optimal_n_clusters <- which.min(gaps)
		optimal_n_clusters <- if (period == 'present') {
			4
		} else if (period == 'mid') {
			3
		} else {
			3
		}

		# cluster predictions
		dists <- dist(scores)
		dendro <- hclust(dists, method = 'ward.D')
		clusters <- cutree(dendro, k = optimal_n_clusters)  # pre-defined number of groups (from previous)
		# clusters <- cutree(dendro, h = max_height * max(dendro$height))  # choose number of clusters
		names(clusters) <- sub(names(clusters), pattern = paste0('_', period), replacement = '')

		dend <- as.dendrogram(dendro)
		dend_data <- dendro_data(dend)
		dev.off()

		# move lower part of graph up because team names are cut off otherwise	
		min_label_y <- min(label(dend_data)$y - 0.32)
		# bottom_margin <- 80
		bottom_margin <- 20
		
		leaf_labels <- label(dend_data)
		leaf_labels$label <- sub(leaf_labels$label, pattern = paste0('_', period), replacement = '')
		leaf_labels$cluster <- as.factor(clusters[match(leaf_labels$label, names(clusters))])
		
		n_clusters <- length(unique(clusters))
		cluster_cols <- get(paste0('cluster_cols_', period))
		cluster_cols <- cluster_cols[1:n_clusters]

		dend_data$labels$label <- sub(dend_data$labels$label, pattern = paste0('_', period), replacement = '')
		dendrogram <- ggplot(segment(dend_data)) +
			geom_segment(aes(x = x, y = y, xend = xend, yend = yend)) +
			labs(title = NULL, x = NULL, y = NULL) +
			scale_y_continuous(expand = c(0, 0), limits = c(min_label_y - 0.05, NA)) +
			scale_x_continuous(expand = c(0, 0), limits = c(min(label(dend_data)$x) - 1, max(label(dend_data)$x) + 0.5)) +
			geom_text(
				data = leaf_labels,
				aes(x = x, y = y - 0.04, label = label, color = cluster),
				size = 4, hjust = 1, vjust = 0.4, angle = 90, show.legend = FALSE, fontface = 'bold'
			) +
			scale_color_manual(values = cluster_cols) +
			theme_minimal() +
			coord_cartesian(clip = 'off') +
			theme(
				panel.grid = element_blank(),
				legend.position = 'none',
				axis.text.x = element_blank(),
				axis.ticks.x = element_blank(),
				axis.text.y = element_blank(),
				axis.ticks.y = element_blank(),
				axis.title.y = element_blank(),
				plot.title = element_text(size = title_text_size),
				plot.margin = margin(t = 10, r = 0, b = bottom_margin, l = 35)
			) +
			ggtitle(title)

		scores$team <- rownames(scores)
		score_clusters <- clusters[match(names(clusters), scores$team)]
		scores$cluster <- score_clusters

		# MCP for each cluster
		polys <- list()
		unique_clusters <- sort(unique(clusters))
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

		# formatting
		title <- if (period == 'present') {
			'D) Present'
		} else if (period == 'mid') {
			'E) Mid-century'
		} else if (period == 'late') {
			'F) Late century'
		}


		team_codes <- team_codes[team_codes$team %in% rownames(scores)]

		# values <- team_codes$color
		# names(values) <- team_codes$team

		n_clusters <- length(unique(clusters))
		cluster_cols <- get(paste0('cluster_cols_', period))
		cluster_cols <- cluster_cols[1:n_clusters]

		var1 <- sprintf('%.1f', 100 * pca$sdev[1]^2 / sum(pca$sdev^2))
		var2 <- sprintf('%.1f', 100 * pca$sdev[2]^2 / sum(pca$sdev^2))

		x_lab <- paste0('PC 1 (', var1, '%)')
		y_lab <- paste0('PC 2 (', var2, '%)')

		base_biplot <- ggplot() +
			# xlim(x_lim[1], x_lim[2]) + ylim(y_lim[1], y_lim[2]) +
			coord_cartesian(clip = 'off') +
			xlab(x_lab) + ylab(y_lab)

		# add cluster polygons
		for (i in unique_clusters) {

			base_biplot <- base_biplot +
				geom_polygon(
					data = polys[[i]],
					aes(x = PC1, y = PC2),
					color = 'gray50',
					# fill = alpha('gray', 0.4)
					fill = alpha(cluster_cols[i], 0.4)
				)

		}

		# save this for subsequent analyses
		biplots_sans_teams[[count_period]] <- list(
			scores = as.data.table(scores),
			biplots = base_biplot
		)

		adj_biplot <- base_biplot +
			# ggtitle(title) +
			theme(
				legend.position = 'none',
				axis.title = element_text(size = 0.7 * title_text_size),
				axis.text = element_text(size = 0.6 * title_text_size),
				plot.title = element_text(size = title_text_size)
			)			

		# add teams
		teams <- rownames(scores)
		team_colors <- rep('black', length(teams))
		names(team_colors) <- teams
		# if (period == 'present' & species_focal == 'Priona') {
		# 	team_colors[names(team_colors) == 'Nobis'] <- cluster_cols[4]
		# } else if (period %in% c('mid', 'late') & species_focal == 'Priona') {
		# 	team_colors[names(team_colors) == 'Nobis'] <- cluster_cols[3]
		# 	team_colors[names(team_colors) %in% c('Zurell 1', 'Zurell 2')] <- cluster_cols[4]
		# }
		
		# if (period %in% c('mid', 'late') & species_focal == 'Zamia') {
		# 	team_colors[names(team_colors) %in% c('Aragon 2')] <- cluster_cols[2]
		# }
		
		biplot[[count_period]] <- adj_biplot +
			geom_text(
				data = scores,
				mapping = aes(x = PC1, y = PC2, label = team, color = team),
				# mapping = aes(x = PC1, y = PC2, label = team),
				size = 6,
				fontface = 'bold'
			) +
			scale_color_manual(values = team_colors)

		dendro_pca[[count_period]] <- plot_grid(dendrogram, biplot[[count_period]], ncol = 1, align = 'v', rel_heights = c(0.6, 1))
		saveRDS(clusters, paste0('./Outputs ', species_full, '/', tolower(species_focal), '_clusters_of_teams_', period, '.rds'))

	} # next period

	names(biplots_sans_teams) <- c('present', 'mid', 'late')
	saveRDS(biplots_sans_teams, paste0('./Outputs ', species_full, '/pca_biplots_on_teams_with_groupings.rds'))

	dendro_pcas <- plot_grid(plotlist = dendro_pca, ncol = 3, align = 'h')
	ggsave(dendro_pcas, filename = paste0('./Outputs ', species_full, '/Clustering of Teams.png'), width = 18, height = 7.4, dpi = 600, bg = 'white')
print(NON)
say('#########################')
say('### cluster constancy ###')
say('#########################')

	# How often do teams stay in a cluster with the same other team? Calculating this gives us a metric for cluster constancy.

	present <- readRDS(paste0('./Analysis/', tolower(species_focal), '_clusters_of_teams_present.rds'))
	mid <- readRDS(paste0('./Analysis/', tolower(species_focal), '_clusters_of_teams_mid.rds'))
	late <- readRDS(paste0('./Analysis/', tolower(species_focal), '_clusters_of_teams_late.rds'))

	# functions to calculate Jaccard index for each team's membership in clusters across time
	get_neighbors <- function(clusters, focal_team) {
		
		neighbors <- c()
		for (cluster in clusters) {
			clust <- cluster[[1]]
			if (focal_team %in% clust) {
				neighbors <- unique(c(neighbors, setdiff(clust, focal_team)))
			}
		}
		neighbors
	}

	jaccard_similarity <- function(n1, n2) {

		if (length(n1) == 0 && length(n2) == 0) {
			0
		} else {
			length(intersect(n1, n2)) / length(union(n1, n2))
		}
	
	}

	team_stability <- function(clusters_by_time, focal_team) {
		
		n <- length(clusters_by_time)
		similarities <- numeric(n - 1)
		for (j in 1:(n - 1)) {
			n1 <- get_neighbors(clusters_by_time[[j]], focal_team)
			n2 <- get_neighbors(clusters_by_time[[j + 1]], focal_team)
			similarities[j] <- jaccard_similarity(n1, n2)
		}
		mean(similarities)

	}



	### construct list of lists
	# each sublist is a time period
	# each sub-sublist is a cluster in that time period and contains the team name(s)
	# to account for cases where teams submitted >1 future raster but had just one present-day rasters, we duplicate the present-day so they have the same names as those in the future (we're lucky in that the rasters from the team that had >1 present-day raster was grouped in the same cluster)

	clusters_by_time <- list()
	cluster_numbers <- sort(unique(present))
	for (i in cluster_numbers) {

		members <- names(present)[present == i]

		if (any(members == 'Chefaoui')) {
			
			members <- members[members != 'Chefaoui']
			members <- c(members, c('Chefaoui 1', 'Chefaoui 2'))

		}

		if (any(members == 'Jiménez-Valverde')) {
			
			members <- members[members != 'Jiménez-Valverde']
			members <- c(members, c('Jiménez-Valverde 1', 'Jiménez-Valverde 2'))

		}

		if (any(members == 'Ascanio')) {
			
			members <- members[members != 'Ascanio 2']
			members <- c(members, c('Ascanio 1', 'Ascanio 2'))

		}

		if (any(members == 'Zarzo-Arias')) {
			
			members <- members[members != 'Zarzo-Arias 2']
			members <- c(members, c('Zarzo-Arias 1', 'Zarzo-Arias 2', 'Zarzo-Arias 3', 'Zarzo-Arias 4'))

		}

		if (any(members == 'Zurell')) {
			
			members <- members[members != 'Zurell 1']
			members <- c(members, c('Zurell 1', 'Zurell 2'))

		}

		clusters_by_time$present[[i]] <- list(members)

	}

	cluster_numbers <- sort(unique(mid))
	for (i in cluster_numbers) {
		clusters_by_time$mid[[i]] <- list(names(mid)[mid == i])
	}
	cluster_numbers <- sort(unique(late))
	for (i in cluster_numbers) {
		clusters_by_time$late[[i]] <- list(names(late)[late == i])
	}

	teams_present <- unlist(clusters_by_time$present)
	constancy_present_vs_mid <- constancy_present_vs_late <- rep(NA_real_, length(teams_present))
	for (i in seq_along(teams_present)) {
	
		focal_team <- teams_present[i]
		constancy_present_vs_mid[i] <- team_stability(clusters_by_time[c('present', 'mid')], focal_team)
		constancy_present_vs_late[i] <- team_stability(clusters_by_time[c('present', 'late')], focal_team)
	
	}

	sink(paste0('./Analysis/', species_focal, ' Cluster Constancy.txt'), split = TRUE)
	say('Mean Jaccard index across teams for its constancy of membership with other teams in clusters:')

	mu <- mean(constancy_present_vs_mid)
	sem <- sd(constancy_present_vs_mid) / sqrt(length(constancy_present_vs_mid))
	say('Present vs mid-20th century: mean = ', mu, ' +- ', sem, ' sem')

	mu <- mean(constancy_present_vs_late)
	sem <- sd(constancy_present_vs_late) / sqrt(length(constancy_present_vs_late))
	say('Present vs late 20th century: mean = ', mu, ' +- ', sem, ' sem')

	sink()

say('####################################################')
say('### heatmaps of correlations between predictions ###')
say('####################################################')

	### Create heatmaps of correlations between predictions made by each team in each time period for one species.
	periods <- c('present', 'mid', 'late')
	min_cor <- Inf
	heats <- list()
	for (period in periods) {
		
		wide <- load_predictions(species_focal = species_focal, period = period, scaled = FALSE, subset_teams = TRUE)

		if (period == 'present') {
			title <- 'A) Present'
		} else if (period == 'mid') {
			title <- 'B) Mid-20th century'
		} else if (period == 'late') {
			title <- 'C) Late 20th century'
		}

		### correlation heatmap

		cors <- cor(wide, method = 'spearman')
		mean_cors <- rowMeans(cors)

		# clustering by correlation
		hc <- hclust(as.dist(1 - abs(cors)))
		order <- hc$order
		team_names <- colnames(cors)[order]

		# reorder the correlation matrix
		cors_ordered <- cors[team_names, team_names]

		# long form for ggplot
		cors_long <- melt(cors_ordered)
		colnames(cors_long) <- c('team_1', 'team_2', 'correl')

		if (anonymize) {
			cors_long$team_1 <- anonymize_teams(cors_long$team_1, species_focal = species_focal)
			cors_long$team_2 <- anonymize_teams(cors_long$team_2, species_focal = species_focal)
		}

		# heatmap
		heats[[length(heats) + 1]] <- ggplot(cors_long, aes(x = team_1, y = team_2, fill = correl)) +
			geom_tile() +
			theme_minimal() +
			theme(
				axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
				axis.text.y = element_text(size = 8),
				aspect.ratio = 1
			) +
			labs(title = title, x = NULL, y = NULL)

		min_cor <- min(min_cor, cors_long$correl)

	} # next period

	for (i in 1:3) heats[[i]] <- heats[[i]] +
		scale_fill_viridis_c(option = 'magma', name = 'Spearman\nCorrelation', limits = c(min_cor, 1))

	heatmaps <- plot_grid(plotlist = heats, ncol = 3)

	ggsave(heatmaps, filename = paste0('./Analysis/', species_focal, ' Heatmap of Spearman Correlations Between Teams', ifelse(anonymize, ' Anonymized', ''), '.png'), width = 14, height = 4, dpi = 600, bg = 'white')

say('############################################')
say('### distributions of predictions by team ###')
say('############################################')

	# Create a set of violin plots of the distributions of predictions by each team. We color the violins (teams) by the clusters identified by hierarchical clustering of PC loadings, and display the mean prediction across teams alongside the team predictions (which is not part of the clustering).

	cols <- cluster_cols_present

	periods <- c('present', 'mid', 'late')
	distrib <- list()
	for (count_period in seq_along(periods)) {

		period <- periods[count_period]

		# cluster
		clusters <- readRDS(paste0('./Analysis/', tolower(species_focal), '_clusters_of_teams_', period, '.rds'))
		clusters <- sort(clusters)

		wide <- load_predictions(species_focal = species_focal, period = period, scaled = TRUE, subset_teams = TRUE)
		wide[ , Mean := rowMeans(wide)]

		# convert to long format
		long <- wide_to_long(wide)

		# add cluster number
		long[ , cluster := clusters[team]]

		long$cluster[long$team == 'Mean'] <- 0

		# order factors by mean predictions
		ordered_teams <- names(clusters)
		if (anonymize) {
		
			ordered_teams <- anonymize_teams(ordered_teams, species_focal = species_focal)
			long[ , team := anonymize_teams(team, species_focal = species_focal)]	
			long$team[is.na(long$team)] <- 'Mean'

		}

		ordered_teams <- c('Mean' = 'Mean', ordered_teams)
		long$team <- factor(long$team, levels = ordered_teams)
		long[ , cluster := as.factor(cluster)]

		n_teams <- length(unique(long$team))

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

		cluster_cols <- get(paste0('cluster_cols_', period))
		n_clusters <- length(unique(clusters))
		cluster_cols <- cluster_cols[1:n_clusters]

		distrib[[count_period]] <- ggplot(long, aes(x = team, y = prediction, fill = cluster)) +
			geom_violin(width = width, alpha = 0.6) +
			scale_fill_manual(values = cluster_cols, name = 'Cluster') +
			scale_x_discrete(expand = expansion(mult = c(0.01, 0.01))) +
			coord_cartesian(xlim = c(0, n_teams + 0.75)) +
			ylab('Prediction (scaled to [0, 1])') +
			theme(
				legend.position = 'none',
				axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
				axis.title.x = element_blank()
			) +
			ggtitle(title)

	}

	distribs <- plot_grid(plotlist = distrib, nrow = 3, align = 'v')

	height <- if (anonymize) { 8 } else { 11 }
	ggsave(distribs, filename = paste0('./Analysis/', species_focal, ' Distribution of Predictions by Team', ifelse(anonymize, ' Anonymized', ''), '.png'), width = 8.5, height = 11, dpi = 600, bg = 'white')

say('###########################################################################')
say('### test for associations between team clusters and workflow attributes ###')
say('###########################################################################')

	# Implements a *single* Kruskal-Wallis or contingency table analysis (with Fisher's exact test and simulate P values)-- according to covariate type--with cluster ID as the predictor.
	#
	# Count number of each unique value in y
	#
	# fields		data.table with workflow attributes
	# nice      	Nice name for association (e.g., 'Occurrences: Number' or 'Occurrences: Source)
	# period		'present, 'mid', 'late': time period of predictions
	# results		data.table with results so far
	# y				predictor vector (factor or numeric)
	# test			type of test: 'contingency' (table) for categorical y, or 'kw' (Kruskal-Wallis) for numeric y
	analyze_association <- function(fields, nice, period, results, y, test) {

		# remove missing values from fields and from response
		clusts <- fields$cluster

		missing <- is.na(y)
		if (any(missing)) {
		
			clusts <- clusts[!missing]
			y <- y[!missing]

		}

		if (test == 'contingency') {
			
			x_table <- table(clusts, y)

			y_sufficient <- (sum(y) > 1 & sum(1 - y) > 1) | length(unique(y)) > 2

			### Fisher's exact test with simulate P value for non-2x2 tables: categorical vs categorical

			if (y_sufficient) {

				x_table <- table(clusts, y)
				# chi <- chisq.test(x_table)
				fisher <- fisher.test(x_table, simulate.p.value = TRUE, B = 100000)

				results <- rbind(
					results,
					data.table(
						nice = nice,
						species_focal = species_focal,
						period = period,
						test = 'Contingency',
						kw_chi_sq = NA_real_,
						p = fisher$p.value,
						sig = ifelse(fisher$p.value <= 0.05, '*', '.')
					)
				)

			} else {
			
				results <- rbind(
					results,
					data.table(
						nice = nice,
						species_focal = species_focal,
						period = period,
						test = NA,
						kw_chi_sq = NA_real_,
						p = NA,
						sig = 'Insufficient number of categories'
					)
				)
			
			}

		} else if (test == 'kw') {
			
			### Kruskal-Wallis (numeric vs categorical): categorical vs numeric (non-normal)

			data <- data.table(y = y, cluster = clusts)
			kw <- kruskal.test(y ~ cluster, data = data)

			results <- rbind(
				results,
				data.table(
					nice = nice,
					species_focal = species_focal,
					period = period,
					test = 'Kruskal-Wallis',
					kw_chi_sq = kw$statistic,
					p = kw$p.value,
					sig = ifelse(kw$p.value <= 0.05, '*', '.')
				)
			)

		}
		results

	}

	# Test for possible associations between all sets of workflow attributes and cluster identity to see what (if anything) explains cluster identity
	#
	# fields 	data.table with workflow attributed
	# period	'present' (present day), 'future' (mid- or later-century), or 'difference' (future - present)
	#
	# Returns a data.table with tests and their p-values
	analyze_associations <- function(fields, period) {

		results <- data.table()

		### thresholded predictions

			y <- as.numeric(fields$prediction_type_thresholded)
			
			test <- 'contingency'

			nice <- 'Prediction Type (thresholded/continuous)'
			results <- analyze_association(fields = fields, period = period, nice = nice, results = results, y = y, test = test)

		### number of occurrences

			y <- as.numeric(fields$num_occurrences_minimum)
			test <- 'kw'

			nice <- 'Occurrences: Number of occurrences'
			results <- analyze_association(fields = fields, period = period, nice = nice, results = results, y = y, test = test)

		### collinearity: managed at all

			field_names <- c('collinearity_pca', 'collinearity_correlation', 'collinearity_vif', 'collinearity_other_method')
			y <- fields[ , ..field_names]
			y <- y[ , lapply(.SD, as.numeric)]

			y <- rowSums(y)
			y <- as.numeric(y > 0)
			test <- 'contingency'

			nice <- 'Collinearity: Explicitly managed'
			results <- analyze_association(fields = fields, period = period, nice = nice, results = results, y = y, test = test)

		### collinearity: method

			field_names <- c('collinearity_pca', 'collinearity_correlation', 'collinearity_vif', 'collinearity_other_method')
			for (i in seq_along(field_names)) {

				field_name <- field_names[i]
				y <- fields[[field_name]]
				y <- as.numeric(y)

				test <- 'contingency'

				nice <- if (grepl(field_name, pattern = 'pca')) {
					'Collinearity: Used PC axes as predictors'
				} else if (grepl(field_name, pattern = 'correlation')) {
					'Collinearity: Screened using correlation'
				} else if (grepl(field_name, pattern = 'vif')) {
					'Collinearity: Screened using VIF'
				} else if (grepl(field_name, pattern = 'other_method')) {
					'Collinearity: Screened using other method'
				}

				results <- analyze_association(fields = fields, period = period, nice = nice, results = results, y = y, test = test)

			}

		# ### BIOCLIM predictors: identity

		# 	for (bio in 1:19) {

		# 		field_name <- paste0('predictors_climate_bio', bio)
		# 		y <- fields[[field_name]]
		# 		y <- as.numeric(y)

		# 		test <- 'contingency'

		# 		nice <- paste('Predictors: Used BIO', bio)
		# 		results <- analyze_association(fields = fields, period = period, nice = nice, results = results, y = y, test = test)

		# 	}

		# ### BIOCLIM predictors: number

		# 	field_name <- 'predictors_climate_num_bioclim'
		# 	y <- fields[[field_name]]
		# 	y <- as.numeric(y)

		# 	test <- 'kw'

		# 	nice <- 'Predictors: Number of BIOCLIM predictors'
		# 	results <- analyze_association(fields = fields, period = period, nice = nice, results = results, y = y, test = test)

		### climate predictors: number

			y <- fields$predictors_climate_num_total
			y <- as.numeric(y)

			test <- 'kw'

			nice <- 'Predictors: Total number of climate predictors'
			results <- analyze_association(fields = fields, period = period, nice = nice, results = results, y = y, test = test)

		### non-climate predictors: identity

			# field_names <- c('predictors_nonclimate_elevation', 'predictors_nonclimate_aspect', 'predictors_nonclimate_forest_cover', 'predictors_nonclimate_soil', 'predictors_nonclimate_npp_ndvi', 'predictors_nonclimate_terrain_ruggedness_position', 'predictors_nonclimate_land_cover', 'predictors_nonclimate_human_impact', 'predictors_nonclimate_other')

			# for (pred in seq_along(field_names)) {

			# 	field_name <- field_names[pred]
			# 	y <- fields[[field_name]]
			# 	y <- as.numeric(y)

			# 	test <- 'contingency'

			# 	nice <- if (grepl(field_name, pattern = 'elevation')) {
			# 		'Predictors: Used elevation'
			# 	} else if (grepl(field_name, pattern = 'aspect')) {
			# 		'Predictors: Used aspect'
			# 	} else if (grepl(field_name, pattern = 'forest_cover')) {
			# 		'Predictors: Used forest cover'
			# 	} else if (grepl(field_name, pattern = 'soil')) {
			# 		'Predictors: Used soil'
			# 	} else if (grepl(field_name, pattern = 'npp_ndvi')) {
			# 		'Predictors: Used NPP/NDVI'
			# 	} else if (grepl(field_name, pattern = 'terrain_ruggedness_position')) {
			# 		'Predictors: Used TRI/TPI'
			# 	} else if (grepl(field_name, pattern = 'land_cover')) {
			# 		'Predictors: Used LULC'
			# 	} else if (grepl(field_name, pattern = 'human_impact')) {
			# 		'Predictors: Used human impact predictor(s)'
			# 	} else if (grepl(field_name, pattern = 'other')) {
			# 		'Predictors: Used other non-climate predictor(s)'
			# 	}

			# 	results <- analyze_association(fields = fields, period = period, nice = nice, results = results, y = y, test = test)

			# }

		### non-climate: number of predictors

			y <- fields$predictors_nonclimate_num
			y <- as.numeric(y)

			test <- 'kw'

			nice <- 'Predictors: Number of non-climate predictors'
			results <- analyze_association(fields = fields, period = period, nice = nice, results = results, y = y, test = test)

		### source of climate predictors

			y <- fields$predictors_climate_source_worldclim
			y <- as.numeric(y)

			test <- 'contingency'

			nice <- 'Predictors: Climate data source'
			results <- analyze_association(fields = fields, period = period, nice = nice, results = results, y = y, test = test)

		### number of predictors

			y <- fields$predictors_climate_nonclimate_num_total
			y <- as.numeric(y)

			test <- 'kw'

			nice <- 'Predictors: Total number'
			results <- analyze_association(fields = fields, period = period, nice = nice, results = results, y = y, test = test)

		### modeling_software

			set_names <- c('modeling_software_enmeval', 'modeling_software_enmtools', 'modeling_software_wallace', 'modeling_software_biomod2', 'modeling_software_sabinansdm', 'modeling_software_sdm', 'modeling_software_miamaxent', 'modeling_software_enmsdmx', 'modeling_software_flexsdm', 'modeling_software_sdmtune', 'modeling_software_piecemeal')

			test <- 'contingency'

			for (i in seq_along(set_names)) {

				field_name <- set_names[i]
				y <- fields[[field_name]]
				y <- as.numeric(y)

				nice <- if (grepl(field_name, pattern = '_enmeval')) {
					'Software: Used ENMeval'
				} else if (grepl(field_name, pattern = '_enmtools')) {
					'Software: Used ENMTools'
				} else if (grepl(field_name, pattern = '_wallace')) {
					'Software: Used Wallace'
				} else if (grepl(field_name, pattern = '_biomod2')) {
					'Software: Used BIOMOD2'
				} else if (grepl(field_name, pattern = '_sabinansdm')) {
					'Software: Used SabinaNSDM'
				} else if (grepl(field_name, pattern = '_sdmtune')) {
					'Software: Used SDMtune'
				} else if (grepl(field_name, pattern = '_sdm')) {
					'Software: Used sdm'
				} else if (grepl(field_name, pattern = '_miamaxent')) {
					'Software: Used MIAmaxent'
				} else if (grepl(field_name, pattern = '_enmsdmx')) {
					'Software: Used enmSdmX'
				} else if (grepl(field_name, pattern = '_flexsdm')) {
					'Software: Used flexsdm'
				} else if (grepl(field_name, pattern = '_piecemeal')) {
					'Software: Used piecemeal R packages'
				}

				results <- analyze_association(fields = fields, period = period, nice = nice, results = results, y = y, test = test)

			}

		### modeling_software: ENVeval / Wallace

			set_names <- c('modeling_software_enmeval', 'modeling_software_wallace')
			y <- fields [ , ..set_names]
			y <- y[ , lapply(.SD, as.numeric)]
			y <- rowSums(y)
			y <- as.numeric(y > 0)

			nice <- 'Used ENMeval/Wallace'

			results <- analyze_association(fields = fields, period = period, nice = nice, results = results, y = y, test = test)

		### algorithm: number

			y <- fields$algo_num
			y <- as.numeric(y)

			test <- 'kw'
			nice <- 'Algorithm: Number of algorithms'

			results <- analyze_association(fields = fields, period = period, nice = nice, results = results, y = y, test = test)

		### algorithm: used ensemble
		
			y <- fields$algo_ensemble
			y <- as.numeric(y)

			test <- 'contingency'
			nice <- 'Algorithm: Used ensemble'

			results <- analyze_association(fields = fields, period = period, nice = nice, results = results, y = y, test = test)

		### algorithm: number of algorithms used in ensemble (including 0)
		
			y <- fields$algo_ensemble_number_of_models
			y <- as.numeric(y)

			test <- 'kw'
			nice <- 'Algorithm: Number of algorithms in ensemble (inc. 0)'

			results <- analyze_association(fields = fields, period = period, nice = nice, results = results, y = y, test = test)

		### algorithm: identity
		
			field_names <- c('algo_maxent', 'algo_maxnet', 'algo_brt_gbm', 'algo_glm', 'algo_gam', 'algo_rf', 'algo_xgboost')

			for (i in seq_along(field_names)) {

				field_name <- field_names[i]
				y <- fields[[field_name]]
				y <- as.numeric(y)

				test <- 'contingency'

				nice <- if (grepl(field_name, pattern = 'maxent')) {
					'Algorithms: Used MaxEnt'
				} else if (grepl(field_name, pattern = 'maxnet')) {
					'Algorithms: Used MaxNet'
				} else if (grepl(field_name, pattern = 'brt_gbm')) {
					'Algorithms: Used BRTs/GBMs'
				} else if (grepl(field_name, pattern = 'glm')) {
					'Algorithms: Used GLM'
				} else if (grepl(field_name, pattern = 'gam')) {
					'Algorithms: Used GAM'
				} else if (grepl(field_name, pattern = 'rf')) {
					'Algorithms: Used RFs'
				} else if (grepl(field_name, pattern = 'mars')) {
					'Algorithms: Used MARS'
				} else if (grepl(field_name, pattern = 'fda')) {
					'Algorithms: Used FDA'
				} else if (grepl(field_name, pattern = 'xgboost')) {
					'Algorithms: Used XGBoost'
				}

				results <- analyze_association(fields = fields, period = period, nice = nice, results = results, y = y, test = test)

			}

		### algorithm: MaxEnt / MaxNet

			set_names <- c('algo_maxent', 'algo_maxnet')
			y <- fields [ , ..set_names]
			y <- y[ , lapply(.SD, as.numeric)]
			y <- rowSums(y)
			y <- as.numeric(y > 0)

			nice <- 'Used MaxEnt/MaxNet'

			results <- analyze_association(fields = fields, period = period, nice = nice, results = results, y = y, test = test)

		### bias correction: did any
			
			field_names <- c('bias_correction_spatial_thinning', 'bias_correction_environmental_thinning', 'bias_correction_target_background', 'bias_correction_nonrandom_background')
			
			y <- fields[ , ..field_names]
			y <- y[ , lapply(.SD, as.numeric)]
			y <- rowSums(y)
			y <- as.numeric(y > 0)

			test <- 'contingency'

			nice <- 'Bias correction: Implemented'
			results <- analyze_association(fields = fields, period = period, nice = nice, results = results, y = y, test = test)

		### bias correction: method

			field_names <- c('bias_correction_spatial_thinning', 'bias_correction_environmental_thinning', 'bias_correction_target_background', 'bias_correction_nonrandom_background')

			for (i in seq_along(field_names)) {

				field_name <- field_names[i]
				y <- fields[[field_name]]
				y <- as.numeric(y)

				test <- 'contingency'

				nice <- if (grepl(field_name, pattern = 'spatial_thinning')) {
					'Bias correction: Spatial thinning'
				} else if (grepl(field_name, pattern = 'environmental_thinning')) {
					'Bias correction: Environmental thinning'
				} else if (grepl(field_name, pattern = 'target_background')) {
					'Bias correction: Target background'
				} else if (grepl(field_name, pattern = 'nonrandom_background')) {
					'Bias correction: Non-random background'
				}

				results <- analyze_association(fields = fields, period = period, nice = nice, results = results, y = y, test = test)

			}
			
		# ### occurrence data: number of sources

		# 	field_names <- c('occurrence_data_gbif', 'occurrence_data_idigbio', 'occurrence_data_vertnet', 'occurrence_data_inaturalist', 'occurrence_data_publications')
		# if (species_focal == 'Zamia') field_names <- c(field_names, occurrence_data_conabio)

		# 	y <- fields[ , ..field_names]
		# 	y <- y[ , lapply(.SD, as.numeric)]			
		# 	y <- rowSums(y)

		# 	test <- 'kw'
		# 	nice <- 'Occurrence data: Number of sources'

		# 	results <- analyze_association(fields = fields, period = period, nice = nice, results = results, y = y, test = test)

		# ### occurrence data: source
			
		# 	field_names <- c('occurrence_data_gbif', 'occurrence_data_idigbio', 'occurrence_data_vertnet', 'occurrence_data_inaturalist', 'occurrence_data_publications', 'occurrence_data_conabio)
		# if (species_focal == 'Zamia') field_names <- c(field_names, occurrence_data_conabio)

		# 	for (i in seq_along(field_names)) {

		# 		field_name <- field_names[i]
		# 		y <- fields[[field_name]]
		# 		y <- as.numeric(y)

		# 		test <- 'contingency'

		# 		nice <- if (grepl(field_name, pattern = 'gbif')) {
		# 			'Occurrence data: From GBIF'
		# 		} else if (grepl(field_name, pattern = 'idigbio')) {
		# 			'Occurrence data: From iDigBio'
		# 		} else if (grepl(field_name, pattern = 'vertnet')) {
		# 			'Occurrence data: From VertNet'
		# 		} else if (grepl(field_name, pattern = 'inaturalist')) {
		# 			'Occurrence data: From iNaturalist'
		# 		} else if (grepl(field_name, pattern = 'publications')) {
		# 			'Occurrence data: From publications'
		# 		}

		# 		results <- analyze_association(fields = fields, period = period, nice = nice, results = results, y = y, test = test)

		# 	}

		### non-presence type
		
			# non-presences: type
			field_names <- c('nonpres_type_background', 'nonpres_type_pseudoabsence', 'nonpres_type_target_background')

			y <- rep(NA_character_, nrow(fields))

			for (i in seq_along(field_names)) {

				field_name <- field_names[i]
				y <- fields[[field_name]]
				y <- as.numeric(y)

				test <- 'contingency'

				nice <- if (grepl(field_name, pattern = 'nonpres_type_background')) {
					'Non-presence type: Background sites'
				} else if (grepl(field_name, pattern = 'pseudoabsence')) {
					'Non-presence type: Pseudoabsences'
				} else if (grepl(field_name, pattern = 'target_background')) {
					'Non-presence type: Target background'
				}

				results <- analyze_association(fields = fields, period = period, nice = nice, results = results, y = y, test = test)

			}
			
		### calibration region boundary
		
			# non-presences: type
			field_names <- c('boundary_rectangle', 'boundary_natural', 'boundary_convex_hull', 'boundary_range_map', 'boundary_political', 'boundary_buffer_around_occurrences')

			for (i in seq_along(field_names)) {

				field_name <- field_names[i]
				y <- fields[[field_name]]
				y <- as.numeric(y)

				test <- 'contingency'

				nice <- if (grepl(field_name, pattern = 'rectangle')) {
					'Calibration region: Rectangle'
				} else if (grepl(field_name, pattern = 'natural')) {
					'Calibration region: Natural (e.g., ecoregions)'
				} else if (grepl(field_name, pattern = 'convex_hull')) {
					'Calibration region: Convex hull (possibly with buffer)'
				} else if (grepl(field_name, pattern = 'range_map')) {
					'Calibration region: Range map (possibly with buffer)'
				} else if (grepl(field_name, pattern = 'political')) {
					'Calibration region: Political'
				} else if (grepl(field_name, pattern = 'buffer_around_occurrences')) {
					'Calibration region: Buffer around occurrences'
				}

				results <- analyze_association(fields = fields, period = period, nice = nice, results = results, y = y, test = test)

			}

		### calibration region extent
			
			y <- as.numeric(fields$extent_calibration_sans_water_km2)

			test <- 'kw'
			nice <- 'Calibration extent: Area'

			results <- analyze_association(fields = fields, period = period, nice = nice, results = results, y = y, test = test)
		
		### spatial resolution: cell size
			
			# y <- as.numeric(fields$res_mean_cell_size_km2)
			y <- as.numeric(fields$res)

			test <- 'contingency'
			# nice <- 'Spatial resolution: Cell size (km2)'
			nice <- 'Spatial resolution (arcmin)'

			results <- analyze_association(fields = fields, period = period, nice = nice, results = results, y = y, test = test)
		
		### taxonomy: accounted for subspecies
			
			if (species_focal == 'Priona') {

				y <- as.numeric(fields$taxonomy_mainland_only)

				test <- 'contingency'
				nice <- 'Modeled only mainland subspecies'

				results <- analyze_association(fields = fields, period = period, nice = nice, results = results, y = y, test = test)
			
			}

		### ODMAP *mean* score

			y <- as.numeric(fields$odmap_mean)
			
			test <- 'kw'
			nice <- 'ODMAP: Mean score'

			results <- analyze_association(fields = fields, period = period, nice = nice, results = results, y = y, test = test)

		### ODMAP *minimum* of mean score

			y <- as.numeric(fields$odmap_min)
			
			test <- 'kw'
			nice <- 'ODMAP: Minimum score across criteria'

			results <- analyze_association(fields = fields, period = period, nice = nice, results = results, y = y, test = test)

		if (period != 'present') {
		
			### extrapolation: individual methods

				field_names <- c('extrapolation_clamping_masking_clipping', 'extrapolation_exdet', 'extrapolation_mess', 'extrapolation_shape', 'extrapolation_area_of_applicability', 'extrapolation_response_curve_inspection', 'extrapolation_kissmig')

				for (i in seq_along(field_names)) {

					field_name <- field_names[i]
					y <- fields[[field_name]]
					y <- as.numeric(y)

					test <- 'contingency'

					nice <- if (grepl(field_name, pattern = 'clamping_masking_clipping')) {
						'Extrapolation: Clamping/masking/clipping'
					} else if (grepl(field_name, pattern = 'exdet')) {
						'Extrapolation: ExDet'
					} else if (grepl(field_name, pattern = 'mess')) {
						'Extrapolation: MESS'
					} else if (grepl(field_name, pattern = 'shape')) {
						'Extrapolation: SHAPE'
					} else if (grepl(field_name, pattern = 'area_of_applicability')) {
						'Extrapolation: Area of Applicability'
					} else if (grepl(field_name, pattern = 'response_curve_inspection')) {
						'Extrapolation: Response curve inspection'
					} else {
						NA
					}

					results <- analyze_association(fields = fields, period = period, nice = nice, results = results, y = y, test = test)

				}

			### extrapolation: any method

				field_names <- c('extrapolation_clamping_masking_clipping', 'extrapolation_exdet', 'extrapolation_mess', 'extrapolation_shape', 'extrapolation_area_of_applicability', 'extrapolation_response_curve_inspection', 'extrapolation_kissmig')
				
				y <- fields[ , ..field_names]
				y <- y[ , lapply(.SD, as.numeric)]
				y <- rowSums(y)
				y <- as.numeric(y > 0)
			
				test <- 'contingency'
				nice <- 'Managed extrapolation (any method)'

				results <- analyze_association(fields = fields, period = period, nice = nice, results = results, y = y, test = test)

			### future: CMIP

				y <- as.numeric(fields$future_scenario_cmip)
				y[y == 'NA'] <- NA
				y <- as.numeric(y == 'RCP')
			
				test <- 'contingency'
				nice <- 'Future: CMIP'

				results <- analyze_association(fields = fields, period = period, nice = nice, results = results, y = y, test = test)

			### future: mid-century year

				if (period == 'mid') {

					y <- fields$future_scenario_midcentury_year
					y[y == 'NA'] <- NA
					y[y == '2041-2060'] <- '0'
					y[y == '2041-2070'] <- '1'
					y <- as.numeric(y)

					test <- 'contingency'
					nice <- 'Future: Mid-century period'

					results <- analyze_association(fields = fields, period = period, nice = nice, results = results, y = y, test = test)

				}

			### future: late-century year

				if (period == 'late') {

					y <- fields$future_scenario_latecentury_year
					y[y == 'NA'] <- NA
					y[y == '2061-2080'] <- '0'
					y[y == '2071-2100'] <- '1'
					y[y == '2081-2100'] <- '2'
					y <- as.numeric(y)

					test <- 'contingency'
					nice <- 'Future: Late-century period'

					results <- analyze_association(fields = fields, period = period, nice = nice, results = results, y = y, test = test)

				}

		}

		results
		
	} # EOF

	fields <- load_fields(species_focal)

	### add clusters to fields
	clusters_present <- readRDS(paste0('./Analysis/', species_focal, '_clusters_of_teams_present.rds'))
	clusters_mid <- readRDS(paste0('./Analysis/', species_focal, '_clusters_of_teams_mid.rds'))
	clusters_late <- readRDS(paste0('./Analysis/', species_focal, '_clusters_of_teams_late.rds'))

	fields_present <- fields_mid <- fields_late <- fields

	# present
	fields_present[ , cluster := clusters_present[match(fields_present$team, names(clusters_present))]]

	# mid-century... we duplicate the team's row in the table for each duplicate raster
	team_versions_mid <- fread('./Analysis/team_versions_mid.csv')

	bases <- unique(team_versions_mid$base)
	for (i in seq_along(bases)) {
	
		base <- bases[i]
		row_index <- which(fields_mid$team == base)
		team <- fields_mid$team[row_index]
		n_versions <- sum(team_versions_mid$base == base)

		if (n_versions > 1) {
		
			this_row <- fields_mid[row_index]
			fields_mid$team[row_index] <- paste0(team, ' 1')

			for (j in 2:n_versions) {
				this_row_copy <- this_row
				this_row_copy$team <- paste(this_row_copy$team, j)
				fields_mid <- rbind(fields_mid, this_row_copy)
			}
		
		}	
	
	}

	fields_mid[ , cluster := clusters_mid[match(fields_mid$team, names(clusters_mid))]]
	# fields_mid <- fields_mid[!is.na(cluster)]

	# late century... we duplicate the team's row in the table for each duplicate raster
	team_versions_late <- fread('./Analysis/team_versions_late.csv')

	bases <- unique(team_versions_late$base)
	for (i in seq_along(bases)) {
	
		base <- bases[i]
		row_index <- which(fields_late$team == base)
		team <- fields_late$team[row_index]
		n_versions <- sum(team_versions_late$base == base)

		if (n_versions > 1) {
		
			this_row <- fields_late[row_index]
			fields_late$team[row_index] <- paste0(team, ' 1')

			for (j in 2:n_versions) {
				this_row_copy <- this_row
				this_row_copy$team <- paste(this_row_copy$team, j)
				fields_late <- rbind(fields_late, this_row_copy)
			}
		
		}	
	
	}

	fields_late[ , cluster := clusters_late[match(fields_late$team, names(clusters_late))]]
	fields_late <- fields_late[!is.na(cluster)]

	fields_present$cluster <- as.factor(fields_present$cluster)
	fields_mid$cluster <- as.factor(fields_mid$cluster)
	fields_late$cluster <- as.factor(fields_late$cluster)

	### add MEAN ODMAP score to fields
	odmap <- readRDS('./Analysis/Summary of Assessment of SDM Workflows by SDM Standards.rds')

	y <- odmap$means
	y <- y[grepl(y$species, pattern = species_focal)]
	criteria <- c(paste0(1, LETTERS[1:5]), paste0(2, LETTERS[1:3]), paste0(3, LETTERS[1:4]), paste0(4, LETTERS[1:3]))
	odmap_scores <- y[ , ..criteria]
	odmap_means <- rowMeans(odmap_scores)
	
	odmap_means <- 4 - odmap_means # reverse ranks so 4 = gold, 0 = deficient

	odmap_teams <- y$first_author
	odmap_teams[odmap_teams == 'JIMENEZ-VALVERDE'] <- 'Jiménez-Valverde'
	odmap_teams <- tolower(odmap_teams)
	names(odmap_means) <- odmap_teams

	teams <- fields_present$team
	names(odmap_means) <- teams[match(odmap_teams, tolower(teams))]

	fields_present$odmap_mean <- odmap_means[match(fields_present$team, names(odmap_means))]

	fields_mid[ , odmap_mean := NA_real_]
	for (i in seq_along(odmap_means)) {
		odmap_team <- names(odmap_means)[i]
		odmap_team <- tolower(odmap_team)
		index <- which(grepl(tolower(fields_mid$team), pattern = odmap_team))
		fields_mid$odmap_mean[index] <- odmap_means[i]
	}

	fields_late[ , odmap_mean := NA_real_]
	for (i in seq_along(odmap_means)) {
		odmap_team <- names(odmap_means)[i]
		odmap_team <- tolower(odmap_team)
		index <- which(grepl(tolower(fields_late$team), pattern = odmap_team))
		fields_late$odmap_mean[index] <- odmap_means[i]
	}

	### add MINIMUM of MEAN ODMAP score to fields
	y <- odmap$means # reverse ranks so 4 = gold, 0 = deficient
	y <- y[grepl(y$species, pattern = species_focal)]
	criteria <- c(paste0(1, LETTERS[1:5]), paste0(2, LETTERS[1:3]), paste0(3, LETTERS[1:4]), paste0(4, LETTERS[1:3]))
	odmap_scores <- y[ , ..criteria]
	odmap_mins <- apply(odmap_scores, 1, min)

	odmap_mins <- 4 - odmap_mins # reverse ranks so 4 = gold, 0 = deficient

	odmap_teams <- y$first_author
	odmap_teams[odmap_teams == 'JIMENEZ-VALVERDE'] <- 'Jiménez-Valverde'
	odmap_teams <- tolower(odmap_teams)
	names(odmap_mins) <- odmap_teams

	teams <- fields_present$team
	names(odmap_mins) <- teams[match(odmap_teams, tolower(teams))]

	fields_present$odmap_min <- odmap_mins[match(fields_present$team, names(odmap_mins))]

	fields_mid[ , odmap_min := NA_real_]
	for (i in seq_along(odmap_mins)) {
		odmap_team <- names(odmap_mins)[i]
		odmap_team <- tolower(odmap_team)
		index <- which(grepl(tolower(fields_mid$team), pattern = odmap_team))
		fields_mid$odmap_min[index] <- odmap_mins[i]
	}

	fields_late[ , odmap_min := NA_real_]
	for (i in seq_along(odmap_mins)) {
		odmap_team <- names(odmap_mins)[i]
		odmap_team <- tolower(odmap_team)
		index <- which(grepl(tolower(fields_late$team), pattern = odmap_team))
		fields_late$odmap_min[index] <- odmap_mins[i]
	}

	results_present <- analyze_associations(fields = fields_present, period = 'present')
	results_mid <- analyze_associations(fields = fields_mid, period = 'mid')
	results_late <- analyze_associations(fields = fields_late, period = 'late')
	
	results <- rbind(
		results_present,
		results_mid,
		results_late
	)
	write.csv(results, paste0('./Analysis/', species_focal, ' Associations between Team Clusters and Workflow Attributes.csv'), row.names = FALSE)

say('####################################################################')
say('### illustrations of team-level clusters and workflow attributes ###')
say('####################################################################')

	### Display a PCA biplot (created above) for a single species for each time period in which teams are clustered by hierarchical partitioning of PC loadings. Each team is coded by a workflow attribute.

	# Adds one point per team with color-coding by attribute
	#
	# count_plot	Integer >= 1, counter for plot (to label it 'A', 'B', etc.)
	# y				Numeric or character vector with attributes to plot
	# y_type		'numeric', 'factor' (type of response in y)
	# period		'present', 'mid', 'late' (time period of predictions)
	# legend		title for legend
	# title			plot title
	# trans			transformation function (in ggplot2 format) for the response (e.g., 'trans10')
	# biplots		list with `biplot` and `scores` elements
	# fields		data.table with workflow attributes
	make_biplot_with_attributes <- function(count_plot, y, y_type, period, legend, title, trans, biplots, fields) {

		this_title <- paste0(LETTERS[count_plot], ') ', title)

		# match metric to teams
		scores <- biplots$present$scores
		scores_teams <- scores$team
		fields_teams <- fields$team
		if (anonymize) fields_teams <- anonymize_teams(fields_teams, species_focal = species_focal)
		names(y) <- fields_teams
		y <- y[match(scores_teams, fields_teams)]
		scores[ , y := y]

		# remove NAs
		scores <- scores[!is.na(y)]

		biplot <- biplots[[period]]$biplot

		y_range <- range(scores$PC2, na.rm = TRUE)
		y_expanded <- c(y_range[1] - 0.4 * diff(y_range), y_range[2])

		graph <- biplot +
			geom_point(
				data = scores,
				mapping = aes(x = PC1, y = PC2, fill = y),
				pch = 21, size = 2
			) +
			coord_cartesian(ylim = y_expanded) +
			theme(
				legend.position = c(0.98, 0.02),
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
			ggtitle(this_title)

		if (y_type == 'numeric' & !is.na(trans)) {
		
			graph <- graph +
				scale_fill_viridis(
					name = legend,
					option = 'magma',
					trans = trans
				)

		} else if (y_type == 'numeric' & is.na(trans)) {
		
			graph <- graph +
				scale_fill_viridis(
					name = legend,
					option = 'magma'
				)

		} else if (y_type == 'factor') {

			graph <- graph +		
				labs(
					fill = legend
				)
		
		}

		# if (length(unique(y)) > 4) graph <- guides(fill = guide_legend(ncol = 2))

		graph

	}

	fields <- load_fields(species_focal)
	biplots <- readRDS(paste0('./Analysis/', tolower(species_focal), '_pca_biplots_on_teams_with_groupings.rds'))

	fields_present <- fields_mid <- fields_late <- fields

	### add MEAN ODMAP score to fields
	odmap <- readRDS('./Analysis/Summary of Assessment of SDM Workflows by SDM Standards.rds')

	y <- odmap$means # reverse ranks so 4 = gold, 0 = deficient
	y <- y[grepl(y$species, pattern = species_focal)]
	criteria <- c(paste0(1, LETTERS[1:5]), paste0(2, LETTERS[1:3]), paste0(3, LETTERS[1:4]), paste0(4, LETTERS[1:3]))
	odmap_scores <- y[ , ..criteria]
	odmap_means <- rowMeans(odmap_scores)

	odmap_means <- 4 - odmap_means # reverse ranks so 4 = gold, 0 = deficient

	odmap_teams <- y$first_author
	odmap_teams[odmap_teams == 'JIMENEZ-VALVERDE'] <- 'Jiménez-Valverde'
	odmap_teams <- tolower(odmap_teams)
	names(odmap_means) <- odmap_teams

	teams <- fields_present$team
	names(odmap_means) <- teams[match(odmap_teams, tolower(teams))]

	fields_present$odmap_mean <- odmap_means[match(fields_present$team, names(odmap_means))]

	fields_mid[ , odmap_mean := NA_real_]
	for (i in seq_along(odmap_means)) {
		odmap_team <- names(odmap_means)[i]
		odmap_team <- tolower(odmap_team)
		index <- which(grepl(tolower(fields_mid$team), pattern = odmap_team))
		fields_mid$odmap_mean[index] <- odmap_means[i]
	}

	fields_late[ , odmap_mean := NA_real_]
	for (i in seq_along(odmap_means)) {
		odmap_team <- names(odmap_means)[i]
		odmap_team <- tolower(odmap_team)
		index <- which(grepl(tolower(fields_late$team), pattern = odmap_team))
		fields_late$odmap_mean[index] <- odmap_means[i]
	}

	### add MINIMUM of MEAN ODMAP score to fields
	y <- odmap$means # reverse ranks so 4 = gold, 0 = deficient
	y <- y[grepl(y$species, pattern = species_focal)]
	criteria <- c(paste0(1, LETTERS[1:5]), paste0(2, LETTERS[1:3]), paste0(3, LETTERS[1:4]), paste0(4, LETTERS[1:3]))
	odmap_scores <- y[ , ..criteria]
	odmap_mins <- apply(odmap_scores, 1, min)

	odmap_teams <- y$first_author
	odmap_teams[odmap_teams == 'JIMENEZ-VALVERDE'] <- 'Jiménez-Valverde'
	odmap_teams <- tolower(odmap_teams)
	names(odmap_mins) <- odmap_teams

	odmap_mins <- 4 - odmap_mins # reverse ranks so 4 = gold, 0 = deficient

	teams <- fields_present$team
	names(odmap_mins) <- teams[match(odmap_teams, tolower(teams))]

	fields_present$odmap_min <- odmap_mins[match(fields_present$team, names(odmap_mins))]

	fields_mid[ , odmap_min := NA_real_]
	for (i in seq_along(odmap_mins)) {
		odmap_team <- names(odmap_mins)[i]
		odmap_team <- tolower(odmap_team)
		index <- which(grepl(tolower(fields_mid$team), pattern = odmap_team))
		fields_mid$odmap_min[index] <- odmap_mins[i]
	}

	fields_late[ , odmap_min := NA_real_]
	for (i in seq_along(odmap_mins)) {
		odmap_team <- names(odmap_mins)[i]
		odmap_team <- tolower(odmap_team)
		index <- which(grepl(tolower(fields_late$team), pattern = odmap_team))
		fields_late$odmap_min[index] <- odmap_mins[i]
	}

	collection <- list()
	count_plot <- 0

	### ODMAP mean scores

		this_fields <- fields_present
		title <- 'Mean ODMAP rank'
		legend <- 'Rank'
		y_type <- 'numeric'
		trans <- NA
		period <- 'present'

		y <- this_fields$odmap_mean
		y <- 4 - y # reverse ranks so 4 = gold, 0 = deficient

		count_plot <- count_plot + 1

		collection[[count_plot]] <- make_biplot_with_attributes(count_plot = count_plot, y = y, y_type = y_type, period = period, legend = legend, title = title, trans = trans, biplots = biplots, fields = this_fields)

	### ODMAP minimum score across all criteria

		this_fields <- fields_present
		title <- 'Minimum ODMAP rank'
		legend <- 'Rank'
		y_type <- 'numeric'
		trans <- NA
		period <- 'present'

		y <- this_fields$odmap_min
		y <- 4 - y # reverse ranks so 4 = gold, 0 = deficient

		count_plot <- count_plot + 1

		collection[[count_plot]] <- make_biplot_with_attributes(count_plot = count_plot, y = y, y_type = y_type, period = period, legend = legend, title = title, trans = trans, biplots = biplots, fields = this_fields)

	### AUC

		this_fields <- fields_present
		title <- 'AUC'
		legend <- 'AUC'
		y_type <- 'numeric'
		trans <- NA
		period <- 'present'

		y <- as.numeric(fields$eval_metric_auc_roc_value)
		count_plot <- count_plot + 1

		collection[[count_plot]] <- make_biplot_with_attributes(count_plot = count_plot, y = y, y_type = y_type, period = period, legend = legend, title = title, trans = trans, biplots = biplots, fields = this_fields)
		
	### number of occurrences

		this_fields <- fields_present
		title <- 'Number of occurrences'
		legend <- 'Number'
		y_type <- 'numeric'
		trans <- 'log2'
		period <- 'present'

		y <- as.numeric(fields$num_occurrences_minimum)
		count_plot <- count_plot + 1

		collection[[count_plot]] <- make_biplot_with_attributes(count_plot = count_plot, y = y, y_type = y_type, period = period, legend = legend, title = title, trans = trans, biplots = biplots, fields = this_fields)
		
	### thresholding

		this_fields <- fields_present
		title <- 'Thresholded vs. continuous'
		legend <- 'Type'
		y_type <- 'factor'
		trans <- NA
		period <- 'present'

		y1 <- as.numeric(this_fields$prediction_type_continuous)
		y2 <- as.numeric(this_fields$prediction_type_thresholded)

		y <- rep(NA, nrow(this_fields))
		for (i in seq_along(y)) {
			if (y1[i] == 1) {
				y[i] <- 'Continuous'
			} else if (y2[i] == 1) {
				y[i] <- 'Thresholded'
			}
		}

		count_plot <- count_plot + 1

		collection[[count_plot]] <- make_biplot_with_attributes(count_plot = count_plot, y = y, y_type = y_type, period = period, legend = legend, title = title, trans = trans, biplots = biplots, fields = this_fields)

	### collinearity

		this_fields <- fields_present
		title <- 'Collinearity managed'
		legend <- 'Explicitly\nmanaged'
		y_type <- 'factor'
		trans <- NA
		period <- 'present'

		field_names <- c('collinearity_pca', 'collinearity_correlation', 'collinearity_vif', 'collinearity_other_method')
		y <- this_fields[ , ..field_names]
		y <- y[ , lapply(.SD, as.numeric)]

		y <- rowSums(y)
		y <- as.character(y > 0)

		y[y == 'TRUE'] <- 'Yes'
		y[y == 'FALSE'] <- 'No'

		count_plot <- count_plot + 1

		collection[[count_plot]] <- make_biplot_with_attributes(count_plot = count_plot, y = y, y_type = y_type, period = period, legend = legend, title = title, trans = trans, biplots = biplots, fields = this_fields)

	### WorldClim vs CHELSA

		this_fields <- fields_present
		title <- 'Climate data source'
		legend <- 'Source'
		y_type <- 'factor'
		trans <- NA
		period <- 'present'

		y1 <- as.numeric(this_fields$predictors_climate_source_worldclim)
		y2 <- as.numeric(this_fields$predictors_climate_source_chelsa)

		y <- rep(NA, nrow(this_fields))
		for (i in seq_along(y)) {
			if (y1[i] == 1) {
				y[i] <- 'WorldClim'
			} else if (y2[i] == 1) {
				y[i] <- 'CHELSA'
			}
		}

		count_plot <- count_plot + 1

		collection[[count_plot]] <- make_biplot_with_attributes(count_plot = count_plot, y = y, y_type = y_type, period = period, legend = legend, title = title, trans = trans, biplots = biplots, fields = this_fields)

	### number of predictors

		this_fields <- fields_present
		title <- 'Predictors'
		legend <- 'Number'
		y_type <- 'numeric'
		trans <- 'log2'
		period <- 'present'

		y <- as.numeric(this_fields$predictors_climate_nonclimate_num_total)

		count_plot <- count_plot + 1

		collection[[count_plot]] <- make_biplot_with_attributes(count_plot = count_plot, y = y, y_type = y_type, period = period, legend = legend, title = title, trans = trans, biplots = biplots, fields = this_fields)

	### number of climate predictors

		this_fields <- fields_present
		title <- 'Climate predictors'
		legend <- 'Number'
		y_type <- 'numeric'
		trans <- 'log2'
		period <- 'present'

		y <- as.numeric(this_fields$predictors_climate_num_total)

		count_plot <- count_plot + 1

		collection[[count_plot]] <- make_biplot_with_attributes(count_plot = count_plot, y = y, y_type = y_type, period = period, legend = legend, title = title, trans = trans, biplots = biplots, fields = this_fields)

	### number of non-climate predictors

		this_fields <- fields_present
		title <- 'Non-climate predictors'
		legend <- 'Number'
		y_type <- 'numeric'
		trans <- 'log2'
		period <- 'present'

		y <- as.numeric(this_fields$predictors_climate_num_total)

		count_plot <- count_plot + 1

		collection[[count_plot]] <- make_biplot_with_attributes(count_plot = count_plot, y = y, y_type = y_type, period = period, legend = legend, title = title, trans = trans, biplots = biplots, fields = this_fields)

	### modeling software

		this_fields <- fields_present
		title <- 'Modeling software'
		legend <- 'Software'
		y_type <- 'factor'
		trans <- NA
		period <- 'present'

		enmeval <- as.numeric(this_fields$modeling_software_enmeval)
		enmtools <- as.numeric(this_fields$modeling_software_enmtools)
		wallace <- as.numeric(this_fields$modeling_software_wallace)
		biomod2 <- as.numeric(this_fields$modeling_software_biomod2)
		sabinansdm <- as.numeric(this_fields$modeling_software_sabinansdm)
		sdm <- as.numeric(this_fields$modeling_software_sdm)
		miamaxent <- as.numeric(this_fields$modeling_software_miamaxent)
		enmSdmX <- as.numeric(this_fields$modeling_software_enmsdmx)
		flexsdm <- as.numeric(this_fields$modeling_software_flexsdm)
		sdmtune <- as.numeric(this_fields$modeling_software_sdmtune)
		piecemeal <- as.numeric(this_fields$modeling_software_piecemeal)

		# only doing software used by 5 or more teams
		y <- rep(NA_character_, nrow(fields))
		for (i in seq_along(y)) {
		
			if (enmeval[i] == 1 | wallace[i] == 1) {
				y[i] <- 'ENMeval/Wallace'
			} else if (biomod2[i] == 1) {
				y[i] <- 'BIOMOD2'
			# } else if (piecemeal[i]  == 1) {
				# y[i] <- 'Various\npackages'
			} else {
				y[i] <- 'Other'
			}

		}

		count_plot <- count_plot + 1

		collection[[count_plot]] <- make_biplot_with_attributes(count_plot = count_plot, y = y, y_type = y_type, period = period, legend = legend, title = title, trans = trans, biplots = biplots, fields = this_fields)

	### ensemble model

		this_fields <- fields_present
		title <- 'Ensemble model'
		legend <- 'Ensemble'
		y_type <- 'factor'
		trans <- NA
		period <- 'present'

		y <- as.character(this_fields$algo_ensemble)
		y[y == '1'] <- 'Yes'
		y[y == '0'] <- 'No'

		count_plot <- count_plot + 1

		collection[[count_plot]] <- make_biplot_with_attributes(count_plot = count_plot, y = y, y_type = y_type, period = period, legend = legend, title = title, trans = trans, biplots = biplots, fields = this_fields)

	### ensemble model: number of models

		this_fields <- fields_present
		title <- 'Ensemble model: Number of models'
		legend <- 'Models'
		y_type <- 'numeric'
		trans <- NA
		period <- 'present'

		y <- as.numeric(this_fields$algo_ensemble_number_of_models)

		count_plot <- count_plot + 1

		collection[[count_plot]] <- make_biplot_with_attributes(count_plot = count_plot, y = y, y_type = y_type, period = period, legend = legend, title = title, trans = trans, biplots = biplots, fields = this_fields)

	### used MaxEnt/MaxNet

		this_fields <- fields_present
		title <- 'MaxEnt/MaxNet (not ensemble)'
		legend <- 'MaxEnt/Net'
		y_type <- 'factor'
		trans <- NA
		period <- 'present'

		y <- rep('Other algorithm(s)', nrow(fields))
		y[as.character(fields$algo_maxent) == '1'] <- 'MaxEnt/Net'
		y[as.character(fields$algo_maxnet) == '1'] <- 'MaxEnt/Net'

		count_plot <- count_plot + 1

		collection[[count_plot]] <- make_biplot_with_attributes(count_plot = count_plot, y = y, y_type = y_type, period = period, legend = legend, title = title, trans = trans, biplots = biplots, fields = this_fields)

	### bias correction

		this_fields <- fields_present
		title <- 'Bias correction'
		legend <- 'Method'
		y_type <- 'factor'
		trans <- NA
		period <- 'present'

		spatial <- as.numeric(this_fields$bias_correction_spatial_thinning)
		env <- as.numeric(this_fields$bias_correction_environmental_thinning)
		target <- as.numeric(this_fields$bias_correction_target_background)
		nonrand <- as.numeric(this_fields$bias_correction_nonrandom_background)
		none <- as.numeric(this_fields$bias_correction_none)
		unclear <- as.numeric(this_fields$bias_correction_unclear_no_response)
		
		y <- rep(NA_character_, nrow(fields))
		y[spatial == 1] <- 'Spatial thin.'
		y[env == 1] <- 'Env thin.'
		y[target == 1] <- 'Target BG'
		y[nonrand == 1] <- 'Non-random BG'
		y[none == 1] <- 'None (explicit)'
		y[unclear == 1] <- 'U/NR'

		count_plot <- count_plot + 1

		collection[[count_plot]] <- make_biplot_with_attributes(count_plot = count_plot, y = y, y_type = y_type, period = period, legend = legend, title = title, trans = trans, biplots = biplots, fields = this_fields)

	### non-presence types

		this_fields <- fields_present
		title <- 'Type of non-presences'
		legend <- 'Type'
		y_type <- 'factor'
		trans <- NA
		period <- 'present'

		y <- rep(NA, nrow(this_fields))
		y[as.numeric(this_fields$nonpres_type_background) == 1] <- 'Background'
		y[as.numeric(this_fields$nonpres_type_pseudoabsence) == 1] <- 'Pseudoabs.'
		y[as.numeric(this_fields$nonpres_type_target_background) == 1] <- 'Target'
		y[as.numeric(this_fields$nonpres_type_unclear_no_response) == 1] <- 'Unknown'

		count_plot <- count_plot + 1

		collection[[count_plot]] <- make_biplot_with_attributes(count_plot = count_plot, y = y, y_type = y_type, period = period, legend = legend, title = title, trans = trans, biplots = biplots, fields = this_fields)

	### calibration area boundary

		this_fields <- fields_present
		title <- 'Calibration region definition'
		legend <- 'Type'
		y_type <- 'factor'
		trans <- NA
		period <- 'present'

		y <- rep(NA, nrow(this_fields))
		y[as.numeric(this_fields$boundary_rectangle) == 1] <- 'Bounding box'
		y[as.numeric(this_fields$boundary_natural) == 1] <- 'Ecoregions'
		y[as.numeric(this_fields$boundary_convex_hull) == 1] <- 'Convex hull'
		y[as.numeric(this_fields$boundary_range_map) == 1] <- 'Range map + buffer'
		y[as.numeric(this_fields$boundary_political) == 1] <- 'Political'
		y[as.numeric(this_fields$boundary_buffer_around_occurrences) == 1] <- 'Buffer a/r occs.'
		y[as.numeric(this_fields$boundary_unclear_not_reported) == 1] <- 'Unknown'

		count_plot <- count_plot + 1

		collection[[count_plot]] <- make_biplot_with_attributes(count_plot = count_plot, y = y, y_type = y_type, period = period, legend = legend, title = title, trans = trans, biplots = biplots, fields = this_fields)

	### calibration region extent

		this_fields <- fields_present
		title <- 'Extent of calibration region'
		legend <- 'Extent (km²)'
		y_type <- 'numeric'
		trans <- 'log10'
		period <- 'present'

		y <- as.numeric(this_fields$extent_calibration_sans_water_km2)

		count_plot <- count_plot + 1

		collection[[count_plot]] <- make_biplot_with_attributes(count_plot = count_plot, y = y, y_type = y_type, period = period, legend = legend, title = title, trans = trans, biplots = biplots, fields = this_fields)

	### spatial resolution

		this_fields <- fields_present
		title <- 'Spatial resolution'
# 		legend <- 'Cell size (km²)'
		legend <- 'Arcminutes'
# 		y_type <- 'factor'
		y_type <- 'numeric'
		trans <- 'log2'
		period <- 'present'

		# y <- as.numeric(this_fields$res_mean_cell_size_km2)
		y <- as.numeric(this_fields$res)

		count_plot <- count_plot + 1

		collection[[count_plot]] <- make_biplot_with_attributes(count_plot = count_plot, y = y, y_type = y_type, period = period, legend = legend, title = title, trans = trans, biplots = biplots, fields = this_fields)

	### occurrence data time period

		this_fields <- fields_present
		title <- 'Occurrence data time period'
		legend <- 'Years'
		y_type <- 'factor'
		trans <- NA
		period <- 'present'

		y <- this_fields$occurrence_data_time_period

		count_plot <- count_plot + 1

		collection[[count_plot]] <- make_biplot_with_attributes(count_plot = count_plot, y = y, y_type = y_type, period = period, legend = legend, title = title, trans = trans, biplots = biplots, fields = this_fields)

	### taxonomy

		if (species_focal == 'Priona') {

			this_fields <- fields_present
			title <- 'Modeled only mainland subspecies'
			legend <- 'Mainland\nonly'
			y_type <- 'factor'
			trans <- NA
			period <- 'present'

			y <- as.character(this_fields$taxonomy_mainland_only)
			y[y == '1'] <- 'Yes'
			y[y == '0'] <- 'No'

			count_plot <- count_plot + 1

			collection[[count_plot]] <- make_biplot_with_attributes(count_plot = count_plot, y = y, y_type = y_type, period = period, legend = legend, title = title, trans = trans, biplots = biplots, fields = this_fields)

		}

	collections <- plot_grid(plotlist = collection, ncol = 4, align = 'h')

	ggsave(collections, filename = paste0('./Analysis/', species_focal, ' PCA Clusters by Workflow Attributes.png'), width = 8.5, height = 11, dpi = 600, bg = 'white')

say('######################################')
say('### CART analysis of team clusters ###')
say('######################################')

	### COnstructs a classification and regression tree (CART) for the clusters of teams identified above for each time period, with workflow attributes used as predictors. Leave-one-out (LOO)

	# modeling decisions: fields to use in analysis
	fields <- load_fields(species_focal)

	attribs <- fields[ , 'team']

	### match workflow attributes to new columns

		# inserts the predictor variable into the "long" data.table
		match_y <- function(y, attribs, resp) {

			names(y) <- fields$team

			# # if (is.character(y)) y[is.na(y)] <- 'U/NR'

			y <- y[match(attribs$team, names(y))]
			attribs[ , DUMMY := y]
			if (is.character(y)) attribs[ , DUMMY := factor(DUMMY)]
			names(attribs)[names(attribs) == 'DUMMY'] <- resp
			attribs

		}

		# prediction type

			resp <- 'Prediction Type'

			y1 <- as.numeric(fields$prediction_type_continuous)
			y2 <- as.numeric(fields$prediction_type_thresholded)
			y <- rep(NA_character_, nrow(fields))
			y[y1 == 1] <- 'continuous'
			y[y2 == 1] <- 'thresholded'

			attribs <- match_y(y = y, attribs = attribs, resp = resp)

		# AUC

			resp <- 'AUC'

			y <- as.numeric(fields$eval_metric_auc_roc_value)
			attribs <- match_y(y = y, attribs = attribs, resp = resp)

		# extrapolation: managed

			resp <- 'Managed Extrapolation'

			field_names <- c('extrapolation_clamping_masking_clipping', 'extrapolation_exdet', 'extrapolation_mess', 'extrapolation_shape', 'extrapolation_area_of_applicability', 'extrapolation_response_curve_inspection', 'extrapolation_kissmig')

			y1 <- fields[ , ..field_names]
			y1 <- y1[ , lapply(.SD, as.numeric)]
			y1 <- rowSums(y1)
			y1 <- y1 > 0
			y <- rep(NA_character_, nrow(fields))
			y[y1] <- 'managed'
			y[!y1] <- 'not managed'

			attribs <- match_y(y = y, attribs = attribs, resp = resp)

		### extrapolation: method

			resp <- 'Extrapolation Method'

			clamp <- as.numeric(fields$extrapolation_clamping_masking_clipping)
			exdet <- as.numeric(fields$extrapolation_exdet)
			mess <- as.numeric(fields$extrapolation_mess)
			shape <- as.numeric(fields$extrapolation_shape)
			aoa <- as.numeric(fields$extrapolation_area_of_applicability)
			resp_curves <- as.numeric(fields$extrapolation_response_curve_inspection)
			kissmig <- as.numeric(fields$extrapolation_kissmig)

			y <- rep(NA_character_, nrow(fields))
			y[clamp == 1] <- ' clamp/mask'
			y[exdet == 1] <- ' ExDet'
			y[mess == 1] <- ' MESS'
			y[shape == 1] <- ' Shape'
			y[aoa == 1] <- ' Area of Applic.'
			y[resp_curves == 1] <- ' response curves'
			y[kissmig == 1] <- ' kissmig'

			attribs <- match_y(y = y, attribs = attribs, resp = resp)

		# number of occurrences

			resp <- 'Number of Occurrences'

			y <- as.numeric(fields$num_occurrences_minimum)

			attribs <- match_y(y = y, attribs = attribs, resp = resp)

		### collinearity: managed

			resp <- 'Collinearity Managed'

			field_names <- c('collinearity_pca', 'collinearity_correlation', 'collinearity_vif', 'collinearity_other_method')

			y1 <- fields[ , ..field_names]
			y1 <- y1[ , lapply(.SD, as.numeric)]
			y1 <- rowSums(y1)
			y1 <- y1 > 0
			y <- rep(NA_character_, nrow(fields))
			y[y1] <- 'yes'
			y[!y1] <- 'no'

			attribs <- match_y(y = y, attribs = attribs, resp = resp)

		### collinearity: method

			resp <- 'Collinearity Method'

			pca <- as.numeric(fields$collinearity_pca)
			correl <- as.numeric(fields$collinearity_correlation)
			vif <- as.numeric(fields$collinearity_vif)
			other_method <- as.numeric(fields$collinearity_other_method)

			y <- rep(NA_character_, nrow(fields))
			y[pca == 1] <- ' PCA'
			y[correl == 1] <- ' correlation'
			y[vif == 1] <- ' VIF'
			y[other_method == 1] <- ' other method'

			attribs <- match_y(y = y, attribs = attribs, resp = resp)

		# number of climate predictors

			resp <- 'Number of Climate Predictors'

			y <- as.numeric(fields$predictors_climate_num_total)

			attribs <- match_y(y = y, attribs = attribs, resp = resp)

		# number of non-climate predictors

			resp <- 'Number of Non-climate Predictors'

			y <- as.numeric(fields$predictors_nonclimate_num)

			attribs <- match_y(y = y, attribs = attribs, resp = resp)

		# total number of predictors

			resp <- 'Number of Predictors'

			y <- as.numeric(fields$predictors_climate_nonclimate_num_total)

			attribs <- match_y(y = y, attribs = attribs, resp = resp)

		# climate data source

			resp <- 'Climate Data Source'

			y1 <- as.numeric(fields$predictors_climate_source_worldclim)
			y2 <- as.numeric(fields$predictors_climate_source_chelsa)
			y <- rep(NA_character_, nrow(fields))
			y[y1 == 1] <- 'WorldClim'
			y[y2 == 1] <- 'CHELSA'

			names(y) <- fields$team
			attribs <- match_y(y = y, attribs = attribs, resp = resp)

		# modeling software: ENMeval

			resp <- 'Used ENMeval/Wallace'
			
			enmeval <- as.numeric(fields$modeling_software_enmeval)
			wallace <- as.numeric(fields$modeling_software_wallace)

			y <- rep(NA_character_, nrow(fields))
			y[enmeval == 1 | wallace == 1] <- 'yes'
			y[enmeval == 0 & wallace == 0] <- 'no'

			names(y) <- fields$team
			attribs <- match_y(y = y, attribs = attribs, resp = resp)

		# modeling software: BIOMOD2

			resp <- 'Used BIOMOD2'
			
			y1 <- as.numeric(fields$modeling_software_biomod2)
			y <- rep(NA_character_, nrow(fields))
			y[y1 == 1] <- 'yes'
			y[y1 == 0] <- 'no'
			
			names(y) <- fields$team
			attribs <- match_y(y = y, attribs = attribs, resp = resp)

		# model ensemble

			resp <- 'Ensemble SDM'

			y1 <- as.numeric(fields$algo_ensemble)
			y <- rep(NA_character_, nrow(fields))
			y[y1 == 1] <- 'yes'
			y[y1 == 0] <- 'no'

			names(y) <- fields$team
			attribs <- match_y(y = y, attribs = attribs, resp = resp)

		# number of models in ensemble

			resp <- 'Number of Models in Ensemble SDM'

			y <- as.numeric(fields$algo_ensemble_number_of_models)

			names(y) <- fields$team
			attribs <- match_y(y = y, attribs = attribs, resp = resp)

		# used MaxEnt/MaxNet

			resp <- 'Used MaxEnt/MaxNet'
			y1 <- as.numeric(fields$algo_maxent)
			y2 <- as.numeric(fields$algo_maxnet)
			
			y <- rep(NA_character_, nrow(fields))
			y[y1 == 1 | y2 == 1] <- 'yes'
			y[y1 == 0 & y2 == 0] <- 'no'

			names(y) <- fields$team
			attribs <- match_y(y = y, attribs = attribs, resp = resp)

		# bias correction: method

			resp <- 'Bias Correction Method'

			spatial <- as.numeric(fields$bias_correction_spatial_thinning)
			env <- as.numeric(fields$bias_correction_environmental_thinning)
			target <- as.numeric(fields$bias_correction_target_background)
			nonrand <- as.numeric(fields$bias_correction_nonrandom_background)
			none <- as.numeric(fields$bias_correction_none)
			unclear <- as.numeric(fields$bias_correction_unclear_no_response)

			y <- rep(NA_character_, nrow(fields))
			y[spatial == 1] <- ' spatial thin.'
			y[env == 1] <- ' env. thin.'
			y[target == 1] <- ' target BG'
			y[nonrand == 1] <- ' non-random BG'
			y[none == 1] <- ' explicitly none'
			# y[unclear == 1] <- 'Unclear/no response'

			names(y) <- fields$team
			attribs <- match_y(y = y, attribs = attribs, resp = resp)

		## bias: managed

			resp <- 'Bias Correction'

			field_names <- c('bias_correction_spatial_thinning', 'bias_correction_environmental_thinning', 'bias_correction_target_background', 'bias_correction_nonrandom_background')
			
			y1 <- fields[ , ..field_names]
			y1 <- y1[ , lapply(.SD, as.numeric)]
			y1 <- rowSums(y1)
			y1 <- y1 > 0
			y <- rep(NA_character_, nrow(fields))
			y[y1] <- 'yes'
			y[!y1] <- 'no'

			names(y) <- fields$team
			attribs <- match_y(y = y, attribs = attribs, resp = resp)

		# type of non-presences

			resp <- 'Non-presence Type'

			y1 <- as.numeric(fields$nonpres_type_background)
			y2 <- as.numeric(fields$nonpres_type_pseudoabsence)
			y <- rep(NA_character_, nrow(fields))
			y[y1 == 1] <- 'background'
			y[y2 == 1] <- 'pseudoabsence'

			names(y) <- fields$team
			attribs <- match_y(y = y, attribs = attribs, resp = resp)
			
		# type of calibration region

			resp <- 'Calibration Region'

			rect <- as.numeric(fields$boundary_rectangle)
			ecoregions <- as.numeric(fields$boundary_natural)
			ch <- as.numeric(fields$boundary_convex_hull)
			range_map <- as.numeric(fields$boundary_range_map)
			political <- as.numeric(fields$boundary_political)
			buffer_occs <- as.numeric(fields$boundary_buffer_around_occurrences)
			unr <- as.numeric(fields$boundary_unclear_not_reported)

			y <- rep(NA, nrow(fields))
			y[rect == 1] <- ' bounding box'
			y[ecoregions == 1] <- ' ecoregions'
			y[ch == 1] <- ' convex hull'
			y[range_map == 1] <- ' range map + buffer'
			y[political == 1] <- ' political'
			y[buffer_occs == 1] <- ' buffer a/r occs.'
			# y[unr == 1] <- 'U/NR'

			names(y) <- fields$team
			attribs <- match_y(y = y, attribs = attribs, resp = resp)

		# calibration extent

			resp <- 'Calibration Region Extent (km²)'

			y <- fields$extent_calibration_sans_water_km2
			y <- as.numeric(y)
			
			names(y) <- fields$team
			attribs <- match_y(y = y, attribs = attribs, resp = resp)

		# spatial resolution

			resp <- 'Spatial Resolution'

# 			y <- fields$res_mean_cell_size_km2
			y <- as.numeric(fields$res)
			
			names(y) <- fields$team
			attribs <- match_y(y = y, attribs = attribs, resp = resp)

		# occurrence data time period

			resp <- 'Occurrence Data Time Period'

			y <- as.numeric(fields$occurrence_data_time_period)
			
			names(y) <- fields$team
			attribs <- match_y(y = y, attribs = attribs, resp = resp)

		# sensitive to infra-species taxonomy
		
			if (species_focal == 'Priona') {

				resp <- 'Modeled Only Mainland Subspecies'

				y <- as.numeric(fields$taxonomy_mainland_only)
				
				names(y) <- fields$team
				attribs <- match_y(y = y, attribs = attribs, resp = resp)

			}

		## MEAN ODMAP score
		
			resp <- 'ODMAP Mean'

			odmap <- readRDS('./Analysis/Summary of Assessment of SDM Workflows by SDM Standards.rds')

			y <- odmap$means # reverse ranks so 4 = gold, 0 = deficient
			y <- y[grepl(y$species, pattern = species_focal)]
			criteria <- c(paste0(1, LETTERS[1:5]), paste0(2, LETTERS[1:3]), paste0(3, LETTERS[1:4]), paste0(4, LETTERS[1:3]))
			odmap_scores <- y[ , ..criteria]
			odmap_means <- rowMeans(odmap_scores)

			odmap_means <- 4 - odmap_means # reverse ranking so 4 = gold, 0 = deficient

			odmap_teams <- y$first_author
			odmap_teams[odmap_teams == 'JIMENEZ-VALVERDE'] <- 'Jiménez-Valverde'
			odmap_teams <- tolower(odmap_teams)
			names(odmap_means) <- odmap_teams

			teams <- attribs$team
			names(odmap_means) <- teams[match(odmap_teams, tolower(teams))]

			y <- odmap_means[match(attribs$team, names(odmap_means))]
			attribs <- match_y(y = y, attribs = attribs, resp = resp)

		# MINIMUM of MEAN ODMAP score

			resp <- 'ODMAP Minimum'

			y <- odmap$means # reverse ranks so 4 = gold, 0 = deficient
			y <- y[grepl(y$species, pattern = species_focal)]
			criteria <- c(paste0(1, LETTERS[1:5]), paste0(2, LETTERS[1:3]), paste0(3, LETTERS[1:4]), paste0(4, LETTERS[1:3]))
			odmap_scores <- y[ , ..criteria]
			odmap_mins <- apply(odmap_scores, 1, min)

			odmap_mins <- 4 - odmap_mins # reverse ranking so 4 = gold, 0 = deficient

			odmap_teams <- y$first_author
			odmap_teams[odmap_teams == 'JIMENEZ-VALVERDE'] <- 'Jiménez-Valverde'
			odmap_teams <- tolower(odmap_teams)
			names(odmap_mins) <- odmap_teams

			teams <- attribs$team
			names(odmap_mins) <- teams[match(odmap_teams, tolower(teams))]

			y <- odmap_mins[match(attribs$team, names(odmap_mins))]
			attribs <- match_y(y = y, attribs = attribs, resp = resp)

		# PCA cluster

			resp <- 'cluster'

			y <- readRDS(paste0('./Analysis/', tolower(species_focal), '_clusters_of_teams_present.rds'))
			attribs <- match_y(y = y, attribs = attribs, resp = resp)
			attribs[ , cluster := factor(cluster)]

		### futures

			# construct future attribs... insert new rows for each duplicate
			for (period in c('mid', 'late')) {

				attribs_fut <- data.table()
				clusters <- readRDS(paste0('./Analysis/', tolower(species_focal), '_clusters_of_teams_', period, '.rds'))
				versions <- fread(paste0('./Analysis/team_versions_', period, '.csv'))

				for (i in 1:nrow(attribs)) {

					base <- attribs$team[i]
					index <- versions$base == base

					if (any(index)) {

						version <- versions[index]

						# just one future version
						index <- attribs$team == base
						attribs_this <- attribs[index]

						if (nrow(version) > 1) {

							n_versions <- nrow(version)
							attribs_this <- attribs_this[rep(1, n_versions)]
							attribs_this[ , team := version$version]
						
						}

						# add GCM, emisions, scenario, etc.
						attribs_this[ , c('CMIP', 'GCM', 'Emissions Scenario', 'Number of GCMs in Climate Ensemble') := NA_character_]

						index <- match(version$version, attribs_this$team)
						
						y <- version$cmip[index]
						y <- paste0(' ', y)
						attribs_this$CMIP <- y

						y <- version$gcm[index]
						y <- paste0(' ', y)
						attribs_this$GCM <- y

						y <- version$scenario[index]
						y <- paste0(' ', y)
						attribs_this$`Emissions Scenario` <- y

						y <- version$num_gcms_in_ensemble[index]
						attribs_this$`Number of GCMs in Climate Ensemble` <- y

						# re-assign cluster number (existing value is from present time period)
						index <- match(version$version, names(clusters))
						y <- clusters[index]
						attribs_this$cluster <- y

						attribs_fut <- rbind(attribs_fut, attribs_this)

					}

					if (period == 'mid') {
						attribs_mid <- attribs_fut
					} else if (period == 'late') {
						attribs_late <- attribs_fut
					}

				} # if team sent a raster for this time period

			} # next future period

			attribs_mid[ , cluster := factor(cluster)]
			attribs_late[ , cluster := factor(cluster)]

	### tune and run CARTs
	######################

	if (species_focal == 'Priona') {

		form_present <- cluster ~ 
			`Prediction Type` +  
			`Number of Occurrences` +  
			`Collinearity Managed` +  
			`Collinearity Method` +  
			`Number of Climate Predictors` +  
			`Number of Non-climate Predictors` +  
			`Number of Predictors` +  
			`Climate Data Source` +  
			`Used ENMeval/Wallace` +  
			`Used BIOMOD2` +  
			`Ensemble SDM` +  
			`Number of Models in Ensemble SDM` +  
			`Used MaxEnt/MaxNet` +  
			`Bias Correction Method` +  
			`Bias Correction` +  
			`Non-presence Type` +  
			`Calibration Region` +  
			`Calibration Region Extent (km²)` +  
			# `Spatial Resolution (km²)`+
			`Spatial Resolution (arcmin)`+
			`Modeled Only Mainland Subspecies` +
			`ODMAP Mean` +
			`ODMAP Minimum`

		form_fut <- cluster ~ 
			`Prediction Type` +  
			`Number of Occurrences` +  
			`Collinearity Managed` +  
			`Collinearity Method` +  
			`Number of Climate Predictors` +  
			`Number of Non-climate Predictors` +  
			`Number of Predictors` +  
			`Climate Data Source` +  
			`Used ENMeval/Wallace` +  
			`Used BIOMOD2` +  
			`Ensemble SDM` +  
			`Number of Models in Ensemble SDM` +  
			`Used MaxEnt/MaxNet` +  
			`Bias Correction Method` +  
			`Bias Correction` +  
			`Non-presence Type` +  
			`Calibration Region` +  
			`Calibration Region Extent (km²)` +  
			# `Spatial Resolution (km²)`+
			`Spatial Resolution (arcmin)`+
			`Modeled Only Mainland Subspecies` +
			`ODMAP Mean` +
			`ODMAP Minimum` +
			`Managed Extrapolation` +  
			`Extrapolation Method` +  
			`CMIP` +
			`GCM` +
			`Emissions Scenario` +
			`Number of GCMs in Climate Ensemble`

	} else if (species_focal == 'Zamia') {

		form_present <- cluster ~ 
			`Prediction Type` +  
			`Number of Occurrences` +  
			`Collinearity Managed` +  
			`Collinearity Method` +  
			`Number of Climate Predictors` +  
			`Number of Non-climate Predictors` +  
			`Number of Predictors` +  
			`Climate Data Source` +  
			`Used ENMeval/Wallace` +  
			`Used BIOMOD2` +  
			`Ensemble SDM` +  
			`Number of Models in Ensemble SDM` +  
			`Used MaxEnt/MaxNet` +  
			`Bias Correction Method` +  
			`Bias Correction` +  
			`Non-presence Type` +  
			`Calibration Region` +  
			`Calibration Region Extent (km²)` +  
			# `Spatial Resolution (km²)`+
			`Spatial Resolution (arcmin)`+
			`ODMAP Mean` +
			`ODMAP Minimum`

		form_fut <- cluster ~ 
			`Prediction Type` +  
			`Number of Occurrences` +  
			`Collinearity Managed` +  
			`Collinearity Method` +  
			`Number of Climate Predictors` +  
			`Number of Non-climate Predictors` +  
			`Number of Predictors` +  
			`Climate Data Source` +  
			`Used ENMeval/Wallace` +  
			`Used BIOMOD2` +  
			`Ensemble SDM` +  
			`Number of Models in Ensemble SDM` +  
			`Used MaxEnt/MaxNet` +  
			`Bias Correction Method` +  
			`Bias Correction` +  
			`Non-presence Type` +  
			`Calibration Region` +  
			`Calibration Region Extent (km²)` +  
			# `Spatial Resolution (km²)`+
			`Spatial Resolution (arcmin)`+
			`ODMAP Mean` +
			`ODMAP Minimum` +
			`Managed Extrapolation` +  
			`Extrapolation Method` +  
			`CMIP` +
			`GCM` +
			`Emissions Scenario` +
			`Number of GCMs in Climate Ensemble`

	}

	### LOO cross-validation to select best CART parameters
	#######################################################

	### present

		for (period in c('present', 'mid', 'late')) {

			say('Tuning model for ', period, '...')

			if (period == 'present') {
				form <- form_present # which formula to use
				data <- attribs
			} else if (period == 'mid') {
				form <- form_fut # which formula to use
				data <- attribs_mid		
			} else if (period == 'late') {
				form <- form_fut # which formula to use
				data <- attribs_late		
			}

			n_teams <- nrow(data)
			tune <- expand.grid(cp = seq(0.001, 0.1, by = 0.01), minsplit = seq_len(n_teams / 2))
			tune <- as.data.table(tune)
			tune[ , accuracy := NA_real_]

			for (i in 1:nrow(tune)) {
			
				cp <- tune$cp[i]
				ms <- tune$minsplit[i]

				accuracies <- rep(NA_real_, n_teams)
				for (count_team in 1:n_teams) {

					attributes_train <- data[-count_team]
					attributes_test <- data[count_team]

					# all
					model <- rpart(form, data = attributes_train, method = 'class', control = rpart.control(cp = cp, minsplit = ms))
			
					pred <- tryCatch(predict(model, attributes_test, type = 'class'), error = function(x) FALSE)
					
					if (!is.logical(pred)) {
						
						conf_mat <- table(True = attributes_test$cluster, Predicted = pred)
						accuracy <- sum(diag(conf_mat)) / sum(conf_mat)

					
						accuracies[count_team] <- accuracy
					}

				}

				tune$accuracy[i] <- mean(accuracies, na.rm = TRUE)

			}

			max_val <- max(tune$accuracy)
			maxs <- tune[which(tune$accuracy == max_val)]

			cp_opt <- mean(maxs$cp)
			ms_opt <- median(maxs$minsplit)

			model <- rpart(form, data = data, method = 'class', control = rpart.control(cp = cp_opt, minsplit = ms_opt))

			assign(paste0('model_', period), model)

		} # next period

	# within-sample accuracy
	sink('./Analysis/CART Analysis of Team Clusters.txt')
	say('CART ANALYSIS')
	say(date(), post = 2)

		say('Accuracy for present-day model:', level = 2)
		pred <- predict(model_present, attribs, type = 'class')
		conf_mat <- table(True = attribs$cluster, Predicted = pred)
		accuracy <- sum(diag(conf_mat)) / sum(conf_mat)
		print(conf_mat)
		say('Overall accuracy:', round(accuracy, 3))

		say('Accuracy for mid-20th century model:', level = 2)
		pred <- predict(model_mid, attribs_mid, type = 'class')
		conf_mat <- table(True = attribs_mid$cluster, Predicted = pred)
		accuracy <- sum(diag(conf_mat)) / sum(conf_mat)
		print(conf_mat)
		say('Overall accuracy:', round(accuracy, 3))

		say('Accuracy for late 20th century model:', level = 2)
		pred <- predict(model_late, attribs_late, type = 'class')
		conf_mat <- table(True = attribs_late$cluster, Predicted = pred)
		accuracy <- sum(diag(conf_mat)) / sum(conf_mat)
		print(conf_mat)
		say('Overall accuracy:', round(accuracy, 3))

	sink()

	cart_cluster_cols <- list(cluster_cols_present[1:3])

	png(paste0('./Analysis/', species_focal, ' CART on Team Clusters - Present.png'), width = 1000, height = 1200, res = 600)

		rpart.plot(
			model_present,
			box.palette = cart_cluster_cols,
			tweak = 1,
			type = 5,
			extra = 101       # class, probability, and number of observations
		)

	dev.off()

	png(paste0('./Analysis/', species_focal, ' CART on Team Clusters - Mid-20th Century.png'), width = 1000, height = 1200, res = 600)

		rpart.plot(
			model_mid,
			box.palette = cart_cluster_cols,
			tweak = 1,
			type = 5,
			extra = 101       # class, probability, and number of observations
		)

	dev.off()

	png(paste0('./Analysis/', species_focal, ' CART on Team Clusters - Late 20th Century.png'), width = 1000, height = 1200, res = 600)

		rpart.plot(
			model_late,
			cex = 0.2,
			box.palette = cart_cluster_cols,
			tweak = 1,
			type = 5,
			extra = 101       # class, probability, and number of observations
		)

	dev.off()

say('DONE', level = 1)

