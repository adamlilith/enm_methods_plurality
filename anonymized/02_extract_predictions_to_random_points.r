### SDM METHODOLOGICAL PLURALITY
### <author names redacted for review>
### <author contact information redacted for review> | 2025-10
###
### Extract predictions from rasters to random points.
###
### source('C:/Kaji/Research Group/ENMs - Plurality of Modeling Workflows/enm_methods_plurality/02_extract_predictions_to_random_points.r')
###
### CONTENTS ###
### setup ###
### create random points from which to draw predictions across all team rasters ###
### make nice map of area in common between all rasters ###
### extract values for predictions at same sites across all rasters ###

#############
### setup ###
#############

	rm(list = ls())
	source('C:/Kaji/Research Group/ENMs - Plurality of Modeling Workflows/enm_methods_plurality/00_shared_constants_and_functions.r')

# say('###################################################################################')
# say('### create random points from which to draw predictions across all team rasters ###')
# say('###################################################################################')

# 	# Here we create a set of random points from which to draw predictions across all teams' rasters. Since each set of rasters has a different extent, we'll fine the smallest region that encompasses  all of the rasters, then within this extent place a large number of random sites. We do this by converting each raster to 1/NA values, converting to a polygon, then taking the intersection of the polygons across all species.

# 	# NB for Zamia, the region that encompasses non-NA cells across all rasters is exceedingly small. To ameliorate this, we'll take the liberty of replacing terrestrial NA cells with a value of 0. We will use the appropriate data set (CHELSA or WorldClim) and resolution to fill in terrestrial cells. We will not expand the extent of rasters.

# 	team_info <- load_team_fields(species_focal = species_focal)

# 	### get common extent
# 	# Convert each rasters to a polygon
# 	say('Collating projection extents...')
# 	polys <- list()
# 	periods <- c('present', 'mid', 'late')
# 	count_polys <- 1 # counter for list item storing polygons
# 	for (count_period in seq_along(periods)) {

# 		period <- periods[count_period]
# 		period_nice <- get_nice_period(period)

# 		files <- listFiles(paste0('./Submissions/Rasters ', species_full, ' ', period_nice, ' Anonymized'))

# 		for (count_file in seq_along(files)) {

# 			file <- files[count_file]
# 			r <- rast(file)
# 			name <- basename(file)
# 			say(name, ' ', period)

# 			# fill terrestrial NA cells with 0s
# 			if (species_focal == 'Zamia') {
# 				team_code <- sub(name, pattern = '.tif', replacement = '')
# 				team_code <- substr(team_code, 1, 1)
# 				r <- fill_terrestrial_NAs(r, team_code = team_code, team_info = team_info)
# 			}

# 			r[!is.na(r)] <- 1
# 			v <- as.polygons(r)
# 			polys[[count_polys]] <- v

# 			count_polys <- count_polys + 1

# 		} # next file

# 	} # next period

# 	# find area in common among all polygons
# 	say('Intersecting...')
# 	common <- polys[[1]]
# 	for (i in 2:length(polys)) {
	
# 		if (!same.crs(common, polys[[i]])) polys[[i]] <- project(polys[[i]], common)
# 		poly <- crop(polys[[i]], common)
# 		common <- intersect(common, poly)
	
# 	}

# 	common_buff <- buffer(common, 10000)
# 	common_buff <- aggregate(common_buff)
# 	n <- round(2 * n_rand_points_to_keep)
# 	rands <- spatSample(common_buff, n)

# 	writeVector(common, paste0('./Outputs ', species_full, '/common_study_region.gpkg'), overwrite = TRUE)
# 	writeVector(rands, paste0('./Outputs ', species_full, '/common_study_region_random_points.gpkg'), overwrite = TRUE)

# say('###########################################################')
# say('### make nice map of area in common between all rasters ###')
# say('###########################################################')

# 	world <- rnaturalearth::ne_countries(scale = 'medium', returnclass = 'sv')
# 	common <- vect(paste0('./Outputs ', species_full, '/common_study_region.gpkg'))

# 	extent <- ext(common)
# 	extent <- as.polygons(extent, crs = crs(common))
# 	buffer_size <- 110 * 0.025 * abs(xmax(common) - xmin(common)) # assuming ~110 km per degree
# 	extent <- buffer(extent, buffer_size)
# 	extent <- as.vector(ext(extent))

# 	map <- ggplot() +
# 		layer_spatial(world, fill = 'gray') +
# 		layer_spatial(common, fill = 'black', color = NA) +
# 		layer_spatial(world, fill = NA) +
# 		xlim(extent[1], extent[2]) + ylim(extent[3], extent[4]) +
# 		ggtitle('Area in Common between All Rasters')	

# 	ggsave(map, filename = paste0(out_dir, '/Map of Area in Common between All Rasters.png'), width = 10, height = 10, dpi = 600, bg = 'white')

# say('#######################################################################')
# say('### extract values for predictions at same sites across all rasters ###')
# say('#######################################################################')

# 	# This chunk extracts values from prediction rasters for the present and two future time periods for each team's rasters at a pre-defined set of random points. It also collates the minimum and maximum values of predictions across each raster for standardization of predictions in some subsequent analyses.

# 	team_info <- load_team_fields(species_focal = species_focal)

# 	### random coordinates across each study region
# 	coords_vect <- vect(paste0('./Outputs ', species_full, '/common_study_region_random_points.gpkg'))
# 	extracts <- crds(coords_vect)
# 	extracts <- as.data.table(extracts)

# 	periods <- c('present', 'mid', 'late')
# 	min_max <- data.table()
# 	for (period in periods) {

# 		period_nice <- get_nice_period(period)

# 		files <- listFiles(paste0('./Submissions/Rasters ', species_full, ' ', period_nice, ' Anonymized'))
# 		for (i in seq_along(files)) {

# 			file <- files[i]

# 			say(period, ' ', basename(file))

# 			# raster file name
# 			r <- rast(file)
# 			name <- basename(file)

# 			# fill terrestrial NA cells with 0s
# 			if (species_focal == 'Zamia') {
# 				team_code <- sub(name, pattern = '.tif', replacement = '')
# 				team_code <- substr(team_code, 1, 1)
# 				r <- fill_terrestrial_NAs(r, team_code = team_code, team_info = team_info)
# 			}

# 			# extract values to random points
# 			stats <- minmax(r)
# 			stats <- t(stats)
# 			stats <- as.data.table(stats)
# 			stats$raster <- names(r)
# 			stats$period  <- period

# 			# thresholded?
# 			unis <- unique(r)
# 			if (nrow(unis) == 2) {
# 				stats$binary_threshold <- TRUE
# 			} else {
# 				stats$binary_threshold <- FALSE
# 			}

# 			min_max <- rbind(min_max, stats)

# 			extraction <- extract(r, coords_vect, ID = FALSE)
# 			names(extraction) <- paste0(names(extraction), '_', period)
# 			extracts <- cbind(extracts, extraction)

# 		} # next file

# 	} # next period

# 	extracts <- extracts[complete.cases(extracts)]
# 	if (nrow(extracts) > n_rand_points_to_keep) {
# 		extracts <- extracts[sample(nrow(extracts), n_rand_points_to_keep)]
# 	} else {
# 		stop('Cannot ensure ', n_rand_points_to_keep, ' random points have predictions for all rasters.')
# 	}

# 	colnames(extracts)[1:2] <- c('longitude', 'latitude')

# 	saveRDS(extracts, paste0('./Outputs ', species_full, '/Extractions to Random Sites.rds'))
# 	saveRDS(min_max, paste0('./Outputs ', species_full, '/Extractions to Random Sites Minium & Maximum Values across Rasters.rds'))
	
say('DONE', level = 1)
