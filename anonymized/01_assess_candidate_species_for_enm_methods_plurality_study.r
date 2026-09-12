### ASSESSING CANDIDATE SPECIES FOR PLURALITY OF SDM WORKFLOWS PROJECT
### Adam B. Smith | Missouri Botanical Garden | adam.smith@mobot.org | 2023-10
###
### source('C:/Ecology/Drive/Research/ENMs - Plurality of Modeling Workflows/Code/Assess Candidate Species for Plurality of SDM Workflows.r')
### source('E:/Adam/Research/ENMs - Plurality of Modeling Workflows/Code/Assess Candidate Species for Plurality of SDM Workflows.r')
###
### CONTENTS ###
### setup ###
### make maps and tally occurrences of selected species: cycads of the world ###
### make maps and tally occurrences of selected species: selected cycads ###
### make maps and tally occurrences of selected species: mammals of Thailand ###
### make maps and tally occurrences of selected species: Nepenthes ###

#############
### setup ###
#############

rm(list = ls())

library(data.table)
library(enmSdmX)
library(geodata)
library(ggplot2)
library(ggspatial)
library(omnibus)
library(terra)

# drive <- 'C:/Ecology/Drive/'
drive <- 'E:/Adam/'

setwd(paste0(drive, '/Research/ENMs - Plurality of Modeling Workflows'))

################################################################################
### make maps and tally occurrences of selected species: cycads of the world ###
################################################################################

world <- vect('C:/Ecology/Drive/Research Data/GADM/Version 4.1/gadm_410.gpkg')
world <- aggregate(world, by = 'NAME_0')

occs <- fread('./Analysis/Candidate Species/All Cycads/0024866-231002084531237.csv')

occs$decimalLongitude <- as.numeric(occs$decimalLongitude)
occs$decimalLatitude <- as.numeric(occs$decimalLatitude)
occs$coordinateUncertaintyInMeters <- as.numeric(occs$coordinateUncertaintyInMeters)

speciesList <- sort(unique(occs$species))
speciesList <- speciesList[speciesList != '']

tallies <- data.table()
for (thisSpecies in speciesList) {

	say(thisSpecies)

	this_occs <- occs[species == thisSpecies]

	if (thisSpecies == 'Cycas micholitzii') this_occs <- this_occs[this_occs$country != 'US']
	
	this_coords <- this_occs[!is.na(decimalLongitude) & !is.na(decimalLatitude)]
	this_coord_uncer <- this_coords[!is.na(coordinateUncertaintyInMeters)]
	this_coord_uncer_lte5000m <- this_coord_uncer[coordinateUncertaintyInMeters <= 5000]
	this_coord_uncer_lte2500m <- this_coord_uncer[coordinateUncertaintyInMeters <= 2500]
	this_coord_uncer_lte1000m <- this_coord_uncer[coordinateUncertaintyInMeters <= 1000]

	n_occs <- nrow(this_occs)
	n_coords <- nrow(this_coords)
	n_coord_uncer <- nrow(this_coord_uncer)
	n_coord_uncer_lte5000m <- nrow(this_coord_uncer_lte5000m)
	n_coord_uncer_lte2500m <- nrow(this_coord_uncer_lte2500m)
	n_coord_uncer_lte1000m <- nrow(this_coord_uncer_lte1000m)

	tallies <- rbind(
		tallies,
		data.table(
			species = thisSpecies,
			n_occs = n_occs,
			n_coords = n_coords,
			n_coord_uncer = n_coord_uncer,
			n_coord_uncer_lte5000m = n_coord_uncer_lte5000m,
			n_coord_uncer_lte2500m = n_coord_uncer_lte2500m,
			n_coord_uncer_lte1000m = n_coord_uncer_lte1000m
		)
	)

	if (n_coords >= 30) {
		
		this_coords <- vect(this_coords, geom = c('decimalLongitude', 'decimalLatitude'), crs = getCRS('wgs84'))
		coordUncerLte5000m <- !is.na(this_coords$coordinateUncertaintyInMeters) & this_coords$coordinateUncertaintyInMeters <= 5000
		
		pch <- ifelse(coordUncerLte5000m, 21, 1)
		fill <- ifelse(coordUncerLte5000m, 'chartreuse', NA)
		
		stats <- paste0('N = ', n_occs, ' | N_coords = ', n_coords, ' | n_coord_uncer_lte5000m = ', n_coord_uncer_lte5000m, ' | n_coord_uncer_lte1000m = ', n_coord_uncer_lte1000m)
		
		bbox <- ext(this_coords)
		bbox <- bbox + 1
		
		this_world <- crop(world, bbox)

		map <- ggplot() +
			layer_spatial(this_world, fill = 'gray', color = 'gray30') +
			layer_spatial(this_coords, pch = pch, fill = fill, size = 2) +
			ggtitle(thisSpecies, sub = stats)
			
		ggsave(map, filename = paste0('./Analysis/Candidate Species/All Cycads/', thisSpecies, '.png'), width = 10, height = 10)
		
	}
		
}

############################################################################
### make maps and tally occurrences of selected species: selected cycads ###
############################################################################

world <- vect('C:/Ecology/Drive/Research Data/GADM/Version 4.1/gadm_410.gpkg')
world <- aggregate(world, by = 'NAME_0')

occs <- fread('./Analysis/Candidate Species/All Cycads/0024866-231002084531237.csv')

occs$decimalLongitude <- as.numeric(occs$decimalLongitude)
occs$decimalLatitude <- as.numeric(occs$decimalLatitude)
occs$coordinateUncertaintyInMeters <- as.numeric(occs$coordinateUncertaintyInMeters)

speciesList <- c('Ceratozamia miqueliana', 'Dioon spinulosum', 'Zamia prasina')

tallies <- data.table()
for (thisSpecies in speciesList) {

	say(thisSpecies)

	this_occs <- occs[species == thisSpecies]

	this_coords <- this_occs[!is.na(decimalLongitude) & !is.na(decimalLatitude)]

	if (thisSpecies == 'Ceratozamia miqueliana') {
		this_coords <- this_coords[this_coords$decimalLongitude < 0, ]
	} else if (thisSpecies == 'Dioon spinulosum') {
		this_coords <- this_coords[this_coords$decimalLongitude < 0, ]
	} else if (thisSpecies == 'Zamia prasina') {
		this_coords <- this_coords[this_coords$decimalLongitude < 0, ]
	}
	
	this_coord_uncer <- this_coords[!is.na(coordinateUncertaintyInMeters)]
	this_coord_uncer_lte5000m <- this_coord_uncer[coordinateUncertaintyInMeters <= 5000]
	this_coord_uncer_lte2500m <- this_coord_uncer[coordinateUncertaintyInMeters <= 2500]
	this_coord_uncer_lte1000m <- this_coord_uncer[coordinateUncertaintyInMeters <= 1000]

	n_occs <- nrow(this_occs)
	n_coords <- nrow(this_coords)
	n_coord_uncer <- nrow(this_coord_uncer)
	n_coord_uncer_lte5000m <- nrow(this_coord_uncer_lte5000m)
	n_coord_uncer_lte2500m <- nrow(this_coord_uncer_lte2500m)
	n_coord_uncer_lte1000m <- nrow(this_coord_uncer_lte1000m)

	tallies <- rbind(
		tallies,
		data.table(
			species = thisSpecies,
			n_occs = n_occs,
			n_coords = n_coords,
			n_coord_uncer = n_coord_uncer,
			n_coord_uncer_lte5000m = n_coord_uncer_lte5000m,
			n_coord_uncer_lte2500m = n_coord_uncer_lte2500m,
			n_coord_uncer_lte1000m = n_coord_uncer_lte1000m
		)
	)

	this_coords <- vect(this_coords, geom = c('decimalLongitude', 'decimalLatitude'), crs = getCRS('wgs84'))
	coordUncerLte5000m <- !is.na(this_coords$coordinateUncertaintyInMeters) & this_coords$coordinateUncertaintyInMeters <= 5000
	
	pch <- ifelse(coordUncerLte5000m, 21, 1)
	fill <- ifelse(coordUncerLte5000m, 'chartreuse', NA)
	
	stats <- paste0('N = ', n_occs, ' | N_coords = ', n_coords, ' | n_coord_uncer_lte5000m = ', n_coord_uncer_lte5000m, ' | n_coord_uncer_lte1000m = ', n_coord_uncer_lte1000m)
	
	bbox <- ext(this_coords)
	bbox <- bbox + 1
	
	this_world <- crop(world, bbox)

	map <- ggplot() +
		layer_spatial(this_world, fill = 'gray', color = 'gray30') +
		layer_spatial(this_coords, pch = pch, fill = fill, size = 2) +
		ggtitle(thisSpecies, sub = stats)
		
	ggsave(map, filename = paste0('./Analysis/Candidate Species/All Cycads/', thisSpecies, ' Rapid-Cleaned.png'), width = 10, height = 10)
		
}

################################################################################
### make maps and tally occurrences of selected species: mammals of Thailand ###
################################################################################

world <- vect('C:/Ecology/Drive/Research Data/GADM/Version 4.1/gadm_410.gpkg')
world <- aggregate(world, by = 'NAME_0')

occs <- fread('./Analysis/Candidate Species/Mammals of Thailand/occurrence.csv')

occs$decimalLongitude <- as.numeric(occs$decimalLongitude)
occs$decimalLatitude <- as.numeric(occs$decimalLatitude)
occs$coordinateUncertaintyInMeters <- as.numeric(occs$coordinateUncertaintyInMeters)

speciesList <- sort(unique(occs$species))
speciesList <- speciesList[speciesList != '']

tallies <- data.table()
for (thisSpecies in speciesList) {

	say(thisSpecies)

	this_occs <- occs[species == thisSpecies]

	this_coords <- this_occs[!is.na(decimalLongitude) & !is.na(decimalLatitude)]
	this_coord_uncer <- this_coords[!is.na(coordinateUncertaintyInMeters)]
	this_coord_uncer_lte5000m <- this_coord_uncer[coordinateUncertaintyInMeters <= 5000]
	this_coord_uncer_lte2500m <- this_coord_uncer[coordinateUncertaintyInMeters <= 2500]
	this_coord_uncer_lte1000m <- this_coord_uncer[coordinateUncertaintyInMeters <= 1000]

	n_occs <- nrow(this_occs)
	n_coords <- nrow(this_coords)
	n_coord_uncer <- nrow(this_coord_uncer)
	n_coord_uncer_lte5000m <- nrow(this_coord_uncer_lte5000m)
	n_coord_uncer_lte2500m <- nrow(this_coord_uncer_lte2500m)
	n_coord_uncer_lte1000m <- nrow(this_coord_uncer_lte1000m)

	tallies <- rbind(
		tallies,
		data.table(
			species = thisSpecies,
			n_occs = n_occs,
			n_coords = n_coords,
			n_coord_uncer = n_coord_uncer,
			n_coord_uncer_lte5000m = n_coord_uncer_lte5000m,
			n_coord_uncer_lte2500m = n_coord_uncer_lte2500m,
			n_coord_uncer_lte1000m = n_coord_uncer_lte1000m
		)
	)

	if (n_coords >= 30) {
		
		this_coords <- vect(this_coords, geom = c('decimalLongitude', 'decimalLatitude'), crs = getCRS('wgs84'))
		coordUncerLte5000m <- !is.na(this_coords$coordinateUncertaintyInMeters) & this_coords$coordinateUncertaintyInMeters <= 5000
		
		pch <- ifelse(coordUncerLte5000m, 21, 1)
		fill <- ifelse(coordUncerLte5000m, 'chartreuse', NA)
		
		stats <- paste0('N = ', n_occs, ' | N_coords = ', n_coords, ' | n_coord_uncer_lte5000m = ', n_coord_uncer_lte5000m, ' | n_coord_uncer_lte1000m = ', n_coord_uncer_lte1000m)
		
		bbox <- ext(this_coords)
		bbox <- bbox + 1
		
		this_world <- crop(world, bbox)

		map <- ggplot() +
			layer_spatial(this_world, fill = 'gray', color = 'gray30') +
			layer_spatial(this_coords, pch = pch, fill = fill, size = 2) +
			ggtitle(thisSpecies, sub = stats)
			
		ggsave(map, filename = paste0('./Analysis/Candidate Species/Mammals of Thailand/', thisSpecies, '.png'), width = 10, height = 10)
		
	}
		
}

######################################################################
### make maps and tally occurrences of selected species: Nepenthes ###
######################################################################

world <- vect(paste0(drive, '/Research Data/GADM/Version 4.1/gadm_410.gpkg'))
world <- aggregate(world, by = 'NAME_0')

occs <- fread('./Analysis/Candidate Species/Nepenthes/occurrence.csv')

occs$decimalLongitude <- as.numeric(occs$decimalLongitude)
occs$decimalLatitude <- as.numeric(occs$decimalLatitude)
occs$coordinateUncertaintyInMeters <- as.numeric(occs$coordinateUncertaintyInMeters)

speciesList <- sort(unique(occs$species))
speciesList <- speciesList[speciesList != '']

tallies <- data.table()
for (thisSpecies in speciesList) {

	say(thisSpecies)

	this_occs <- occs[species == thisSpecies]

	this_coords <- this_occs[!is.na(decimalLongitude) & !is.na(decimalLatitude)]
	this_coord_uncer <- this_coords[!is.na(coordinateUncertaintyInMeters)]
	this_coord_uncer_lte5000m <- this_coord_uncer[coordinateUncertaintyInMeters <= 5000]
	this_coord_uncer_lte2500m <- this_coord_uncer[coordinateUncertaintyInMeters <= 2500]
	this_coord_uncer_lte1000m <- this_coord_uncer[coordinateUncertaintyInMeters <= 1000]

	n_occs <- nrow(this_occs)
	n_coords <- nrow(this_coords)
	n_coord_uncer <- nrow(this_coord_uncer)
	n_coord_uncer_lte5000m <- nrow(this_coord_uncer_lte5000m)
	n_coord_uncer_lte2500m <- nrow(this_coord_uncer_lte2500m)
	n_coord_uncer_lte1000m <- nrow(this_coord_uncer_lte1000m)

	tallies <- rbind(
		tallies,
		data.table(
			species = thisSpecies,
			n_occs = n_occs,
			n_coords = n_coords,
			n_coord_uncer = n_coord_uncer,
			n_coord_uncer_lte5000m = n_coord_uncer_lte5000m,
			n_coord_uncer_lte2500m = n_coord_uncer_lte2500m,
			n_coord_uncer_lte1000m = n_coord_uncer_lte1000m
		)
	)

	# if (n_coord_uncer_lte1000m >= 30) {
	if (n_occs >= 30 & nrow(this_coords) > 0) {
		
		this_coords <- vect(this_coords, geom = c('decimalLongitude', 'decimalLatitude'), crs = getCRS('wgs84'))
		coordUncerLte5000m <- !is.na(this_coords$coordinateUncertaintyInMeters) & this_coords$coordinateUncertaintyInMeters <= 5000
		
		pch <- ifelse(coordUncerLte5000m, 21, 1)
		fill <- ifelse(coordUncerLte5000m, 'chartreuse', NA)
		
		stats <- paste0('N = ', n_occs, ' | N_coords = ', n_coords, ' | n_coord_uncer_lte5000m = ', n_coord_uncer_lte5000m, ' | n_coord_uncer_lte1000m = ', n_coord_uncer_lte1000m)
		
		bbox <- ext(this_coords)
		bbox <- bbox + 1
		
		this_world <- crop(world, bbox)

		map <- ggplot() +
			layer_spatial(this_world, fill = 'gray', color = 'gray30') +
			layer_spatial(this_coords, pch = pch, fill = fill, size = 2) +
			ggtitle(thisSpecies, sub = stats)
			
		ggsave(map, filename = paste0('./Analysis/Candidate Species/Nepenthes/!', thisSpecies, '.png'), width = 10, height = 10)
		
	}
		
}

say('DONE!', level = 3)
