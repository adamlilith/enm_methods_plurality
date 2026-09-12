### SDM METHODOLOGICAL PLURALITY
### <author names redacted for review>
### <author contact information redacted for review> | 2025-10
###
### Make maps of each team's rasters for inspection.
###
### source('C:/Kaji/Research Group/ENMs - Plurality of Modeling Workflows/enm_methods_plurality/03_maps_of_rasters_for_inspection.r')
###
### CONTENTS ###
### setup ###
### make maps of all rasters for inspection ###

#############
### setup ###
#############

	rm(list = ls())
	source('C:/Kaji/Research Group/ENMs - Plurality of Modeling Workflows/enm_methods_plurality/00_shared_constants_and_functions.r')
	
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

say('DONE', level = 1)
