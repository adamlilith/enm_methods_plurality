### SDM METHODOLOGICAL PLURALITY
### <author names redacted for review>
### <author contact information redacted for review> | 2025-10
###
### This code anonymizes rasters.
###
### source('C:/Kaji/Research Group/ENMs - Plurality of Modeling Workflows/enm_methods_plurality/xx_anonymize.r')
###
### CONTENTS ###
### setup ###
### anonymize rasters ###
### anonymize ODMAP team names ###

#############
### setup ###
#############

	rm(list = ls())

	library(data.table)
	library(omnibus)
	library(terra)
	library(readxl)

	setwd('C:/Kaji/Research Group/ENMs - Plurality of Modeling Workflows')

#########################
### anonymize rasters ###
#########################

	### user-defined
	period <- 'present'
	# period <- 'mid'
	# period <- 'late'

	species <- 'cycad'
	# species <- 'cat'


	scores_file <- './Data/Model_choices_2025_12_30.xlsx'

	say(species, ' ', period, level = 2)
	say(scores_file, level = 2)

	rast_info <- read_xlsx(scores_file, sheet = paste0('Scoring by Raster ', toupper(species)))
	rast_info <- data.table(rast_info)
	rast_info <- rast_info[time_period == period]
	n <- nrow(rast_info)

	file <- paste0('./Outputs Shared NOT Anonymized/team_info_not_anonymized.csv')
	team_codes <- fread(file)

	period_nice <- if (period == 'present') {
		'Present'
	} else if (period == 'mid') {
	   'Mid-20th Century'
	} else {
		'Late 20th Century'
	}

	in_dir <- paste0('./Submissions/Rasters ', ifelse(species == 'cat', 'Prionailurus bengalensis', 'Zamia prasina'), ' ', period_nice, '/')
	out_dir <- paste0('./Submissions/Rasters ', ifelse(species == 'cat', 'Prionailurus bengalensis', 'Zamia prasina'), ' ', period_nice, ' Anonymized/')
	dirCreate(out_dir)

	for (i in 1:nrow(rast_info)) {
	
		team_code <- rast_info$team_code[i]
		rast_code <- rast_info$raster_code[i]
		rast_suffix <- rast_info$raster_name_suffix[i]
		team_proper <- team_codes$team[team_codes$code == team_code]
		team_proper <- sub(team_proper, pattern = '-', replacement = '_')
		team_proper <- sub(team_proper, pattern = 'é', replacement = 'e')

		say(team_proper, ' ', team_code, ' ', rast_code)

		if (rast_code == 'NA') rast_code <- NULL

		file <- paste0(in_dir, '/', team_proper)
		if (rast_suffix != 'NA') file <- paste0(file, '_', rast_suffix)
		file <- paste0(file, '.tif')

		r <- rast(file)
		if (period == 'present' & team_proper == 'Zurell') r <- r[['mean_prob']]
		name <- paste0(team_code, rast_code)
		names(r) <- name

		writeRaster(r, paste0(out_dir, name, '.tif'))
	
	}

# say('##################################')
# say('### anonymize ODMAP team names ###')
# say('##################################')

# 	odmap <- readRDS('./Analysis/Summary of Assessment of SDM Workflows by SDM Standards.rds')
# 	teams_codes <- fread('./Outputs Shared NOT Anonymized/team_info_not_anonymized.csv')

# 	code_teams <- teams_codes$team
# 	code_teams <- sub(code_teams, pattern = 'é', replacement = 'e')
# 	code_teams <- tolower(code_teams)

# 	for (i in seq_along(odmap)) {
	
# 		odmap_teams <- odmap[[i]]$first_author
# 		odmap_teams <- tolower(odmap_teams)

# 		matches <- match(odmap_teams, code_teams)
# 		odmap_code <- teams_codes$code[matches]

# 		odmap[[i]]$first_author <- odmap_code
# 		names(odmap[[i]])[names(odmap[[i]]) == 'first_author'] <- 'team_code'
	
# 	}

# 	saveRDS(odmap, './Outputs Shared/ODMAP Scoring Anonymized.rds')


