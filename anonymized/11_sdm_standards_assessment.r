### ASSESSING CANDIDATE SPECIES FOR PLURALITY OF SDM WORKFLOWS PROJECT
### Adam B. Smith | Missouri Botanical Garden | adam.smith@mobot.org | 2023-10
###
### source('C:/Kaji/Research Group/ENMs - Plurality of Modeling Workflows/enm_methods_plurality/11_sdm_standards_assessment.r')
###
### CONTENTS ###
### setup ###
### assess consistency between raters of SDM standards ###
### calculate mean score for each study and ODMAP criterion across assessors ###
### plot of SDM standard scores by standard ###

#############
### setup ###
#############

	rm(list = ls())

	library(data.table)
	library(ggplot2)
	library(omnibus)
	library(readxl)

	drive <- 'C:/Kaji/'

	setwd(paste0(drive, '/Research Group/ENMs - Plurality of Modeling Workflows'))

say('##########################################################')
say('### assess consistency between raters of SDM standards ###')
say('##########################################################')

	### This chunk compiles two data tables, one for each species. Rows are studies, columns are ratings from the assessment of each study by SDM standards. The first few columns have information on the study and assessment (study author, who was assigned to rate it, and if they actually did rate it). The second set of columns have the ratings by each of the two raters, separated by a '/'. For example, a value could be "Deficient/Bronze", indicating that a particular study was rated as Deficient by rater #1 and Bronze by rater #2. If a rater did not rate a study, the value is represented by a "-". The third set of columns represents the completeness of each study's ODMAP sections. Values can be integers separated by "/". For example, "7/6" would mean rater #1 said 7 cells were filled, and rater #2 said 6 cells were filled. Non-responses is indicated by a "-".

	# assignments
	assigns <- read_excel('./Analysis/ODMAP Analysis/Assessment of SDM Workflows by SDM Standards - Assignments.xlsx', sheet = 'Sheet1', skip = 1)
	names(assigns) <- c('number', 'author', 'Adam', 'Anna', 'Nikki', 'Toni Lyn', 'Uzma', 'assessors')
	assigns <- as.data.frame(assigns)

	# ratings
	ratings <- read_excel('./Analysis/ODMAP Analysis/Assessment of SDM Workflows by SDM Standards (Responses) 2025-02-27.xlsx', sheet = 'Form Responses 1')
	ratings <- as.data.frame(ratings)
	ratings <- ratings[!grepl(ratings$`The Awesome Person Doing this Assessment!`, pattern = 'TEST'), ]

	authors <- sort(unique(ratings$`Study First Author`))

	# get shorthand codes for each question
	questions <- names(ratings)
	questions <- questions[grepl(questions, pattern = '\\{')]
	questions <- questions[!grepl(questions, pattern = 'CONFWHY')]
	questions <- questions[!grepl(questions, pattern = 'CONFIDENCE')]
	start <- regexpr(questions, pattern = '\\{')
	end <- regexpr(questions, pattern = '\\}')

	for (i in seq_along(questions)) {
		questions[i] <- substr(questions[i], start[i] + 1, end[i] - 1)
	}

	questions_responses <- questions[grepl(questions, pattern = ' RESPONSE')]
	questions_completeness <- questions[grepl(questions, pattern = ' COMPLETENESS')]

	# create data table for recording consistence of responses
	template <- data.frame(
		author = authors,
		rater1 = NA_character_,
		rater2 = NA_character_,
		rated_by_1 = NA,
		rated_by_2 = NA
	)
	rownames(template) <- authors
	
	for (i in seq_along(questions)) {
		under_question <- gsub(questions[i], pattern = ' ', replacement = '_')
		template <- cbind(template, data.frame(q = NA_character_))
		names(template)[ncol(template)] <- under_question
	}
	
	# note consistence of each study and who assessed it
	for (species in c('Prionailurus', 'Zamia')) {
		
		agreement <- template
		
		for (author in authors) {
		
			this_ratings <- ratings[grepl(ratings$Species, pattern = species) & grepl(ratings$`Study First Author`, pattern = author), ]
			if (nrow(this_ratings) > 0) {
			
				# who was assigned to rate this study?
				cond <- which(assigns$author == author)
				raters <- assigns[cond, ]
				raters <- raters[ , c('Adam', 'Anna', 'Nikki', 'Toni Lyn', 'Uzma')]
				raters <- unlist(raters)
				raters <- raters[!is.na(raters)]
				
				rater1 <- raters[1]
				rater2 <- raters[2]

				# who actually completed the rating?
				rated_by_1 <- raters[1] %in% this_ratings$`The Awesome Person Doing this Assessment!`
				rated_by_2 <- raters[2] %in% this_ratings$`The Awesome Person Doing this Assessment!`
			
				index <- which(agreement$author == author)
				agreement$rater1[index] <- rater1
				agreement$rater2[index] <- rater2
			
				agreement$rated_by_1[index] <- rated_by_1
				agreement$rated_by_2[index] <- rated_by_2
			
				# assess rating for each "RESPONSE" question (i.e., Deficient/Bronze/Silver/Gold)
				for (question in questions_responses) {
				
					# first rater
					row <- which(this_ratings$`The Awesome Person Doing this Assessment!` == rater1)
					col <- which(grepl(colnames(this_ratings), pattern = question))
					this_rating <- this_ratings[row, col]
					# this_rating <- if (is.null(this_rating) || length(this_rating) == 0) {
						# '-'
					# } else if (grepl(this_rating, pattern = 'Deficient')) {
						# 'Deficient'
					# } else if (grepl(this_rating, pattern = 'Bronze')) {
						# 'Bronze'
					# } else if (grepl(this_rating, pattern = 'Silver')) {
						# 'Silver'
					# } else if (grepl(this_rating, pattern = 'Gold')) {
						# 'Gold'
					# }
					this_rating <- if (is.null(this_rating) || length(this_rating) == 0) {
						NA
					} else if (grepl(this_rating, pattern = 'Deficient')) {
						0
					} else if (grepl(this_rating, pattern = 'Bronze')) {
						1
					} else if (grepl(this_rating, pattern = 'Silver')) {
						2
					} else if (grepl(this_rating, pattern = 'Gold')) {
						3
					}
					rating1 <- this_rating
					
					# second rater
					row <- which(this_ratings$`The Awesome Person Doing this Assessment!` == rater2)
					this_rating <- this_ratings[row, col]
					# this_rating <- if (is.null(this_rating) || length(this_rating) == 0) {
						# '-'
					# } else if (grepl(this_rating, pattern = 'Deficient')) {
						# 'Deficient'
					# } else if (grepl(this_rating, pattern = 'Bronze')) {
						# 'Bronze'
					# } else if (grepl(this_rating, pattern = 'Silver')) {
						# 'Silver'
					# } else if (grepl(this_rating, pattern = 'Gold')) {
						# 'Gold'
					# }
					this_rating <- if (is.null(this_rating) || length(this_rating) == 0) {
						NA
					} else if (grepl(this_rating, pattern = 'Deficient')) {
						0
					} else if (grepl(this_rating, pattern = 'Bronze')) {
						1
					} else if (grepl(this_rating, pattern = 'Silver')) {
						2
					} else if (grepl(this_rating, pattern = 'Gold')) {
						3
					}
					rating2 <- this_rating
					
					# rating <- paste0(rating1, '/', rating2)
					# rating <- ifelse(rating1 == rating2, '.', '!!!')
					rating <- abs(rating1 - rating2)
					
					question_under <- gsub(question, pattern = ' ', replacement = '_')
					cond <- which(names(agreement) == question_under)
					agreement[index, cond] <- rating
				
				} # next question
			
				# assess rating for each "COMPLETENESS" question (i.e., number of non-empty entries)
				for (question in questions_completeness) {
				
					# first rater
					row <- which(this_ratings$`The Awesome Person Doing this Assessment!` == rater1)
					col <- which(grepl(colnames(this_ratings), pattern = question))
					this_rating <- this_ratings[row, col]
					this_rating <- unlist(this_rating)
					this_rating <- if (is.null(this_rating) || length(this_rating) == 0) {
						'-'
					} else {
						unlist(this_rating)
					}
					rating1 <- this_rating
					
					# second rater
					row <- which(this_ratings$`The Awesome Person Doing this Assessment!` == rater2)
					this_rating <- this_ratings[row, col]
					this_rating <- if (is.null(this_rating) || length(this_rating) == 0) {
						'-'
					} else {
						unlist(this_rating)
					}
					rating2 <- this_rating
					
					rating <- paste0(rating1, ' vs ', rating2)
					
					question_under <- gsub(question, pattern = ' ', replacement = '_')
					cond <- which(names(agreement) == question_under)
					agreement[index, cond] <- rating
				
				} # next question
			
			} # if this study was rated at all
	
		} # next author
	
		write.csv(agreement, paste0('./Analysis/Assessment of SDM Workflows by SDM Standards - Agreements ', species, '.csv'))
	
	} # next species
	
say('################################################################################')
say('### calculate mean score for each study and ODMAP criterion across assessors ###')
say('################################################################################')
	
	# For each species and study, this chunk calculates the mean ODMAP score for each criterion.

	### INPUTS
	
	# criteria
	criteria <- read_excel('./Analysis/ODMAP Analysis/SDM Standards Metadata.xlsx', sheet = 'Sheet1')
	
	# results
	ratings <- read_excel('./Analysis/ODMAP Analysis/Assessment of SDM Workflows by SDM Standards (Responses) 2025-02-27.xlsx', sheet = 'Form Responses 1')
	ratings <- as.data.frame(ratings)
	ratings <- ratings[!grepl(ratings$`The Awesome Person Doing this Assessment!`, pattern = 'TEST'), ]
	
	criterion_codes <- sort(unique(criteria$criterion_code))
	criterion_codes <- criterion_codes[criterion_codes != 'NA']

	# get tags that flag each criterion
	tags <- names(ratings)
	responses <- tags[grepl(tags, pattern = 'RESPONSE}')]
	confidence <- tags[grepl(tags, pattern = 'CONFIDENCE}')]

	### output
	tallies <- data.table()
	for (rating in 1:nrow(ratings)) {
	
		species <- if (grepl(ratings$Species[rating], pattern = 'Prionailurus bengalensis')) {
			'Prionailurus bengalensis'
		} else if (grepl(ratings$Species[rating], pattern = 'Zamia prasina')) {
			'Zamia prasina'
		}
	
		this_tallies <- data.table(
			first_author = ratings$`Study First Author`[rating],
			species = species,
			rater = ratings$`The Awesome Person Doing this Assessment!`[rating]
		)
		
		for (criterion_code in criterion_codes) {
		
			column <- grepl(names(ratings), pattern = criterion_code) &
				grepl(names(ratings), pattern = 'PREDICTION') &
				grepl(names(ratings), pattern = 'RESPONSE')
			column <- which(column)
				
			score <- ratings[rating, column, drop = TRUE]
			score <- if (grepl(score, pattern = 'Deficient')) {
				4
			} else if (grepl(score, pattern = 'Bronze')) {
				3
			} else if (grepl(score, pattern = 'Silver')) {
				2
			} else if (grepl(score, pattern = 'Gold')) {
				1
			}
			
			this_tallies[ , (criterion_code) := score]
		
		}
		
		tallies <- rbind(tallies, this_tallies)
	
	} # next rating
	
	
	### calculate min/max/mean/range of scores
	mean_tallies <- min_tallies <- max_tallies <- range_tallies <- data.table()
	first_authors <- unique(tallies$first_author)
	species <- unique(tallies$species)
	for (this_first_author in first_authors) {
	
		for (this_species in species) {
		
			# min/mean/max/range of scores
			these <- tallies[first_author == this_first_author & species == this_species]
			means <- apply(these[ , ..criterion_codes], 2, mean)
			mins <- apply(these[ , ..criterion_codes], 2, min)
			maxs <- apply(these[ , ..criterion_codes], 2, max)
			range <- maxs - mins
			
			this_mean_tallies <- data.table(first_author = this_first_author, species = this_species)
			this_min_tallies <- data.table(first_author = this_first_author, species = this_species)
			this_max_tallies <- data.table(first_author = this_first_author, species = this_species)
			this_range_tallies <- data.table(first_author = this_first_author, species = this_species)
			
			for (criterion_code in criterion_codes) {
			
				this_mean_tallies[ , (criterion_code) := means[[criterion_code]]]
				this_min_tallies[ , (criterion_code) := mins[[criterion_code]]]
				this_max_tallies[ , (criterion_code) := maxs[[criterion_code]]]
				this_range_tallies[ , (criterion_code) := range[[criterion_code]]]
			
			}
			
			mean_tallies <- rbind(mean_tallies, this_mean_tallies)
			min_tallies <- rbind(min_tallies, this_min_tallies)
			max_tallies <- rbind(max_tallies, this_max_tallies)
			range_tallies <- rbind(range_tallies, this_range_tallies)
			
		}
	
	}
	
	tallies <- list(
		means = mean_tallies,
		mins = min_tallies,
		maxs = max_tallies,
		range = range_tallies	
	)
	
	saveRDS(tallies, './Analysis/Summary of Assessment of SDM Workflows by SDM Standards.rds')

say('###############################################')
say('### plot of SDM standard scores by standard ###')
say('###############################################')

	# Here, for each species, we make a plot of scores by standard across studies. We will make a plot that has two components, the raw (mean) scores shown as points, and an area-based graph that rounds the scores to the nearest integer and shows the proportion of studies in the deficient/bronze/silver/gold classes.

	### inputs

	# summaries of scores
	tallies <- readRDS('./Analysis/Summary of Assessment of SDM Workflows by SDM Standards.rds')

	means <- tallies$means

	# Convert means to long format for ggplot2
	means_long <- melt(means, id.vars = c('first_author', 'species'), variable.name = 'criterion', value.name = 'score')

	means_long <- as.data.table(means_long)

	# Round scores to nearest integer
	means_long$rounded_score <- round(means_long$score)

	# Calculate proportions
	proportions <- means_long[, .N, by = .(species, criterion, rounded_score)]
	proportions[ , proportion := N / sum(N), by = .(species, criterion)]

	proportions$criterion <- factor(proportions$criterion)
	
	proportions$rank <- NA_character_
	proportions$rank[proportions$rounded_score == 1] <- 'Gold'
	proportions$rank[proportions$rounded_score == 2] <- 'Silver'
	proportions$rank[proportions$rounded_score == 3] <- 'Bronze'
	proportions$rank[proportions$rounded_score == 4] <- 'Deficient'
	proportions$rank <- factor(proportions$rank, levels = rev(c('Deficient', 'Bronze', 'Silver', 'Gold')), ordered = TRUE)

	scores <- ggplot(proportions, aes(x = criterion, y = proportion, fill = rank)) +
		geom_bar(stat = 'identity', position = 'fill', color = 'black') +
		scale_fill_manual(
			values = c(
				'Deficient' = 'gray30',
				'Bronze' = 'burlywood1',
				'Silver' = 'lightcyan2',
				'Gold' = 'yellow'
			)
		) +
		facet_wrap(~ species, scales = 'free_x') +
		labs(x = 'Standard', y = 'Proportion of studies', fill = 'Rank') +
		theme_minimal() +
		theme(panel.grid = element_blank()) +
		theme(
			strip.text = element_text(size = 12, face = 'bold.italic'),
			axis.title = element_text(size = 12),
			axis.text.x = element_text(size = 12, vjust = 7)
		)

	# ggsave(scores, filename = './Analysis/SDM Standards.png', width = 12, height = 6, dpi = 600, bg = 'white')
	ggsave(scores, filename = './Analysis/SDM Standards.svg', width = 12, height = 6, dpi = 600, bg = 'white')
	
say('FINIS', deco = '~', level = 1)
