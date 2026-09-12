### SDM METHODOLOGICAL PLURALITY
### Anna Thonis, Nikki Calavari, Uzma Ashraf, Toni Lyn Morelli, and Adam B. Smith*
### * adam.smith@mobot.org | Missouri Botanical Garden | 2025-10
###
### Create PCA of teams and make biplot and map of PC scores.
###
### source('C:/Kaji/Research Group/ENMs - Plurality of Modeling Workflows (Anna Thonis)/enm_methods_plurality/09_pca_of_predictions_vs_teams.r')
###
#############
### setup ###
#############

	rm(list = ls())
	source('C:/Kaji/Research Group/ENMs - Plurality of Modeling Workflows (Anna Thonis)/enm_methods_plurality/00_shared_constants_and_functions.r')

	this_out_dir <- paste0(out_dir, '/PCA on Teams')
	dirCreate(this_out_dir)

####################
### PCA on teams ###
####################

	### PCA-based distances between team predictions
	################################################

	preds <- load_predictions(species_focal = species_focal, period = 'all', scale = TRUE, subset_teams = TRUE)

	removes <- c('N3a', 'N3b', 'N4a', 'N4b')
	removes <- paste0(removes, rep(c('_mid', '_late'), each = 4))
	preds[ , (removes) := NULL]

preds <- preds[complete.cases(preds)]
preds <- preds[1:20000]

	pca <- prcomp(preds)
	# scores <- pca$x[ , 1:2]
	scores <- pca$x
	scores <- as.data.table(scores)

	# Get loadings for arrows
	loadings <- as.data.frame(pca$rotation[, 1:2])
	loadings$team <- rownames(loadings)

	mult <- 5
	loadings$PC1 <- mult * loadings$PC1
	loadings$PC2 <- mult * loadings$PC2
	
	biplot <- ggplot(scores, aes(x = PC1, y = PC2)) +
		geom_point(pch = 16, color = 'cornflowerblue', alpha = 0.1) +
		geom_segment(data = loadings, 
					 aes(x = 0, y = 0, xend = PC1, yend = PC2),
					 arrow = arrow(length = unit(0.3, "cm")),
					 color = 'red', alpha = 0.7) +
		geom_text(data = loadings,
				  aes(x = PC1, y = PC2, label = team),
				  hjust = 0, vjust = 0, size = 3, color = 'red')

	print(biplot)



