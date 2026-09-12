### SDM METHODOLOGICAL PLURALITY
### <author names redacted for review>
### <author contact information redacted for review> | 2025-10
###
### Calculate statistics for testing if there is clustering of predictions between teams and how many clusters there should be. 
###
### source('C:/Kaji/Research Group/ENMs - Plurality of Modeling Workflows/enm_methods_plurality/04_exploratory_cluster_analysis_of_teams.r')
###
### CONTENTS ###
### setup ###
### assess propensity to cluster and optimal number of clusters ###

#############
### setup ###
#############

	rm(list = ls())
	source('C:/Kaji/Research Group/ENMs - Plurality of Modeling Workflows/enm_methods_plurality/00_shared_constants_and_functions.r')

	dirCreate(paste0('./Outputs ', species_full, '/Cluster Analysis of Rasters'))

say('###################################################################')
say('### assess propensity to cluster and optimal number of clusters ###')
say('###################################################################')

	sink(paste0('./Outputs ', species_full, '/Cluster Analysis of Rasters/Propensity and Optimal Number of Clusters.txt'), split = TRUE)

		say('ANALYSIS OF PROPENSITY TO CLUSTER AND OPTIMAL NUMBER OF CLUSTERS')
		say(date(), post = 2)

		# # user-defined values
		# n_sites_for_clustering <- 20000

		### PCA on predictions	
		preds <- load_predictions(species_focal = species_focal, period = 'all', scale = TRUE, subset_teams = TRUE)
		# preds <- preds[1:n_sites_for_clustering] # for development
		trans <- t(preds)

		pca <- prcomp(trans)
		scores <- pca$x[ , 1:2]
		max_clusts <- floor(nrow(scores) / 2) - 1

		### are there true clusters?
		############################

		### VAT (visual assessment of cluster tendency; Bezdek and Hathaway 2002)
		# compare observed vs randomized distance matrix
		dists <- distances::distances(scores)
		dists <- distance_matrix(dists)

		dists_rand <- apply(dists, 2, function(x){ runif(length(x), min(x), (max(x)))} )
		dists_rand <- as.dist(dists_rand)

		obs <- fviz_dist(dists, show_labels = FALSE) +
			ggtitle('Observed distances') +
			theme(
				legend.position = 'none'
			)

		rand <- fviz_dist(dists_rand, show_labels = FALSE) +
			ggtitle('Randomized distances') +
			theme(
				legend.position = 'none'
			)

		combo <- obs + rand

		ggsave(combo, filename = paste0('./Outputs ', species_full, '/Cluster Analysis of Rasters/VAT Test - Observed vs Random Distance Matrices.png'), dpi = 150, width = 12, height = 6, bg = 'white')

		### Hopkins statistic
		say('HOPKINS STATISTIC FROM hopkins::hopkins()', level = 1)
		say('From ?hopkins: "Calculated values 0-0.3 indicate regularly-spaced data. Values around 0.5 indicate random data. Values 0.7-1 indicate clustered data."')
		
		n <- 100
		hops <- rep(NA_real_, n)
		for (i in 1:n) {
			hops[i] <- hopkins::hopkins(scores)
		}
		say('Mean +/-sd Hopkins score across ', n, ' iterations:')
		say(mean(hops), ' +/- ', sd(hops))

		#### optimal number of clusters
		###############################

		### Multiple statistics with clValid()

		say('clValid TEST OF OPTIMAL NUMBER OF CLUSTERS from clValid()', level = 1)
		say('Tested from 2 to ', max_clusts, ' clusters.', post = 2)
		valid <- clValid(scores, nClust = 2:max_clusts, clMethods = 'hierarchical', method = 'complete', validation = c('internal', 'stability'))
		print(summary(valid))

		say('clValid TEST OF OPTIMAL NUMBER OF CLUSTERS from NbClust()', level = 1)
		say('Tested from 2 to ', max_clusts, ' clusters.', post = 2)
		n_clusts <- NbClust(data = scores, max.nc = max_clusts, method = 'complete')

		# For Pronia, optimal number of clusters is 59.

		gap <- fviz_nbclust(scores, FUNcluster = hcut, k.max = max_clusts, method = 'gap_stat')
		sil <- fviz_nbclust(scores, FUNcluster = hcut, k.max = max_clusts, method = 'silhouette')

		gap <- gap + ggtitle('Gap statistic')
		sil <- sil + ggtitle('Silhouette statistic')

		combo <- gap + sil

		ggsave(combo, filename = paste0('./Outputs ', species_full, '/Cluster Analysis of Rasters/Optimal Number of Clusters.png'), dpi = 150, width = 12, height = 6, bg = 'white')

		# for Priona, optimal number is near any value of k.max used.

		### plot of mean silhouette width by cluster size using hierarchical clustering
		###############################################################################

		# looking for peak

		# We'll explore a range of cluster numbers:
		k_range <- 2:max_clusts

		clust <- hclust(dists, method = 'complete')

		# Compute the average silhouette width for each k
		sil_means <- sapply(k_range, function(k) {
			cl <- cutree(clust, k = k)
			sil <- silhouette(cl, dists)
			mean(sil[ , 'sil_width'])
		})

		sills <- data.frame(
			k = k_range,
			mean_sil = sil_means
		)

		# get optimal number of clusters assuming quadratic fit
		# fit quadratic regression, find k at maximum silhouette, find max silhouette, subtract 1 SE from the fit, get value of k that is within 1 SE of k that maximizes silhouette (on the lower side)
		m <- lm(mean_sil ~ k + I(k^2), data = sills)
		coeffs <- coefficients(m)
		k_at_max_sil <- -1 * coeffs['k'] / (2 * coeffs['I(k^2)'])
		
		max_sil <- coeffs['(Intercept)'] + coeffs['k'] * k_at_max_sil + coeffs['I(k^2)'] * k_at_max_sil^2

		sigma <- summary(m)$sigma
		max_sil_minus_se <- max_sil - sigma
		
		a <- coeffs['I(k^2)']
		b <- coeffs['k']
		c <- coeffs['(Intercept)'] - max_sil_minus_se
		
		k_opt_lower <- (-1 * b + sqrt(b^2 - 4 * a * c)) / (2 * a)
		k_opt_upper <- (-1 * b - sqrt(b^2 - 4 * a * c)) / (2 * a)
		
		k_opt_lower <- round(k_opt_lower)

		sil_plot <- ggplot(sills, aes(x = k, y = mean_sil)) +
			geom_line(size = 1) +
			geom_point(size = 2) +
			geom_smooth(
				method = 'lm', formula = y ~ x + I(x^2), se = FALSE,
				color = 'red', linetype = 'dashed'
			) +
			geom_vline(
				xintercept = k_at_max_sil,
				linetype = 'dashed', color = 'black', size = 1
			) +
			geom_hline(
				yintercept = max_sil_minus_se,
				linetype = 'dashed', color = 'blue', size = 1
			) +
			geom_vline(xintercept = k_opt_lower,
				linetype = 'dashed', color = 'orchid', size = 1
			) +
			annotate('text', 
				x = k_opt_lower,
				y = min(sills$mean_sil),
				label = paste0('optimum = ', round(k_opt_lower)),
				vjust = -0.5, color = 'orchid2', size = 4) +
			labs(x = 'Number of clusters (k)',
				y = 'Average silhouette width',
				title = 'Silhouette width by number of clusters',
				subtitle = 'Hierarchical clustering'
			) +
			theme_minimal(base_size = 14)

		say('RELATIONSHIP BETWEEN MEAN SILHOUETTE SIZE AND CLUSTER NUMBER USING HIERARCHICAL CLUSTERING', level = 1)
		say('Optimal number of clusters using "+/-1SE rule" is ', k_opt_lower, '.')

		ggsave(sil_plot, filename = paste0('./Outputs ', species_full, '/Cluster Analysis of Rasters/Optimal Cluster Number from Silhouette Width vs k Hierarchical.png'), width = 10, height = 10, bg = 'white')
		saveRDS(k_opt_lower, paste0('./Outputs ', species_full, '/Cluster Analysis of Rasters/Optimal Cluster Number from Silhouette Width vs k Hierarchical.rds'))

	sink()

say('DONE', level = 1)

