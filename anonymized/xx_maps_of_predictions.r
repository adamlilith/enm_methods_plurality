### SDM METHODOLOGICAL PLURALITY
### <author names redacted for review>
### <author contact information redacted for review> | 2025-10
###
### Make nice maps of each team's rasters for publication.
###
### source('C:/Kaji/Research Group/ENMs - Plurality of Modeling Workflows/enm_methods_plurality/xx_maps_of_predictions.r')
###
### make maps of rasters for publication ###
###
#############
### setup ###
#############

	rm(list = ls())
	source('C:/Kaji/Research Group/ENMs - Plurality of Modeling Workflows/enm_methods_plurality/00_shared_constants_and_functions.r')

say('############################################')
say('### make maps of rasters for publication ###')
say('############################################')

	library(rnaturalearth)
	world <- ne_countries(scale = 'medium', returnclass = 'sv')
	focal <- vect(paste0(out_dir, '/common_study_region.gpkg'))
	files <- listFiles(paste0('./Submissions/Rasters ', species_full, ' Present Anonymized'), pattern = '.tif')

	summary_stat <- numeric()
	maps <- list()
	for (i in seq_along(files)) {
	
		r <- rast(files[i])
		name <- names(r)
		say(name)

# # for development... make faster!
# r <- aggregate(r, 8)

		# plot extent
		focal_proj <- project(focal, r)
		extent <- ext(focal_proj)
		extent <- as.vector(extent)
	
		# get mean prediction for sorting of maps
		r_crop <- crop(r, focal_proj)
		r_crop <- stretch(r_crop)
		summary_stat <- c(summary_stat, globalx(r_crop, 'sum'))

		world_proj <- project(world, r)

		maps[[i]] <- ggplot() +
			layer_spatial(world_proj, fill = 'gray80') +
			layer_spatial(r, aes(fill = after_stat(band1)), na.rm = TRUE) +
			scale_fill_viridis_c(option = 'magma', na.value = 'transparent') +
			# scale_fill_viridis_c(option = 'cividis', na.value = 'transparent') +
			# scale_fill_gradientn(colors = rev(RColorBrewer::brewer.pal(11, 'Spectral')), na.value = 'transparent') +
			coord_sf(xlim = extent[1:2], ylim = extent[3:4], expand = FALSE) +
			annotate('rect', 
				xmin = extent[1] + 0.01 * (extent[2] - extent[1]), 
				xmax = extent[1] + 0.16 * (extent[2] - extent[1]),
				ymin = extent[4] - 0.16 * (extent[4] - extent[3]),
				ymax = extent[4] - 0.01 * (extent[4] - extent[3]),
				fill = 'white', color = 'black', linewidth = 0.5) +
			annotate('text',
				x = extent[1] + 0.085 * (extent[2] - extent[1]),
				y = extent[4] - 0.075 * (extent[4] - extent[3]),
				label = name, size = 7, fontface = 'bold'
				) +
			theme(
				legend.position = 'none',
				axis.title = element_blank(),
				axis.text = element_blank(),
				axis.ticks = element_blank(),
				panel.background = element_blank(),
				panel.border = element_rect(colour = 'black', fill = NA, linewidth = 1),
				plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), 'cm')
			)
	
	} # next raster

	# order by mean predicted value
	order <- order(summary_stat)
	maps <- maps[order]

	panels <- plot_grid(plotlist = maps, nrow = 4, align = 'hv', axis = 'tblr', 
		rel_widths = 1, rel_heights = 1, greedy = FALSE)

	ggsave(panels, filename = paste0(out_dir, '/Maps across Common Area - Present.png'), width = 20.6, height = 10.8, dpi = 300, bg = 'white')

# # # say('######################################')
# # # say('### make map illustrating quartile ###')
# # # say('######################################')

# # # 	world <- rnaturalearth::ne_countries(scale = 'medium', returnclass = 'sv')
# # # 	focal <- vect(paste0(out_dir, '/common_study_region.gpkg'))

# # # 	preds_ll <- load_predictions(species_focal = species_focal, period = 'present', scale = TRUE, subset_teams = FALSE)
# # # 	preds <- load_predictions(species_focal = species_focal, period = 'present', scale = TRUE, subset_teams = TRUE)

# # # 	# calculate summary statistics across predictions
# # # 	inter_quart_range_fx <- function(x) quantile(x, 0.75) - quantile(x, 0.25)
# # # 	preds_ll[ , min := apply(preds, 1, min)]
# # # 	preds_ll[ , max := apply(preds, 1, max)]
# # # 	preds_ll[ , range := max - min]
# # # 	preds_ll[ , mean := apply(preds, 1, mean)]
# # # 	preds_ll[ , sd := apply(preds, 1, sd)]
# # # 	preds_ll[ , quant0.10 := apply(preds, 1, quantile, 0.1)]
# # # 	preds_ll[ , quant0.90 := apply(preds, 1, quantile, 0.9)]
# # # 	preds_ll[ , iqr := apply(preds, 1, inter_quart_range_fx)]

# # # 	# binary columns for cases >=Xth or Yth quantile threshold
# # # 	quant_low <- 0.10
# # # 	quant_high <- 0.90
# # # 	cols_to_check <- setdiff(names(preds_ll), c('longitude', 'latitude'))
# # # 	for (col in cols_to_check) {

# # # 		new_col <- paste0(col, '_hotspot')
# # # 		threshold <- quantile(preds_ll[[col]], quant_high, na.rm = TRUE)
# # # 		preds_ll[ , (new_col) := get(col) >= threshold]

# # # 		new_col <- paste0(col, '_coldspot')
# # # 		threshold <- quantile(preds_ll[[col]], quant_low, na.rm = TRUE)
# # # 		preds_ll[ , (new_col) := get(col) < threshold]

# # # 	}

# # # 	# sum hotspot columns
# # # 	hotspot_cols <- grep('_hotspot$', names(preds_ll), value = TRUE)
# # # 	preds_ll[ , prop_hotspots := rowSums(.SD) / length(hotspot_cols), .SDcols = hotspot_cols]

# # # 	# sum coldspot columns
# # # 	coldspot_cols <- grep('_coldspot$', names(preds_ll), value = TRUE)
# # # 	preds_ll[ , prop_coldspots := rowSums(.SD) / length(coldspot_cols), .SDcols = coldspot_cols]

# # # 	preds_ll <- vect(preds_ll, geom = c('longitude', 'latitude'), crs = getCRS('WGS84'))

# # # 	extent <- ext(preds_ll)
# # # 	extent <- as.vector(extent)

# # # 	# pal <- 'magma'
# # # 	# pal <- 'cividis'

# # # 	title_size <- 21

# # # 		iqr <- ggplot() +
# # # 			layer_spatial(world, fill = 'gray80') +
# # # 			layer_spatial(preds_ll, aes(color = iqr), size = 0.5) +
# # # 			scale_color_gradientn(name = 'IQR', colors = rev(RColorBrewer::brewer.pal(11, 'Spectral')), limits = c(0, 1)) +
# # # 			xlim(extent[1], extent[2]) + ylim(extent[3], extent[4]) +
# # # 			ggtitle('Interquartile Range') +
# # # 			theme(legend.position = 'right', plot.title = element_text(size = title_size))


# # # 		hotspots <- ggplot() +
# # # 			layer_spatial(world, fill = 'gray80') +
# # # 			layer_spatial(preds_ll, aes(color = prop_hotspots), size = 0.5) +
# # # 			# scale_color_viridis_c(name = 'Proportion\nagreement', option = pal, limits = c(0, 1)) +
# # # 			scale_color_gradientn(name = 'Proportion\nagreement', colors = rev(RColorBrewer::brewer.pal(11, 'Spectral')), limits = c(0, 1)) +
# # # 			xlim(extent[1], extent[2]) + ylim(extent[3], extent[4]) +
# # # 			ggtitle('Agreement in Highly Suitable Areas') +
# # # 			theme(legend.position = 'right', plot.title = element_text(size = title_size))

# # # 		coldspots <- ggplot() +
# # # 			layer_spatial(world, fill = 'gray80') +
# # # 			layer_spatial(preds_ll, aes(color = prop_coldspots), size = 0.5) +
# # # 			# scale_color_viridis_c(name = 'Proportion\nagreement', option = pal, limits = c(0, 1)) +
# # # 			scale_color_gradientn(name = 'Proportion\nagreement', colors = rev(RColorBrewer::brewer.pal(11, 'Spectral')), limits = c(0, 1)) +
# # # 			xlim(extent[1], extent[2]) + ylim(extent[3], extent[4]) +
# # # 			ggtitle('Agreement in Low-suitability Areas') +
# # # 			theme(legend.position = 'right', plot.title = element_text(size = title_size))

# # # 		low <- ggplot() +
# # # 			layer_spatial(world, fill = 'gray80') +
# # # 			layer_spatial(preds_ll, aes(color = quant0.10), size = 0.5) +
# # # 			# scale_color_viridis_c(name = '10th\nquantile', option = pal, limits = c(0, 1)) +
# # # 			scale_color_gradientn(name = '10th\nquantile', colors = rev(RColorBrewer::brewer.pal(11, 'Spectral')), limits = c(0, 1)) +
# # # 			xlim(extent[1], extent[2]) + ylim(extent[3], extent[4]) +
# # # 			ggtitle('10th quantile') +
# # # 			theme(legend.position = 'right', plot.title = element_text(size = title_size))

# # # 		high <- ggplot() +
# # # 			layer_spatial(world, fill = 'gray80') +
# # # 			layer_spatial(preds_ll, aes(color = quant0.90), size = 0.5) +
# # # 			# scale_color_viridis_c(name = '90th\nquantile', option = pal, limits = c(0, 1)) +
# # # 			scale_color_gradientn(name = '90th\nquantile', colors = rev(RColorBrewer::brewer.pal(11, 'Spectral')), limits = c(0, 1)) +
# # # 			xlim(extent[1], extent[2]) + ylim(extent[3], extent[4]) +
# # # 			ggtitle('90th quantile') +
# # # 			theme(legend.position = 'right', plot.title = element_text(size = title_size))

# # # 		mean <- ggplot() +
# # # 			layer_spatial(world, fill = 'gray80') +
# # # 			layer_spatial(preds_ll, aes(color = mean), size = 0.5) +
# # # 			# scale_color_viridis_c(name = 'Mean', option = pal, limits = c(0, 1)) +
# # # 			scale_color_gradientn(name = 'Mean', colors = rev(RColorBrewer::brewer.pal(11, 'Spectral')), limits = c(0, 1)) +
# # # 			xlim(extent[1], extent[2]) + ylim(extent[3], extent[4]) +
# # # 			ggtitle('Mean') +
# # # 			theme(legend.position = 'right', plot.title = element_text(size = title_size))


# # # 	# empty_plot <- ggplot() + theme_void()
# # # 	combo <- plot_grid(mean, high, hotspots, iqr, low, coldspots, nrow = 2, align = 'hv')
# # # 	ggsave(combo, filename = paste0(out_dir, '/Map of Agreement - Present - Square.png'), width = 18, height = 10, dpi = 600, bg = 'white')

say('DONE', level = 1)
