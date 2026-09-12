### PLOTS
### Plots of distribution of individual workflow decisions
###
### source('C:/Kaji/Research Group/ENMs - Plurality of Modeling Workflows (Anna Thonis)/enm_methods_plurality/xx_plots.r')

	rm(list = ls())
	source('C:/Kaji/Research Group/ENMs - Plurality of Modeling Workflows (Anna Thonis)/enm_methods_plurality/00_shared_constants_and_functions.r')

	this_out_dir <- paste0(out_dir, '/Miscellaneous Plots')
	dirCreate(this_out_dir)

	library(readxl)

	plots <- list()

### sample size
###############

	d <- read_xlsx('./Data/Univariate Workflow Extracts for Illustrative Plots.xlsx', sheet = 'Sample Size')

	plots$n_occs <- ggplot(d, aes(x = n)) +
		geom_histogram(binwidth = 50) +
		xlab('Number of occurrences') + ylab('Number of teams')

### predictors
##############

	d <- read_xlsx('./Data/Univariate Workflow Extracts for Illustrative Plots.xlsx', sheet = 'Predictors')
	d$Predictor <- factor(d$Predictor, levels = d$Predictor)

	plots$predictors <- ggplot(d, aes(x = Predictor, y = n)) +
		geom_col(fill = c(rep('cornflowerblue', 20), rep('darkred', 10))) +
		xlab('') + ylab('Number of teams') +
		theme(
			axis.text.x = element_text(angle = 90, hjust = 1)
		)

### software
############

	d <- read_xlsx('./Data/Univariate Workflow Extracts for Illustrative Plots.xlsx', sheet = 'Software')
	d$Software <- factor(d$Software, levels = d$Software)

	plots$software <- ggplot(d, aes(x = Software, y = n, fill = Software)) +
		geom_col() +
		# scale_fill_brewer(palette = 'Set2') +
		xlab('') + ylab('Number of teams') +
		theme(
			axis.text.x = element_text(angle = 90, hjust = 1)
		)

### climate data
################

	d <- read_xlsx('./Data/Univariate Workflow Extracts for Illustrative Plots.xlsx', sheet = 'Climate Data')

	plots$climate_data <- ggplot(d, aes(x = '', y = n, fill = Source)) +
		geom_col(width = 1) +
		coord_polar('y', start = 0) +
		theme_void() +
		theme(legend.title = element_blank())


### algorithm
#############

	d <- read_xlsx('./Data/Univariate Workflow Extracts for Illustrative Plots.xlsx', sheet = 'Algorithm')

	plots$algorithms <- ggplot(d, aes(x = '', y = n, fill = Algorithm)) +
		geom_col(width = 1) +
		coord_polar('y', start = 0) +
		scale_fill_brewer(palette = 'Set2') +
		theme_void() +
		theme(
			legend.title = element_blank(),
			axis.text.x = element_blank(),
			axis.text.y = element_blank()
		)


### boundary
#############

	d <- read_xlsx('./Data/Univariate Workflow Extracts for Illustrative Plots.xlsx', sheet = 'Boundary')
	d$Method <- factor(d$Method, levels = d$Method)

	plots$calibration_region_definition <- ggplot(d, aes(x = Method, y = n, fill = Method)) +
		geom_col() +
		scale_fill_brewer(palette = 'Set2') +
		xlab('') + ylab('Number of cases') +
		theme(
			legend.position = 'none',
			axis.text.x = element_text(angle = 90, hjust = 1)
		)

### resolution
##############

	d <- read_xlsx('./Data/Univariate Workflow Extracts for Illustrative Plots.xlsx', sheet = 'Resolution km2')

	plots$resolution_km2 <- ggplot(d, aes(x = res)) +
		geom_histogram() +
		scale_x_log10() +
		xlab('Resolution (km²)') + ylab('Number of teams')

	d <- read_xlsx('./Data/Univariate Workflow Extracts for Illustrative Plots.xlsx', sheet = 'Resolution arcs')
	d$res <- factor(d$res, levels = d$res)

	plots$resolution_arc <- ggplot(d, aes(x = res, y = n)) +
		geom_col() +
		xlab('') + ylab('Number of teams') +
		theme(
			axis.text.x = element_text(angle = 60, hjust = 1)
		)

### format and save
###################

for (i in seq_along(plots)) {

	plots[[i]] <- plots[[i]] +
		theme(
			plot.title = element_blank(),
			axis.title = element_text(size = 42),
			axis.text = element_text(size = 28),
			# panel.background = element_blank()
		)

	ggsave(plots[[i]], filename = paste0(this_out_dir, '/', names(plots)[i], '.png'), width = 12, height = 8, bg = 'white')

}
