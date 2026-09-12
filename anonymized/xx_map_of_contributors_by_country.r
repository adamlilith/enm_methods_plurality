### Map of contributors by country

# Install and load required package
# install.packages("rnaturalearth")
# install.packages("rnaturalearthdata")

library(rnaturalearth)

# Get world country outlines
world <- ne_countries(scale = 'medium', returnclass = 'sv')

contribs <- c('Argentina', 'Australia', 'Canada', 'China', 'Denmark', 
			  'France', 'Germany', 'Israel', 'Japan', 'Netherlands', 
			  'New Zealand', 'Spain', 'Switzerland', 'United Kingdom', 
			  'United States of America', 'Zimbabwe')

indices <- world$admin %in% contribs

world$contrib <- 0
world$contrib[indices] <- 1

col <- ifelse(world$contrib == 0, 'gray90', '#E7BC29')

wt <- project(world, getCRS('Winkel Tripel NGS Version'))

png('C:/!scratch/world.png', width = 1400, height = 600, res = 150)
par(mar = c(0, 0, 0, 0), mai = c(0, 0, 0, 0))
plot(wt, col = col)
dev.off()



