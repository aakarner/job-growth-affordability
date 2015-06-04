# Effect of high-wage job growth on housing demand on the San Francisco Bay Area
# Analysis using the Longitudinal Employer-Household Dynamics Origin-Destination
# Employment Statistics (LODES) data 2008-2011 and the American Community Survey
#
# Alex Karner, alex.karner@asu.edu
# Chris Benner, ccbenner@ucdavis.edu
#
# Purpose:
# Create maps of job growth and decline in job categories by jurisdiction.
# 
# Output: 
# Map images for the Bay Area showing job growth and decline in various categories
# over the time period of interest.

library(plyr); library(dplyr)
library(reshape2)
library(ggplot2)
library(scales) # pretty_breaks()
library(RColorBrewer)
library(grid) # unit() functionality
library(rgdal) # interface for the Geospatial Abstraction Library
library(rgeos)
library(ggmap) # add base images to maps

# Uncomment this line by removing the '#' in front..
# setwd("C:/My Directory/LEHD")
# setwd("D:/Dropbox/Work/high-wage job growth")
# .. in order to set your current working directory.

# Load previously saved dataset
load("data/BayAreaLEHD.RData")

# Prepare growth data ----------------------------------------------------------

# Create absolute annual and average (3 year) job growth numbers for each 
# jurisdiction for workers in each job category of interest.
# Naming convention is as follows: wac.year1.year2 where year1 > year 2

# Identify common places across all years
# It's possible that there are places with workers but not
# jobs or vice versa. 
all.places <- union(union(wac.place.2011$placename, wac.place.2010$placename), wac.place.2009$placename)

# Create three new (empty) data frames with only common rows between all years
wac.place.2009.v2 <- data.frame("placename" = all.places)
wac.place.2010.v2 <- data.frame("placename" = all.places)
wac.place.2011.v2 <- data.frame("placename" = all.places)

# Populate these data frames using merge (rows with missing info will get NA)
wac.place.2009.v2 <- left_join(wac.place.2009.v2, wac.place.2009)
wac.place.2010.v2 <- left_join(wac.place.2010.v2, wac.place.2010)
wac.place.2011.v2 <- left_join(wac.place.2011.v2, wac.place.2011)
# This generates warnings because the larger data frame still has factor
# levels based on the entire state

# Identify rows with missing data
which(apply(wac.place.2009.v2, 1, function(x) any(is.na(x))))
which(apply(wac.place.2010.v2, 1, function(x) any(is.na(x))))
which(apply(wac.place.2011.v2, 1, function(x) any(is.na(x))))
# ...the corresponding cells should receive zeros
# They were in one table but not the other
# For 2011 we have one CDP that went down to zero jobs
# Sereno del Mar, CDP
wac.place.2011.v2[wac.place.2011.v2$placename == "Sereno del Mar CDP, CA", 2] <- "Sonoma County, CA"
wac.place.2011.v2[wac.place.2011.v2$placename == "Sereno del Mar CDP, CA", 3:55] <- 0
wac.place.2011.v2[wac.place.2011.v2$placename == "Sereno del Mar CDP, CA", ]

# Error checking
# Are all placenames the same? 
stopifnot(wac.place.2009.v2$placename == wac.place.2010.v2$placename & 
		wac.place.2010.v2$placename == wac.place.2011.v2$placename)

# Replace previous variables with the new versions
wac.place.2009 <- wac.place.2009.v2
wac.place.2010 <- wac.place.2010.v2
wac.place.2011 <- wac.place.2011.v2

rm(wac.place.2009.v2); rm(wac.place.2010.v2); rm(wac.place.2011.v2)
		
# Take the difference to generate absolute growth numbers
wac.2011.2010 <- cbind(wac.place.2011[, 4:54] - wac.place.2010[, 4:54], wac.place.2011[, 1:3])
wac.2010.2009 <- cbind(wac.place.2010[, 4:54] - wac.place.2009[, 4:54], wac.place.2011[, 1:3])
wac.2011.2009 <- cbind(wac.place.2011[, 4:54] - wac.place.2009[, 4:54], wac.place.2011[, 1:3])

# Calculate percentage differences
wac.2011.2010.pct <- cbind((wac.place.2011[, 4:54] - wac.place.2010[, 4:54])/wac.place.2010[, 4:54], 
	wac.place.2011[, 1:3])
wac.2010.2009.pct <- cbind((wac.place.2010[, 4:54] - wac.place.2009[, 4:54])/wac.place.2009[, 4:54], 
	wac.place.2011[, 1:3])
wac.2011.2009.pct <- cbind((wac.place.2011[, 4:54] - wac.place.2009[, 4:54])/wac.place.2009[, 4:54], 
	wac.place.2011[, 1:3])

# The percentage change tables have many NaN values resulting
# from 0/0. Recode them. 
wac.2011.2010.pct[is.na(wac.2011.2010.pct)] <- 0
wac.2010.2009.pct[is.na(wac.2010.2009.pct)] <- 0
wac.2011.2009.pct[is.na(wac.2011.2009.pct)] <- 0

wac.diff.pct[is.na(wac.diff.pct)] <- 0

# Prepare geographic data ------------------------------------------------------

# Import shapefile of 2010 California Census places.
places.bay <- readOGR("data/GIS/Bay Area Place (2010, clipped)", layer = "tl_2010_06_place10")

# Extract only Bay Area counties
# counties <- readOGR("GIS/CA Counties (2010)", layer = "tl_2010_06_county10")
# counties <- counties[counties$GEOID10 %in% 
# 	c("06001", "06013", "06041", "06055", "06075", "06081", "06085", "06095", "06097"), ]
# 
# places.bay <- gIntersects(places, counties, byid = TRUE)
# clipped <- apply(places.bay == FALSE, 2, all)
# places.bay <- places[which(!clipped), ]

# Reproject to NAD83 California Albers
places.NAD83_CA <- spTransform(places.bay, CRS("+init=epsg:3310"))
places.f <- fortify(places.NAD83_CA, region = "GEOID10")
places.f <- merge(places.f, places.NAD83_CA@data, by.x = "id", by.y = "GEOID10")

# Map the places
ggplot(places.f, aes(long, lat, group = group)) + geom_polygon() + coord_equal()

# Add some basemap imagery
places.box <- spTransform(places.bay, CRS("+init=epsg:4326"))
b <- bbox(places.box)
b[1, ] <- (b[1, ] - mean(b[1, ])) * 1.05 + mean(b[1, ])
b[2, ] <- (b[2, ] - mean(b[2, ])) * 1.05 + mean(b[2, ])
# scale longitude and latitude (increase bb by 5% for plot) replace 1.05
# with 1.xx for an xx% increase in the plot size

basemap <- ggmap(get_map(location = b, source = "google", maptype = "satellite", crop = TRUE), darken = 0.8)
places.box.f <- fortify(places.box, region = "GEOID10")
places.box.f <- merge(places.box.f, places.box@data, by.x = "id", by.y = "GEOID10")

# Visualize results ------------------------------------------------------------

# Aspatial summaries of the absolute numbers of jobs
# in each cateogry are most helpful.
# Plot the 25 jurisdictions with the highest number of total jobs 
# in various job categories.

# Order the data frame in  descending order of total jobs
wac.place.2011 <- wac.place.2011[order(-wac.place.2011$C000), ]

# Identify the threshold that separates the top 25 from the bottom
threshold <- wac.place.2011$C000[26]

# What proportion of jobs are covered by these top 25 places?
sum(wac.place.2011$C000[wac.place.2011$C000 > threshold]) / sum(wac.place.2011$C000)
# ... 72%

# Create a more parsimonious place label
for(year in years.to.download)
 	assign(paste0("wac.place.", year), 
 		transform(eval(parse(text = paste0("wac.place.", year))), place = gsub(", CA", "", placename)))

# Plot the results
ggplot(subset(wac.place.2011, wac.place.2011$C000 > threshold), 
	aes(x = reorder(place, C000, max), y = C000)) + geom_bar(stat = "identity") + 
	xlab(NULL) + ylab("Total jobs") + coord_flip() + theme_bw()

ggsave("output/BayArea_LEHD_TotalJobs_2011.png", width = 8, height = 6)

# Mapping ----------------------------------------------------------------------

## Spatial summaries for growth/decline rates by job type

write.table(wac.2011.2010.pct, "output/wac_2011_2010_pct.csv", sep = ",", row.names = FALSE)

# Merge LEHD data with the map
# First need to make a consistent identifier
# by adding a ', CA' to NAMELSAD10
places.box.f$NAMELSAD10 <- paste0(places.box.f$NAMELSAD10, ", CA")

# Use this merge for percentage changes
places.box.f <- left_join(places.box.f, wac.diff.pct, by = c("NAMELSAD10" = "placename"))

# .... and this one for absolute
places.box.f <- left_join(places.box.f, wac.diff, by = c("NAMELSAD10" = "placename"))


# Plot the map

# Get long/lat of polygons
# ggplot can't label them directly because of the need
# to fortify
label_points <- data.frame(coordinates(places.bay))
label_points <- cbind(label_points, places.bay@data$NAMELSAD10)
names(label_points) <- c("long_", "lat_", "name")
label_points <- left_join(label_points, wac.place.2011[, c("place", "C000")], by = c("name" = "place"))
# Remove suffix from place name
label_points <- transform(label_points, pretty_name = gsub("(city)|(town)|(CDP)", "", label_points$name))
 
# Manual adjustments so that labels don't overlap
label_points[label_points$name == "Mountain View city", "lat_"] <- 37.42
label_points[label_points$name == "Santa Clara city", "lat_"] <- 37.35
label_points[label_points$name == "Sunnyvale city", "long_"] <- -121.95

plot.categories <- data.frame(
	variable = c("C000", "CE01", "CE02", "CE03", "CNS09", "CNS10", "CNS12", "CNS13", "CD01", "CD02", "CD03", "CD04",
		"naics_hi", "naics_lo"),
	name = c("total", "tier 1", "tier 2", "tier 3", "information", "finance", "professional", "management",
		"below highschool", "highschool", "some college", "bachelor's or greater", "high-wage NAICS", "low-wage NAICS"))
i <- 14
# Use this map for percentage changes
for(i in 1:nrow(plot.categories)) {
	basemap + geom_polygon(data = places.box.f, aes(x = long, y = lat, group = group, 
		fill = eval(parse(text = paste0(plot.categories[, 1][i])))), 
		color = grey(0.4), alpha = 0.7, lwd = 0.1) + 
		geom_text(data = label_points[label_points$C000 > threshold, ], aes(x = long_, y = lat_, label = pretty_name), 
			size = 2, fontface = 2, color = "yellow") + 
		scale_fill_gradient2(name = paste0("Change in ", plot.categories[, 2][i], " jobs\n 2008-2010 vs. 2011"), 
			limits = c(-1, 1), breaks = pretty_breaks(n = 10), low = "#2C7BB6", mid = "#FFFFBF", 
			high = "#A50026", midpoint = 0, space = "Lab", na.value = "black", guide = "legend") +
		guides(fill = guide_legend(override.aes = list(colour = NULL))) +
		theme_nothing(legend = TRUE) + 
		theme(legend.title = element_text(size = 5), legend.text = element_text(size = 5), 
			legend.key.size = unit(8, "points")) + coord_map()

	ggsave(paste0("output/", plot.categories[, 2][i], ".png"), width = 5.5, height = 4)
}

# ... and this one for absolute growth numbers
for(i in 1:nrow(plot.categories)) {
	basemap + geom_polygon(data = places.box.f, aes(x = long, y = lat, group = group, 
		fill = eval(parse(text = paste0(plot.categories[, 1][i])))), 
		color = grey(0.4), alpha = 0.7, lwd = 0.1) + 
		geom_text(data = label_points[label_points$C000 > threshold, ], aes(x = long_, y = lat_, label = pretty_name), 
			size = 2, fontface = 2, color = "yellow") + 
		scale_fill_gradient2(name = paste0("Change in ", plot.categories[, 2][i], " jobs\n 2008-2010 vs. 2011"), 
			breaks = pretty_breaks(n = 10), low = "#2C7BB6", mid = "#FFFFBF", 
			high = "#A50026", midpoint = 0, space = "Lab", na.value = "black", guide = "legend") +
		guides(fill = guide_legend(override.aes = list(colour = NULL))) +
		theme_nothing(legend = TRUE) + 
		theme(legend.title = element_text(size = 5), legend.text = element_text(size = 5), 
			legend.key.size = unit(8, "points")) + coord_map()

	ggsave(paste0("output/", plot.categories[, 2][i], "_absolute.png"), width = 5.5, height = 4)
}