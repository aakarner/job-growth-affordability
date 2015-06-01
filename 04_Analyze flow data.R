# Anlayze LEHD flow data
# Create metrics of internal capture

# Alex Karner, alex.karner@asu.edu
# Chris Benner, ccbenner@ucdavis.edu

# Set your working directory. 
# The LEHD data files will be stored here. 

# Uncomment this line by removing the '#' in front..
# setwd("C:/My Directory/LEHD")
# setwd("D:/Dropbox/Work/high-wage job growth/data")
# .. in order to set your current working directory.

options(scipen = 999) # Supress scientific notation so we can see census geocodes

library(plyr); library(dplyr)
library(MonetDB.R)
library(R.utils)
require(igraph)
require(googleVis)
library(ggmap) # To query driving distances
library(reshape2)
library(grid) # unit() functionality
library(rgdal) # interface for the Geospatial Abstraction Library
library(rgeos)
library(scales)

# Load previously saved dataset
# containing block level flows for California
load("data/CA_LEHD_od_blocks.RData")
load("data/CountySkims_Google.RData")


# Plot results

# identify top 25 workplaces for visualization
# Grab from '01_Calculate balance measures.R'

places <- gsub(", CA", "", places)
places[places == "Unincorporated Sonoma County"] = "Unincorporated Sonoma county"

commutes.plot <- rbind(commutes.2011, commutes.2010, commutes.2009, commutes.2008)
commutes.plot <- filter(commutes.plot, w_plc %in% places)
commutes.plot$w_plc <- gsub(" city", "", commutes.plot$w_plc)
commutes.plot$w_plc[commutes.plot$w_plc == "Unincorporated Sonoma county"] <- "Uninc. Sonoma County"

# Income groups
inc <- ggplot(filter(commutes.plot, variable %in% c("SE01", "SE02", "SE03")), 
	aes(x = year, y = value, col = variable)) + geom_point() + geom_line(aes(group = variable)) + 
	scale_color_brewer(palette = "Dark2", 
		labels = c("tier 1 jobs", "tier 2 jobs", "tier 3 jobs")) + 
	xlab(NULL) + ylab("distance for workers commuting into city (miles)") + 
	theme_bw() + 
	theme(plot.title = element_text(face = "bold"), legend.title = element_blank(),
		axis.text.x = element_text(angle=45, vjust = 0.5),
		legend.position = "bottom")
	
inc + facet_wrap(~ w_plc)

setwd("D:/Dropbox/Work/high-wage job growth/output")
ggsave("Commute_Income.png", width = 14, height = 15, scale = 0.6)

# industry categories
ind <- ggplot(filter(commutes.plot, variable %in% c("SI01", "SI02", "SI03")), 
	aes(x = year, y = value, col = variable)) + geom_point() + geom_line(aes(group = variable)) + 
	scale_color_brewer(palette = "Set1", 
		labels = c("goods producing", "trade, transport, utilities", "other services")) + 
	xlab(NULL) + ylab("distance for workers commuting into city (miles)") + 
	theme_bw() + 
	theme(plot.title = element_text(face = "bold"), legend.title = element_blank(),
		axis.text.x = element_text(angle=45, vjust = 0.5),
		legend.position = "bottom")
	
ind + facet_wrap(~ w_plc)

ggsave("Commute_Income.png", width = 14, height = 15, scale = 0.6)

# Age
age <- ggplot(filter(commutes.plot, variable %in% c("SA01", "SA02", "SA03")), 
	aes(x = year, y = value, col = variable)) + geom_point() + geom_line(aes(group = variable)) + 
	scale_color_brewer(palette = "Set1", 
		labels = c("< 29", "30-54", "> 55")) + 
	xlab(NULL) + ylab("distance for workers commuting into city (miles)") + 
	theme_bw() + 
	theme(plot.title = element_text(face = "bold"), legend.title = element_blank(),
		axis.text.x = element_text(angle=45, vjust = 0.5),
		legend.position = "bottom")
	
age + facet_wrap(~ w_plc)

ggsave("Commute_age.png", width = 14, height = 15, scale = 0.6)


# identify silicon valley places

# Commutes into silicon valley compared to the rest of the Bay Area
sv.places <- read.table("Silicon_Valley_Places.csv", sep = ",", header = TRUE, row.names = NULL)
sv.places <- data.frame("place" = sv.places[, "NAMELSAD10"])

commutes.2011.sv <- semi_join(commutes.2011, sv.places, by = c("w_plc" = "place"))

# 2011 and 2008 v 2011
# Top 25 silicon valley, top 25 bay area
# Sankey plots
# Internal capture
# disparities between low, mid, high? - maybe not meaningful. could do disparities
# between low, mid, and overall mean

# Internal capture -------------------------------------------------------------

# Use the total number of jobs as the denominator
# We want jurisdictions to be punished for having too little housing 
for(year in years.to.download) { 
	this.year <- eval(parse(text = paste0("od.", year, ".place")))
		
	this.year.total <- group_by(this.year, w_plc)

	# Summary by home location
	this.year.total <- summarize(this.year.total, 
		S000 = sum(S000),
		SA01 = sum(SA01), SA02 = sum(SA02), SA03 = sum(SA03),
		SE01 = sum(SE01), SE02 = sum(SE02), SE03 = sum(SE03),
		SI01 = sum(SI01), SI02 = sum(SI02), SI03 = sum(SI03))

	# Extract internal flows
	this.year.internal <- this.year[this.year$h_plc == this.year$w_plc, ]

	names(this.year.internal)[4:13] <- paste0("i_", names(this.year.internal)[4:13])
	this.year.internal <- left_join(this.year.internal, this.year.total, by = c("w_plc" = "w_plc"))
	
	names(this.year.internal)[15:24] <- paste0("t_", names(this.year.internal)[15:24])
	
	internal.cap <- cbind("plc" = this.year.internal$w_plc, this.year.internal[, 4:13] / this.year.internal[, 15:24])

	internal.cap$year <- year
	
	internal.cap <- melt(internal.cap, id = c("plc", "year"))
	
	assign(paste0("internal.cap.", year), internal.cap)
	
	rm(internal.cap)
}
	
internal.plot <- rbind(internal.cap.2011, internal.cap.2010, internal.cap.2009, internal.cap.2008)

internal.plot <- filter(internal.plot, plc %in% places)
internal.plot$plc <- gsub(", CA", "", internal.plot$plc)

internal.plot$plc[internal.plot$plc == "Unincorporated Sonoma county"] <- "Uninc. Sonoma County"
internal.plot$plc <- gsub(" city", "", internal.plot$plc)

# Income groups
inc <- ggplot(filter(internal.plot, variable %in% c("i_SE01", "i_SE02", "i_SE03")), 
	aes(x = year, y = value, col = variable)) + geom_point() + geom_line(aes(group = variable)) + 
	scale_color_brewer(palette = "Dark2", 
		labels = c("tier 1 jobs", "tier 2 jobs", "tier 3 jobs")) + 
	xlab(NULL) + ylab("internal capture (%)") + 
	theme_bw() + 
	theme(plot.title = element_text(face = "bold"), legend.title = element_blank(),
		axis.text.x = element_text(angle=45, vjust = 0.5),
		legend.position = "bottom")
	
inc + facet_wrap(~ plc)

ggsave("Internal capture_Income.png", width = 14, height = 15, scale = 0.6)

# Industry groups
ind <- ggplot(filter(internal.plot, variable %in% c("i_SI01", "i_SI02", "i_SI03")), 
	aes(x = year, y = value, col = variable)) + geom_point() + geom_line(aes(group = variable)) + 
	scale_color_brewer(palette = "Set1", 
		labels = c("goods producing", "trade, transport, utilities", "other services")) + 
	xlab(NULL) + ylab("internal capture (%)") + 
	theme_bw() + 
	theme(plot.title = element_text(face = "bold"), legend.title = element_blank(),
		axis.text.x = element_text(angle=45, vjust = 0.5),
		legend.position = "bottom")
	
ind + facet_wrap(~ plc)

setwd("D:/Dropbox/Work/high-wage job growth/output")
ggsave("Internal capture_Industry.png", width = 14, height = 15, scale = 0.6)

# Age
age <- ggplot(filter(internal.plot, variable %in% c("i_SA01", "i_SA02", "i_SA03")), 
	aes(x = year, y = value, col = variable)) + geom_point() + geom_line(aes(group = variable)) + 
	scale_color_brewer(palette = "Set1", 
		labels = c("< 29", "30 - 54", "> 55")) + 
	xlab(NULL) + ylab("internal capture (%)") + 
	theme_bw() + 
	theme(plot.title = element_text(face = "bold"), legend.title = element_blank(),
		axis.text.x = element_text(angle=45, vjust = 0.5),
		legend.position = "bottom")
	
age + facet_wrap(~ plc)

ggsave("output/Internal capture_age.png", width = 14, height = 15, scale = 0.6)


# Prepare geographic data ------------------------------------------------------

# Import shapefile of 2010 California Census places.
places.bay <- readOGR("GIS/Bay Area Place (2010, clipped)", layer = "tl_2010_06_place10")

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

# Merge commute and internal capture data with the map
places.box.f <- left_join(places.box.f, internal.2011, by = c("NAMELSAD10" = "plc"))
places.box.f <- left_join(places.box.f, commutes.2011.2, by = c("NAMELSAD10" = "w_plc"))

# Plot the map

# Get long/lat of polygons
# ggplot can't label them directly because of the need
# to fortify
label_points <- data.frame(coordinates(places.bay))
label_points <- cbind(label_points, places.bay@data$NAMELSAD10)
names(label_points) <- c("long_", "lat_", "name")
label_points <- left_join(label_points, wac.place.2011[, c("place", "C000")], by = c("name" = "place"))
label_points <- transform(label_points, pretty_name = gsub("(city)|(town)|(CDP)", "", label_points$name))
 
# Manual adjustments so that labels don't overlap
label_points[label_points$name == "Mountain View city", "lat_"] <- 37.42
label_points[label_points$name == "Santa Clara city", "lat_"] <- 37.35
label_points[label_points$name == "Sunnyvale city", "long_"] <- -121.95

basemap + geom_polygon(data = places.box.f, aes(x = long, y = lat, group = group, 
	fill = S000)) + 
	scale_color_brewer(name = "Internal cap_s000") +
		guides(fill = guide_legend(override.aes = list(colour = NULL))) +
		theme_nothing(legend = TRUE) + 
		theme(legend.title = element_text(size = 5), legend.text = element_text(size = 5)) + coord_map()
			
			
			legend.key.size = unit(8, "points")) + coord_map()

	ggsave(paste0(plot.categories[, 2][i], ".png"), width = 5.5, height = 4)
}