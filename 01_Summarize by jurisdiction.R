# Create summaries by jurisdcition

# Alex Karner, alex.karner@asu.edu
# Chris Benner, ccbenner@ucdavis.edu

library(plyr); library(dplyr)
library(reshape2)
library(rgdal) # interface for the Geospatial Abstraction Library
library(rgeos)
library(ggplot2)
library(ggmap) # add base images to maps
library(scales) # pretty_breaks()
library(RColorBrewer)
library(grid) # unit() functionality

# Load previously saved dataset
load("BayAreaLEHD.RData")

# Program start ----------------------------------------------------------------

# Prepare LEHD data ------------------------------------------------------------

# Merge geographic identifier to each year of LEHD data
for(year in years.to.download) { 
	assign(paste0("wac.", year), merge(eval(parse(text = paste0("wac.", year))), 
		xwalk[, c("tabblk2010", "ctyname", "stplcname")], by.x = "w_geocode", by.y = "tabblk2010"))
	
	assign(paste0("rac.", year), merge(eval(parse(text = paste0("rac.", year))), 
		xwalk[, c("tabblk2010", "ctyname", "stplcname")], by.x = "h_geocode", by.y = "tabblk2010"))
}

# Add an identifier for unincorporated areas 
for(year in years.to.download) {

	# Add a new column `placename` containing stplcname if there is one
	# and identifying the block as falling within an unincorporated area, 
	# if there's not
	assign(paste0("wac.", year), with(eval(parse(text = paste0("wac.", year))), 
		transform(eval(parse(text = paste0("wac.", year))), 
			placename = ifelse(stplcname == "", paste0("Unincorporated ", ctyname), stplcname))))
	
	assign(paste0("rac.", year), with(eval(parse(text = paste0("rac.", year))), 
		transform(eval(parse(text = paste0("rac.", year))), 
			placename = ifelse(stplcname == "", paste0("Unincorporated ", ctyname), stplcname))))
}

# Create summary tables by jurisdiction by year
# Keep both place name and county name for ease of filtering
for(year in years.to.download) { 
	assign(paste0("wac.place.", year), ddply(eval(parse(text = paste0("wac.", year))), 
		.(placename, ctyname), numcolwise(sum)))
	
	assign(paste0("rac.place.", year), ddply(eval(parse(text = paste0("rac.", year))), 
		.(placename, ctyname), numcolwise(sum)))
}

# Extract Bay Area jurisdictions only
# Use filter() for readability and ease
for(year in years.to.download)
	for(j in c("wac", "rac"))
		assign(paste0(j, ".place.", year), filter(eval(parse(text = paste0(j, ".place.", year))),
			ctyname %in% c("Alameda County, CA", "Contra Costa County, CA", "Marin County, CA", "Napa County, CA", 
			"San Francisco County, CA", "Santa Clara County, CA", "San Mateo County, CA", "Solano County, CA", 
				"Sonoma County, CA")))


# Results ----------------------------------------------------------------------

## Job growth

# Create absolute annual and average (3 year) job growth numbers for each 
# jurisdiction for workers in each job category of interest.
# Naming convention is as follows: wac.year1.year2 where year1 > year 2

# Identify common places across all years
all.places <- union(union(wac.place.2011$stplcname, wac.place.2010$stplcname), wac.place.2009$stplcname)

# Create three new (empty) data frames with only common rows between all years
wac.place.2009.v2 <- data.frame("stplcname" = all.places)
wac.place.2010.v2 <- data.frame("stplcname" = all.places)
wac.place.2011.v2 <- data.frame("stplcname" = all.places)

# Populate these data frames using merge (rows with missing info will just get NA)
wac.place.2009.v2 <- merge(wac.place.2009.v2, wac.place.2009, all = TRUE)
wac.place.2010.v2 <- merge(wac.place.2010.v2, wac.place.2010, all = TRUE)
wac.place.2011.v2 <- merge(wac.place.2011.v2, wac.place.2011, all = TRUE)

# Error checking
stopifnot(wac.place.2009.v2$stplcname == wac.place.2010.v2$stplcname & 
		wac.place.2009.v2$ctyname == wac.place.2010.v2$ctyname)

# This check fails because...
stopifnot(wac.place.2011.v2$stplcname == wac.place.2010.v2$stplcname & 
		wac.place.2011.v2$ctyname == wac.place.2010.v2$ctyname)
#...Sereno Del Mar, CDP is in the 2009 and 2010 WAC, but not 2011

# Take the difference to generate absolute growth numbers
wac.2011.2010 <- cbind(wac.place.2011.v2[, 4:54] - wac.place.2010.v2[, 4:54], wac.place.2011.v2[, 1:3])
wac.2010.2009 <- cbind(wac.place.2010.v2[, 4:54] - wac.place.2009.v2[, 4:54], wac.place.2011.v2[, 1:3])
wac.2011.2009 <- cbind(wac.place.2011.v2[, 4:54] - wac.place.2009.v2[, 4:54], wac.place.2011.v2[, 1:3])

# Calculate percentage differences
wac.2011.2010.pct <- cbind((wac.place.2011.v2[, 4:54] - wac.place.2010.v2[, 4:54])/wac.place.2010.v2[, 4:54], 
	wac.place.2011.v2[, 1:3])
wac.2010.2009.pct <- cbind((wac.place.2010.v2[, 4:54] - wac.place.2009.v2[, 4:54])/wac.place.2009.v2[, 4:54], 
	wac.place.2011.v2[, 1:3])
wac.2011.2009.pct <- cbind((wac.place.2011.v2[, 4:54] - wac.place.2009.v2[, 4:54])/wac.place.2009.v2[, 4:54], 
	wac.place.2011.v2[, 1:3])

# Remove the NA row
wac.2011.2010 <- wac.2011.2010[!is.na(wac.2011.2010[, 1]), ]
wac.2011.2009 <- wac.2011.2009[!is.na(wac.2011.2009[, 1]), ]

# The percentage change tables have many NaN values resulting
# from 0/0. Recode them. 
wac.2011.2010.pct[is.na(wac.2011.2010.pct)] <- 0
wac.2010.2009.pct[is.na(wac.2010.2009.pct)] <- 0
wac.2011.2009.pct[is.na(wac.2011.2009.pct)] <- 0

# Output results
write.table(wac.2011.2010, "wac_2011_2010.csv", sep = ",", row.names = FALSE)
write.table(wac.2010.2009, "wac_2010_2009.csv", sep = ",", row.names = FALSE)
write.table(wac.2011.2009, "wac_2011_2009.csv", sep = ",", row.names = FALSE)

write.table(wac.2011.2009.pct, "wac_2011_2009_pct.csv", sep = ",", row.names = FALSE)

# Prepare geographic data ------------------------------------------------------

# Import shapefile of 2010 California Census places.
places.bay <- readOGR("GIS/Bay Area Place (2010, clipped)", layer = "tl_2010_06_place10")

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

# Plot the map
basemap + geom_polygon(data = places.box.f, aes(x = long, y = lat, group = group), color = grey(0.4), alpha = 0.2) + 
	theme_nothing()

ggsave("BayAreaMap.png")

# Visaulize results ------------------------------------------------------------

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
 		transform(eval(parse(text = paste0("wac.place.", year))), place = gsub(", CA", "", stplcname)))

ggsave("BayArea_LEHD_TotalJobs_2011.png", width = 8, height = 6)

# Plot the results
ggplot(subset(wac.place.2011, wac.place.2011$C000 > threshold), 
	aes(x = reorder(place, C000, max), y = C000)) + geom_bar(stat = "identity") + 
	xlab(NULL) + ylab("Total jobs") + coord_flip() + theme_bw()
	
# Changes in job totals year-by-year
write.table(wac.place.2010.v2, "wac_2010.csv", sep = ",", row.names = FALSE)
write.table(wac.place.2009.v2, "wac_2009.csv", sep = ",", row.names = FALSE)

## Spatial summaries for growth/decline rates by job type

write.table(wac.2011.2010.pct, "wac_2011_2010_pct.csv", sep = ",", row.names = FALSE)

# Merge LEHD data with the map
# First need to make a consistent identifier
# by removing the ', CA' from stplcname
places.box.f$NAMELSAD10 <- paste0(places.box.f$NAMELSAD10, ", CA")

places.box.f <- left_join(places.box.f, wac.2011.2009.pct, by = c("NAMELSAD10" = "stplcname"))

# Plot the map

# Get long/lat of polygons
# ggplot can't label them directly because of the need
# to fortify
label_points <- data.frame(coordinates(places.bay))
label_points <- cbind(label_points, places.bay@data$NAMELSAD10)
names(label_points) <- c("long_", "lat_", "name")
label_points <- left_join(label_points, wac.place.2011[, c("place", "C000")], by = c("name" = "place"))
label_points <- transform(label_points, pretty_name = gsub("(city)|(town)|(CDP)", "", label_points$name))
 
label_points[label_points$name == "Mountain View city", "lat_"] <- 37.42
label_points[label_points$name == "Santa Clara city", "lat_"] <- 37.35
label_points[label_points$name == "Sunnyvale city", "long_"] <- -121.95

plot.categories <- data.frame(
	variable = c("C000", "CE01", "CE02", "CE03", "CNS09", "CNS10", "CNS12", "CNS13", "CD01", "CD02", "CD03", "CD04"),
	name = c("total", "low-wage", "mid-wage", "high-wage", "information", "finance", "professional", "management",
		"below highschool", "highschool", "some college", "bachelor's or greater"))

for(i in 1:nrow(plot.categories)) {
	basemap + geom_polygon(data = places.box.f, aes(x = long, y = lat, group = group, 
		fill = eval(parse(text = paste0(plot.categories[, 1][i])))), 
		color = grey(0.4), alpha = 0.7, lwd = 0.1) + 
		geom_text(data = label_points[label_points$C000 > threshold, ], aes(x = long_, y = lat_, label = pretty_name), 
			size = 2, fontface = 2, color = "yellow") + 
		scale_fill_gradient2(name = paste0("Change in ", plot.categories[, 2][i], " jobs\n 2009 - 2011"), 
			limits = c(-1, 1), breaks = pretty_breaks(n = 10), low = "#2C7BB6", mid = "#FFFFBF", 
			high = "#A50026", midpoint = 0, space = "Lab", na.value = "black", guide = "legend") +
		guides(fill = guide_legend(override.aes = list(colour = NULL))) +
		theme_nothing(legend = TRUE) + 
		theme(legend.title = element_text(size = 5), legend.text = element_text(size = 5), 
			legend.key.size = unit(8, "points")) + coord_map()

	ggsave(paste0(plot.categories[, 2][i], ".png"), width = 5.5, height = 4)
}



for(i in 1:nrow(plot.categories)) {
	basemap + geom_polygon(data = places.box.f, aes(x = long, y = lat, group = group, 
		fill = eval(parse(text = paste0(plot.categories[, 1][i])))), 
		color = grey(0.4), alpha = 0.7, lwd = 0.1) + 
		geom_text(data = label_points[label_points$C000 > threshold, ], aes(x = long_, y = lat_, label = pretty_name), 
			size = 2, fontface = 2, color = "yellow") + 
		scale_fill_distiller(name = paste0("Change in ", plot.categories[, 2][i], " jobs\n 2010 - 2011"), type = "div",
			palette = "RdYlBu", space = "Lab", na.value = "black", limits = c(-1, 1), breaks = pretty_breaks(n = 10),
			guide = "legend") + 
		guides(fill = guide_legend(override.aes = list(colour = NULL))) +
		theme_nothing(legend = TRUE) + 
		theme(legend.title = element_text(size = 5), legend.text = element_text(size = 5), 
			legend.key.size = unit(8, "points")) + coord_map()

	ggsave("map0_sm.png", width = 5.5, height = 4)
}





basemap + geom_polygon(data = places.box.f, aes(x = long, y = lat, group = group, 
		fill = eval(parse(text = paste0(plot.categories[, 1][i])))), 
		color = grey(0.4), alpha = 0.7, lwd = 0.1) + 
		geom_text(data = label_points[label_points$C000 > threshold, ], aes(x = long_, y = lat_, label = pretty_name), 
			size = 2, fontface = 2, color = "yellow") + 
		scale_fill_gradient2(name = paste0("Percent change in ", plot.categories[, 2][i], "\njobs 2010 - 2011"), 
			limits = c(-1, 1), breaks = pretty_breaks(n = 10), low = "#2C7BB6", mid = "#FFFFBF", 
			high = "#A50026", midpoint = 0, space = "Lab", na.value = "black", guide = "legend") +
		guides(fill = guide_legend(override.aes = list(colour = NULL))) +
		theme_nothing(legend = TRUE) + 
		theme(legend.title = element_text(size = 5), legend.text = element_text(size = 5), 
			legend.key.size = unit(8, "points")) + coord_map()

ggsave("map1.png", width = 5.5, height = 4)


## Area jobs / employed residents ratios ---------------------------------------

# Calculate ratios of jobs to employed residents for each jurisdiction 
# by year, in each job category of interest.
# Illustrate trends over time for the Bay Area's largest jurisdictions

# Identify common places in each year
balance.places <- union(wac.place.2011$placename, rac.place.2011$placename)

for(year in years.to.download) {	
	
	wac.balance.this <- data.frame("place" = balance.places)
	wac.balance.this <- left_join(wac.balance.this, eval(parse(text = paste0("wac.place.", year))), 
			by = c("place" = "placename"))
	wac.balance.this <- select(wac.balance.this, place, C000, CE01, 
		CE02, CE03, CNS01, CNS02, CNS03, CNS04, CNS05, CNS06, CNS07, CNS08, CNS09, CNS10, CNS11, CNS12, CNS13, CNS14, 
		CNS15, CNS16, CNS17, CNS18, CNS19, CNS20, CD01, CD02, CD03, CD04)
	
	rac.balance.this <- data.frame("place" = balance.places)
	rac.balance.this <- left_join(rac.balance.this, eval(parse(text = paste0("rac.place.", year))), 
			by = c("place" = "placename"))

	rac.balance.this <- select(rac.balance.this, place, C000, CE01, 
		CE02, CE03, CNS01, CNS02, CNS03, CNS04, CNS05, CNS06, CNS07, CNS08, CNS09, CNS10, CNS11, CNS12, CNS13, CNS14, 
		CNS15, CNS16, CNS17, CNS18, CNS19, CNS20, CD01, CD02, CD03, CD04)
	
	# Ensure the rows and columns match
	stopifnot(wac.balance.this$place == rac.balance.this$place)
	stopifnot(names(wac.balance.this$place) == names(rac.balance.this$place))

	# Calculate ratios used in Stoker and Ewing
	# Ranges between 0 and 1, captures imbalances in both directions
	# 1 is perfect balance
	# 0 is complete imbalance
	balance.this <- cbind("place" = wac.balance.this[, 1], 
		1 - abs(wac.balance.this[, 2:29] - rac.balance.this[, 2:29]) / 
				(wac.balance.this[, 2:29] + rac.balance.this[, 2:29]))

	# Join total workers and jobs back to the balance table
	balance.this <- left_join(balance.this, wac.balance.this[, c("place", "C000")], by = c("place" = "place"))
	
	balance.this <- left_join(balance.this, rac.balance.this[, c("place", "C000")], by = c("place" = "place"))
	
	names(balance.this)[2] <- "C000"
	names(balance.this)[30] <- "total_jobs"
	names(balance.this)[31] <- "total_residents"

	# Add a variable describing whether the jurisdiction is jobs or housing rich
	balance.this <- mutate(balance.this, jobs_rich = total_jobs > total_residents, year = year)

	# Assign the variable to the global environment
	assign(paste0("balance.", year), balance.this)
	
	## Calculate a raw jobs/employed residents ratio
	ratio.this <- cbind("place" = wac.balance.this[, 1], wac.balance.this[, 2:29] / rac.balance.this[, 2:29])
	
	# Join total workers and jobs back to the ratio table
	ratio.this <- left_join(ratio.this, wac.balance.this[, c("place", "C000")], by = c("place" = "place"))
	
	ratio.this <- left_join(ratio.this, rac.balance.this[, c("place", "C000")], by = c("place" = "place"))
	
	names(ratio.this)[2] <- "C000"
	names(ratio.this)[30] <- "total_jobs"
	names(ratio.this)[31] <- "total_residents"

	# Add a variable describing whether the jurisdiction is jobs or housing rich
	ratio.this <- mutate(ratio.this, jobs_rich = total_jobs > total_residents, year = year)

	# Assign the variable to the global environment
	assign(paste0("ratio.", year), ratio.this)
	
	# Clean up 
	rm(wac.balance.this)
	rm(rac.balance.this)
	rm(balance.this)
	rm(ratio.this)
}


# Visualize the results
# Plot jurisdictions in panels, time on the x-axis, ratios on the y

# Get top 25 places from 2011
places <- balance.2011[balance.2011$total_jobs > threshold, "place"]
places <- places[!is.na(places)]

for(year in years.to.download) { 
	
	# Balance
	balance.this <- eval(parse(text = paste0("balance.", year)))	
	top.25.this <- filter(balance.this, place %in% places)
	top.25.this <- select(top.25.this, place, C000, CE01, CE02, CE03, CD01, CD02, CD03, CD04, 
		year, total_jobs, total_residents)
		
	# Reorder place factor to descending in total jobs
	top.25.this$place <- gsub(" city, CA", "", top.25.this$place)
	top.25.this$place <- factor(top.25.this$place, levels = reorder(top.25.this$place, -top.25.this$total_jobs))
	levels(top.25.this$place)[25] <- "Uninc. Sonoma Cty."
	
	assign(paste0("top.25.", year), top.25.this)
	
	# Raw ratio
	ratio.this <- eval(parse(text = paste0("ratio.", year)))
	top.25.ratio.this <- filter(ratio.this, place %in% places)
	top.25.ratio.this <- select(top.25.ratio.this, place, C000, CE01, CE02, CE03, CD01, CD02, CD03, CD04, 
		year, total_jobs, total_residents)
	
	top.25.ratio.this$place <- gsub(" city, CA", "", top.25.ratio.this$place)
	top.25.ratio.this$place <- factor(top.25.ratio.this$place, 
		levels = reorder(top.25.ratio.this$place, -top.25.ratio.this$total_jobs))
	levels(top.25.ratio.this$place)[25] <- "Uninc. Sonoma Cty."
	
	assign(paste0("top.25.ratio.", year), top.25.ratio.this)
	
	# Clean up
	rm(balance.this)
	rm(ratio.this)
	rm(top.25.this)
	rm(top.25.ratio.this)
	
}
		
balance.merged <- rbind(
	melt(top.25.2009, id = c("place", "year", "total_jobs", "total_residents")),
	melt(top.25.2010, id = c("place", "year", "total_jobs", "total_residents")),
	melt(top.25.2011, id = c("place", "year", "total_jobs", "total_residents")))

balance.merged.educ <- filter(balance.merged, variable %in% c("CD01", "CD02", "CD03", "CD04"))
balance.merged.wage <- filter(balance.merged, variable %in% c("CE01", "CE02", "CE03"))

ratio.merged <- rbind(
	melt(top.25.ratio.2009, id = c("place", "year", "total_jobs", "total_residents")),
	melt(top.25.ratio.2010, id = c("place", "year", "total_jobs", "total_residents")),
	melt(top.25.ratio.2011, id = c("place", "year", "total_jobs", "total_residents")))

ratio.merged.educ <- filter(ratio.merged, variable %in% c("CD01", "CD02", "CD03", "CD04"))
ratio.merged.wage <- filter(ratio.merged, variable %in% c("CE01", "CE02", "CE03"))

# Plot showing change over time for the balance indicator 
# for the 25 jurisdictions with the greatest numbers of jobs in 2011

# Education level
educ <- ggplot(balance.merged.educ, aes(x = as.factor(year), y = value, color = variable)) + geom_point() + 
	# Adjust the aesthetic to correctly plot the line
	geom_line(aes(group = variable)) + 
	scale_color_brewer(palette = "Dark2", 
		labels = c("Less than high school", "High school", "Some college", "Bachelor's or above")) + 
	xlab(NULL) + ylab("symmetric job-employed resident balance (1 = balance, 0 = imbalance)") + 
	theme_bw() + theme(plot.title = element_text(face = "bold"), legend.title = element_blank(),
		legend.position = "bottom")

educ + facet_wrap(~ place) + ggtitle("Job-employed resident balance indicators by education level")

ggsave("Balance_Education.png", width = 13, height = 15, scale = 0.6)

# Wage level
wage <- ggplot(balance.merged.wage, aes(x = as.factor(year), y = value, color = variable)) + geom_point() + 
	# Adjust the aesthetic to correctly plot the line
	geom_line(aes(group = variable)) + 
	scale_color_brewer(palette = "Dark2", 
		labels = c("Low-wage", "Mid-wage", "High-wage")) + 
	xlab(NULL) + ylab("symmetric job-employed resident balance (1 = balance, 0 = imbalance)") + 
	theme_bw() + theme(plot.title = element_text(face = "bold"), legend.title = element_blank(),
		legend.position = "bottom")

wage + facet_wrap(~ place) + ggtitle("Job-employed resident balance indicators by wage level")

ggsave("Balance_Wage.png", width = 13, height = 15, scale = 0.6)


# Plot showing change over time for the ratio indicator 
# for the 25 jurisdictions with the greatest numbers of jobs in 2011
educ <- ggplot(ratio.merged.educ, aes(x = as.factor(year), y = value, color = variable)) + geom_point() + 
	# Adjust the aesthetic to correctly plot the line
	geom_line(aes(group = variable)) + 
	scale_color_brewer(palette = "Dark2", 
		labels = c("Less than high school", "High school", "Some college", "Bachelor's or above")) + 
	xlab(NULL) + ylab("Employed resident to area jobs ratio") + 
	theme_bw() + theme(plot.title = element_text(face = "bold"), legend.title = element_blank(),
		legend.position = "bottom")

educ + facet_wrap(~ place) + ggtitle("Job-worker ratio indicators by education level")

ggsave("Ratio_JER_Education.png", width = 13, height = 15, scale = 0.6)

# Wage level
wage <- ggplot(ratio.merged.wage, aes(x = as.factor(year), y = value, color = variable)) + geom_point() + 
	# Adjust the aesthetic to correctly plot the line
	geom_line(aes(group = variable)) + 
	scale_color_brewer(palette = "Dark2", 
		labels = c("Low-wage", "Mid-wage", "High-wage")) + 
	xlab(NULL) + ylab("Employed resident to area jobs ratio") + 
	theme_bw() + theme(plot.title = element_text(face = "bold"), legend.title = element_blank(),
		legend.position = "bottom")

wage + facet_wrap(~ place) + ggtitle("Job-worker ratio indicators by wage level")

ggsave("Ratio_JER_Wage.png", width = 13, height = 15, scale = 0.6)

