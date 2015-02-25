# Create summaries by jurisdcition

# Alex Karner, alex.karner@asu.edu
# Chris Benner, ccbenner@ucdavis.edu

library(dplyr) # use split-apply-combine logic to data frames
library(rgdal) # interface for the Geospatial Abstraction Library
library(rgeos)
library(ggplot2)
library(ggmap) # add base images to maps

# For the present analysis, we're interested in all post-recession years
# Later census updates will include more recent years of data
years.to.download <- c(2009, 2010, 2011)

# ------------------------------------------------------------------------------
# Program start
# ------------------------------------------------------------------------------

# Merge geographic identifier to each year of LEHD data
for(year in years.to.download) { 
	assign(paste0("wac_", year), merge(eval(parse(text = paste0("wac_", year))), 
		xwalk[, c("tabblk2010", "ctyname", "stplcname")], by.x = "w_geocode", by.y = "tabblk2010"))
}

# Add an identifier for unincorporated areas 
# TODO: Make this assignment generic
levels(wac_2009$stplcname)[1] <- "Unincorporated"
levels(wac_2010$stplcname)[1] <- "Unincorporated"
levels(wac_2011$stplcname)[1] <- "Unincorporated"

# Create summary tables by jurisdiction by year
for(year in years.to.download)
	assign(paste0("wac_place_", year), ddply(eval(parse(text = paste0("wac_", year))), 
		.(stplcname, ctyname), numcolwise(sum)))

# Extract Bay Area jurisdictions only
for(year in years.to.download)
	assign(paste0("wac_place_", year), eval(parse(text = paste0("wac_place_", year, "[wac_place_", year, 
	"$ctyname %in% c(\"Alameda County, CA\", \"Contra Costa County, CA\", \"Marin County, CA\", \"Napa County, CA\", 
	\"San Francisco, CA\", \"Santa Clara County, CA\", \"San Mateo County, CA\", 
		\"Solano County, CA\", \"Sonoma County, CA\"), ]"))))

# Create absolute annual and average (3 year) growth numbers for each jurisdiction
# for workers in each income category.
# Naming convention is as follows: wac.year1.year2 where year1 > year 2

# Identify all common places
all.places <- union(union(wac_place_2011$stplcname, wac_place_2010$stplcname), wac_place_2009$stplcname)

# Create three new (empty) data frames with only common rows between all years
wac_place_2009.v2 <- data.frame("stplcname" = all.places)
wac_place_2010.v2 <- data.frame("stplcname" = all.places)
wac_place_2011.v2 <- data.frame("stplcname" = all.places)

# Populate these data frames using merge (rows with missing info will just get NA)
wac_place_2009.v2 <- merge(wac_place_2009.v2, wac_place_2009, all = TRUE)
wac_place_2010.v2 <- merge(wac_place_2010.v2, wac_place_2010, all = TRUE)
wac_place_2011.v2 <- merge(wac_place_2011.v2, wac_place_2011, all = TRUE)

# Error checking
stopifnot(wac_place_2009.v2$stplcname == wac_place_2010.v2$stplcname & 
		wac_place_2009.v2$ctyname == wac_place_2010.v2$ctyname)

# This check fails because...
stopifnot(wac_place_2011.v2$stplcname == wac_place_2010.v2$stplcname & 
		wac_place_2011.v2$ctyname == wac_place_2010.v2$ctyname)
#...Sereno Del Mar, CDP is in the 2009 and 2010 WAC, but not 2011

# Take the difference to generate absolute growth
wac.2011.2010 <- cbind(wac_place_2011.v2[, 4:54] - wac_place_2010.v2[, 4:54], wac_place_2011.v2[, 1:3])
wac.2010.2009 <- cbind(wac_place_2010.v2[, 4:54] - wac_place_2009.v2[, 4:54], wac_place_2011.v2[, 1:3])

# Calculate percentage differences
wac.2011.2010.pct <- cbind((wac_place_2011.v2[, 4:54] - wac_place_2010.v2[, 4:54])/wac_place_2010.v2[, 4:54], 
	wac_place_2011.v2[, 1:3])
wac.2010.2009.pct <- cbind((wac_place_2010.v2[, 4:54] - wac_place_2009.v2[, 4:54])/wac_place_2009.v2[, 4:54], 
	wac_place_2011.v2[, 1:3])

# Remove the NA row
wac.2011.2010 <- wac.2011.2010[!is.na(wac.2011.2010[, 1]), ]
wac.2011.2010.pct <- wac.2011.2010.pct[!is.na(wac.2011.2010.pct[, 1]), ]

# Output results
write.table(wac.2011.2010, "wac_2011_2010.csv", sep = ",", row.names = FALSE)

# Map resuls -------------------------------------------------------------------

# Shapefile of 2010 California Census places.
# Careful about how you define the path and layer. I always find this odd.

places <- readOGR("GIS/CA Place (2010)", layer = "tl_2010_06_place10")

# Extract only Bay Area counties
counties <- readOGR("GIS/CA Counties (2010)", layer = "tl_2010_06_county10")
counties <- counties[counties$GEOID10 %in% 
	c("06001", "06013", "06041", "06055", "06075", "06081", "06085", "06095", "06097"), ]

places.bay <- gIntersects(places, counties, byid = TRUE)
clipped <- apply(places.bay == FALSE, 2, all)
places.bay <- places[which(!clipped), ]

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

basemap <- ggmap(get_map(location = b, source = "stamen", maptype = "toner", crop = TRUE))
places.box.f <- fortify(places.box, region = "GEOID10")
places.box.f <- merge(places.box.f, places.box@data, by.x = "id", by.y = "GEOID10")

# Plot the map
basemap + geom_polygon(data = places.box.f, aes(x = long, y = lat, group = group), alpha = 0.5)
