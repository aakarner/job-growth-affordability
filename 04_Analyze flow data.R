# Anlayze LEHD flow data

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


# Scatterplot internal capture for ALL jurisdictions
setwd("D:/Dropbox/Work/high-wage job growth/data")

# Get covariates of interest
covariates <- read.table("ACS_5yr_IncomeHousingValue.csv", sep = ",", header = TRUE, row.names = NULL)
names(covariates)[4:11] <- c("pop_e", "pop_m", "inc_e", "inc_m", "rent_e", "rent_m", "value_e", "value_m")
covariates$Geo_NAME <- gsub(", California", "", covariates$Geo_NAME)
covariates$sv <- ifelse(covariates$Geo_NAME %in% sv.places$place, 1, 0)

internal.2011 <- internal.cap.2011
internal.2011$plc <- gsub(", CA", "", internal.2011$plc)
internal.2011 <- dcast(internal.2011, plc + year ~ variable)
commutes.2011.2 <- dcast(commutes.2011, w_plc + year ~ variable)
covariates <- inner_join(internal.2011, covariates, by = c("plc" = "Geo_NAME"))
covariates <- inner_join(covariates, commutes.2011.2, by = c("plc" = "w_plc"))

write.table(covariates, "InternalCapture_2011.csv", sep = ",", row.names = FALSE)

tapply(covariates$i_S000, covariates$sv, mean)
tapply(covariates$i_SE01, covariates$sv, mean)
tapply(covariates$i_SE02, covariates$sv, mean)
tapply(covariates$i_SE03, covariates$sv, mean)

ggplot(covariates, aes(x = inc_e, y = i_S000, color = sv)) + geom_point()

ggplot(covariates, aes(x = log(pop_e), y = i_S000)) + geom_point() + geom_smooth(method = "lm")

ggplot(covariates, aes(x = inc_e, y = i_SE01)) + geom_point()


ggplot(covariates, aes(x = rent_e, y = i_SE01, color = sv)) + geom_point()
ggplot(covariates, aes(x = inc_e, y = i_S000, color = )) + geom_point()

 - highlight silicon valley - different colors
	- it seems like they have very low internal capture rates ... meaning that other people have 
 - to come in to fill the jobs. 
	- get median income of residents as a covariate? 

# Get a dotplot here. see if there are breakpoints or obvious groups
# We want to also relate internal capture to the scale of workers that need to be brought in 


# K-Means cluster analysis
kmeans <- covariates[, c(1, 7, 15, 17, 19, 21, 29)] # Low income

kmeans <- covariates[, c(1, 3, 15, 17, 19, 21, 25)]
kmeans <- kmeans[complete.cases(kmeans), ]
kmeans.fit <- scale(kmeans[, 2:7])

ssPlot <- function(data, maxCluster = 9) {
	# Initialize within sum of squares
  SSw <- (nrow(data) - 1) * sum(apply(data, 2, var))
  SSw <- vector()
  for (i in 2:maxCluster) {
  	SSw[i] <- sum(kmeans(data, centers = i)$withinss)
   }
   plot(1:maxCluster, SSw, type = "b", xlab = "Number of Clusters", ylab = "Within groups sum of squares")
}

ssPlot(kmeans.fit)

fit <- kmeans(kmeans.fit, 5) 

# append cluster assignment
places.fit <- data.frame(kmeans[, 1], fit$cluster)
places.fit$fit.cluster <- as.factor(places.fit$fit.cluster)

fit$centers

places.fit[places.fit$fit.cluster == 1, ]

table(places.fit$fit.cluster)


# Extract the nodes from the edges object

# Sankey plot county flows
edges <- as.data.frame(ungroup(select(od.2011.cty, h_cty, w_cty, S000)))
names(edges) <- c("N1", "N2", "Value")
edges$N1 <- paste0("O_", edges$N1)
edges$N2 <- paste0("D_", edges$N2)

p1 <- gvisSankey(edges, from="N1", to="N2", weight="value",
	options=list(height=250,
               sankey="{link:{color:{fill:'lightblue'}}}"))

plot(p1)

print(p1, tag = "chart", file = "Sankey_Test.html")



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
















# Write the od flows to the database
dbWriteTable(db, "od2011", od.2011)
dbWriteTable(db, "od2010", od.2010)
dbWriteTable(db, "od2009", od.2009)

# Read the full california crosswalk into the database
# We need this to crosswalk blockgroups to places
numLines <- countLines("D:\\Dropbox\\Work\\high-wage job growth\\data\\ca_xwalk_sm.csv")
monet.read.csv(db, "D:\\Dropbox\\Work\\high-wage job growth\\data\\ca_xwalk_sm.csv", "ca_xwalk", numLines, 
	locked = TRUE)

# Query commute distances ------------------------------------------------------

# Check that the blockgroups are consistent between the two datasets
# LEHD should use census 2010 definitions
# Not sure what the smart loc db uses, but hopefully that too. 
# Smart Loc DB should have more bgs than the LEHD since the LEHD only contains 
# bgs for which there are flows
dbGetQuery(db, "SELECT count(*) FROM od2009 t1
	INNER JOIN ca_smartlocdb t2
	ON t1.w_bg = t2.geoid10")

# Count number of block groups in the Bay Area to verify skims
dbGetQuery(db, "SELECT count(distinct(bgrp)) from ca_xwalk") # 23,212
dbGetQuery(db, "SELECT count(distinct(bgrp)) from ca_xwalk WHERE cty IN 
	(6001, 6013, 6041, 6055, 6075, 6081, 6085, 6095, 6097)") # 4,756

dbGetQuery(db, "SELECT count(distinct(origin_id)) from skims1") # 6,539
dbGetQuery(db, "SELECT count(distinct(origin_id)) from skims2") # 10,238

# 538,796,944 OD pairs in California
setwd("D:\\Dropbox\\Work\\JHfit\\data\\EPA road skims\\ODmat_CA")
files <- dir()
sum <- 0

for(i in files) print(i)
	
	sum <- sum + countLines(i)



# OD flows are already Bay Area-limited by their design



# We want overall average commute distances by income category by year
# and overall averages by destination place

# For overall, we just need a weighted mean
# Create a new table with the product of trips and meters (trip-meters)
# use that, in combination with the original trip table to calculate the weighted mean 
# i.e. divide by the total trips in each worker category (income, industry, etc)

dbGetQuery(db, "SELECT * FROM od2009 limit 5")

dbSendUpdate(db, "ALTER TABLE od2009 ADD COLUMN distance double")
dbSendUpdate(db, "ALTER TABLE od2009 drop COLUMN distance")

dbGetQuery(db, "SELECT count(*) FROM skims1 t1 INNER JOIN od2009 t2 ON t1.origin_id = t2.h_bg AND t1.dest_id = t2.w_bg")

dbGetQuery(db, "SELECT count(*) FROM skims2 t1 INNER JOIN od2009 t2 ON t1.origin_id = t2.h_bg AND t1.dest_id = t2.w_bg")

dbGetQuery(db, "SELECT count(*) FROM od2009 WHERE distance is not null")

# I was getting a GDK error on this query before. 
# It seems that if it's the first thing that you run, then it's fine. 
dbSendUpdate(db, "UPDATE od2009 SET distance = 
	(SELECT total_meters FROM skims1
	WHERE skims1.origin_id = od2009.h_bg AND
	od2009.w_bg = skims1.dest_id)")


dbSendUpdate(db, "UPDATE od2009 SET distance = 
	(SELECT total_meters FROM skims2
	WHERE skims2.origin_id = od2009.h_bg  AND
	od2009.w_bg = skims2.dest_id)")

# This still leaves a lot of blanks.
# The skim table only includes trips which are less than 54 miles in length.

dbGetQuery(db, "select max(total_meters) from skims7")
dbGetQuery(db, "select sum(s000) from od2009 where distance is null")
dbGetQuery(db, "select sum(s000) from od2009 where distance is not null")


# This skim exists in the skim table, but the distance wasn't updated in the OD table
dbGetQuery(db, "select max(total_meters) from skims1")
dbGetQuery(db, "select max(total_meters) from skims2")
dbGetQuery(db, "select * from skims2 where origin_id = 60014001001 and dest_id = 60070004021")

dbGetQuery(db, "select * from skims2 where origin_id = 60670001001 and dest_id = 60952532061")


# Extract OD flows by Bay Area jurisdiction ------------------------------------

dbGetQuery(db, "select * from ca_xwalk limit 5")
dbGetQuery(db, "select * from od2009 limit 5")


dbGetQuery(db, "select count(distinct(bgrp)) from ca_xwalk") # 23,212 - correct

# SQL window function trick from http://goo.gl/bQv3sP
# Allows you to perform operations on subsets of the data
# We want to create a table that contains each block group only once, along with its 
# place and county names. 
dbSendUpdate(db, "CREATE TABLE ca_xwalk_sub AS SELECT bgrp, stplcname, ctyname FROM
	(SELECT bgrp, stplcname, ctyname, ROW_NUMBER() OVER (PARTITION BY bgrp) AS rn FROM ca_xwalk) tmp 
	WHERE rn = 1 WITH DATA")
dbSendUpdate(db, "UPDATE ca_xwalk_sub SET stplcname = ctyname WHERE stplcname IS NULL")


dbSendUpdate(db, "ALTER TABLE od2009 ADD COLUMN h_place TEXT")
dbSendUpdate(db, "ALTER TABLE od2009 ADD COLUMN w_place TEXT")
dbSendUpdate(db, "ALTER TABLE od2011 ADD COLUMN h_place TEXT")
dbSendUpdate(db, "ALTER TABLE od2011 ADD COLUMN w_place TEXT")


dbSendUpdate(db, "ALTER TABLE od2009 DROP COLUMN h_place")
dbSendUpdate(db, "ALTER TABLE od2009 DROP COLUMN w_place")


dbSendUpdate(db, "UPDATE od2009 SET h_place = (SELECT stplcname FROM ca_xwalk_sub 
	WHERE od2009.h_bg = ca_xwalk_sub.bgrp)")
dbSendUpdate(db, "UPDATE od2009 SET w_place = (SELECT stplcname FROM ca_xwalk_sub 
	WHERE od2009.w_bg = ca_xwalk_sub.bgrp)")

dbSendUpdate(db, "UPDATE od2011 SET h_place = (SELECT stplcname FROM ca_xwalk_sub 
	WHERE od2011.h_bg = ca_xwalk_sub.bgrp)")
dbSendUpdate(db, "UPDATE od2011 SET w_place = (SELECT stplcname FROM ca_xwalk_sub 
	WHERE od2011.w_bg = ca_xwalk_sub.bgrp)")


dbSendUpdate(db, "CREATE TABLE placeflows2009 AS 
	SELECT h_place, w_place, sum(s000) AS s000, sum(sa01) AS sa01, sum(sa02) AS sa02, sum(sa03) AS sa03, 
	sum(se01) AS se01, sum(se02) AS se02, sum(se03) AS se03, sum(si01) AS si01, sum(si02) AS si02, sum(si03) AS si03
	FROM od2009
	GROUP BY h_place, w_place
	WITH DATA")

flows.2009 <- dbGetQuery(db, "SELECT * FROM placeflows2009")
write.table(flows.2009, "BayAreaCommutes_2009.csv", sep = ",", row.names = FALSE)


dbSendUpdate(db, "CREATE TABLE placeflows2011 AS 
	SELECT h_place, w_place, sum(s000) AS s000, sum(sa01) AS sa01, sum(sa02) AS sa02, sum(sa03) AS sa03, 
	sum(se01) AS se01, sum(se02) AS se02, sum(se03) AS se03, sum(si01) AS si01, sum(si02) AS si02, sum(si03) AS si03
	FROM od2011
	GROUP BY h_place, w_place
	WITH DATA")

flows.2011 <- dbGetQuery(db, "SELECT * FROM placeflows2011")
write.table(flows.2011, "BayAreaCommutes_2011.csv", sep = ",", row.names = FALSE)


# Rush job to get internal capture for a few places prior to a meeting
dbGetQuery(db, "SELECT h_place, SUM(s000), SUM(se01), SUM(se02) FROM placeflows2011 WHERE h_place = 'Cupertino city, CA' GROUP BY h_place")
dbGetQuery(db, "SELECT w_place, SUM(s000), SUM(se01), SUM(se02) FROM placeflows2011 WHERE w_place = 'Cupertino city, CA' AND h_place = 'Cupertino city, CA' GROUP BY w_place")


dbGetQuery(db, "SELECT w_place, SUM(s000), SUM(se01), SUM(se02) FROM placeflows2009 WHERE w_place = 'Cupertino city, CA' GROUP BY w_place")
dbGetQuery(db, "SELECT w_place, SUM(s000), SUM(se01), SUM(se02) FROM placeflows2009 WHERE w_place = 'Cupertino city, CA' AND h_place = 'Cupertino city, CA' GROUP BY w_place")
