# Effect of high-wage job growth on housing demand on the San Francisco Bay Area
# Analysis using the Longitudinal Employer-Household Dynamics Origin-Destination
# Employment Statistics (LODES) data 2008-2011 and the American Community Survey
#
# Alex Karner, alex.karner@asu.edu
# Chris Benner, ccbenner@ucdavis.edu
#
# Purpose:
# This script adds distance skims to the LODES OD flow matrices using
# TAZ-level MTC data for origins and destinations located within the Bay Area,
# and county-county Google Maps skims for origins located outside the Bay Area. 
# 
# Output:
# One .RData file containing place-place skims for origins and destinations in 
# the Bay Area and county-county skims for the rest of the state.

library(MonetDB.R)
library(ggmap)

# Set your working directory. 
# The skim database will be stored here. 

# Uncomment this line by removing the '#' in front..
# setwd("C:/My Directory/LEHD")
# .. in order to set your current working directory.
# setwd("D:/Dropbox/Work/high-wage job growth")

# Create database --------------------------------------------------------------
# (these commands only need to be executed once)
# For more information on using MonetDB, see:
# https://github.com/ajdamico/usgsd/blob/master/MonetDB/monetdb%20installation%20instructions.R
#
# ONLY RUN ONCE: create a monetdb executable (.bat) file for the MTC travel data
# batfile <-
# 	monetdb.server.setup(
# 		
# 		# set the path to the directory where the initialization batch file and all data will be stored
# 		database.directory = paste0(getwd(), "data/flow data/MonetDB"),
# 		# must be empty or not exist
# 		
# 		# find the main path to the monetdb installation program
# 		monetdb.program.path = "C:/Program Files/MonetDB/MonetDB5",
# 		
# 		# choose a database name
# 		dbname = "flowdata",
# 		
# 		# choose a database port
# 		# this port should not conflict with other monetdb databases
# 		# on your local computer. two databases with the same port number
# 		# cannot be accessed at the same time
# 		dbport = 57000
# 	)

# Connect to database
batfile <- paste0(getwd(), "/data/flow data/MonetDB/flowdata.bat")
pid <- monetdb.server.start(batfile)
dbname <- "flowdata" 
dbport <- 57000
drv <- dbDriver("MonetDB")
monet.url <- paste0("monetdb://localhost:", dbport, "/", dbname)
db <- dbConnect(drv, monet.url, "monetdb", "monetdb")

# Disconnect from database
dbDisconnect(db)
monetdb.server.stop(pid)

# Load county and place OD flow data 
load("data/BayAreaLEHD_od_FINAL.RData")

# Query commute distances ------------------------------------------------------

# These distances were created from a variety of sources, including:
#
# 1. MTC's travel model skims for origins and destinations within the Bay Area. 
# These are esimates of distances traveled 
# on the roadway network between traffic analysis zones for 2010. I generated a 
# place-place origin destination matrix by identifying the population-weighted 
# centroid of each census place, using populations in blocks as the weights, 
# and associating that point with a TAZ. Place-place skims
# within the Bay Area can this be thought of as TAZ-TAZ skims. 
# 
# 2. Google Maps queries for origins and destinations outside the Bay Area.
# These trips use population-weighted *county* centroids at both the origin 
# and destination end. The rationale is that the county-county distances are going 
# to be much larger than the difference associated with using place-place flows. 
# Also it cuts down the number of required queries substantially. 

# Population-weighted place centorid associated with a TAZ
places.pt <- read.table("data/MTC_2010_places_pt_wTAZ.csv", sep = ",", header = TRUE, row.names = NULL)
places.pt <- places.pt[, c("NAMELSAD10", "TAZ1454")]
names(places.pt) <- c("o_place", "o_taz")

places <- rep(places.pt$o_place, 228)
tazs <- rep(places.pt$o_taz, 228)

# Build the shell of the OD table
bay.area.od <- data.frame("o_place" = places, "o_taz" = tazs)
temp.od <- data.frame("d_place" = places, "d_taz" = tazs)
temp.od <- temp.od[order(temp.od$d_place), ]
row.names(temp.od) <- 1:(228^2)

bay.area.od <- cbind(bay.area.od, temp.od)

# Write it to the database
dbWriteTable(db, "bay_area_od", bay.area.od)

# Read MTC skims into the database
monet.read.csv(db, "MTC_2010_skims.csv", "mtc_skims_taz", 1454^2, locked = TRUE)

# MTC has no intrazonal skims. We developed our own estimates assuming the radius of a
# circle with the area of the census place. We use these distances to measure the distance 
# of internally-capture commutes. 
# TODO: Estimate internal commute distance using the actual blockgroup-blockgroup distances
# combined with the EPA skim database. 
monet.read.csv(db, "BayAreaInternalCommutes.csv", "mtc_internal_commutes", 220, locked = TRUE)

dbSendUpdate(db, "ALTER TABLE bay_area_od ADD COLUMN skim double")

dbSendUpdate(db, "UPDATE bay_area_od SET skim = 
	(SELECT da FROM mtc_skims_taz
	WHERE bay_area_od.o_taz = mtc_skims_taz.orig 
	AND bay_area_od.d_taz = mtc_skims_taz.dest)")

# Update the internal commute distance
dbSendUpdate(db, "UPDATE bay_area_od SET skim = 
	(SELECT int_trip FROM mtc_internal_commutes
	WHERE bay_area_od.o_place = mtc_internal_commutes.namelsad10
	AND bay_area_od.o_place = bay_area_od.d_place)")

dbGetQuery(db, "SELECT count(*) FROM bay_area_od WHERE skim is null")

bay.area.od <- dbGetQuery(db, "SELECT * FROM bay_area_od")

# Many work trips originate outside of the Bay Area. For these trips, estimate distance
# from county centroid to county centroid. 

# Build list of needed places
places.to.skim <- rbind(select(ungroup(od.2008.place), h_plc, w_cty), select(ungroup(od.2009.place), h_plc, w_cty), 
	select(ungroup(od.2010.place), h_plc, w_cty), select(ungroup(od.2011.place), h_plc, w_cty))
places.to.skim$ID <- paste0(places.to.skim$h_plc, places.to.skim$w_cty)

places.to.skim <- places.to.skim[!duplicated(places.to.skim$ID), ]
places.to.skim <- select(places.to.skim, h_plc, w_cty)
places.to.skim$h_plc <- as.integer(places.to.skim$h_plc) # Bay Area places generate NAs
places.to.skim <- places.to.skim[!is.na(places.to.skim$h_plc), ]

cty.centers <- read.table("data/CountyMeanCenters.csv", sep = ",", header = TRUE, row.names = NULL)

places.to.skim <- left_join(places.to.skim, 
	select(cty.centers, fipsstco, POINT_X, POINT_Y), by = c("h_plc" = "fipsstco"))
names(places.to.skim)[3:4] <- c("h_x", "h_y")

# Revert county names to geoids for join
places.to.skim$w_cty[places.to.skim$w_cty == "Alameda"] <- 6001
places.to.skim$w_cty[places.to.skim$w_cty == "Contra Costa"] <- 6013
places.to.skim$w_cty[places.to.skim$w_cty == "Marin"] <- 6041
places.to.skim$w_cty[places.to.skim$w_cty == "Napa"] <- 6055
places.to.skim$w_cty[places.to.skim$w_cty == "San Francisco"] <- 6075
places.to.skim$w_cty[places.to.skim$w_cty == "San Mateo"] <- 6081
places.to.skim$w_cty[places.to.skim$w_cty == "Santa Clara"] <- 6085
places.to.skim$w_cty[places.to.skim$w_cty == "Solano"] <- 6095
places.to.skim$w_cty[places.to.skim$w_cty == "Sonoma"] <- 6097

places.to.skim$w_cty <- as.integer(places.to.skim$w_cty)

places.to.skim <- left_join(places.to.skim, 
	select(cty.centers, fipsstco, POINT_X, POINT_Y), by = c("w_cty" = "fipsstco"))

names(places.to.skim)[5:6] <- c("w_x", "w_y")
places.to.skim$meters <- 0
places.to.skim$miles <- 0
places.to.skim$time <- 0

for(i in 1:nrow(places.to.skim)) {
	this.skim <- mapdist(c(as.numeric(places.to.skim[i, "h_x"]), as.numeric(places.to.skim[i, "h_y"])),
		c(as.numeric(places.to.skim[i, "w_x"]), as.numeric(places.to.skim[i, "w_y"])))
	
	places.to.skim[i, "meters"] <- this.skim$m
	places.to.skim[i, "miles"] <- this.skim$miles
	places.to.skim[i, "time"] <- this.skim$minutes
}

# Save the skims
save(list = c("places.to.skim", "bay.area.od"), file = "data/MTCandCountySkims_Google.RData")
