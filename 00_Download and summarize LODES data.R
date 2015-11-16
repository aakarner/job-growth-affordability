# Effect of high-wage job growth on housing demand on the San Francisco Bay Area
# Analysis using the Longitudinal Employer-Household Dynamics Origin-Destination
# Employment Statistics (LODES) data 2008-2013 and the American Community Survey
#
# Alex Karner, alex.karner@coa.gatech.edu
# Chris Benner, cbenner@ucsc.edu
#
# Purpose:
# This script downloads the workplace and residence area characteristics and OD flow
# data from the LODES data for the requested years.
#
# Output:
# Once downloaded, summary tables are created for the WAC and RAC files by census place
# for all census places and unincorporated areas in the Bay Area. 
# Flow data are aggregated from the block to the census place level for all destinations
# internal to the nine-county Bay Area. Counties are used for origins outside of the Bay Area.
# Each rac, wac, and od file for each year is output to an .RData file in the 
# working directory.
#
# Inspiration and some code from Anthony Damico's analyze survey data project:
# https://github.com/ajdamico/usgsd

# Set your working directory. 
# The LODES data files will be stored here. 

# Uncomment this line by removing the '#' in front..
# setwd("C:/My Directory/LEHD")
# .. in order to set your current working directory.
# setwd("D:/Dropbox/Work/high-wage job growth")

# Variable definitions --------------------------------------------------------

# Define which years to download

# For the present analysis, we're interested in all post-recession years
years.to.download <- c(2008, 2009, 2010, 2011, 2012, 2013)

options(scipen = 999) # Supress scientific notation so we can see census geocodes

library(plyr); library(dplyr)
library(downloader) # downloads and then runs the source() function on scripts from github
library(R.utils) # load the R.utils package (counts the number of lines in a file quickly)

# Program start ----------------------------------------------------------------

# Create a temporary file and a temporary directory
tf <- tempfile(); td <- tempdir()

# Load the download.cache and related functions
# to prevent re-downloading of files once they've been downloaded.
source_url( 
	"https://raw.github.com/ajdamico/asdfree/master/Download%20Cache/download%20cache.R", 
	prompt = FALSE, 
	echo = FALSE 
)

# Loop through and download each year specified by the user
for(year in years.to.download) {
	
	cat("now loading", year, "...", '\n\r')
	
	# Data import: workplace area characteristics (i.e. job location data)

	# Download each year of data
	# Zipped file to the temporary file on your local disk
	# Naming conventions for LEHD data are described here: http://goo.gl/FnNRoa
	# S000 references all workforce segments
	# JT00 references all job types
	download_cached( 
		url = paste0("http://lehd.ces.census.gov/data/lodes/LODES7/ca/wac/ca_wac_S000_JT00_", year, ".csv.gz"), 
		destfile = tf, 
		mode = 'wb'
	)

	# Create a variable to store the wac file for each year
	assign(paste0("wac.", year), read.table(gzfile(tf), header = TRUE, sep = ",", 
		colClasses = "numeric", stringsAsFactors = FALSE))
	
	# Remove the temporary file from the local disk
	file.remove(tf)
	
	# And free up RAM
	gc()
	
	# Data import: residence area characteristics (i.e. job location data)
	download_cached( 
		url = paste0("http://lehd.ces.census.gov/data/lodes/LODES7/ca/rac/ca_rac_S000_JT00_", year, ".csv.gz"), 
		destfile = tf, 
		mode = 'wb'
	)

	# Create a variable to store the rac file for each year
	assign(paste0("rac.", year), read.table(gzfile(tf), header = TRUE, sep = ",", 
		colClasses = "numeric", stringsAsFactors = FALSE))
	
	# Remove the temporary file from the local disk
	file.remove(tf)
	
	# And free up RAM
	gc()

	# Data import: od flow data
	download_cached( 
		url = paste0("http://lehd.ces.census.gov/data/lodes/LODES7/ca/od/ca_od_main_JT00_", year, ".csv.gz"), 
		destfile = tf, 
		mode = 'wb'
	)
	
	# Create a variable to store the od file for each year
	assign(paste0("od.", year), read.table(gzfile(tf), header = TRUE, sep = ",", 
		colClasses = "numeric", stringsAsFactors = FALSE))
	
	# Remove the temporary file from the local disk
	file.remove(tf)
	
	# And free up RAM
	gc()
	
}
	
# Download the geographic crosswalk file for California
download_cached( 
	url = paste0("http://lehd.ces.census.gov/data/lodes/LODES7/ca/ca_xwalk.csv.gz"), 
	destfile = tf, 
	mode = 'wb'
)

xwalk <- read.table(gzfile(tf), header = TRUE, sep = ",", stringsAsFactors = FALSE)

# Remove the temporary file from the local disk
file.remove(tf)

# Make sure that the files were downloaded correctly
# and that all blocks are represented in the crosswalk file
for(year in years.to.download)
	stopifnot(sum(paste0("wac.", year, "$w_geocode") %in% xwalk$tabblk2010) == dim(paste0("wac.", year))[1])

# Each year of the LEHD data for the rac, wac, and crosswalk file are now imported.

# Summarize rac and wac data by jurisdiction

# Merge geographic identifier on county and place into each year of LEHD data
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
	# and the county name, if there's not
	assign(paste0("wac.", year), with(eval(parse(text = paste0("wac.", year))), 
		transform(eval(parse(text = paste0("wac.", year))), 
			placename = ifelse(stplcname == "", paste0("Unincorporated ", ctyname), stplcname))))
	
	assign(paste0("rac.", year), with(eval(parse(text = paste0("rac.", year))), 
		transform(eval(parse(text = paste0("rac.", year))), 
			placename = ifelse(stplcname == "", paste0("Unincorporated ", ctyname), stplcname))))
}

# Create summary tables by jurisdiction by year
# Keep both place name and county name for ease of filtering below
for(year in years.to.download) { 
	
	assign(paste0("wac.place.", year), ddply(eval(parse(text = paste0("wac.", year))), 
		.(placename, ctyname), numcolwise(sum)))
	
	assign(paste0("rac.place.", year), ddply(eval(parse(text = paste0("rac.", year))), 
		.(placename, ctyname), numcolwise(sum)))
}

# Extract Bay Area jurisdictions only for the rac and wac files 
# Use filter() for readability and ease
for(year in years.to.download)
	for(j in c("wac", "rac"))
		assign(paste0(j, ".place.", year), filter(eval(parse(text = paste0(j, ".place.", year))),
			ctyname %in% c("Alameda County, CA", "Contra Costa County, CA", "Marin County, CA", "Napa County, CA", 
			"San Francisco County, CA", "Santa Clara County, CA", "San Mateo County, CA", "Solano County, CA", 
				"Sonoma County, CA")))

# Save rac and wac data output so that it may be reloaded easily.
# Keep the rac/wac and od files separate. 
save(list = c("years.to.download", paste0("rac.place.", years.to.download), 
	paste0("wac.place.", years.to.download)), file = "BayAreaLEHD.RData")

# The crosswalk file contains two blockgroups that don't actually exist in 
# California. I have emailed the Census Bureau about them. They are 60371370001 
# and 60371370002. The blocks that correspond to these entries in the crosswalk 
# have the wrong prefix, meaing that the blockgroup labels are definitely
# incorrect. Further, there are two blockgroups missing from the crosswalk,
# and the block prefixes match those. To correct, simply re-map the blockgroup 
# prefix for these blocks
xwalk$bgrp[xwalk$bgrp == 60371370001] <- substr(xwalk$tabblk2010[xwalk$bgrp == 60371370001], 1, 11)
xwalk$bgrp[xwalk$bgrp == 60371370002] <- substr(xwalk$tabblk2010[xwalk$bgrp == 60371370002], 1, 11)

# Check
# xwalk[xwalk$bgrp == 60371370001, ] # No records left
# xwalk[xwalk$bgrp == 60371370002, ] # No records left

# xwalk[xwalk$bgrp == 60378002043, ] # Correct
# xwalk[xwalk$bgrp == 60379304011, ] # Correct

save(list = c("years.to.download", paste0("od.", years.to.download), "xwalk"), file = "CA_LEHD_od_blocks.RData")

# Aggregate flow data ----------------------------------------------------------

bay.cty.name <- c("Alameda", "Contra Costa", "Marin", "Napa", "San Francisco", "San Mateo", 
	"Santa Clara", "Solano", "Sonoma")
bay.cty.num <- c("6001", "6013", "6041", "6055", "6075", "6081", "6085", "6095", "6097")

# Get only needed columns from the crosswalk
xwalk.sub <- select(xwalk, tabblk2010, cty, ctyname, stplcname)
xwalk.sub <- tbl_df(xwalk.sub)

# Collapse flow data to the county- and place-level for the Bay Area
for(year in years.to.download) {
	
	this.year <- eval(parse(text = paste0("od.", year)))
	this.year <- tbl_df(this.year)
	this.year <- mutate(this.year, 
		w_cty = as.integer(substr(w_geocode, 1, 4)), h_cty = as.integer(substr(h_geocode, 1, 4)))
	
	# Return only flows where the work location is in the Bay Area
	this.year <- filter(this.year, w_cty %in% bay.cty.num)
		# h_cty %in% c("001", "013", "041", "055", "075", "081", "085", "095", "097"))
	
	# this.year$w_cty[!this.year$w_cty %in% c("001", "013", "041", "055", "075", "081", "085", "095", "097")] <- "Other"
	# this.year$h_cty[!this.year$h_cty %in% c("001", "013", "041", "055", "075", "081", "085", "095", "097")] <- "Other"
	
	# Make readable county names for labeling unincorporated areas
	this.year$w_cty_l[this.year$w_cty == "6001"] <- "Alameda"
	this.year$h_cty_l[this.year$h_cty == "6001"] <- "Alameda"
	this.year$w_cty_l[this.year$w_cty == "6013"] <- "Contra Costa"
	this.year$h_cty_l[this.year$h_cty == "6013"] <- "Contra Costa"
	this.year$w_cty_l[this.year$w_cty == "6041"] <- "Marin"
	this.year$h_cty_l[this.year$h_cty == "6041"] <- "Marin"
	this.year$w_cty_l[this.year$w_cty == "6055"] <- "Napa"
	this.year$h_cty_l[this.year$h_cty == "6055"] <- "Napa"
	this.year$w_cty_l[this.year$w_cty == "6075"] <- "San Francisco"
	this.year$h_cty_l[this.year$h_cty == "6075"] <- "San Francisco"
	this.year$w_cty_l[this.year$w_cty == "6081"] <- "San Mateo"
	this.year$h_cty_l[this.year$h_cty == "6081"] <- "San Mateo"
	this.year$w_cty_l[this.year$w_cty == "6085"] <- "Santa Clara"
	this.year$h_cty_l[this.year$h_cty == "6085"] <- "Santa Clara"
	this.year$w_cty_l[this.year$w_cty == "6095"] <- "Solano"
	this.year$h_cty_l[this.year$h_cty == "6095"] <- "Solano"
	this.year$w_cty_l[this.year$w_cty == "6097"] <- "Sonoma"
	this.year$h_cty_l[this.year$h_cty == "6097"] <- "Sonoma"
	
	# Add in place names for home and work location
	this.year <- left_join(this.year, select(xwalk.sub, tabblk2010, stplcname), by = c("w_geocode" = "tabblk2010"))
	names(this.year)[18] <- "w_plc"
	
	this.year <- left_join(this.year, select(xwalk.sub, tabblk2010, stplcname), by = c("h_geocode" = "tabblk2010"))
	names(this.year)[19] <- "h_plc"
	
	# Add identifiers for unincorporated areas in the Bay
	this.year$w_plc <- ifelse(this.year$w_plc == "" & this.year$w_cty %in% bay.cty.num, 
		paste0("Unincorporated ", this.year$w_cty_l, " county"), this.year$w_plc)
	
	this.year$h_plc <- ifelse(this.year$h_plc == "" & this.year$h_cty %in% bay.cty.num,
		paste0("Unincorporated ", this.year$h_cty_l, " county"), this.year$h_plc)
	
	# Copy county identifiers to the place for origins outside the Bay Area
	this.year$h_plc <- ifelse(!this.year$h_cty %in% bay.cty.num, this.year$h_cty, this.year$h_plc)
	
	# County-level summaries - NOT NEEDED RIGHT NOW
	# 	this.year.cty <- group_by(this.year, h_cty, w_cty)
	# 	
	# 	# Reduce the commute flow data to the county level
	# 	# Flow variables prefixed with S, block group identifers postfixed with _bg
	# 	this.year.cty <- summarize(this.year.cty, 
	# 		S000 = sum(S000),
	# 		SA01 = sum(SA01), SA02 = sum(SA02), SA03 = sum(SA03),
	# 		SE01 = sum(SE01), SE02 = sum(SE02), SE03 = sum(SE03),
	# 		SI01 = sum(SI01), SI02 = sum(SI02), SI03 = sum(SI03))
	# 	
	# 	# QC - do flow totals match in both tables?
	# 	stopifnot(identical(ddply(this.year[, grep("S|_bg", names(this.year))], .(), numcolwise(sum)),
	# 	ddply(this.year.cty[, grep("S|_bg", names(this.year.cty))], .(), numcolwise(sum))))
	
	# Place-level summaries
	this.year.place <- group_by(this.year, h_plc, w_plc, w_cty)
	
	this.year.place <- summarize(this.year.place, 
		S000 = sum(S000),
		SA01 = sum(SA01), SA02 = sum(SA02), SA03 = sum(SA03),
		SE01 = sum(SE01), SE02 = sum(SE02), SE03 = sum(SE03),
		SI01 = sum(SI01), SI02 = sum(SI02), SI03 = sum(SI03))
	
	rm(this.year)

	this.year.place$w_plc <- gsub(", CA", "", this.year.place$w_plc)
	this.year.place$h_plc <- gsub(", CA", "", this.year.place$h_plc)
	
	# assign(paste0("od.", year, ".cty"), this.year.cty)
	assign(paste0("od.", year, ".place"), this.year.place)
	
	# rm(this.year.cty)
	rm(this.year.place)
	
	gc()
	
}

# Add in tier 1 + tier 2 job flow
for(year in years.to.download)
	assign(paste0("od.", year, ".place"), mutate(eval(parse(text = paste0("od.", year, ".place"))), t1t2 = SE01 + SE02))

# Save the results to disk
save(list = c(paste0("od.", years.to.download, ".place"), "years.to.download"), 
	file = "data/BayAreaLEHD_od_FINAL.RData")

# Save entire workspace, for good measure
# This is about 600 MB
save.image(file = "data/LEHD_FINAL_all.RData")

# Validation -------------------------------------------------------------------
# Validate flow data against LEHD on the map website.

# The flow data for California by definition only includes origins and 
# destinations in state. This means that we won't be able to validate flows
# that include cross-state movements. 
# We can validate the internal capture of the state, however.

sum(od.2011$S000) # 14556888 - Correct
sum(od.2010$S000) # 14363254 - Correct
sum(od.2009$S000) # 14037387 - Correct
sum(od.2008$S000) # 14593087 - Correct

# We can validate the data by comparing the internal capture numbers
# to the LEHD on the map tool. 
# All counties match for 2011 except Santa Clara and San Mateo which 
# indicates that the on the map tool might use a slightly different
# crosswalk file than provided online.
# Simlarly, a number of census place results differ here from on the map

# Extract internal capture data for Cupertino from both the database and R
cup.r <- od.2011.place[od.2011.place$h_plc == 'Cupertino city, CA' & od.2011.place$w_plc == 'Cupertino city, CA', ]