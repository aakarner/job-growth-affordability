# Effect of high-wage job growth on housing demand on the San Francisco Bay Area
# Analysis using the Longitudinal Employer-Household Dynamics dataset 2009-2011

# Alex Karner, alex.karner@asu.edu
# Chris Benner, ccbenner@ucdavis.edu


# Set your working directory. 
# The LEHD data files will be stored here. 

# Uncomment this line by removing the '#' in front..
# setwd("C:/My Directory/LEHD")
# .. in order to set your current working directory.

# Define which years to download

# For the present analysis, we're interested in all post-recession years
# Later census updates will include more recent years of data
years.to.download <- c(2009, 2010, 2011)

# ------------------------------------------------------------------------------
# Program start
# ------------------------------------------------------------------------------

options(scipen = 999) # Supress scientific notation so we can see census geocodes

library(downloader) # downloads and then runs the source() function on scripts from github
library(R.utils) # load the R.utils package (counts the number of lines in a file quickly)

# create a temporary file and a temporary directory..
tf <- tempfile(); td <- tempdir()

# load the download.cache and related functions
# to prevent re-downloading of files once they've been downloaded.
source_url( 
	"https://raw.github.com/ajdamico/usgsd/master/Download%20Cache/download%20cache.R", 
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
	download.cache( 
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
	download.cache( 
		url = paste0("http://lehd.ces.census.gov/data/lodes/LODES7/ca/rac/ca_rac_S000_JT00_", year, ".csv.gz"), 
		destfile = tf, 
		mode = 'wb'
	)

	# Create a variable to store the wac file for each year
	assign(paste0("rac.", year), read.table(gzfile(tf), header = TRUE, sep = ",", 
		colClasses = "numeric", stringsAsFactors = FALSE))
	
	# Remove the temporary file from the local disk
	file.remove(tf)
	
	# And free up RAM
	gc()
}
	
# Download the geographic crosswalk file for California
download.cache( 
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

# Each year of the LEHD data for the WAC and the crosswalk file are now imported.

# Now summarize by jurisdiction

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
	# and the county name, if there's not
	assign(paste0("wac.", year), with(eval(parse(text = paste0("wac.", year))), 
		transform(eval(parse(text = paste0("wac.", year))), 
			placename = ifelse(stplcname == "", paste0("Unincorporated ", ctyname), stplcname))))
	
	assign(paste0("rac.", year), with(eval(parse(text = paste0("rac.", year))), 
		transform(eval(parse(text = paste0("rac.", year))), 
			placename = ifelse(stplcname == "", paste0("Unincorporated ", ctyname), stplcname))))
}

# Create summary tables by jurisdiction by year
# Keep both place name and county name for ease of filtering
# later on
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


# Save the output so that it may be reloaded easily. 
save.image("BayAreaLEHD.RData")