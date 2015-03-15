# Anlayze LEHD flow data

# Alex Karner, alex.karner@asu.edu
# Chris Benner, ccbenner@ucdavis.edu

# Set your working directory. 
# The LEHD data files will be stored here. 

# Uncomment this line by removing the '#' in front..
# setwd("C:/My Directory/LEHD")
# .. in order to set your current working directory.

options(scipen = 999) # Supress scientific notation so we can see census geocodes

library(plyr); library(dplyr)

# Load previously saved dataset
load("BayAreaLEHD_od.RData")

# ------------------------------------------------------------------------------
# Program start
# ------------------------------------------------------------------------------

# Collapse flow data to the blockgroup level
# for the Bay Area
for(year in years.to.download) {
	
	this.year <- eval(parse(text = paste0("od.", year)))
	this.year <- mutate(this.year, w_cty = substr(w_geocode, 2, 4), h_cty = substr(h_geocode, 2, 4))
	
	# Return only flows where either the home or work location is in the Bay Area
	this.year <- filter(this.year,
		w_cty %in% c("001", "013", "041", "055", "075", "081", "085", "095", "097") |
		h_cty %in% c("001", "013", "041", "055", "075", "081", "085", "095", "097"))
	
	# Add FIPS code for the home and work block group
	this.year <- mutate(this.year, 
		w_bg = substr(w_geocode, 1, 11), h_bg = substr(h_geocode, 1, 11))
	
	this.year <- group_by(this.year, h_bg, w_bg)
	
	# Reduce the commute flow data to the blockgroup level
	# Flow variables prefixed with S, blog group identifers postfixed with _bg
	this.year.bg <- summarize(this.year, 
		S000 = sum(S000),
		SA01 = sum(SA01), SA02 = sum(SA02), SA03 = sum(SA03),
		SE01 = sum(SE01), SE02 = sum(SE02), SE03 = sum(SE03),
		SI01 = sum(SI01), SI02 = sum(SI02), SI03 = sum(SI03))
	
	# QC - do flow totals match in both tables?
	stopifnot(identical(ddply(this.year[, grep("S|_bg", names(this.year))], .(), numcolwise(sum)),
	ddply(this.year.bg[, grep("S|_bg", names(this.year.bg))], .(), numcolwise(sum))))
	
	rm(this.year)
	
	assign(paste0("od.", year), this.year.bg)
	
	rm(this.year.bg)
	
	gc()
	
}