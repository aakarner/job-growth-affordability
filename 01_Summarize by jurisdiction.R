# Create summaries by jurisdcition

# Alex Karner, alex.karner@asu.edu
# Chris Benner, ccbenner@ucdavis.edu

library(dplyr) # use split-apply-combine logic to data frames

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

stopifnot(wac_place_2011.v2$stplcname == wac_place_2010.v2$stplcname & 
		wac_place_2011.v2$ctyname == wac_place_2010.v2$ctyname)

# Sereno Del Mar, CDP is in the 



# Take the difference

# Want to end up with data frames that I can just take the straight difference of
# Everything has to be sorted and ordered properly
# This will avoid having to do things column-wise

anti_join(wac_place_2009, wac_place_2011, by = c("stplcname"))
