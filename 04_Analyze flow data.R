# Effect of high-wage job growth on housing demand on the San Francisco Bay Area
# Analysis using the Longitudinal Employer-Household Dynamics Origin-Destination
# Employment Statistics (LODES) data 2008-2011 and the American Community Survey
#
# Alex Karner, alex.karner@asu.edu
# Chris Benner, ccbenner@ucdavis.edu
#
# Purpose:
# Create and visualize metrics of internal capture and commute distance.
#
# Output:
# Visualizations of internal capture and commute distance by year and jurisdiction.

# Uncomment this line by removing the '#' in front..
# setwd("C:/My Directory/LEHD")
# .. in order to set your current working directory.
# setwd("D:/Dropbox/Work/high-wage job growth")

options(scipen = 999) # Supress scientific notation so we can see census geocodes

library(plyr); library(dplyr)
library(MonetDB.R)
library(R.utils)
library(ggmap) # To query driving distances
library(reshape2)
library(grid) # unit() functionality
library(rgdal) # interface for the Geospatial Abstraction Library
library(rgeos)
library(scales)

# Load previously saved dataset
# containing block level flows for California
load("data/BayAreaLEHD_od_FINAL.RData")
load("data/MTCandCountySkims_Google.RData")

# Calculate weighted mean commute distance by place of work
for(year in years.to.download) { 
	this.year <- eval(parse(text = paste0("od.", year, ".place")))
	
	# Remove ", CA" suffix to facilitate join
	this.year$h_plc <- gsub(", CA", "", this.year$h_plc)
	this.year$w_plc <- gsub(", CA", "", this.year$w_plc)
	
	# Join skims to table
	this.year <- left_join(this.year, bay.area.od, by = c("h_plc" = "o_place", "w_plc" = "d_place"))
	
	places.to.skim$h_plc <- as.character(places.to.skim$h_plc)
	places.to.skim$w_cty <- as.integer(places.to.skim$w_cty)
	
	# The remaining places are counties of residence outside the Bay Area
	this.year <- left_join(this.year, select(places.to.skim, h_plc, w_cty, miles), by = c("h_plc" = "h_plc", 
		"w_cty" = "w_cty"))
	
	# Update the skim with the county data
	this.year$skim <- ifelse(is.na(this.year$skim), this.year$miles, this.year$skim)
	
	this.year <- mutate(this.year, 
		s000_d = S000 * skim,
		sa01_d = SA01 * skim,
		sa02_d = SA02 * skim,
		sa03_d = SA03 * skim,
		se01_d = SE01 * skim,
		se02_d = SE02 * skim,
		se03_d = SE03 * skim,
		t1t2_d = t1t2 * skim,
		si01_d = SI01 * skim,
		si02_d = SI02 * skim,
		si03_d = SI03 * skim)

	this.year <- group_by(this.year, w_plc)

	this.year <- summarize(this.year,
		S000 = sum(s000_d) / sum(S000),
		SA01 = sum(sa01_d) / sum(SA01), SA02 = sum(sa02_d) / sum(SA02), SA03 = sum(sa03_d) / sum(SA03),
		SE01 = sum(se01_d) / sum(SE01), SE02 = sum(se02_d) / sum(SE02), SE03 = sum(se03_d) / sum(SE03),
		SI01 = sum(si01_d) / sum(SI01), SI02 = sum(si02_d) / sum(SI02), SI03 = sum(si03_d) / sum(SI03),
		t1t2 = sum(t1t2_d) / sum(t1t2))

	# Add year identifier
	this.year$year <- year
	
	this.year <- melt(this.year, id = c("w_plc", "year"))
	
	assign(paste0("commutes.", year), this.year)
	
	rm(this.year)
}


# Plot results

# identify top 25 workplaces for visualization
load("data/Top25Places.RData")
places <- gsub(", CA", "", places)

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

ggsave("output/Commute_Income.png", width = 14, height = 15, scale = 0.6)

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

ggsave("output/Commute_Income.png", width = 14, height = 15, scale = 0.6)

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

ggsave("output/Commute_age.png", width = 14, height = 15, scale = 0.6)

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

ggsave("output/Internal capture_Income.png", width = 14, height = 15, scale = 0.6)

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

ggsave("output/Internal capture_Industry.png", width = 14, height = 15, scale = 0.6)

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