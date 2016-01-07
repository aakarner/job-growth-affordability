# Effect of high-wage job growth on housing demand on the San Francisco Bay Area
# Analysis using the Longitudinal Employer-Household Dynamics Origin-Destination
# Employment Statistics (LODES) data 2008-2013 and the American Community Survey
#
# Alex Karner, alex.karner@coa.gatech.edu
# Chris Benner, cbenner@ucsc.edu
#
# Purpose:
# Visualize changes in job and housing totals in the 19 jurisdictions in the Bay Area 
# with complete housing data available in the 2013-2011 and 2010-2008 thre-year ACS datasets.
#
# The user must have previously prepared differences in housing numbers using the ACS
# three-year datasets since they're not presently available from the Census API. 
#
# Output: 
# A series of figures illustrating housing unit changes, housing unit changes
# in relationship to job growth/decline, and change in metrics of jobs-housing fit. 

# Set your working directory. 
# The LODES data files will be stored here. 

# Uncomment this line by removing the '#' in front..
# setwd("C:/My Directory/LEHD")
# .. in order to set your current working directory.
# setwd("D:/Dropbox/Work/high-wage job growth")

library(ggplot2)
library(reshape2)
library(plyr); library(dplyr)

# This saved file contains the wac and rac data for 2008-2013
load("data/BayAreaLEHD.Rdata")

# Post-process the wac and rac data to contain some additional employment categories.

# Add high and low wage NAICS categories
# Low-wage NAICS codes (Retail trade + Arts, Entertainment, and Recreation + Accommodation and food services)
# High-wage NAICS codes (Information + Finance and Insurance + Professional + Management)
for(year in years.to.download) { 
	assign(paste0("wac.place.", year), mutate(eval(parse(text = paste0("wac.place.", year))),
		naics_hi = CNS09 + CNS10 + CNS12 + CNS13, 
		naics_lo = CNS07 + CNS17 + CNS18 + CNS14 + CNS19))
}

# Drop the 'createdate' column
wac.place.2008 <- wac.place.2008[, -55]
wac.place.2009 <- wac.place.2009[, -55]
wac.place.2010 <- wac.place.2010[, -55]
wac.place.2011 <- wac.place.2011[, -55]
wac.place.2012 <- wac.place.2012[, -55]
wac.place.2013 <- wac.place.2013[, -55]

# Sereno del Mar CDP, CA is not in the 2011 data. Remove it from the others for consistency.
wac.place.2013 <- wac.place.2013[!wac.place.2013$placename %in% "Sereno del Mar CDP, CA", ]
wac.place.2012 <- wac.place.2012[!wac.place.2012$placename %in% "Sereno del Mar CDP, CA", ]
wac.place.2011 <- wac.place.2011[!wac.place.2011$placename %in% "Sereno del Mar CDP, CA", ]
wac.place.2010 <- wac.place.2010[!wac.place.2010$placename %in% "Sereno del Mar CDP, CA", ]
wac.place.2009 <- wac.place.2009[!wac.place.2009$placename %in% "Sereno del Mar CDP, CA", ]
wac.place.2008 <- wac.place.2008[!wac.place.2008$placename %in% "Sereno del Mar CDP, CA", ]

# Create tables that contain three-year averages for 2008-2010 and 2011-2013 to match the ACS three-year
# data. 
wac.place.08.10 <- 
  cbind(wac.place.2008[, 1:2], (wac.place.2008[, 4:56] + wac.place.2009[, 4:56] + wac.place.2010[, 4:56]) / 3)

wac.place.11.13 <- 
  cbind(wac.place.2011[, 1:2], (wac.place.2013[, 4:56] + wac.place.2012[, 4:56] + wac.place.2011[, 4:56]) / 3)

rac.place.08.10 <- 
  cbind(rac.place.2008[, 1:2], (rac.place.2008[, 4:44] + rac.place.2009[, 4:44] + rac.place.2010[, 4:44]) / 3)

rac.place.11.13 <- 
  cbind(rac.place.2011[, 1:2], (rac.place.2013[, 4:44] + rac.place.2012[, 4:44] + rac.place.2011[, 4:44]) / 3)

# Calculate the difference between 2013-2011 and 2008-2010
wac.diff <- cbind(wac.place.08.10[, 1:2], "total_jobs" = wac.place.11.13[, 3], 
                  wac.place.11.13[, 3:55] - wac.place.08.10[, 3:55])

wac.diff.pct <- cbind(wac.place.08.10[, 1:2], "total_jobs" = wac.place.11.13[, 3], 
                      (wac.place.11.13[, 3:55] - wac.place.08.10[, 3:55]) / wac.place.08.10[, 3:55])

# Ensure all place names match
stopifnot(as.character(wac.place.08.10$placename) == as.character(wac.place.2011$placename))

# Read in the ACS data
# Eventually we'll transition to the acs package, but the census API 
# does not currently support the three-year data that we need. 

# Read in the three year housing differences
acs.housing <- read.csv("data/ACS_3yr_Housing.csv", sep = ",", header = TRUE, row.names = NULL)

# Add in combined Tier 1 and Tier 2 category estimates and errors
acs.housing <- mutate(acs.housing, 
	t1t2_r_e = lw_r_e + mw_r_e, t1t2_o_e = lw_o_e + mw_o_e, t1t2_t_e = t1t2_r_e + t1t2_o_e,
	t1t2_r_m = sqrt(lw_r_m^2 + mw_r_m^2), t1t2_o_m = sqrt(lw_o_m^2 + mw_o_m^2), 
	t1t2_t_m = sqrt(t1t2_r_m^2 + t1t2_o_m^2))

# This still has all California jurisdictions in it - 
# filter on only those jurisdictions in the Bay Area
acs.housing$name <- gsub(", California", ", CA", acs.housing$name)
acs.housing <- semi_join(acs.housing, wac.diff, by = c("name" = "placename"))

# Merge housing and jobs data
jobs.housing <- inner_join(wac.diff, acs.housing, by = c("placename" = "name"))

# Save image for later use
save.image(file = "data/LEHD_diffs.RData")

# Visualize results ------------------------------------------------------------
# And color by the statistical significance of the difference (for ACS)

acs.housing.sub <- acs.housing[acs.housing$overall == 1, ]
acs.housing.sub$name <- gsub(" city, CA", "", acs.housing.sub$name)

acs.housing.est <- acs.housing.sub[, c("name", "lw_r_e", "mw_r_e", "lw_o_e", "mw_o_e", "lw_t_e", "mw_t_e", "t1t2_r_e", 
	"t1t2_o_e", "t1t2_t_e", "t_r_e", "t_o_e", "t_e")]

acs.housing.err <- acs.housing.sub[, c("name", "lw_r_m", "mw_r_m", "lw_o_m", "mw_o_m", "lw_t_m", "mw_t_m",
	"t1t2_r_m", "t1t2_o_m", "t1t2_t_m", "t_r_m", "t_o_m", "t_m")]

acs.housing.err <- melt(acs.housing.err, id = "name")
names(acs.housing.err) <- c("name", "error_type", "moe90")

to.plot.change <- cbind(melt(acs.housing.est, id = "name"), acs.housing.err[, 2:3])

levels(to.plot.change$variable) <- c(
	"Tier 1 rentals", "Tier 2 rentals", 
  "Tier 1 owner-occupied", "Tier 2 owner-occupied", 
	"Tier 1 total", "Tier 2 total", 
	"Tier 1 + 2 rentals", "Tier 1 + 2 owner-occupied", "Tier 1 + 2 total",
	"Total rental units", "Total owner-occupied units", "Total units")

to.plot.change$name <- factor(to.plot.change$name, 
	levels = levels(reorder(acs.housing.sub$name, acs.housing.sub$lw_t_e)))

# Create aesthetic mapping for error bars
limits <- aes(xmax = value + moe90, xmin = value - moe90)

# Tier 1 -- Low wage
plot1 <- ggplot(filter(to.plot.change, variable %in% c("Tier 1 rentals", "Tier 1 owner-occupied",  
	"Tier 1 total")), aes(y = name, x = value)) + geom_point() + geom_errorbarh(limits, height = 0) + 
	# scale_color_brewer(name = "", palette = "Set1", breaks = c(0, 1), labels = c("not significant", "significant")) + 
	scale_x_continuous(breaks = c(-4000, -2000, 0, 2000, 4000)) + 
	xlab(NULL) + ylab(NULL) + ggtitle("Change in housing units, 2008-10 - 2011-13 ACS") + 	
	geom_vline(aes(xintercept = 0), color = "#EBC79E") + 
	theme_bw() + 
	theme(plot.title = element_text(size = 15, face = "bold", vjust = 2), legend.position = "bottom")
	
plot1 + facet_wrap(~ variable)

ggsave("output_2013/OverallHousingChange_lw_3yr.png", height = 4.5, width = 12)

# Tier 2 -- Mid wage
to.plot.change$name <- factor(to.plot.change$name, 
	levels = levels(reorder(acs.housing.sub$name, acs.housing.sub$mw_t_e)))

plot2 <- ggplot(filter(to.plot.change, variable %in% c("Tier 2 rentals", "Tier 2 owner-occupied",  
	"Tier 2 total")), aes(y = name, x = value)) + geom_point() + 
	geom_errorbarh(limits, height = 0) + 
	# scale_color_brewer(name = "", palette = "Set1", breaks = c(0, 1), labels = c("not significant", "significant")) + 
	xlab(NULL) + ylab(NULL) + ggtitle("Change in housing units, 2008-10 - 2011-13 ACS") + 	
	geom_vline(aes(xintercept = 0), color = "#EBC79E") + 
	theme_bw() + 
	theme(plot.title = element_text(size = 15, face = "bold", vjust = 2), legend.position = "bottom")
	
plot2 + facet_wrap(~ variable)
ggsave("output_2013/OverallHousingChange_mw_3yr.png", height = 4.5, width = 12)

# Tier 1 + Tier 2 -- Combined low and mid wage
to.plot.change$name <- factor(to.plot.change$name, 
	levels = levels(reorder(acs.housing.sub$name, (acs.housing.sub$t1t2_t_e))))

plot3 <- ggplot(filter(to.plot.change, variable %in% c("Tier 1 + 2 rentals", "Tier 1 + 2 owner-occupied", 
	"Tier 1 + 2 total")), aes(y = name, x = value)) + geom_point() + geom_errorbarh(limits, height = 0) +  
	# scale_color_brewer(name = "", palette = "Set1", breaks = c(0, 1), labels = c("not significant", "significant")) + 
	xlab(NULL) + ylab(NULL) + ggtitle("Change in housing units, 2008-10 - 2011-13 ACS") + 	
	geom_vline(aes(xintercept = 0), color = "#EBC79E") + 
	theme_bw() + 
	theme(plot.title = element_text(size = 15, face = "bold", vjust = 2), legend.position = "bottom")

plot3 + facet_wrap(~ variable)
ggsave("output_2013/OverallHousingChange_t1t2_3yr.png", height = 4.5, width = 12)

# Total
to.plot.change$name <- factor(to.plot.change$name, 
	levels = levels(reorder(acs.housing.sub$name, acs.housing.sub$t_e)))

plot3 <- ggplot(filter(to.plot.change, variable %in% c("Total rental units", "Total owner-occupied units",  
	"Total units")), aes(y = name, x = value)) + geom_point() + geom_errorbarh(limits, height = 0) +  
	# scale_color_brewer(name = "", palette = "Set1", breaks = c(0, 1), labels = c("not significant", "significant")) + 
	xlab(NULL) + ylab(NULL) + ggtitle("Change in housing units, 2008-10 - 2011-13 ACS") + 	
	geom_vline(aes(xintercept = 0), color = "#EBC79E") + 
	theme_bw() + 
	theme(plot.title = element_text(size = 15, face = "bold", vjust = 2), legend.position = "bottom")
	
plot3 + facet_wrap(~ variable)
ggsave("output_2013/OverallHousingChange_total_3yr.png", height = 4.5, width = 12)

# Rank chart
acs.housing.sub$rank_low <- rank(acs.housing.sub$lw_t_e) - 9.5
acs.housing.sub$rank_mid <- rank(acs.housing.sub$mw_t_e) - 9.5
acs.housing.sub$rank_t1t2 <- rank(acs.housing.sub$t1t2_t_e) - 9.5
acs.housing.sub$rank_total <- rank(acs.housing.sub$t_e) - 9.5

ggplot(acs.housing.sub, aes(x = rank_total, y = rank_low)) + geom_point() + geom_text(aes(label = name, vjust = 1.1)) + 
	xlab("total housing production rank") + 
	ylab("tier 1 affordable housing production rank") +
	scale_x_continuous(breaks = c(-5, 0, 5)) + 
	geom_vline(aes(xintercept = 0), color = "black") + 
	geom_hline(aes(yintercept = 0), color = "black") + 
	theme_bw() + 
	theme(axis.text = element_text(size = 13), axis.title = element_text(size = 15))

ggsave("output_2013/RankChart_low.svg", height = 10, width = 10)

ggplot(acs.housing.sub, aes(x = rank_total, y = rank_mid)) + geom_point() + geom_text(aes(label = name, vjust = 1.1)) + 
	xlab("total housing production rank") + 
	ylab("tier 2 affordable housing production rank") +
	scale_x_continuous(breaks = c(-5, 0, 5)) + 
	geom_vline(aes(xintercept = 0), color = "black") + 
	geom_hline(aes(yintercept = 0), color = "black") + 
	theme_bw() + 
	theme(axis.text = element_text(size = 13), axis.title = element_text(size = 15))

ggsave("output_2013/RankChart_mid.svg", height = 10, width = 10)

ggplot(acs.housing.sub, aes(x = t_e, y = t1t2_t_e)) + geom_point(size = 3.5) + 
	geom_text(aes(label = name), vjust = 1.1, size = 5) + 
	xlab("total housing production") + 
	ylab("tier 1 + 2 affordable housing change") +
	#scale_x_continuous(breaks = c(-5, 0, 5)) + 
	geom_vline(aes(xintercept = 0), color = "black") + 
	geom_hline(aes(yintercept = 0), color = "black") + 
	theme_bw() + 
	theme(axis.text = element_text(size = 13), axis.title = element_text(size = 15))

ggsave("output_2013/TotalProdvst1t2Prod.svg", height = 6, width = 9)

ggplot(acs.housing.sub, aes(x = t_r_e, y = t1t2_r_e)) + geom_point(size = 3.5) + 
	geom_text(aes(label = name), vjust = 1.1, size = 5) + 
	xlab("total rental units production") + 
	ylab("tier 1 + 2 affordable rental change") +
	#scale_x_continuous(breaks = c(-5, 0, 5)) + 
	geom_vline(aes(xintercept = 0), color = "black") + 
	geom_hline(aes(yintercept = 0), color = "black") + 
	theme_bw() + 
	theme(axis.text = element_text(size = 13), axis.title = element_text(size = 15))

ggsave("output_2013/TotalProdvst1t2Prod_Rentals.svg", height = 6, width = 9)

# Big 3 figures
ggplot(filter(acs.housing.sub, name %in% c("San Jose", "San Francisco", "Oakland")), aes(x = t_r_e, y = t1t2_r_e)) + 
  geom_point(size = 3) + 
	geom_text(aes(label = name), vjust = -0.7, size = 6) + 
	xlab("total rental unit production") + 
	ylab("tier 1 + 2 affordable\n rental change") +
	#scale_x_continuous(breaks = c(-5, 0, 5)) + 
	geom_vline(aes(xintercept = 0), color = "black") + 
	geom_hline(aes(yintercept = 0), color = "black") + 
	theme_bw() + 
	theme(axis.text = element_text(size = 22), axis.title = element_text(size = 24))

ggsave("output_2013/Big3_TotalProdvst1t2Prod.svg", height = 6, width = 9)


# Comparing job growth and housing unit changes
jobs.housing <- jobs.housing[jobs.housing$overall == 1, ]
jobs.housing$name <- gsub(" city, CA", "", jobs.housing$placename)

## TIER 1

# Rentals
vlimits <- aes(ymax = lw_r_e + lw_r_m, ymin = lw_r_e - lw_r_m)
ggplot(jobs.housing, aes(x = CE01, y = lw_r_e, size = log(total_jobs), label = name)) + 
	geom_errorbar(vlimits, size = 0.7, width = 0, color = grey(0.7)) + geom_point()  +
	geom_text(data = subset(jobs.housing, abs(CE01) > 500 | abs(lw_r_e) > 500), vjust = -1.0, hjust = 0.8, size = 5, 
		color = "black") +
	xlab("change in tier 1 jobs") + ylab("change in tier 1 affordable rental units") + 
	guides(size = FALSE) + 
	scale_color_brewer(name = "", palette = "Set1", breaks = c(FALSE, TRUE), 
		labels = c("not significant", "significant")) + 
	geom_vline(aes(xintercept = 0), color = grey(0.7)) + 
	geom_hline(aes(yintercept = 0), color = grey(0.7)) + 
	theme_bw() + 
	theme(legend.position = "bottom", axis.text = element_text(size = 14), axis.title = element_text(size = 14))

ggsave("output_2013/HousingJobs_lw_r.svg", height = 6, width = 9)

# Owner occupied
vlimits <- aes(ymax = lw_o_e + lw_o_m, ymin = lw_o_e - lw_o_m)
ggplot(jobs.housing, aes(x = CE01, y = lw_o_e, size = log(total_jobs), label = name)) +
	geom_errorbar(vlimits, size = 0.7, width = 0, color = grey(0.7)) + geom_point() +
	geom_text(data = subset(jobs.housing, abs(CE01) > 500 | abs(lw_o_e) > 500), vjust = -1.0, hjust = 0.8, size = 5, 
		color = "black") +
	xlab("change in tier 1 jobs") + ylab("change in tier 1 affordable owner-occupied units") + 
	guides(size = FALSE) + 
	scale_color_brewer(name = "", palette = "Set1", breaks = c(FALSE, TRUE), 
		labels = c("not significant", "significant")) + 
	geom_vline(aes(xintercept = 0), color = grey(0.7)) + 
	geom_hline(aes(yintercept = 0), color = grey(0.7)) + 
	theme_bw() + 
	theme(legend.position = "bottom", axis.text = element_text(size = 14), axis.title = element_text(size = 14))

ggsave("output_2013/HousingJobs_lw_o.svg", height = 6, width = 9)

# Total
vlimits <- aes(ymax = lw_t_e + lw_t_m, ymin = lw_t_e - lw_t_m)
ggplot(jobs.housing, aes(x = CE01, y = lw_t_e, size = log(total_jobs), label = name)) + 
	geom_errorbar(vlimits, size = 0.7, width = 0, color = grey(0.7)) + geom_point() + 
	geom_text(data = subset(jobs.housing, abs(CE01) > 500 | abs(lw_t_e) > 500), vjust = -1.0, hjust = 0.8, size = 5, 
		color = "black") +
	xlab("change in tier 1 jobs") + ylab("change in tier 1 affordable total housing units") + 
	guides(size = FALSE) + 
	scale_color_brewer(name = "", palette = "Set1", breaks = c(FALSE, TRUE), 
		labels = c("not significant", "significant")) + 
	geom_vline(aes(xintercept = 0), color = grey(0.7)) + 
	geom_hline(aes(yintercept = 0), color = grey(0.7)) + 
	theme_bw() + 
	theme(legend.position = "bottom", axis.text = element_text(size = 14), axis.title = element_text(size = 14))

ggsave("output_2013/HousingJobs_lw_t.svg", height = 6, width = 9)

## TIER 2

# Rentals
vlimits <- aes(ymax = mw_r_e + mw_r_m, ymin = mw_r_e - mw_r_m)
ggplot(jobs.housing, aes(x = CE02, y = mw_r_e, size = log(total_jobs), label = name)) + 
	geom_errorbar(vlimits, size = 0.7, width = 0, color = grey(0.7)) + geom_point() + 
	geom_text(data = subset(jobs.housing, abs(CE02) > 500 | abs(mw_r_e) > 500), vjust = -0.7, hjust = 0.8, size = 5, 
		color = "black") +
	xlab("change in tier 2 jobs") + ylab("change in tier 2 affordable rental units") + 
	guides(size = FALSE) + 
	geom_vline(aes(xintercept = 0), color = grey(0.7)) + 
	geom_hline(aes(yintercept = 0), color = grey(0.7)) + 
	theme_bw() + 
	theme(legend.position = "bottom", axis.text = element_text(size = 14), axis.title = element_text(size = 14))
	
ggsave("output_2013/HousingJobs_mw_r.svg", height = 6, width = 9)

# Owner occupied
vlimits <- aes(ymax = mw_o_e + mw_o_m, ymin = mw_o_e - mw_o_m)
ggplot(jobs.housing, aes(x = CE02, y = mw_o_e, size = log(total_jobs), label = name)) +
	geom_errorbar(vlimits, size = 0.7, width = 0, color = grey(0.7)) + geom_point() + 
	geom_text(data = subset(jobs.housing, abs(CE02) > 500 | abs(mw_o_e) > 500), vjust = -0.7, hjust = 0.8, size = 5, 
		color = "black") +
	xlab("change in tier 2 jobs") + ylab("change in tier 2 affordable owner-occupied units") + 
	guides(size = FALSE) + 
	geom_vline(aes(xintercept = 0), color = grey(0.7)) + 
	geom_hline(aes(yintercept = 0), color = grey(0.7)) + 
	theme_bw() + 
	theme(legend.position = "bottom", axis.text = element_text(size = 14), axis.title = element_text(size = 14))

ggsave("output_2013/HousingJobs_mw_o.svg", height = 6, width = 9)

# Total
vlimits <- aes(ymax = mw_t_e + mw_t_m, ymin = mw_t_e - mw_t_m)
ggplot(jobs.housing, aes(x = CE02, y = mw_t_e, size = log(total_jobs), label = name)) + geom_point() + 
	geom_errorbar(vlimits, size = 0.7, width = 0, color = grey(0.7)) + geom_point() + 
	geom_text(data = subset(jobs.housing, abs(CE02) > 500 | abs(mw_t_e) > 500), vjust = -0.7, hjust = 0.8, size = 5, 
		color = "black") +
	xlab("change in tier 2 jobs") + ylab("change in tier 2 affordable total housing units") + 
	guides(size = FALSE) + 
	geom_vline(aes(xintercept = 0), color = grey(0.7)) + 
	geom_hline(aes(yintercept = 0), color = grey(0.7)) + 
	theme_bw() + 
	theme(legend.position = "bottom", axis.text = element_text(size = 14), axis.title = element_text(size = 14))

ggsave("output_2013/HousingJobs_mw_t.svg", height = 6, width = 9)

# Tier 1 + Tier 2 # 

# Rentals
vlimits <- aes(ymax = t1t2_r_e + t1t2_r_m, ymin = t1t2_r_e - t1t2_r_m)
ggplot(jobs.housing, aes(x = (CE01 + CE02), y = t1t2_r_e, size = log(total_jobs), label = name)) + 
	geom_errorbar(vlimits, size = 0.7, width = 0, color = grey(0.7)) + geom_point() + 
	geom_text(data = subset(jobs.housing, abs(CE01 + CE02) > 500 | abs(t1t2_r_e) > 500), vjust = -0.7, hjust = 0.8, 
		size = 5, 
		color = "black") +
	xlab("change in tier 1 + 2 jobs") + ylab("change in tier 1 + 2 affordable rental units") + 
	guides(size = FALSE) + 
	geom_vline(aes(xintercept = 0), color = grey(0.7)) + 
	geom_hline(aes(yintercept = 0), color = grey(0.7)) + 
	theme_bw() + 
	theme(legend.position = "bottom", axis.text = element_text(size = 14), axis.title = element_text(size = 14))
	
ggsave("output_2013/HousingJobs_t1t2_r.svg", height = 6, width = 9)

# Owner occupied
vlimits <- aes(ymax = t1t2_o_e + t1t2_o_m, ymin = t1t2_o_e - t1t2_o_m)
ggplot(jobs.housing, aes(x = (CE01 + CE02), y = t1t2_o_e, size = log(total_jobs), label = name)) +
	geom_errorbar(vlimits, size = 0.7, width = 0, color = grey(0.7)) + geom_point() + 
	geom_text(data = subset(jobs.housing, abs(CE01 + CE02) > 500 | abs(t1t2_o_e) > 500), 
		vjust = -0.7, hjust = 0.8, size = 5, color = "black") +
	xlab("change in tier 1 + 2 jobs") + ylab("change in tier 1 + 2 affordable owner-occupied units") + 
	guides(size = FALSE) + 
	geom_vline(aes(xintercept = 0), color = grey(0.7)) + 
	geom_hline(aes(yintercept = 0), color = grey(0.7)) + 
	theme_bw() + 
	theme(legend.position = "bottom", axis.text = element_text(size = 14), axis.title = element_text(size = 14))

ggsave("output_2013/HousingJobs_t1t2_o.svg", height = 6, width = 9)

# Total
vlimits <- aes(ymax = t1t2_t_e + t1t2_t_m, ymin = t1t2_t_e - t1t2_t_m)
ggplot(jobs.housing, aes(x = (CE01 + CE02), y = t1t2_t_e, size = log(total_jobs), label = name)) + geom_point() + 
	geom_errorbar(vlimits, size = 0.7, width = 0, color = grey(0.7)) + geom_point() + 
	geom_text(data = subset(jobs.housing, abs(CE01 + CE02) > 500 | abs(t1t2_t_e) > 500), 
		vjust = -0.7, hjust = 0.8, size = 5, color = "black") +
	xlab("change in tier 1 + 2 jobs") + ylab("change in tier 1 + 2 affordable total housing units") + 
	guides(size = FALSE) + 
	geom_vline(aes(xintercept = 0), color = grey(0.7)) + 
	geom_hline(aes(yintercept = 0), color = grey(0.7)) + 
	theme_bw() + 
	theme(legend.position = "bottom", axis.text = element_text(size = 14), axis.title = element_text(size = 14))

ggsave("output_2013/HousingJobs_t1t2_t.svg", height = 6, width = 9)


# TOTAL JOBS #

# Rentals
vlimits <- aes(ymax = t_r_e + t_r_m, ymin = t_r_e - t_r_m)
ggplot(jobs.housing, aes(x = C000, y =t_r_e, size = log(total_jobs), label = name)) + 
	geom_errorbar(vlimits, size = 0.7, width = 0, color = grey(0.7)) + geom_point() + 
	geom_text(data = subset(jobs.housing, abs(C000) > 2000 | abs(t_r_e) > 2000), vjust = -0.7, hjust = 0.8, size = 5, 
		color = "black") +
	xlab("change in total jobs") + ylab("change in total rental units") + 
	guides(size = FALSE) + 
	geom_vline(aes(xintercept = 0), color = grey(0.7)) + 
	geom_hline(aes(yintercept = 0), color = grey(0.7)) + 
	theme_bw() + 
	theme(legend.position = "bottom", axis.text = element_text(size = 14), axis.title = element_text(size = 14))

ggsave("output_2013/HousingJobs_t_r.svg", height = 6, width = 9)

# Owner occupied
vlimits <- aes(ymax = t_o_e + t_o_m, ymin = t_o_e - t_o_m)
ggplot(jobs.housing, aes(x = C000, y = t_o_e, size = log(total_jobs), label = name)) + 
	geom_errorbar(vlimits, size = 0.7, width = 0, color = grey(0.7)) + geom_point() + 
	geom_text(data = subset(jobs.housing, abs(C000) > 2000 | abs(t_o_e) > 2000), vjust = -0.7, hjust = 0.8, size = 5, 
		color = "black") +
	xlab("change in total jobs") + ylab("change in total owner-occupied units") + 
	guides(size = FALSE) + 
	geom_vline(aes(xintercept = 0), color = grey(0.7)) + 
	geom_hline(aes(yintercept = 0), color = grey(0.7)) + 
	theme_bw() + 
	theme(legend.position = "bottom", axis.text = element_text(size = 14), axis.title = element_text(size = 14))

ggsave("output_2013/HousingJobs_t_o.svg", height = 6, width = 9)

# Total
vlimits <- aes(ymax = t_e + t_m, ymin = t_e - t_m)
ggplot(jobs.housing, aes(x = C000, y = t_e, size = log(total_jobs), label = name)) + 
	geom_errorbar(vlimits, size = 0.7, width = 0, color = grey(0.7)) + geom_point() +
	geom_text(data = subset(jobs.housing, abs(C000) > 2000 | abs(t_e) > 2000), vjust = -0.7, hjust = 0.8, size = 5, 
		color = "black") +
	xlab("change in total jobs") + ylab("change in total housing units") + 
	guides(size = FALSE) + 
	geom_vline(aes(xintercept = 0), color = grey(0.7)) + 
	geom_hline(aes(yintercept = 0), color = grey(0.7)) + 
	theme_bw() + 
	theme(legend.position = "bottom", axis.text = element_text(size = 14), axis.title = element_text(size = 14))

ggsave("output_2013/HousingJobs_t.svg", height = 6, width = 9)

# Effect of high-wage job growth on housing demand and affordability

# Rentals - high-wage NAICS jobs, low wage affordable
vlimits <- aes(ymax = lw_r_e + lw_r_m, ymin = lw_r_e - lw_r_m)
ggplot(jobs.housing, aes(x = naics_hi, y = lw_r_e, size = log(total_jobs), label = name)) + 
	geom_errorbar(vlimits, size = 0.7, width = 0, color = grey(0.7)) + geom_point() +
	geom_text(data = subset(jobs.housing, abs(CNS12) > 500 | abs(lw_r_e) > 500), vjust = -0.7, hjust = 0.8, size = 5, 
		color = "black") +
	xlab("change in high-wage NAICS jobs") + ylab("change in tier 1 affordable rental units") + 
	guides(size = FALSE) + 
	geom_vline(aes(xintercept = 0), color = grey(0.7)) + 
	geom_hline(aes(yintercept = 0), color = grey(0.7)) + 
	theme_bw() + 
	theme(legend.position = "bottom", axis.text = element_text(size = 14), axis.title = element_text(size = 14))

ggsave("output_2013/HousingJobs_NAICS_hi_lwaff.svg", height = 6, width = 9)

# Rentals - high-wage NAICS jobs, mid wage affordable
vlimits <- aes(ymax = mw_r_e + mw_r_m, ymin = mw_r_e - mw_r_m)
ggplot(jobs.housing, aes(x = naics_hi, y = mw_r_e, size = log(total_jobs), label = name)) + 
	geom_errorbar(vlimits, size = 0.7, width = 0, color = grey(0.7)) + geom_point() +
	geom_text(data = subset(jobs.housing, abs(CNS12) > 500 | abs(mw_r_e) > 1500), vjust = -0.7, hjust = 0.8, size = 5, 
		color = "black") +
	xlab("change in high-wage NAICS jobs") + ylab("change in tier 2 affordable rental units") + 
	guides(size = FALSE) + 
	geom_vline(aes(xintercept = 0), color = grey(0.7)) + 
	geom_hline(aes(yintercept = 0), color = grey(0.7)) + 
	theme_bw() + 
	theme(legend.position = "bottom", axis.text = element_text(size = 14), axis.title = element_text(size = 14))

ggsave("output_2013/HousingJobs_NAICS_hi_mwaff.svg", height = 6, width = 9)

# Rentals - naics_hi, total rental units
vlimits <- aes(ymax = t_r_e + t_r_m, ymin = t_r_e - t_r_m)
ggplot(jobs.housing, aes(x = naics_hi, y = t_r_e, size = log(total_jobs), label = name)) + 
	geom_errorbar(vlimits, size = 0.7, width = 0, color = grey(0.7)) + geom_point() +
	geom_text(data = subset(jobs.housing, abs(CNS12) > 500 | abs(t_r_e) > 1500), vjust = -0.7, hjust = 0.8, size = 5, 
		color = "black") +
	xlab("change in high-wage NAICS jobs") + ylab("change in total rental units") + 
	guides(size = FALSE) + 
	geom_vline(aes(xintercept = 0), color = grey(0.7)) + 
	geom_hline(aes(yintercept = 0), color = grey(0.7)) + 
	theme_bw() + 
	theme(legend.position = "bottom", axis.text = element_text(size = 14), axis.title = element_text(size = 14))

ggsave("output_2013/HousingJobs_NAICS_hi_total.svg", height = 6, width = 9)

# naics_hi total rentals.... Big 3 only
ggplot(filter(jobs.housing, name %in% c("San Francisco", "San Jose", "Oakland")), 
	aes(x = naics_hi, y = t_r_e, label = name)) + 
	geom_point(size = 3) +
	geom_text(vjust = -0.7, hjust = 0.8, size = 6, color = "black") +
	xlab("change in high-wage jobs") + ylab("change in total rental units") + 
	guides(size = FALSE) + 
	geom_vline(aes(xintercept = 0), color = "black") + 
	geom_hline(aes(yintercept = 0), color = "black") + 
	theme_bw() + 
	theme(legend.position = "bottom", axis.text = element_text(size = 22), axis.title = element_text(size = 24))

ggsave("output_2013/Big3_HousingJobs_NAICS_hi_total.svg", height = 6, width = 9)


# Rentals - information jobs, low wage affordable
ggplot(jobs.housing, aes(x = CNS10, y = lw_r_e, size = log(total_jobs), label = name,
	color = as.factor(lw_r_m > 1.64 | lw_r_m < -1.64))) + geom_point() + 
	geom_text(data = subset(jobs.housing, abs(CNS10) > 500 | abs(lw_r_e) > 500), vjust = -0.7, hjust = 0.8, size = 4, 
		color = "black") +
	xlab("change in information jobs") + ylab("change in tier 1 affordable rental units") + 
	guides(size = FALSE) + 
	scale_color_brewer(name = "", palette = "Set1", breaks = c(FALSE, TRUE), 
		labels = c("not significant", "significant")) + 
	geom_vline(aes(xintercept = 0), color = grey(0.7)) + 
	geom_hline(aes(yintercept = 0), color = grey(0.7)) + 
	theme_bw() + theme(legend.position = "bottom")

ggsave("output_2013/HousingJobs_prof_lwaff.png", height = 8, width = 11, dpi = 500)

# Rentals - information jobs, mid wage affordable
ggplot(jobs.housing, aes(x = CNS12, y = mw_r_e, size = log(total_jobs), label = name,
	color = as.factor(mw_r_m > 1.64 | mw_r_m < -1.64))) + geom_point() + 
	geom_text(data = subset(jobs.housing, abs(CNS12) > 500 | abs(mw_r_e) > 1500), vjust = -0.7, hjust = 0.8, size = 4, 
		color = "black") +
	xlab("change in professional jobs") + ylab("change in tier 2 affordable rental units") + 
	guides(size = FALSE) + 
	scale_color_brewer(name = "", palette = "Set1", breaks = c(FALSE, TRUE), 
		labels = c("not significant", "significant")) + 
	geom_vline(aes(xintercept = 0), color = grey(0.7)) + 
	geom_hline(aes(yintercept = 0), color = grey(0.7)) + 
	theme_bw() + theme(legend.position = "bottom")

ggsave("HousingJobs_prof_mwaff.png", height = 8, width = 11, dpi = 500)

# Jobs-housing fit -------------------------------------------------------------
	
# Read in three-year ACS data for 2013-2011 and 2010-2008

# 2013
jhfit.2013 <- read.table("data/ACS_3yr_Housing (2013 only).csv", sep = ",", header = TRUE, row.names = NULL)
jhfit.2013$Geo_NAME <- gsub(", California", ", CA", jhfit.2013$Geo_NAME)
jhfit.2013 <- jhfit.2013[jhfit.2013$both_present == "Pass", ]

jhfit.2013 <- inner_join(wac.place.11.13, jhfit.2013, by = c("placename" = "Geo_NAME"))

jhfit.2013 <- transmute(jhfit.2013, 
	place = placename, 
	fit_t1_r = CE01 / lw_r_e,
	fit_t1_o = CE01 / lw_o_e,
	fit_t1_t = CE01 / lw_t_e,
	fit_t2_r = CE02 / mw_r_e,
	fit_t2_o = CE02 / mw_o_e,
	fit_t2_t = CE02 / mw_t_e, 
	fit_t1t2_r = (CE01 + CE02) / (lw_r_e + mw_r_e),
	fit_t1t2_o = (CE01 + CE02) / (lw_o_e + mw_o_e),
	fit_t1t2_t = (CE01 + CE02) / (lw_t_e + mw_t_e),
	balance_r = (C000) / t_r_e,
	balance_o = (C000) / t_o_e, 
	balance_t = (C000) / (t_r_e + t_o_e))

jobs.housing.plot.2013 <- melt(jhfit.2013, id = c("place"))
jobs.housing.plot.2013$place <- 
	factor(jobs.housing.plot.2013$place, levels = levels(reorder(jhfit.2013$place, jhfit.2013$fit_t1_t)))
jobs.housing.plot.2013$place_label <- jobs.housing.plot.2013$place
levels(jobs.housing.plot.2013$place_label) <- gsub(" city, CA", "", levels(jobs.housing.plot.2013$place_label))

levels(jobs.housing.plot.2013$variable) <- c(
	"Tier 1 fit (rentals)", "Tier 1 fit (owner-occupied)", "Tier 1 fit (all units)",
	"Tier 2 fit (rentals)", "Tier 2 fit (owner-occupied)", "Tier 2 fit (all units)",
	"Tier 1 + 2 fit (rentals)", "Tier 1 + 2 fit (owner-occupied)", "Tier 1 + 2 fit (all units)",
	"Overall balance (rentals)", "Overall balance (owner-occupied)", "Overall balance (all units)")

# jhplot1 <- ggplot(jobs.housing.plot.2013, aes(x = value, y = place_label)) + 
# 	ylab(NULL) + geom_point() + 
# 	geom_vline(xintercept = 1, color = "#EBC79E") + 
# 	xlab("jobs-housing fit or balance ratio (number of jobs divided by number of affordable units in each category)") +
# 	ggtitle("Jobs-Housing Fit and Balance measures, 2011-2013 average") +
# 	theme_bw()
# 
# jhplot1 + facet_wrap(~variable, nrow = 4, ncol = 3)
# 
# sumstats <- ddply(jobs.housing.plot, .(variable), summarize, stddev  = sd(value), avg = mean(value))
# sumstats <- melt(sumstats, id = "variable")
# names(sumstats)[1] <- "id"
# 
# ggplot(sumstats, aes(x = id, y = value, color = variable)) + 
# 	geom_hline(yintercept = 1, color = "black") + 
# 	theme_bw() + geom_point()

# 2010
jhfit.2010 <- read.table("data/ACS_3yr_Housing (2010 only).csv", sep = ",", header = TRUE, row.names = NULL)
jhfit.2010$Geo_NAME <- gsub(", California", ", CA", jhfit.2010$Geo_NAME)
jhfit.2010 <- jhfit.2010[jhfit.2010$both_present == "Pass", ]

jhfit.2010 <- inner_join(wac.place.08.10, jhfit.2010, by = c("placename" = "Geo_NAME"))

jhfit.2010 <- transmute(jhfit.2010, 
	place = placename, 
	fit_t1_r = CE01 / lw_r_e,
	fit_t1_o = CE01 / lw_o_e,
	fit_t1_t = CE01 / lw_t_e,
	fit_t2_r = CE02 / mw_r_e,
	fit_t2_o = CE02 / mw_o_e,
	fit_t2_t = CE02 / mw_t_e, 
	fit_t1t2_r = (CE01 + CE02) / (lw_r_e + mw_r_e),
	fit_t1t2_o = (CE01 + CE02) / (lw_o_e + mw_o_e),
	fit_t1t2_t = (CE01 + CE02) / (lw_t_e + mw_t_e),
	balance_r = (C000) / t_r_e,
	balance_o = (C000) / t_o_e, 
	balance_t = (C000) / (t_r_e + t_o_e))

jobs.housing.plot.2010 <- melt(jhfit.2010, id = c("place"))
jobs.housing.plot.2010$place <- factor(jobs.housing.plot.2010$place, levels = levels(reorder(jhfit.2010$place, 
	jhfit.2010$fit_t1_t)))
jobs.housing.plot.2010$place_label <- jobs.housing.plot.2010$place
levels(jobs.housing.plot.2010$place_label) <- gsub(" city, CA", "", levels(jobs.housing.plot.2010$place_label))

levels(jobs.housing.plot.2010$variable) <- c(
	"Tier 1 fit (rentals)", "Tier 1 fit (owner-occupied)", "Tier 1 fit (all units)",
	"Tier 2 fit (rentals)", "Tier 2 fit (owner-occupied)", "Tier 2 fit (all units)",
	"Tier 1 + 2 fit (rentals)", "Tier 1 + 2 fit (owner-occupied)", "Tier 1 + 2 fit (all units)",
	"Overall balance (rentals)", "Overall balance (owner-occupied)", "Overall balance (all units)")

# Change figure
jobs.housing.plot.2013$year <- 2013
jobs.housing.plot.2010$year <- 2010

jobs.housing.plot.2013 <- semi_join(jobs.housing.plot.2013, jobs.housing.plot.2010, 
	by = c("place_label" = "place_label"))
jobs.housing.plot.2010 <- semi_join(jobs.housing.plot.2010, jobs.housing.plot.2013,
	by = c("place_label" = "place_label"))

jobs.housing.plot <- rbind(jobs.housing.plot.2013, jobs.housing.plot.2010)

jhfit.2013$place <- gsub(" city, CA", "", jhfit.2013$place)

# Tier 1
jobs.housing.plot$place_label <- factor(jobs.housing.plot.2013$place_label, 
	levels = levels(reorder(jhfit.2013$place, jhfit.2013$fit_t1_t)))
t1 <- ggplot(filter(jobs.housing.plot, variable %in% 
	c("Tier 1 fit (rentals)", "Tier 1 fit (owner-occupied)", "Tier 1 fit (all units)")),
	aes(x = value, y = place_label, color = as.factor(year))) + geom_point(size = 4, alpha = 0.8) + 
	geom_vline(xintercept = 1, color = "#EBC79E") + 
	xlab("jobs-housing fit (num. jobs divided by num. of tier 1 affordable units)") + ylab(NULL) + 
	scale_color_brewer(palette = "Dark2") + 
	theme_bw() + theme(legend.position = "bottom", legend.title = element_blank(),
		axis.text = element_text(size = 12), axis.title = element_text(size = 14))

t1 + facet_wrap(~variable, nrow = 1, ncol = 3) + 
	theme(strip.text.x = element_text(size = 12))

ggsave("output_2013/T1_jhfit.png", width = 9, height = 6, dpi = 300)

# Tier 2
jobs.housing.plot$place_label <- factor(jobs.housing.plot.2013$place_label, 
	levels = levels(reorder(jhfit.2013$place, jhfit.2013$fit_t2_t)))
t2 <- ggplot(filter(jobs.housing.plot, variable %in% 
	c("Tier 2 fit (rentals)", "Tier 2 fit (owner-occupied)", "Tier 2 fit (all units)")),
	aes(x = value, y = place_label, color = as.factor(year))) + geom_point(size = 4, alpha = 0.8) + 
	geom_vline(xintercept = 1, color = "#EBC79E") + 
	xlab("jobs-housing fit (num. jobs divided by num. of tier 2 affordable units)") + ylab(NULL) + 
	scale_color_brewer(palette = "Dark2") + 
	theme_bw() + theme(legend.position = "bottom", legend.title = element_blank(),
		axis.text = element_text(size = 12), axis.title = element_text(size = 14))

t2 + facet_wrap(~variable, nrow = 1, ncol = 3) + 
	theme(strip.text.x = element_text(size = 12))

ggsave("output_2013/T2_jhfit.png", width = 9, height = 6, dpi = 300)

# Tier 1 + Tier 2
jobs.housing.plot$place_label <- factor(jobs.housing.plot.2013$place_label, 
	levels = levels(reorder(jhfit.2013$place, jhfit.2013$fit_t1t2_t)))
t1t2 <- ggplot(filter(jobs.housing.plot, variable %in% 
	c("Tier 1 + 2 fit (rentals)", "Tier 1 + 2 fit (owner-occupied)", "Tier 1 + 2 fit (all units)")),
	aes(x = value, y = place_label, color = as.factor(year))) + geom_point(size = 4, alpha = 0.8) + 
	geom_vline(xintercept = 1, color = "#EBC79E") + 
	xlab("jobs-housing fit (num. jobs divided by num. of tier 1 + 2 affordable units)") + ylab(NULL) + 
	scale_color_brewer(palette = "Dark2") + 
	theme_bw() + theme(legend.position = "bottom", legend.title = element_blank(),
		axis.text = element_text(size = 12), axis.title = element_text(size = 14))

t1t2 + facet_wrap(~variable, nrow = 1, ncol = 3) +
		theme(strip.text.x = element_text(size = 12))

ggsave("output_2013/T1T2_jhfit.png", width = 9, height = 6, dpi = 300)

# Total
jobs.housing.plot$place_label <- factor(jobs.housing.plot.2013$place_label, 
	levels = levels(reorder(jhfit.2013$place, jhfit.2013$balance_t)))
total <- ggplot(filter(jobs.housing.plot, variable %in% 
	c("Overall balance (rentals)", "Overall balance (owner-occupied)", "Overall balance (all units)")),
	aes(x = value, y = place_label, color = as.factor(year))) + geom_point(size = 4, alpha = 0.8) + 
	geom_vline(xintercept = 1, color = "#EBC79E") + 
	xlab("jobs-housing fit (num. jobs divided by num. units)") + ylab(NULL) + 
	scale_color_brewer(palette = "Dark2") + 
	theme_bw() + theme(legend.position = "bottom", legend.title = element_blank(),
		axis.text = element_text(size = 12), axis.title = element_text(size = 14))

total + facet_wrap(~variable, nrow = 1, ncol = 3) + 
	theme(strip.text.x = element_text(size = 12))

ggsave("output_2013/TOTAL_jhfit.png", width = 9, height = 6)
