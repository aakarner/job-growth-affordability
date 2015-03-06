# Create summary job-employed resident balance measures by jurisdiction

# Alex Karner, alex.karner@asu.edu
# Chris Benner, ccbenner@ucdavis.edu

library(plyr); library(dplyr)
library(reshape2)
library(ggplot2)
library(scales) # pretty_breaks()
library(RColorBrewer)
library(grid) # unit() functionality

# Load previously saved dataset
load("BayAreaLEHD.RData")


		

# Results ----------------------------------------------------------------------


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
	
	# Add another column representing low-wage and high-wage professions
	wac.balance.this <- mutate(wac.balance.this, low_wage_jobs = CNS07 + CNS17 + CNS18, 
		high_wage_jobs = CNS09 + CNS10 + CNS12 + CNS13)
	
	rac.balance.this <- data.frame("place" = balance.places)
	rac.balance.this <- left_join(rac.balance.this, eval(parse(text = paste0("rac.place.", year))), 
			by = c("place" = "placename"))

	rac.balance.this <- select(rac.balance.this, place, C000, CE01, 
		CE02, CE03, CNS01, CNS02, CNS03, CNS04, CNS05, CNS06, CNS07, CNS08, CNS09, CNS10, CNS11, CNS12, CNS13, CNS14, 
		CNS15, CNS16, CNS17, CNS18, CNS19, CNS20, CD01, CD02, CD03, CD04)
	
	rac.balance.this <- mutate(rac.balance.this, low_wage_jobs = CNS07 + CNS17 + CNS18, 
		high_wage_jobs = CNS09 + CNS10 + CNS12 + CNS13)
	
	# Ensure the rows and columns match
	stopifnot(wac.balance.this$place == rac.balance.this$place)
	stopifnot(names(wac.balance.this$place) == names(rac.balance.this$place))

	# Calculate ratios used in Stoker and Ewing
	# Ranges between 0 and 1, captures imbalances in both directions
	# 1 is perfect balance
	# 0 is complete imbalance
	balance.this <- cbind("place" = wac.balance.this[, 1], 
		1 - abs(wac.balance.this[, 2:31] - rac.balance.this[, 2:31]) / 
				(wac.balance.this[, 2:31] + rac.balance.this[, 2:31])) 

	# Join total workers and jobs back to the balance table
	balance.this <- left_join(balance.this, wac.balance.this[, c("place", "C000")], by = c("place" = "place"))
	
	balance.this <- left_join(balance.this, rac.balance.this[, c("place", "C000")], by = c("place" = "place"))
	
	names(balance.this)[2] <- "C000"
	names(balance.this)[32] <- "total_jobs"
	names(balance.this)[33] <- "total_residents"

	# Add a variable describing whether the jurisdiction is jobs or housing rich
	balance.this <- mutate(balance.this, jobs_rich = total_jobs > total_residents, year = year)

	# Assign the variable to the global environment
	assign(paste0("balance.", year), balance.this)
	
	## Calculate a raw jobs/employed residents ratio
	ratio.this <- cbind("place" = wac.balance.this[, 1], wac.balance.this[, 2:31] / rac.balance.this[, 2:31])
	
	# Join total workers and jobs back to the ratio table
	ratio.this <- left_join(ratio.this, wac.balance.this[, c("place", "C000")], by = c("place" = "place"))
	
	ratio.this <- left_join(ratio.this, rac.balance.this[, c("place", "C000")], by = c("place" = "place"))
	
	names(ratio.this)[2] <- "C000"
	names(ratio.this)[32] <- "total_jobs"
	names(ratio.this)[33] <- "total_residents"

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



# Get top 25 places from 2011 and create
# separate data frames for them
# Identify the threshold that separates the top 25 from the bottom
# Order the data frame in  descending order of total jobs
threshold <- wac.place.2011[order(-wac.place.2011$C000), ]$C000[26]

places <- balance.2011[balance.2011$total_jobs > threshold, "place"]
places <- places[!is.na(places)]

for(year in years.to.download) { 
	
	# Balance
	balance.this <- eval(parse(text = paste0("balance.", year)))	
	top.25.this <- filter(balance.this, place %in% places)
	top.25.this <- select(top.25.this, place, C000, CE01, CE02, CE03, CD01, CD02, CD03, CD04, CNS01, CNS02, CNS03, 
		CNS04, CNS05, CNS06, CNS07, CNS08, CNS09, CNS10, CNS11, CNS12, CNS13, CNS14, 
		CNS15, CNS16, CNS17, CNS18, CNS19, CNS20, low_wage_jobs, high_wage_jobs,
		year, total_jobs, total_residents)
	
	top.25.this$place <- factor(top.25.this$place)
	assign(paste0("top.25.", year), top.25.this)
	
	# Raw ratio
	ratio.this <- eval(parse(text = paste0("ratio.", year)))
	top.25.ratio.this <- filter(ratio.this, place %in% places)
	top.25.ratio.this <- select(top.25.ratio.this, place, C000, CE01, CE02, CE03, CD01, CD02, CD03, CD04, CNS01, CNS02, 
		CNS03, CNS04, CNS05, CNS06, CNS07, CNS08, CNS09, CNS10, CNS11, CNS12, CNS13, CNS14, 
		CNS15, CNS16, CNS17, CNS18, CNS19, CNS20, low_wage_jobs, high_wage_jobs,
		year, total_jobs, total_residents)
	
	top.25.ratio.this$place <- factor(top.25.ratio.this$place)
	
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

# Reorder place factor to descend in total jobs in 2011
# to be used below for plotting
balance.merged$place <- gsub(" city, CA", "", balance.merged$place)

balance.merged$place <- factor(balance.merged$place, levels = levels(reorder(
	 filter(balance.merged, year == 2011)$place, 
	-filter(balance.merged, year == 2011)$total_jobs)))

# Rename this factor for ease of plotting
levels(balance.merged$place)[25] <- "Uninc. Sonoma Cty."

balance.merged.educ <- filter(balance.merged, variable %in% c("CD01", "CD02", "CD03", "CD04"))
balance.merged.wage <- filter(balance.merged, variable %in% c("CE01", "CE02", "CE03"))
balance.merged.naics <- filter(balance.merged, variable %in% c("CNS12", "CNS18"))
balance.merged.lwhw <- filter(balance.merged, variable %in% c("low_wage_jobs", "high_wage_jobs"))

ratio.merged <- rbind(
	melt(top.25.ratio.2009, id = c("place", "year", "total_jobs", "total_residents")),
	melt(top.25.ratio.2010, id = c("place", "year", "total_jobs", "total_residents")),
	melt(top.25.ratio.2011, id = c("place", "year", "total_jobs", "total_residents")))

# Reorder place factor to descend in total jobs in 2011
# to be used below for plotting
ratio.merged$place <- gsub(" city, CA", "", ratio.merged$place)

ratio.merged$place <- factor(ratio.merged$place, levels = levels(reorder(
	 filter(ratio.merged, year == 2011)$place, 
	-filter(ratio.merged, year == 2011)$total_jobs)))

# Rename this factor for ease of plotting
levels(ratio.merged$place)[25] <- "Uninc. Sonoma Cty."

ratio.merged.educ <- filter(ratio.merged, variable %in% c("CD01", "CD02", "CD03", "CD04"))
ratio.merged.wage <- filter(ratio.merged, variable %in% c("CE01", "CE02", "CE03"))
ratio.merged.naics <- filter(ratio.merged, variable %in% c("CNS12", "CNS18"))
ratio.merged.lwhw <- filter(ratio.merged, variable %in% c("low_wage_jobs", "high_wage_jobs"))


# Visualize JER ratio and balance measures -------------------------------------

# Plot jurisdictions in panels, time on the x-axis, ratios on the y

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
	scale_color_brewer(palette = "Dark2", labels = c("Low-wage", "Mid-wage", "High-wage")) + 
	xlab(NULL) + ylab("symmetric job-employed resident balance (1 = balance, 0 = imbalance)") + 
	theme_bw() + theme(plot.title = element_text(face = "bold"), legend.title = element_blank(),
		legend.position = "bottom")

wage + facet_wrap(~ place) + ggtitle("Job-employed resident balance indicators by wage level")

ggsave("Balance_Wage.png", width = 13, height = 15, scale = 0.6)

# NAICS codes
naics <- ggplot(balance.merged.naics, aes(x = as.factor(year), y = value, color = variable)) + geom_point() + 
	# Adjust the aesthetic to correctly plot the line
	geom_line(aes(group = variable)) + 
	scale_color_brewer(palette = "Dark2", 
		labels = c("Professional, scientific, technical", "Accommodations and food services")) + 
	xlab(NULL) + ylab("symmetric job-employed resident balance (1 = balance, 0 = imbalance)") + 
	theme_bw() + theme(plot.title = element_text(face = "bold"), legend.title = element_blank(),
		legend.position = "bottom")

naics + facet_wrap(~ place) + ggtitle("Job-employed resident balance indicators by NAICS code")

ggsave("Balance_NAICS.png", width = 13, height = 15, scale = 0.6)


# Aggregate low/high wage jobs
lwhw <- ggplot(balance.merged.lwhw, aes(x = as.factor(year), y = value, color = variable)) + geom_point() + 
	# Adjust the aesthetic to correctly plot the line
	geom_line(aes(group = variable)) + 
	scale_color_brewer(palette = "Dark2", labels = c("Low-wage NAICS codes", "High-wage NAICS codes")) + 
	xlab(NULL) + ylab("symmetric job-employed resident balance (1 = balance, 0 = imbalance)") + 
	theme_bw() + theme(plot.title = element_text(face = "bold"), legend.title = element_blank(),
		legend.position = "bottom")

lwhw + facet_wrap(~ place) + ggtitle("Job-employed resident balance indicators by\nlow-wage and high-wage NAICS codes")

ggsave("Balance_lwhw.png", width = 13, height = 15, scale = 0.6)


# Plot showing change over time for the RATIO indicator 
# for the 25 jurisdictions with the greatest numbers of jobs in 2011
educ <- ggplot(ratio.merged.educ, aes(x = as.factor(year), y = value, color = variable)) + geom_point() + 
	# Adjust the aesthetic to correctly plot the line
	geom_line(aes(group = variable)) + 
	scale_color_brewer(palette = "Dark2", 
		labels = c("Less than high school", "High school", "Some college", "Bachelor's or above")) + 
	xlab(NULL) + ylab("jobs-employed residents ratio") + 
	theme_bw() + theme(plot.title = element_text(face = "bold"), legend.title = element_blank(),
		legend.position = "bottom")

educ + facet_wrap(~ place) + ggtitle("Job-employed resident indicators by education level")

ggsave("Ratio_JER_Education.png", width = 13, height = 15, scale = 0.6)

# wage levels
wage <- ggplot(ratio.merged.wage, aes(x = as.factor(year), y = value, color = variable)) + geom_point() + 
	# Adjust the aesthetic to correctly plot the line
	geom_line(aes(group = variable)) + 
	scale_color_brewer(palette = "Dark2", 
		labels = c("Low-wage", "Mid-wage", "High-wage")) + 
	xlab(NULL) + ylab("jobs-employed residents ratio") + 
	theme_bw() + theme(plot.title = element_text(face = "bold"), legend.title = element_blank(),
		legend.position = "bottom")

wage + facet_wrap(~ place) + ggtitle("Job-employed resident indicators by wage level")

ggsave("Ratio_JER_Wage.png", width = 13, height = 15, scale = 0.6)

# NAICS code
naics <- ggplot(ratio.merged.naics, aes(x = as.factor(year), y = value, color = variable)) + geom_point() + 
	# Adjust the aesthetic to correctly plot the line
	geom_line(aes(group = variable)) + 
	scale_color_brewer(palette = "Dark2", 
		labels = c("Professional, scientific, technical", "Accommodations and food services")) + 
	xlab(NULL) + ylab("jobs-employed residents ratio") + 
	theme_bw() + theme(plot.title = element_text(face = "bold"), legend.title = element_blank(),
		legend.position = "bottom")

naics + facet_wrap(~ place) + ggtitle("Job-employed resident indicators by NAICS code")

ggsave("Ratio_JER_naics.png", width = 13, height = 15, scale = 0.6)

# Aggregate low/high wage jobs
lwhw <- ggplot(ratio.merged.lwhw, aes(x = as.factor(year), y = value, color = variable)) + geom_point() + 
	# Adjust the aesthetic to correctly plot the line
	geom_line(aes(group = variable)) + 
	scale_color_brewer(palette = "Dark2", labels = c("Low-wage NAICS codes", "High-wage NAICS codes")) + 
	xlab(NULL) + ylab("jobs-employed residents ratio") + 
	theme_bw() + theme(plot.title = element_text(face = "bold"), legend.title = element_blank(),
		legend.position = "bottom")

lwhw + facet_wrap(~ place) + ggtitle("Job-employed resident indicators by\nlow-wage and high-wage NAICS codes")

ggsave("Ratio_lwhw.png", width = 13, height = 15, scale = 0.6)

# Export combined results ------------------------------------------------------

# Create a .csv file that working group members
# can use to conveniently view the data

outfile <- data.frame()

for(year in years.to.download) {
	
	wac.this <- eval(parse(text = paste0("wac.place.", year)))
	rac.this <- eval(parse(text = paste0("rac.place.", year)))
	
	all.places <- union(wac.this$placename, rac.this$placename)
	
	wac.this.2 <- data.frame("placename" = all.places)
	wac.this.2 <- left_join(wac.this.2, wac.this)
	
	rac.this.2 <- data.frame("placename" = all.places)
	rac.this.2 <- left_join(rac.this.2, rac.this)
	
	wac.this <- wac.this.2
	rac.this <- rac.this.2
	
	rm(wac.this.2); rm(rac.this.2)
	
	outfile <- rbind(outfile, data.frame(
		"placename" = wac.this$placename,
		"year" = year,
		"total_j" = wac.this$C000,
		"total_r" = rac.this$C000,
		"low_wage_j" = wac.this$CE01,
		"low_wage_r" = rac.this$CE01,
		"mid_wage_j" = wac.this$CE02,
		"mid_wage_r" = rac.this$CE02,
		"high_wage_j" = wac.this$CE03,
		"high_wage_r" = rac.this$CE03,
		"information_j" = wac.this$CNS09,
		"information_r" = rac.this$CNS09,
		"finance_j" = wac.this$CNS10,
		"finance_r" = rac.this$CNS10,
		"professional_j" = wac.this$CNS12,
		"professional_r" = rac.this$CNS12,
		"management_j" = wac.this$CNS13,
		"management_r" = rac.this$CNS13))
	
	rm(wac.this); rm(rac.this)
	
}

write.table(outfile, "BayArea_LEHD_2009-2011 (Raw values).csv", sep = ",", row.names = FALSE)

# Balance and ratio outfiles

outfile <- data.frame()

for(year in years.to.download) {
	
	balance.this <- eval(parse(text = paste0("balance.", year)))
	ratio.this <- eval(parse(text = paste0("ratio.", year)))
	
	outfile <- rbind(outfile, data.frame(
		"placename" = balance.this$place,
		"year" = year,
		"total_bal" = balance.this$C000,
		"total_rat" = ratio.this$C000,
		"low_wage_bal" = balance.this$CE01,
		"low_wage_rat" = ratio.this$CE01,
		"mid_wage_bal" = balance.this$CE02,
		"mid_wage_rat" = ratio.this$CE02,
		"high_wage_bal" = balance.this$CE03,
		"high_wage_rat" = ratio.this$CE03,
		"information_bal" = balance.this$CNS09,
		"information_rat" = ratio.this$CNS09,
		"finance_bal" = balance.this$CNS10,
		"finance_rat" = ratio.this$CNS10,
		"professional_bal" = balance.this$CNS12,
		"professional_rat" = ratio.this$CNS12,
		"management_bal" = balance.this$CNS13,
		"management_rat" = ratio.this$CNS13))
	
	rm(balance.this); rm(ratio.this)
	
}

write.table(outfile, "BayArea_LEHD_2009-2011 (JER Measures).csv", sep = ",", row.names = FALSE)
