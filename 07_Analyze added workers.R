setwd("D:/Dropbox/Work/high-wage job growth")

# Load required data
load("data/BayAreaLEHD_od_FINAL.RData")
load("data/CountySkims_Google.RData")


# PROGRAM START

vars <- c("h_plc", "w_cty", "S000", "SA01", "SA02", "SA03", "SE01", "SE02", "SE03", "t1t2", "SI01", "SI02", "SI03")
cities <- unique(od.2011.place$w_plc)

# Read in housing value and vacancy indicators
value <- read.table("data/ACS/ACS_5yr_2013-2009_MedianValue&Vacancy.csv", sep = ",", header = TRUE, row.names = NULL,
	stringsAsFactors = FALSE)

value <- value[value$county == 1 | value$Geo_NAME %in% cities, ]

# Vacancy rates by affordability category
value.plot <- melt(value, id = c("Geo_NAME", "Geo_QName"))

ggplot(filter(value.plot, variable %in% c("vac_r", "vac_t1", "vac_t2")), aes(x = value, fill = variable)) + 
	geom_density(alpha = 0.2) + 
	xlab("vacancy rate") + ylab("density") +
	scale_fill_brewer(palette = "Set1", labels = c("all rentals", "tier 1 rentals", "tier 2 rentals")) + 
	scale_x_continuous(limits = c(-0, 0.25)) + 
	theme_bw() + theme(legend.title = element_blank(), legend.position = "bottom")

ggsave("output/VacRateDensity.png", height = 6, width = 8, dpi = 300)


vmt.impacts <- data.frame("place" = cities, 
	"s000_i" = 0, "se01_i" = 0, "se02_i" = 0, "se03_i" = 0, "t1t2_i" = 0, "sa01_i" = 0, "sa02_i" = 0, "sa03_i" = 0,
	"si01_i" = 0, "si02_i" = 0, "si03_i" = 0, 
	"s000_m" = 0, "se01_m" = 0, "se02_m" = 0, "se03_m" = 0, "t1t2_m" = 0, "sa01_m" = 0, "sa02_m" = 0, "sa03_m" = 0,
	"si01_m" = 0, "si02_m" = 0, "si03_m" = 0)

price.impacts <- data.frame("place" = cities, 
	"s000_i" = 0, "se01_i" = 0, "se02_i" = 0, "se03_i" = 0, "t1t2_i" = 0, "sa01_i" = 0, "sa02_i" = 0, "sa03_i" = 0,
	"si01_i" = 0, "si02_i" = 0, "si03_i" = 0, 
	"s000_m" = 0, "se01_m" = 0, "se02_m" = 0, "se03_m" = 0, "t1t2_m" = 0, "sa01_m" = 0, "sa02_m" = 0, "sa03_m" = 0,
	"si01_m" = 0, "si02_m" = 0, "si03_m" = 0)

vacancy.impacts <- data.frame("place" = cities, 
	"s000_i" = 0, "se01_i" = 0, "se02_i" = 0, "se03_i" = 0, "t1t2_i" = 0, "sa01_i" = 0, "sa02_i" = 0, "sa03_i" = 0,
	"si01_i" = 0, "si02_i" = 0, "si03_i" = 0, 
	"s000_m" = 0, "se01_m" = 0, "se02_m" = 0, "se03_m" = 0, "t1t2_m" = 0, "sa01_m" = 0, "sa02_m" = 0, "sa03_m" = 0,
	"si01_m" = 0, "si02_m" = 0, "si03_m" = 0)

for(city in cities) {
	flows <- od.2008.place[od.2008.place$w_plc == city, vars]
	names(flows)[3:length(vars)] <- paste0(names(flows[3:length(vars)]), "_2008")

	flows <- full_join(od.2009.place[od.2009.place$w_plc == city, vars], flows, 
		by = c("h_plc" = "h_plc", "w_cty" = "w_cty"))
	names(flows)[3:length(vars)] <- paste0(names(flows[3:length(vars)]), "_2009")

	flows <- full_join(od.2010.place[od.2010.place$w_plc == city, vars], flows, 
		by = c("h_plc" = "h_plc", "w_cty" = "w_cty"))
	names(flows)[3:length(vars)] <- paste0(names(flows[3:length(vars)]), "_2010")

	flows[is.na(flows)] <- 0

	flows <- ungroup(flows)
	
	# Average flows for first three-year period
	flows <- transmute(flows,
		h_plc = h_plc,
		w_cty = w_cty,
		s000_0810 = (S000_2008 + S000_2009 + S000_2010) / 3,
		se01_0810 = (SE01_2008 + SE01_2009 + SE01_2010) / 3,
		se02_0810 = (SE02_2008 + SE02_2009 + SE02_2010) / 3,
		se03_0810 = (SE03_2008 + SE03_2009 + SE03_2010) / 3,
		t1t2_0810 = (t1t2_2008 + t1t2_2009 + t1t2_2010) / 3,
		sa01_0810 = (SA01_2008 + SA01_2009 + SA01_2010) / 3,
		sa02_0810 = (SA02_2008 + SA02_2009 + SA02_2010) / 3,
		sa03_0810 = (SA03_2008 + SA03_2009 + SA03_2010) / 3,
		si01_0810 = (SI01_2008 + SI01_2009 + SI01_2010) / 3,
		si02_0810 = (SI02_2008 + SI02_2009 + SI02_2010) / 3,
		si03_0810 = (SI03_2008 + SI03_2009 + SI03_2010) / 3)

	flows <- full_join(od.2011.place[od.2011.place$w_plc == city, vars], flows, 
		by = c("h_plc" = "h_plc", "w_cty" = "w_cty"))
	names(flows)[3:length(vars)] <- paste0(names(flows[3:length(vars)]), "_2011")

	flows[is.na(flows)] <- 0

	# Eliminate improbable residences in southern california
	flows <- filter(flows, !h_plc %in% c(6079, 6029, 6071, 6083, 6111, 6037, 6059, 6065, 6073, 6025))
	# San Luis Obispo, Kern, San Bernardino, Santa Barbara, Ventura, Los Angeles, Orange, Riverside, San Diego, Imperial

	# Only capture positive changes, since we're interested in where workers are growing
	flows <- mutate(flows, 
		s000_mgnl = ifelse(S000_2011 - s000_0810 > 0, S000_2011 - s000_0810, NA),
		se01_mgnl = ifelse(SE01_2011 - se01_0810 > 0, SE01_2011 - se01_0810, NA),
		se02_mgnl = ifelse(SE02_2011 - se02_0810 > 0, SE02_2011 - se02_0810, NA),
		se03_mgnl = ifelse(SE03_2011 - se03_0810 > 0, SE03_2011 - se03_0810, NA),
		t1t2_mgnl = ifelse(t1t2_2011 - t1t2_0810 > 0, t1t2_2011 - t1t2_0810, NA),
		sa01_mgnl = ifelse(SA01_2011 - sa01_0810 > 0, SA01_2011 - sa01_0810, NA),
		sa02_mgnl = ifelse(SA02_2011 - sa02_0810 > 0, SA02_2011 - sa02_0810, NA),
		sa03_mgnl = ifelse(SA03_2011 - sa03_0810 > 0, SA03_2011 - sa03_0810, NA),
		si01_mgnl = ifelse(SI01_2011 - si01_0810 > 0, SI01_2011 - si01_0810, NA),
		si02_mgnl = ifelse(SI02_2011 - si02_0810 > 0, SI02_2011 - si02_0810, NA),
		si03_mgnl = ifelse(SI03_2011 - si03_0810 > 0, SI03_2011 - si03_0810, NA))

# 	flows <- mutate(flows, 
# 		s000_mgnl = S000_2011 - s000_0810,
# 		se01_mgnl = SE01_2011 - se01_0810,
# 		se02_mgnl = SE02_2011 - se02_0810,
# 		se03_mgnl = SE03_2011 - se03_0810,
# 		t1t2_mgnl = t1t2_2011 - t1t2_0810,
# 		sa01_mgnl = SA01_2011 - sa01_0810,
# 		sa02_mgnl = SA02_2011 - sa02_0810,
# 		sa03_mgnl = SA03_2011 - sa03_0810,
# 		si01_mgnl = SI01_2011 - si01_0810,
# 		si02_mgnl = SI02_2011 - si02_0810,
# 		si03_mgnl = SI03_2011 - si03_0810)
	
	skims <- bay.area.od[bay.area.od$d_place == city, ]
	
	flows <- left_join(flows, skims, by = c("h_plc" = "o_place"))
	places.to.skim$h_plc <- as.character(places.to.skim$h_plc)
	flows <- left_join(flows, select(places.to.skim, h_plc, w_cty, miles),
		by = c("h_plc" = "h_plc", "w_cty" = "w_cty"))

	flows$skim <- ifelse(is.na(flows$skim), flows$miles, flows$skim)

	flows <- mutate(flows, 
		# Overall
		s000_m_vmt = s000_mgnl * skim,
		s000_i_vmt = s000_0810 * skim,
		
		# Income
		se01_m_vmt = se01_mgnl * skim,
		se02_m_vmt = se02_mgnl * skim,
		se03_m_vmt = se03_mgnl * skim,
		se01_i_vmt = se01_0810 * skim,
		se02_i_vmt = se02_0810 * skim,
		se03_i_vmt = se03_0810 * skim,
		t1t2_i_vmt = t1t2_0810 * skim,
		t1t2_m_vmt = t1t2_mgnl * skim,
		
		# Age
		sa01_m_vmt = sa01_mgnl * skim,
		sa02_m_vmt = sa02_mgnl * skim,
		sa03_m_vmt = sa03_mgnl * skim,
		sa01_i_vmt = sa01_0810 * skim,
		sa02_i_vmt = sa02_0810 * skim,
		sa03_i_vmt = sa03_0810 * skim,
		
		# Industry
		si01_m_vmt = si01_mgnl * skim,
		si02_m_vmt = si02_mgnl * skim,
		si03_m_vmt = si03_mgnl * skim,
		si01_i_vmt = si01_0810 * skim,
		si02_i_vmt = si02_0810 * skim,
		si03_i_vmt = si03_0810 * skim)
	
	# Add in median house prices and vacancy rates at origin jurisdiction
	flows <- left_join(flows, select(value, Geo_NAME, crent_med, vac_r), by = c("h_plc" = "Geo_NAME"))
	
	# Vacancy rates
	flows <- mutate(flows, 
		# Overall
		s000_m_vac = s000_mgnl * vac_r,
		s000_i_vac = s000_0810 * vac_r,
		
		# Income
		se01_m_vac = se01_mgnl * vac_r,
		se02_m_vac = se02_mgnl * vac_r,
		se03_m_vac = se03_mgnl * vac_r,
		t1t2_m_vac = t1t2_mgnl * vac_r,
		se01_i_vac = se01_0810 * vac_r,
		se02_i_vac = se02_0810 * vac_r,
		se03_i_vac = se03_0810 * vac_r,
		t1t2_i_vac = t1t2_0810 * vac_r,
		
		# Age
		sa01_m_vac = sa01_mgnl * vac_r,
		sa02_m_vac = sa02_mgnl * vac_r,
		sa03_m_vac = sa03_mgnl * vac_r,
		sa01_i_vac = sa01_0810 * vac_r,
		sa02_i_vac = sa02_0810 * vac_r,
		sa03_i_vac = sa03_0810 * vac_r,
		
		# Industry
		si01_m_vac = si01_mgnl * vac_r,
		si02_m_vac = si02_mgnl * vac_r,
		si03_m_vac = si03_mgnl * vac_r,
		si01_i_vac = si01_0810 * vac_r,
		si02_i_vac = si02_0810 * vac_r,
		si03_i_vac = si03_0810 * vac_r)
	
	# Rents
	flows <- mutate(flows, 
		# Overall
		s000_m_rent = s000_mgnl * crent_med,
		s000_i_rent = s000_0810 * crent_med,
		
		# Income
		se01_m_rent = se01_mgnl * crent_med,
		se02_m_rent = se02_mgnl * crent_med,
		se03_m_rent = se03_mgnl * crent_med,
		t1t2_m_rent = t1t2_mgnl * crent_med,
		se01_i_rent = se01_0810 * crent_med,
		se02_i_rent = se02_0810 * crent_med,
		se03_i_rent = se03_0810 * crent_med,
		t1t2_i_rent = t1t2_0810 * crent_med,
		
		# Age
		sa01_m_rent = sa01_mgnl * crent_med,
		sa02_m_rent = sa02_mgnl * crent_med,
		sa03_m_rent = sa03_mgnl * crent_med,
		sa01_i_rent = sa01_0810 * crent_med,
		sa02_i_rent = sa02_0810 * crent_med,
		sa03_i_rent = sa03_0810 * crent_med,
		
		# Industry
		si01_m_rent = si01_mgnl * crent_med,
		si02_m_rent = si02_mgnl * crent_med,
		si03_m_rent = si03_mgnl * crent_med,
		si01_i_rent = si01_0810 * crent_med,
		si02_i_rent = si02_0810 * crent_med,
		si03_i_rent = si03_0810 * crent_med)
	
	
	## Trips
	trips.m <- apply(flows[, c("s000_mgnl", "se01_mgnl", "se02_mgnl", "se03_mgnl", "t1t2_mgnl", "sa01_mgnl",
		"sa02_mgnl", "sa03_mgnl", "si01_mgnl", "si02_mgnl", "si03_mgnl")  ], 2, sum, na.rm = TRUE)
	trips.i <- apply(flows[, c("s000_0810", "se01_0810", "se02_0810", "se03_0810", "t1t2_0810", "sa01_0810",
		"sa02_0810", "sa03_0810", "si01_0810", "si02_0810", "si03_0810")], 2, sum, na.rm = TRUE)
	
	## VMTs/Distances
	
	# marginal trips
	vmt.m <- apply(flows[, c("s000_m_vmt", "se01_m_vmt", "se02_m_vmt", "se03_m_vmt", "t1t2_m_vmt", "sa01_m_vmt",
		"sa02_m_vmt", "sa03_m_vmt", "si01_m_vmt", "si02_m_vmt", "si03_m_vmt")], 2, sum, na.rm = TRUE)
	
	vmt.impacts[vmt.impacts$place == city, 13:23] <- vmt.m / trips.m
	
	# original trips
	vmt.i <- apply(flows[, c("s000_i_vmt", "se01_i_vmt", "se02_i_vmt", "se03_i_vmt", "t1t2_i_vmt", "sa01_i_vmt",
		"sa02_i_vmt", "sa03_i_vmt", "si01_i_vmt", "si02_i_vmt", "si03_i_vmt")], 2, sum, na.rm = TRUE)
	
	vmt.impacts[vmt.impacts$place == city, 2:12] <- vmt.i / trips.i

	## Median contract rent
	
	# marginal contract rent
	rent.m <- apply(flows[, c("s000_m_rent", "se01_m_rent", "se02_m_rent", "se03_m_rent", "t1t2_m_rent", "sa01_m_rent",
		"sa02_m_rent", "sa03_m_rent", "si01_m_rent", "si02_m_rent", "si03_m_rent")], 2, sum, na.rm = TRUE)
	
	price.impacts[price.impacts$place == city, 13:23] <- rent.m / trips.m
	
	# original contract rent 
	rent.i <- apply(flows[, c("s000_i_rent", "se01_i_rent", "se02_i_rent", "se03_i_rent", "t1t2_i_rent", "sa01_i_rent",
		"sa02_i_rent", "sa03_i_rent", "si01_i_rent", "si02_i_rent", "si03_i_rent")], 2, sum, na.rm = TRUE)
	
	price.impacts[price.impacts$place == city, 2:12] <- rent.i / trips.i
	
	## Vacancy rates
	
	# marginal vacany rate
	vac.m <- apply(flows[, c("s000_m_vac", "se01_m_vac", "se02_m_vac", "se03_m_vac", "t1t2_m_vac", "sa01_m_vac",
		"sa02_m_vac", "sa03_m_vac", "si01_m_vac", "si02_m_vac", "si03_m_vac")], 2, sum, na.rm = TRUE)

	vacancy.impacts[vacancy.impacts$place == city, 13:23] <- vac.m / trips.m
	
	# original vacancy rates
	vac.i <- apply(flows[, c("s000_i_vac", "se01_i_vac", "se02_i_vac", "se03_i_vac", "t1t2_i_vac", "sa01_i_vac",
		"sa02_i_vac", "sa03_i_vac", "si01_i_vac", "si02_i_vac", "si03_i_vac")], 2, sum, na.rm = TRUE)
	
	vacancy.impacts[vacancy.impacts$place == city, 2:12] <- vac.i / trips.i
}
	
vmt.impacts <- vmt.impacts[complete.cases(vmt.impacts), ]

# For VMTs, higher is worse, so if the difference is positive, there's a problem
vmt.impacts <- mutate(vmt.impacts, 
	s000_gap = s000_m - s000_i,
	se01_gap = se01_m - se01_i,
	se02_gap = se02_m - se02_i,
	se03_gap = se03_m - se03_i,
	t1t2_gap = t1t2_m - t1t2_i)

vmt.impacts$name <- as.character(vmt.impacts$place)
vmt.impacts$name <- gsub(" city", "", vmt.impacts$name)

price.impacts <- mutate(price.impacts, 
	s000_gap = s000_m - s000_i,
	se01_gap = se01_m - se01_i,
	se02_gap = se02_m - se02_i,
	se03_gap = se03_m - se03_i,
	t1t2_gap = t1t2_m - t1t2_i,
	sa01_gap = sa01_m - sa01_i,
	sa02_gap = sa02_m - sa02_i,
	sa03_gap = sa03_m - sa03_i)

vacancy.impacts <- mutate(vacancy.impacts,
	s000_gap = s000_m - s000_i,
	se01_gap = se01_m - se01_i,
	se02_gap = se02_m - se02_i,
	se03_gap = se03_m - se03_i,
	t1t2_gap = t1t2_m - t1t2_i)

flows.plot <- melt(vmt.impacts, id = c("place", "name"))
price.plot <- melt(price.impacts, id = "place")
vac.plot <- melt(vacancy.impacts, id = "place")

levels(flows.plot$variable)[24:27] <- c("tier 1", "tier 2", "tier 3", "tier 1 + 2")
levels(price.plot$variable)[24:30] <- c("tier 1", "tier 2", "tier 3", "tier 1 + 2", "< 29", "30-54", "> 55")
levels(vac.plot$variable)[24:27] <- c("tier 1", "tier 2", "tier 3", "tier 1 + 2")

	place %in% c("San Francisco city", "San Jose city", "Oakland city", "Fremont city", "Sunnyvale city", 
			"Santa Rosa city", "Hayward city", "Mountain View city", "Redwood City city", "Concord city", 
			"San Leandro city", "Fairfield city", "Vallejo city", "Napa city", "Richmond city", "Vacaville city", 
			"Union City city", "Antioch city", "Pittsburg city")), 


# Average....

# VMT
flows.plot$name <- factor(flows.plot$name, levels = levels(reorder(vmt.impacts$name, vmt.impacts$s000_gap)))
ggplot(filter(flows.plot, variable %in% c("s000_gap") &
		place %in% c("San Francisco city", "San Jose city", "Oakland city", "Fremont city", "Sunnyvale city", 
			"Santa Rosa city", "Hayward city", "Mountain View city", "Redwood City city", "Concord city", 
			"San Leandro city", "Fairfield city", "Vallejo city", "Napa city", "Richmond city", "Vacaville city", 
			"Union City city", "Antioch city", "Pittsburg city")), 
	aes(x = value, y = name, color = variable)) + geom_point(size = 4) +
	scale_color_brewer(palette = "Dark2") + 
	xlab("incremental commute distance for added workers (miles)") + ylab("place-of-work jurisdiction") +
	theme_bw() + 
	theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12))

ggsave("output/AverageCommuteGap.png", width = 8, height = 6, dpi = 300)

# Prices - contract rent
price.plot$place <- factor(price.plot$place, levels = levels(reorder(price.impacts$place, price.impacts$s000_gap)))
ggplot(filter(price.plot, variable %in% c("s000_gap") &
		place %in% c("San Francisco city", "San Jose city", "Oakland city", "Fremont city", "Sunnyvale city", 
			"Santa Rosa city", "Hayward city", "Mountain View city", "Redwood City city", "Concord city", 
			"San Leandro city", "Fairfield city", "Vallejo city", "Napa city", "Richmond city", "Vacaville city", 
			"Union City city", "Antioch city", "Pittsburg city")), 
	aes(x = value, y = place, color = variable)) + 
	scale_color_brewer(palette = "Dark2") + 
	xlab("incremental difference in contract rent for added workers ($)") + ylab("place-of-work jurisdiction") +
	geom_point(size = 4) + theme_bw() + 
	theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12))

ggsave("output/AveragePriceGap.png", width = 8, height = 6, dpi = 300)

# Vacancy
vac.plot$place <- factor(vac.plot$place, levels = levels(reorder(vacancy.impacts$place, vacancy.impacts$s000_gap)))
ggplot(filter(vac.plot, variable %in% c("s000_gap") &
		place %in% c("San Francisco city", "San Jose city", "Oakland city", "Fremont city", "Sunnyvale city", 
			"Santa Rosa city", "Hayward city", "Mountain View city", "Redwood City city", "Concord city", 
			"San Leandro city", "Fairfield city", "Vallejo city", "Napa city", "Richmond city", "Vacaville city", 
			"Union City city", "Antioch city", "Pittsburg city")), 
	aes(x = value, y = place, color = variable)) + 
	scale_color_brewer(palette = "Dark2") + 
	xlab("incremental difference in rental vacancy rate for added workers") + 
	ylab("place-of-work jurisdiction") + geom_point(size = 4) + theme_bw() + 
	theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12))

ggsave("output/AverageVacGap.png", width = 8, height = 6, dpi = 300)


## Income category

# Flows
flows.plot$name <- factor(flows.plot$name, levels = levels(reorder(vmt.impacts$name, vmt.impacts$se01_gap)))
ggplot(filter(flows.plot, variable %in% c("tier 1", "tier 2", "tier 3") &
		place %in% c("San Francisco city", "San Jose city", "Oakland city", "Fremont city", "Sunnyvale city", 
			"Santa Rosa city", "Hayward city", "Mountain View city", "Redwood City city", "Concord city", 
			"San Leandro city", "Fairfield city", "Vallejo city", "Napa city", "Richmond city", "Vacaville city", 
			"Union City city", "Antioch city", "Pittsburg city")), 
	aes(x = value, y = name, color = variable)) + 
	geom_point(size = 4) + 
	scale_color_brewer(name = "worker category", palette = "Dark2") + 
	xlab("incremental commute distance for added workers (miles)") + ylab("place-of-work jurisdiction") + 
	theme_bw() + 
	theme(legend.position = "bottom", 
		axis.title = element_text(size = 14), axis.text = element_text(size = 12))

ggsave("output/AverageCommuteGap_Income.png", width = 8, height = 6, dpi = 300)

# Big 3 

flows.plot$name <- factor(flows.plot$name, levels = levels(reorder(vmt.impacts$name, vmt.impacts$se01_gap)))
ggplot(filter(flows.plot, variable %in% c("tier 1", "tier 2", "tier 3") &
		name %in% c("San Francisco", "San Jose", "Oakland")), 
	aes(x = value, y = name, color = variable)) + 
	scale_color_brewer(name = "worker category", palette = "Dark2") + 
	xlab("additional distance traveled for\n added workers (miles)") + ylab(NULL) + geom_point(size = 7) + theme_bw() + 
	theme(legend.position = "bottom", legend.text = element_text(size = 22), legend.title = element_text(size = 22),
		axis.title = element_text(size = 22), axis.text = element_text(size = 24))

ggsave("output/Big3_AverageCommuteGap_Income.png", width = 9, height = 6, dpi = 300)

# Prices
price.plot$name <- factor(price.plot$name, levels = levels(reorder(price.impacts$name, price.impacts$se01_gap)))
ggplot(filter(price.plot, variable %in% c("tier 1", "tier 2", "tier 3") & 
		place %in% c("San Francisco city", "San Jose city", "Oakland city", "Fremont city", "Sunnyvale city", 
			"Santa Rosa city", "Hayward city", "Mountain View city", "Redwood City city", "Concord city", 
			"San Leandro city", "Fairfield city", "Vallejo city", "Napa city", "Richmond city", "Vacaville city", 
			"Union City city", "Antioch city", "Pittsburg city")), 
	aes(x = value, y = place, color = variable)) + 
	scale_color_brewer(name = "worker category", palette = "Dark2") + 
	xlab("difference in average contract rent") + ylab("place-of-work jurisdiction") + geom_point(size = 4) + 
	theme_bw() + 
	theme(legend.position = "bottom", 
		axis.title = element_text(size = 14), axis.text = element_text(size = 12))

ggsave("output/AverageHousingPriceGap_Income.png", width = 8, height = 6, dpi = 300)


# Vacancy
vac.plot$place <- factor(vac.plot$place, levels = levels(reorder(vacancy.impacts$place, vacancy.impacts$se01_gap)))
ggplot(filter(vac.plot, variable %in% c("tier 1", "tier 2", "tier 3") & 
		place %in% c("San Francisco city", "San Jose city", "Oakland city", "Fremont city", "Sunnyvale city", 
			"Santa Rosa city", "Hayward city", "Mountain View city", "Redwood City city", "Concord city", 
			"San Leandro city", "Fairfield city", "Vallejo city", "Napa city", "Richmond city", "Vacaville city", 
			"Union City city", "Antioch city", "Pittsburg city")), 
	aes(x = value, y = place, color = variable)) + 
	scale_color_brewer(name = "worker category", palette = "Dark2") + 
	xlab("incremental difference in rental vacancy rate for added workers") + 
	ylab("place-of-work jurisdiction") + geom_point(size = 4) + theme_bw() + 
	theme(legend.position = "bottom", 
		axis.title = element_text(size = 14), axis.text = element_text(size = 12))

ggsave("output/AverageVacancyRateGap_Income.png", width = 8, height = 6, dpi = 300)


# Age
ggsave("output/AverageHousingPriceGap_Income.png", width = 8, height = 4, dpi = 300)
