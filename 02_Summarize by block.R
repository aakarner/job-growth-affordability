# Count increase in number of high wage jobs, increase in number of low wage jobs 
# that are in these blocks. And the increase of the low wage jobs elsewhere in the 
# jurisdiction. Do this for all NAICS categories too.

all.blocks <- union(wac.2011$w_geocode, wac.2009$w_geocode)

wac.block.2011 <- data.frame(block = all.blocks)
wac.block.2011 <- left_join(wac.block.2011, wac.2011, by = c("block" = "w_geocode"))

wac.block.2009 <- data.frame(block = all.blocks)
wac.block.2009 <- left_join(wac.block.2009, wac.2009, by = c("block" = "w_geocode"))

wac.block.2011.2009 <- cbind(wac.block.2011[, 2:52] - wac.block.2009[, 2:52], wac.block.2011[, c(1, 54:56)])

# Total number of blocks in which low-wage jobs grew from 2009 - 2011
sum(wac.block.2011.2009$CE01 > 0, na.rm = TRUE)
# 81,401

# Total number of blocks in which high-wage jobs grew from 2009 - 2011
sum(wac.block.2011.2009$CE03 > 0, na.rm = TRUE)
# 64,750

# Total number of blocks that experienced low wage job growth 
# with high wage job growth, 2009 - 2011
sum(wac.block.2011.2009$CE01 > 0 & wac.block.2011.2009$CE03 > 0, na.rm = TRUE)
# 36,395

# Total number of blocks that experienced low wage job growth with 
# NO high wage job growth or high-wage job decline, 2009 - 2011
sum(wac.block.2011.2009$CE01 > 0 & wac.block.2011.2009$CE03 <= 0, na.rm = TRUE)
# 45,006


# NET growth in low-wage jobs, only in high-wage locations
blk.growth <- tbl_df(wac.block.2011.2009)

# Count low-wage job growth where high-wage jobs grew
ce01.total.with.ce03 <- tbl_df(ddply(filter(blk.growth,  CE03 > 0), .(placename, ctyname), numcolwise(sum)))
ce01.total.with.ce03 <- filter(ce01.total.with.ce03, ctyname %in% c("Alameda County, CA", "Contra Costa County, CA", 
	"Marin County, CA", "Napa County, CA", "San Francisco County, CA", "Santa Clara County, CA", "San Mateo County, CA",
	"Solano County, CA", "Sonoma County, CA"))

# Count low-wage job growth elsewhere in the jurisdiction
ce01.total.without.ce03 <- tbl_df(ddply(filter(blk.growth,  CE03 <= 0), .(placename, ctyname), numcolwise(sum)))
ce01.total.without.ce03 <- filter(ce01.total.without.ce03, ctyname %in% c("Alameda County, CA", "Contra Costa County, CA", 
	"Marin County, CA", "Napa County, CA", "San Francisco County, CA", "Santa Clara County, CA", "San Mateo County, CA",
	"Solano County, CA", "Sonoma County, CA"))

multipliers.with <- data.frame("place" = union(ce01.total.with.ce03$placename, ce01.total.without.ce03$placename))
multipliers.with <- left_join(multipliers.with, ce01.total.with.ce03, by = c("place" = "placename"))

multipliers.without <- data.frame("place" = union(ce01.total.with.ce03$placename, ce01.total.without.ce03$placename))
multipliers.without <- left_join(multipliers.without, ce01.total.without.ce03, by = c("place" = "placename"))

multipliers <- cbind(multipliers.without[, 1:2], multipliers.without[, 3:53] / multipliers.with[, 3:53])
			