wac.diff

ggplot(wac.diff, aes(x = total_jobs)) + geom_histogram()

# Pairwise correlations
# Ideas from: http://www.gettinggeneticsdone.com/2012/08/more-on-exploring-correlations-in-r.html

## Correlation matrix with p-values. See http://goo.gl/nahmV for documentation of this function
corProb <- function (X, dfr = nrow(X) - 2) {
  R <- cor(X, use="pairwise.complete.obs")
  above <- row(R) < col(R)
  r2 <- R[above]^2
  Fstat <- r2 * dfr/(1 - r2)
  R[above] <- 1 - pf(Fstat, 1, dfr)
  R[row(R) == col(R)] <- NA
  R
}

## Use this to dump the cor.prob output to a 4 column matrix
## with row/column indices, correlation, and p-value.
## See StackOverflow question: http://goo.gl/fCUcQ
flattenSquareMatrix <- function(m) {
  if( (class(m) != "matrix") | (nrow(m) != ncol(m))) stop("Must be a square matrix.") 
  if(!identical(rownames(m), colnames(m))) stop("Row and column names must be equal.")
  ut <- upper.tri(m)
  data.frame(i = rownames(m)[row(m)[ut]],
             j = rownames(m)[col(m)[ut]],
             cor=t(m)[ut],
             p=m[ut])
}

big.3 <- wac.diff[wac.diff$placename %in% c("San Francisco city, CA", "Oakland city, CA", "San Jose city, CA"), ]
big.25 <- wac.diff[wac.diff$total_jobs > threshold, ]
big.23 <- wac.diff[wac.diff$total_jobs > threshold & !wac.diff$placename %in% c("San Francisco city, CA", "Mountain View city, CA"), ]
others <- wac.diff[wac.diff$total_jobs < threshold, ]

big.25$place <- gsub(" city, CA", "", big.25$placename)
big.25$place <- gsub(" County, CA", "", big.25$place)

# Income categories
cor.data <- others[, c(8:10)]
cor.data <- big.25[, c(8:10)] 

# Race categories
cor.data <- wac.diff[, c(3, 31:36)]

# Industry categories
cor.data <- others[, c(55:56)]
cor.data <- big.3[, c(55:56)]
cor.data <- big.25[, c(55:56)]
cor.data <- big.23[, c(55:56)]

# correlation matrix
cors <- cor(cor.data)
 
# correlation matrix with p-values
corProb(cor.data)
 
# "flatten" that table
write.table(flattenSquareMatrix(cor.prob(cor.data)), "LEHD_Change_Industry_Correlations.csv", sep = ",", 
	row.names = FALSE)

# Scatterplot: high vs. low wage NAICS codes
ggplot(wac.diff, aes(x = naics_hi, y = naics_lo, label = placename)) + geom_point() + 
	geom_text(data = filter(wac.diff, naics_hi > 2500 | naics_hi < -5000)) + 
	xlab("change in high-wage NAICS jobs") + ylab("change in low-wage NAICS jobs") +
	theme_bw()

ggsave("output/Scatter_HiLo.svg", width = 8, height = 6)

# Scatterplot: high vs. low wage categories
ggplot(wac.diff, aes(x = CE02, y = CE03, label = placename)) + geom_point() + 
	# geom_text(data = filter(wac.diff, naics_hi > 2500 | naics_hi < -5000)) + 
	xlab("change in tier 1 jobs (< $1,251/month)") + ylab("change in tier 3 jobs (> $3,333/month)") +
	theme_bw()

ggsave("output/Scatter_HiLo.svg", width = 8, height = 6)



big.25$place <- factor(big.25$place, levels = levels(reorder(as.factor(big.25$place), big.25$naics_lo/big.25$naics_hi)))
	
ggplot(filter(big.25, naics_hi > 0 & naics_lo > 0), aes(x = naics_lo / naics_hi, y = place)) + geom_point() + 
	xlab("change in low-wage NAICS jobs per change in high-wage NAICS jobs") + ylab(NULL) +
	#scale_x_continuous(breaks = c(-3, -1, 1, 3, 5)) + 
	geom_vline(aes(xintercept = 1), col = "#EBC79E") + 
	theme_bw()

ggsave("output/LoJobsperHi.png", width = 7, height = 5, dpi = 300)


ggplot(big.25, aes(x = naics_hi, y = naics_lo)) + geom_point()

ggplot(wac.diff, aes(x = CNS07, y = CNS18)) + geom_point()

# plot the data
library(PerformanceAnalytics)
chart.Correlation(cor.data)


# Jobs-housing
jobs.housing <- jobs.housing[jobs.housing$overall == 1, ]

# Tier 1
cor.data <- jobs.housing[, c(8:10, 58, 62, 66)]
corProb(cor.data)

# Tier 2
cor.data <- jobs.housing[, c(8:10, 60, 64, 68)]
corProb(cor.data)

# Tier 1 + 2
jobs.housing$ce01ce02 <- jobs.housing$CE01 + jobs.housing$CE02
cor.data <- jobs.housing[, c(82, 76:78)]
corProb(cor.data)

# Total
cor.data <- jobs.housing[, c(4, 70, 72, 74)]
corProb(cor.data)

# Naics hi and rentals
cor.data <- jobs.housing[, c(55, 58, 60, 70)]
corProb(cor.data)
