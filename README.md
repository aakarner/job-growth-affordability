Job growth, housing affordability, and commuting in the Bay Area
==========

The goal of this work is to understand how post-recession changes in job and housing markets are affecting affordability for workers earning in the lowest income tiers. To understand these patterns, we employ the [Longitudinal Employer-Household Dynamics Origin-Destination Employment Statistics (LODES)](http://lehd.ces.census.gov/data/) 
data and the [American Community Survey (ACS)](http://www.census.gov/acs/www/). 

Sequentially executing the scripts included in this repository will reproduce the analyses and figures developed by the study's authors. They are:
* 00_Download and summarize LODES data.R. This script downloads the requested years of LODES data (including workplace and residence area characteristics and origin-destionation commute flows) from the Census Bureau's website and aggregates to the census place level. Finally, it creates .Rdata files for each year and file type. 
* 01_Calculate balance measures.R. This scipt loads required residence and workplace area characteristics and calculates measures  