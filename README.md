Job growth, housing affordability, and commuting in the Bay Area
==========

The goal of this work is to understand how post-recession changes in job and housing markets are affecting affordability for workers earning in the lowest income tiers. To understand these patterns, we employ two main data sources, the [Longitudinal Employer-Household Dynamics Origin-Destination Employment Statistics (LODES)](http://lehd.ces.census.gov/data/) data and the [American Community Survey (ACS)](http://www.census.gov/acs/www/). Several other data sources are employed to generate estimates of roadway-traveled commute distances in the Bay Area. Preparation of these data requires some additional processing

Sequentially executing the scripts included in this repository will reproduce the analyses and figures developed by the study's authors. 

Overview of included scripts:
* 00_Download and summarize LODES data.R. This script downloads the requested years of LODES data (including workplace and residence area characteristics and origin-destionation commute flows) from the Census Bureau's website and aggregates to the census place level. Finally, it creates .Rdata files for each year and file type. 
* 01_Add skims.R. In order to add distance traveled data to the OD flows, this script creates a MonetDB database and connects 
* 02_Calculate balance measures.R. This scipt loads required residence and workplace area characteristics, calculates job-employed resident ratios for the Bay Area's 25 largest employers and visualizes the results. 
* 03_Map job growth.R. Create maps illustrating changes in job totals and proportions from the three-year 2008-2010 average to 2011.
* 04_Analyze flow data.R. Calculate metrics of internal capture from the OD flow data.
* 05_Analyze housing data.R. Visualize changes in job and housing totals in the 19 jurisdictions in the Bay Area with complete housing data available in the 2013-2011 and 2010-2008 thre-year ACS datasets.
* 06_Analyze correlations.R. Calculate correlation coefficients for some of the job and housing variables. 
* 07_Analyze added workers.R. Calculate and visualize the distances traveled for workers added in 2011 relative to the 2008-2010 three-year average.