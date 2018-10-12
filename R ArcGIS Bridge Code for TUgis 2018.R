# R Code Example for TUgis 2018 Conference Presentation
# Point-of-contact: Jonathan Gross, jonathan.gross@baltimorecity.gov
# Disclaimer: This code is for educational purposes and comes with no guarantees.
# Created using R 3.4.3 and R Studio 1.1.423 

# Instructions
# Download the ZIP file from https://goo.gl/EqxjE3 and unzip it.
# Review presentation slides.
# Next, install the R-ArcGIS Bridge before running. Then run a few lines of code below at a time.
# Setup your Working Directory and file pathnames on lines 24, 53, and 113.
# Open crime data was downloaded from Open Baltimore (https://data.baltimorecity.gov/Public-Safety/BPD-Part-1-Victim-Based-Crime-Data/wsfq-mvij/data)
# The Inside/Outside column was missing/null at the time of download. See the *.csv file.

# Sets timezone
Sys.setenv(TZ='EST')

# Loads arcgisbinding package/library .
# Note: This package has already been installed by the Python Toolbox/R-ArcGIS Bridge.
# See also, arcgisbinding vignette at: https://r-arcgis.github.io/assets/arcgisbinding-vignette.html.
library(arcgisbinding)
arc.check_product()

# Set YOUR Working Directory. Use Forward Slashes / or //.
setwd('YOUR WORKING DIRECTORY HERE')

# Method 1 use arcgisbinding library to Import Data: 
# Imports a feature class of open crime data.
input<-file.path(getwd(),'OpenCrimeData.gdb','PartICrime_Projected')
open_crime<-arc.open(input)

# Creates a tabular dataframe (r_open_crime) and converts Spatial Data to Sp Objects (spatial_open_crime).
r_open_crime<-arc.select(open_crime)
spatial_open_crime<-arc.data2sp(r_open_crime)

# Fixes dates that were imported incorrectly
r_open_crime$CrimeDate <- as.Date(r_open_crime$CrimeDate,format ='%m/%d/%Y' )
spatial_open_crime$CrimeDate <- as.Date(spatial_open_crime$CrimeDate,format ='%m/%d/%Y' )

###############################################################################################################
# Alternatively, you can import using the rgdal library.
# Uncomment and install packages BEFORE running. 
# This prevents overwriting your version of a package--if you already have them installed.
# The sp and sf packages are important spatial packages. Consider adding dependencies=TRUE.

#install.packages("sp")
#install.packages("rgdal")
#install.packages("sf")
library(sp)
library(rgdal)
library(sf)

# Crates variable for geodatabase location and calls open GIS data drivers
geodata <- 'YOUR FILEPATH TO THE GEODATABASE ENDING in OpenCrimeData.gdb'
subset(ogrDrivers(), grepl("GDB", name))

# Reads OGR vector into Spatial objects
fc_list <- ogrListLayers(geodata)
print(fc_list)

# Read the feature class
spatial_rgdal_data <- readOGR(dsn=geodata,layer="PartICrime_Projected")
tabular_rgdal_data <- st_read(dsn=geodata,"PartICrime_Projected")

###############################################################################################################
# Learning more about our tabular data, what data type, variable names, examples of values
str(r_open_crime)
head(r_open_crime)

# Creates a variable for year
r_open_crime$year<-format(r_open_crime$CrimeDate,"%Y")
spatial_open_crime$year<-format(spatial_open_crime$CrimeDate,"%Y")

# Frequency Tables for Year, Crime Type (Homicide, Non-fatal Shooting) and Cross-Tabulation
#install.packages("summarytools")
library(summarytools)
freq(r_open_crime$year,order="freq")
freq(r_open_crime$Description,order="freq")
ctable(r_open_crime$year,r_open_crime$Description )

# Subset/keep Homicides, and then as an Example of Aggregation, Summarize by Neighborhood
#install.packages("stats")
library(stats)
r_open_crime<-r_open_crime[r_open_crime$Description=="HOMICIDE",]

# And/or you could keep the year.
#r_open_crime<-r_open_crime[r_open_crime$year==2017,]
crime_sum<-aggregate(r_open_crime$Total_Incidents~r_open_crime$Neighborhood,data=r_open_crime,sum)

# Note: Total_Incidents actually refers to the number of victims (not incidents). Each row is a victim.
###############################################################################################################

# Examples of a Treemap Visualization
# install.packages("treemap")
# install.packages("RColorBrewer")
library(treemap)
library(RColorBrewer)
colnames(crime_sum)<-c("Neighborhood","Total_Incidents")
treemap(crime_sum,index=c("Neighborhood"),
        vSize=c("Total_Incidents"),
        palette="Reds",
        vColor="Total_Incidents",
        fontsize.labels = 8,
        title="Baltimore City: Number of Homicide Victims by Neighborhood, January 2012 - Summer 2018",
        mapping=c(0,10,50),type="value",
        force.print.labels = TRUE)

###############################################################################################################
# Example of R as a GIS. We will use a loop to create kernel density of homicides by month for 2017.
# Note: Here we use the spatial data file: spatial_open_crime instead of the tabular data.

# Add Boundary File
# Imports Neighborhood Census boundary/polygon file 
census<-arc.open(path='YOUR FILE PATHWAY ending in OpenCrimeData.gdb/Neigh_Census_2010_Projected')
r_census<-arc.select(census)
neighborhoods<-arc.data2sp(r_census)
# You'll receive a warning message ignoring length and area fields.

  # Removes misc datasets
    remove(r_census)
    remove(census)
    
# Installs required package.
#install.packages("GISTools")
#install.packages("raster")
#install.packages("spatstat")
library(GISTools)
library(raster)
#library(spatstat)
    
# Kernel Density Plots by Month for 2017 Homicides. Subsets data.
spatial_open_crime<-spatial_open_crime[spatial_open_crime$Description=="HOMICIDE",]
spatial_open_crime<-spatial_open_crime[spatial_open_crime$year==2017,]
spatial_open_crime$month<-format(spatial_open_crime$CrimeDate,"%m")

# New graphics frame
plot.new()

# Set graphical parameters. Map sequence in 3 rows, 4 columns
par(mfrow=c(3,4),mai=c(0.5,0.5,0.5,0.5))

# Create loop and add kernel density
for (month in c("01","02","03","04","05","06","07","08","09","10","11","12")){
  density<-kde.points(spatial_open_crime[spatial_open_crime$month==month,],lims=neighborhoods)
  level.plot(density)
  masker<-poly.outer(density,neighborhoods,extend=100)
  add.masking(masker)
  plot(spatial_open_crime[spatial_open_crime$month==month,],pch=20,cex=1.5,add=TRUE)
  title(main=month,outer=F)
}

# Places title around border with map series in the middle.
title(main="Baltimore City: Kernel Density for Homicides by Month, 2017",outer=T,line=-1)

# Note: You will receive a warnings about projections because of how this particular map is created.
# There are other packages (e.g. spatstat) to calculate kernel density in R, which are easier to re-project (spTransform).

# For more information on projected and geographic coordinate systems in R,
# see: https://www.nceas.ucsb.edu/~frazier/RSpatialGuides/OverviewCoordinateReferenceSystems.pdf

### END OF PROGRAM ###