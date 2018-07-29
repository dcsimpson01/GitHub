# Author: David Simpson
# Date: 2018.07.16
# Updated: 2018.07.16
# Script: Lessons.R
# Resource: https://homepage.divms.uiowa.edu/~luke/classes/STAT4580/maps.html

rm(list=ls())
# Libraries
library("ggplot2")
library("tidyr")
library("reshape2")
library("dplyr") # view data better
library("foreign") # Read STATA files
library("haven") # Needed to read STATA files
library("gmodels") # will allow you to use CrossTable() to do cross tabs
# install.packages("mapproj")
library("mapproj")

setwd("/Users/dsimp/GitHub/IowaMapCourse/Raw/CensusPop") # Identify location for raw data

if (! file.exists("PEP_2017_PEPANNRES.zip")) {
  download.file("http://www.stat.uiowa.edu/~luke/data/PEP_2017_PEPANNRES.zip",
                "PEP_2017_PEPANNRES.zip")
  unzip("PEP_2017_PEPANNRES.zip")
}

pep2017 <- read.csv("PEP_2017_PEPANNRES_with_ann.csv", stringsAsFactors = FALSE)

setwd("/Users/dsimp/GitHub/IowaMapCourse/Raw/CensusPop")

glimpse(pep2017)
head(pep2017)
tail(pep2017)

# Looking at data
head(filter(pep2017, grepl(", Iowa", GEO.display.label)))
pep2017[1803,]
filter(pep2017, GEO.id2 ==19141)
filter(pep2017, GEO.id2 == 22001)


# Safer to work with FIPS Councty Code
library(maps)
head(county.fips) # A file available in the maps library
cfp = left_join(county.fips, select(pep2017, pop=respop72017, fips = GEO.id2), "fips")
# The above joins fips code information with the info from pep2017 using select()
head(cfp)
cfp = mutate(cfp, state = sub(",.*", "", polyname)) #creates a new state column
head(cfp)

# *******************************
# Basic Map Data
# *******************************

# Draw Simple Map of the United States
# Map data from the map() function in package maps consists of the x and y
# coordinates of polygons and names for the regions.
usa = map("state", fill = TRUE, plot = FALSE)
plot(usa$x,usa$y, type="n")
polygon(usa$x, usa$y)

sum(is.na(usa$x))
length(usa$names)
usa$names

# GGPLOT
# The ggplot2 function map_data reorganizes this data into a data frame
library(ggplot2)
gusa = map_data("state") # reorganize data for a data frame!
head(gusa)
head(filter(gusa, region =="virginia"))

# Plot the USA with GGPLOT
p = ggplot(usa) + 
  geom_polygon(aes(long, lat, group = group), color = "white")
p

# *******************************
# Approximate Centroids
# *******************************

# A quick approximation to the centroids (center of gravity) of the
# polygons is to compute the center of the bouding rectangle. This
# is easiest to do with data from map_data

state_centroids = summarize(group_by(gusa, region),
                            x = mean(range(long)), y = mean(range(lat)))
names(state_centroids)[1] = "state"
head(state_centroids)

# *******************************
# Projections
# *******************************

# Longitude/latitude position points on a sphere; maps are drawn
# on a flat surface. A simple approach, the default in map, uses
# rectangular projection with the aspect raito chose so that 
# longitude and latitude scales are equivalent at the center of
# the map

map("state") # longitude and latitude scales are equivalent at center
# Other projections try to preserve angles or areas
map("state", project  = "mercator")
map("state", project = "bonne", param = 45)

# Adding points or line to a map drawn by the map() function
# with the default projection can be added using points or
# lines.

map("state")
with(state_centroids, points(x,y))

# When using other projections with map(), any points or
# lines added to the plot need to have their coordinates
# projected as well. This can be done using  mapproject()
# from the mapproj() package.

library(mapproj)
map("state", project = "mercator")
msc = mapproject(state_centroids$x, state_centroids$y)
points(msc$x, msc$y)

map("state", project = "bonne", param = 45)
bsc = mapproject(state_centroids$x, state_centroids$y)
points(bsc$x, bsc$y)

# Using coord_quickmap in ggplot seems to be comparable to
# the map default.
#     # coord_map can be used to specify more sophisticated projections
#     # The projections applied with coord_map() will be used in all layers

pp = p + geom_point(aes(x,y), data = state_centroids, color = "red")
pp + coord_quickmap() # quickmap fixes the picture

pp + coord_map() # can be used for sophisticated projections

pp + coord_map("bonne", parameters = 45)

# *******************************
# Symbol Plots of State Population
# *******************************

# Aggregate the county populations to the state level:
state_pops = summarize(group_by(cfp, state),
                       pop = sum(pop, na.rm = TRUE))
# Group By Code!!
state_pops = left_join(state_pops, state_centroids, "state")
head(state_pops)
# Dot Plots
library(lattice)
dotplot(state ~ pop, data = state_pops) # State vs Pop
dotplot(reorder(state, pop) ~ pop, data = state_pops) # order by pop
# The population distribution is heavily skewed; a color coding
# would need to account for this.

# A proportional symbol map using circles and map():
map("state")
with(state_pops,
     symbols(x,y,
             circles = sqrt(pop), add = TRUE,
             inches = 0.1, bg = "black"))

# The thermometer symbol corresponds to the approach suggested
# by Cleveland and McGiill (1984)

map("state")
with(state_pops, symbols(x, y,
                         thermometers = cbind(1, 3, pop / max(pop)),
                         inches = .3, add = TRUE))


# A proportional symbol map using ggplot
ggplot(gusa) +
  geom_polygon(aes(long, lat, group = group),
               fill = NA, color = "grey") +
  geom_point(aes(x, y, size = pop), data = state_pops) +
  scale_size_area() +
  coord_map("bonne", parameters=45) +
  ggthemes::theme_map()

# *************************************
# Chloropleth Maps of State Population
# *************************************
sp = select(state_pops, region = state, pop)
gusa_pop = left_join(gusa, sp,"region")

ggplot(gusa_pop) + 
  geom_polygon(aes(long,lat,group = group, fill = pop)) +
  coord_map("bonne", parameters = 45) + 
  ggthemes::theme_map()

# This image is dominated by the fact that most state populations are small
# Showing population ranks, or percentile vlaues can help see the
# the variation better.

spr = mutate(sp, rpop = rank(pop))
gusa_rpop = left_join(gusa, spr, "region")

ggplot(gusa_rpop) +
  geom_polygon(aes(long, lat, group = group, fill = rpop))+
  coord_map("bonne", parameters = 45) +
  ggthemes::theme_map()

# USING QUITILE BINS INSTEAD OF A CONTINUOUS SCALE

ncls = 6
spr = mutate(spr,
             pcls = cut(pop,quantile(pop, seq(0,1,len = ncls)),
                        include.lowest =  TRUE))
gusa_rpop = left_join(gusa, spr, "region")


ggplot(gusa_rpop) +
  geom_polygon(aes(long, lat, group = group, fill = pcls),
               color = "grey") +
  coord_map("bonne", parameters=45) +
  ggthemes::theme_map() +
  scale_fill_brewer(palette = "Reds")

# A percentile bin map using the map() funciton requires a vector of 
# colors for the regions
# library(RColorBrewer)
usa_states = sub(":.*", "", usa$names)
usa_pcls = spr$pcls[match(usa_states, spr$region)]
pal = RColorBrewer::brewer.pal(nlevels(usa_pcls), "Reds")
map("state",fill = TRUE, col = pal[usa_pcls], border = "grey")
# *************************************
# Chloropleth Maps of State Population
# *************************************

?brewer.pal()

install.packages(c('getopt', 'cacheSweave', 'stashR'))
install.packages('pgfSweave', repos='http://R-Forge.R-project.org')


?symbols()
?reorder
?cut()
quantile(spr$pop, seq(0,1,len =ncls))
