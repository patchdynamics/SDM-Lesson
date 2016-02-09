# first create a new project to work in.

# install some libraries
devtools::install_github("ropensci/spocc")
devtools::install_github("ropensci/spoccutils")
install.packages("ggplot2")
install.packages("rJava")
install.packages("dismo")
install.packages("raster")


# load some libraries
library(spocc)
library(spoccutils)
library(ggplot2)
library(raster)
library(rgeos)
library(dismo)

# Red Tree Vole (Oregon) - Arborimus longicaudus

# get species data
species.occurence <- occ(query = "Arborimus longicaudus", 
                         from = c("gbif", "bison", "ecoengine", "inat"),
                         has_coords = TRUE,
                         limit = 1500)
nrow(species.occurence$gbif$data[[1]])
nrow(species.occurence$bison$data[[1]])
nrow(species.occurence$ecoengine$data[[1]])
nrow(species.occurence$inat$data[[1]])


#  You could also use MOL in the same way
#  TS.points.range  = MOLget("Tympanuchus pallidicinctus", type=c("points"))

# inspect locations of data
map_ggplot(species.occurence)
map_leaflet(species.occurence)

# inspect the data object returned
names(species.occurence)
names(species.occurence$gbif)
names(species.occurence$gbif$data)
names(species.occurence$gbif$data[[1]])

nrow(species.occurence$gbif$data[[1]]) # show me how many records I got

# look around at some of the information in this data (if it exists)
head(species.occurence$gbif$data[[1]]$occurrenceRemarks)
head(species.occurence$gbif$data[[1]]$type, 20)
head(species.occurence$gbif$data[[1]]$collectionCode, 20)

# save data locally so you don't have to query the internet every time
save(species.occurence, file='species.occurence.Rdata')

# load from the file instead of from the web
load('species.occurence.Rdata')

# optionally export to spreadsheet for visualize cleaning / analysis
write.csv(species.occurence$bison$data[[1]], file='my_occurences.gbif.csv')
write.csv(species.occurence$bison$data[[1]], file='my_occurences.bison.csv')

# Save occurence data to a shapefile so we can explore it in QGIS
# We are just going to use results from BISON database for now
occurences.bison = species.occurence$bison$data[[1]]
occurences = occurences.bison[!is.na(occurences.bison$latitude) & !(occurences.bison$latitude == 0),]  # here we filter out records with missing coordinates
coordinates(occurences) = c('longitude', 'latitude')
crs(occurences) = CRS("+init=epsg:4326")  # 4326 is WGS84, the standard lat/lon coordinate system
shapefile(occurences, 'occurences', overwrite=TRUE)

# In QGIS create the project area

project.area = shapefile('ProjectAreaArborimus.shp')


# You could also load only occurence points matching your project area
wkt = writeWKT(project.area)   # this converts the shape into the Well Known Text format
species.occurence = occ(query = "Arborimus longicaudus", 
                        geometry=wkt,
                        from = c("gbif", "bison", "ecoengine", "inat"),
                        has_coords = TRUE,
                        limit = 1500)
occurences.bison = species.occurence$bison$data[[1]]
occurences = occurences.bison[!is.na(occurences.bison$latitude) & !(occurences.bison$latitude == 0),]  # here we filter out records with missing coordinates
coordinates(occurences) = c('longitude', 'latitude')
crs(occurences) = CRS("+init=epsg:4326")  # 4326 is WGS84, the standard lat/lon coordinate system
shapefile(occurences, 'occurences_project')


# Loading BioClim data
# this can happen a few ways
# 1) Load directly into R using the methods Giuseppe demo'd.  To do this you should know coordinates
# within each time that you need
#
# bioclim = getData('worldclim' , var='bio' , res=0.5, lon=-40, lat=-15 ) 
#
# 2) Download the entire BioClim data set manually http://www.worldclim.org/tiles.php
# and then clip to your region
# We'll use this method for clarity.




# Now we load all the bioclim data into a RasterStack
bioclim = stack('GIS/bio_11/bio1_11.tif', 'GIS/bio_11/bio2_11.tif', 'GIS/bio_11/bio3_11.tif',
                'GIS/bio_11/bio4_11.tif', 'GIS/bio_11/bio5_11.tif', 'GIS/bio_11/bio6_11.tif',
                'GIS/bio_11/bio7_11.tif', 'GIS/bio_11/bio8_11.tif', 'GIS/bio_11/bio9_11.tif',
                'GIS/bio_11/bio10_11.tif', 'GIS/bio_11/bio11_11.tif', 'GIS/bio_11/bio12_11.tif',
                'GIS/bio_11/bio13_11.tif', 'GIS/bio_11/bio14_11.tif', 'GIS/bio_11/bio15_11.tif',
                'GIS/bio_11/bio16_11.tif', 'GIS/bio_11/bio17_11.tif', 'GIS/bio_11/bio18_11.tif',
                'GIS/bio_11/bio19_11.tif' )
names(bioclim)  # see what to call the layers

# plot one layer
par(mfrow=c(1,1))
plot(bioclim$bio1_11) 

# plot all (first 16) layers
plot(bioclim)

# crop bioclim to study area
bioclim.local = crop(bioclim, project.area)
plot(bioclim.local)

# save the raster stack as a brick
writeRaster(bioclim.local, 'GIS/bioclim.Arborimus.local.tiff', overwrite=TRUE)
# or write out as individual files
writeRaster(bioclim.local, 'GIS/bioclim.Arborimus.local.tiff', bylayer=TRUE, overwrite=TRUE)




# first we must resample our occurence data to deal with sampling bias
project.area.raster = raster(project.area)       # create a grid within the project area
res(project.area.raster) = res(bioclim.local) * 10  # set the grid resolution to bioclim resolution, a multiple thereof
occurences.resampled = gridSample(occurences, project.area.raster , n=1)  # do the resampling
occurences.resampled = as.data.frame(occurences.resampled)   # convert results to a data.frame for ease of use
occurences.resampled$Presence = 1

nrow(occurences)
nrow(occurences.resampled)

# make the resampled output a spatial object
coordinates(occurences.resampled) = c('longitude', 'latitude')   # assigns coordinates
crs(occurences.resampled) = crs(occurences)                      # assigns map projection

# let's compare results
plot(occurences)
lines(project.area, col='blue', typ='l')
points(occurences.resampled, pch=1, col='red')


# let's save this as a shapefile so we can have a closer look
shapefile(occurences.resampled, 'occurences_resampled', overwrite=TRUE)


#
#
# Finally time to do some spatial regression / prediction!
#
#

# rename variables for simplicity
predictors = bioclim.local                      
presence.locations = occurences.resampled

# first let's create a training subset
training.locations = presence.locations[sample(nrow(presence.locations), 50),]

# take a look..
plot(presence.locations)
points(training.locations, pch=1, col='red')

# prepare the data for predictions
presence.predictors = raster::extract(predictors, presence.locations )  # extract values from rasters for each point
presence.predictors = as.data.frame(presence.predictors)   # change to data frame object just because             

present = rep(1, nrow(presence.predictors ))   # a column of ones identifies these as the values for presence
presence.predictors = cbind(present, presence.predictors)  # assemble the complete matrix
head(presence.predictors)  # take look for understanding


# with presence and background (pseudo-absence) values
# first the presense values as before
predictors = bioclim.local                      # rename variables for simplicity
presence.locations = occurences.resampled 

presence.predictors = raster::extract(predictors, presence.locations )  # extract values from rasters for each point
presence.predictors = as.data.frame(presence.predictors)   # change to data frame object just because             

# now get background values
background.points = randomPoints(project.area.raster, 500)
background.predictors = extract(predictors, background.points)
present.or.background = c(rep(1, nrow(presence.predictors)), rep(0, nrow(background.predictors)))
presence.background.predictors = cbind(present.or.background, rbind(presence.predictors, background.predictors)  )
head(presence.background.predictors)  # take a look 

# because we are on the coast, some of our random points could have fallen in the water
presence.background.predictors = presence.background.predictors[!is.na(presence.background.predictors$bio1_11),]

# now we can think about this as a logistic regression
# one might check graphs for logistic response, for instance
plot(presence.background.predictors$bio1_11, presence.background.predictors$present)
plot(presence.background.predictors$bio2_11, presence.background.predictors$present)
plot(presence.background.predictors$bio3_11, presence.background.predictors$present)
plot(presence.background.predictors$bio4_11, presence.background.predictors$present)
plot(presence.background.predictors$bio5_11, presence.background.predictors$present)
plot(presence.background.predictors$bio6_11, presence.background.predictors$present)


# and see if we can get a good prediction, by setting up a formula that makes sense
formula = present.or.background ~ bio6_11 + bio4_11  # be selective
#formula = present.or.background ~ .   # or just rhow in the kitchen sink (but might suffer from correlated predictors)

# compute the model and check the AIC
glm.model = glm(formula, data = presence.background.predictors, family=binomial(link = logit))  # family=binomial(link = logit) makes this a logistic regression
summary(glm.model)  # we want to have the lowest AIC

# compute the predicted distribution
glm.prediction = predict(predictors, glm.model)

# plot the results
par(mfrow=c(1,1))    # this just makes sure we only have 1 plot on the screen       
plot(glm.prediction)
points(training.locations, pch=10, col='blue')
points(occurences)


# write the prediction out to a file to view in QGIS
# note that 'zoom' doesn't always layout correctly, QGIS better for visual analysis!
writeRaster(glm.prediction, 'GIS/predictions/glm.prediction.tif', overwrite=TRUE)




# instead of do a direct linear regression, we can use more advanced models
# fit the 'bioclim' (climate-envolope) model, which only uses presence locations and predictors
bioclim.model = bioclim(predictors, training.locations)
bioclim.prediction = predict(predictors, bioclim.model)

par(mfrow=c(1,1))      
plot(bioclim.prediction)
points(training.locations, pch=10, col='blue')
points(occurences)


# we can also write this prediction out to a file to view in QGIS if we want to
writeRaster(bioclim.prediction, 'GIS/predictions/bioclim.prediction.tiff', overwrite=TRUE)


#
# these predictors didn't work very well
# let's get some other data
bbox(bioclim.local)  # use this to download other data from earthexplorer, worldgrids, etc.

#
# bring in an elevation layer
#
elevation = raster('GIS/DEMSRE2a.tif')
elevation = projectRaster(elevation, bioclim.local) # clip and project to our project area
predictors = stack(bioclim.local, elevation)
names(predictors)[20] = "elevation"

# then do the same stuff we did before
presence.predictors = raster::extract(predictors, presence.locations )  # extract values from rasters for each point
presence.predictors = as.data.frame(presence.predictors)   # change to data frame object just because             
background.points = randomPoints(project.area.raster, 500)
background.predictors = extract(predictors, background.points)
present.or.background = c(rep(1, nrow(presence.predictors)), rep(0, nrow(background.predictors)))
presence.background.predictors = cbind(present.or.background, rbind(presence.predictors, background.predictors)  )
presence.background.predictors = presence.background.predictors[!is.na(presence.background.predictors$bio1_11),]

plot(presence.background.predictors$elevation, presence.background.predictors$present)

glm.model = glm(formula = present.or.background ~ bio6_11 + bio4_11 + elevation # extend our model 
                , data = presence.background.predictors)
glm.prediction = predict(predictors, glm.model)

plot(glm.prediction)
points(training.locations, pch=10, col='blue')
points(occurences)

writeRaster(glm.prediction, 'GIS/predictions/glm.prediction.elevation.2.tif', overwrite=TRUE)



#
# Hmm, was elevation really a good idea ?
# maybe bringing in NLCD is going to help us
#
NLCD.local = raster('GIS/NLCD.local.tif')
NLCD.local = projectRaster(NLCD.local, predictors)
stack(predictors, NLCD.local)  # etc...




# fit a maxent model
# this requires a special file
# download from http://www.cs.princeton.edu/~schapire/maxent/
# on MacOS X then, using terminal:
# cp maxent/maxent.jar /Library/Frameworks/R.framework/Versions/3.2/Resources/library/dismo/java/maxent.jar
library(rJava)
predictors = bioclim.local
maxent.model = maxent(predictors, training.locations)

maxent.prediction = predict(predictors, maxent.model)

plot(maxent.prediction)
points(training.locations, pch=10, col='blue')
points(occurences)

writeRaster(maxent.prediction, 'GIS/predictions/maxent.prediction.tif', overwrite=TRUE)





