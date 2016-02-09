# load some libraries
library(spocc)
library(spoccutils)
library(ggplot2)
library(raster)
library(rgeos)
library(dismo)

# get species data
species.occurence <- occ(query = "Tympanuchus pallidicinctus", 
                         from = c("gbif", "bison", "ecoengine"),
                         has_coords = TRUE,
                         limit = 1500)

# look at species data
map_ggplot(species.occurence)

# create raster stack
bioclim = stack('GIS/bio_11/bio1_11.tif', 'GIS/bio_11/bio2_11.tif', 'GIS/bio_11/bio3_11.tif',
                'GIS/bio_11/bio4_11.tif', 'GIS/bio_11/bio5_11.tif', 'GIS/bio_11/bio6_11.tif',
                'GIS/bio_11/bio7_11.tif', 'GIS/bio_11/bio8_11.tif', 'GIS/bio_11/bio9_11.tif',
                'GIS/bio_11/bio10_11.tif', 'GIS/bio_11/bio11_11.tif', 'GIS/bio_11/bio12_11.tif',
                'GIS/bio_11/bio13_11.tif', 'GIS/bio_11/bio14_11.tif', 'GIS/bio_11/bio15_11.tif',
                'GIS/bio_11/bio16_11.tif', 'GIS/bio_11/bio17_11.tif', 'GIS/bio_11/bio18_11.tif',
                'GIS/bio_11/bio19_11.tif' )

# resample our occurence data to deal with sampling bias
project.area.raster = raster(project.area)       # create a grid within the project area
res(project.area.raster) = res(bioclim.local) * 10  # set the grid resolution to bioclim resolution, a multiple thereof
occurences.resampled = gridSample(occurences, project.area.raster , n=1)  # do the resampling
occurences.resampled = as.data.frame(occurences.resampled)   # convert results to a data.frame for ease of use
occurences.resampled$Presence = 1

# make the resampled output a spatial object
coordinates(occurences.resampled) = c('longitude', 'latitude')
crs(occurences.resampled) = crs(occurences)

# rename variables for simplicity
predictors = bioclim.local                      
presence.locations = occurences.resampled

# first let's create a training subset
training.locations = presence.locations[sample(nrow(presence.locations), 50),]

# prepare the data for predictions
presence.predictors = raster::extract(predictors, presence.locations )  # extract values from rasters for each point
presence.predictors = as.data.frame(presence.predictors)   # change to data frame object just because             
present = rep(1, nrow(presence.predictors ))   # a column of ones identifies these as the values for presence
presence.predictors = cbind(present, presence.predictors)  # assemble the complete matrix


# now get background values
background.points = randomPoints(project.area.raster, 500)
background.predictors = extract(predictors, background.points)
present.or.background = c(rep(1, nrow(presence.predictors)), rep(0, nrow(background.predictors)))
presence.background.predictors = cbind(present.or.background, rbind(presence.predictors, background.predictors)  )
head(presence.background.predictors)  # take a look 

# because we are on the coast, some of our random points could have fallen in the water
presence.background.predictors = presence.background.predictors[!is.na(presence.background.predictors$bio1_11),]

# and see if we can get a good prediction, by setting up a formula that makes sense
formula = present.or.background ~ bio6_11 + bio4_11  # be selective

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

