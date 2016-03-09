library(spocc)
library(spoccutils)
library(ggplot2)
library(raster)
library(rgeos)
library(dismo)
library(rJava)


predictors = stack('GIS/bioclim.Arborimus.local_1.tif',
                      'GIS/bioclim.Arborimus.local_2.tif',
                      'GIS/bioclim.Arborimus.local_3.tif',
                      'GIS/bioclim.Arborimus.local_4.tif',
                      'GIS/bioclim.Arborimus.local_5.tif',
                      'GIS/bioclim.Arborimus.local_6.tif',
                      'GIS/bioclim.Arborimus.local_7.tif',
                      'GIS/bioclim.Arborimus.local_8.tif',
                      'GIS/bioclim.Arborimus.local_9.tif',
                      'GIS/bioclim.Arborimus.local_10.tif',
                      'GIS/bioclim.Arborimus.local_11.tif',
                      'GIS/bioclim.Arborimus.local_12.tif',
                      'GIS/bioclim.Arborimus.local_13.tif',
                      'GIS/bioclim.Arborimus.local_14.tif',
                      'GIS/bioclim.Arborimus.local_15.tif',
                      'GIS/bioclim.Arborimus.local_16.tif',
                      'GIS/bioclim.Arborimus.local_17.tif',
                      'GIS/bioclim.Arborimus.local_18.tif',
                      'GIS/bioclim.Arborimus.local_19.tif'
                      )
names(predictors) = c('bio1','bio2','bio3','bio4','bio5','bio6','bio7','bio8','bio9',
                      'bio10','bio11','bio12','bio13','bio14','bio15','bio16','bio17',
                      'bio18','bio19')

presence.locations = shapefile('GIS/occurences_resampled.shp')

# create a training subset
training.locations = presence.locations[sample(nrow(presence.locations), 50),]

# fit a maxent model
maxent.model = maxent(predictors, training.locations)
maxent.model  # view some details on the results

maxent.prediction.current = predict(predictors, maxent.model)
plot(maxent.prediction.current)
points(training.locations, pch=10, col='blue')
points(occurences)


# load RCP85 - this is the whole world
# so we are going to clip them down one at a time
project.area = shapefile('GIS/ProjectAreaArborimus.shp')

for(i in 1:19){
  bioclim.layer = raster(paste0('~/Downloads/he85bi50/he85bi50',i,'.tif'))
  
  bioclim.layer.local = crop(bioclim.layer, project.area)
  writeRaster(bioclim.layer.local, paste0('GIS/bioclim_rcp85_', i,'.tif'), overwrite = TRUE)
  rm(bioclim.layer)
  rm(bioclim.layer.local)
}

bioclim.rcp85 = stack('GIS/bioclim_rcp85_1.tif',
                      'GIS/bioclim_rcp85_2.tif',
                      'GIS/bioclim_rcp85_3.tif',
                      'GIS/bioclim_rcp85_4.tif',
                      'GIS/bioclim_rcp85_5.tif',
                      'GIS/bioclim_rcp85_6.tif',
                      'GIS/bioclim_rcp85_7.tif',
                      'GIS/bioclim_rcp85_8.tif',
                      'GIS/bioclim_rcp85_9.tif',
                      'GIS/bioclim_rcp85_10.tif',
                      'GIS/bioclim_rcp85_11.tif',
                      'GIS/bioclim_rcp85_12.tif',
                      'GIS/bioclim_rcp85_13.tif',
                      'GIS/bioclim_rcp85_14.tif',
                      'GIS/bioclim_rcp85_15.tif',
                      'GIS/bioclim_rcp85_16.tif',
                      'GIS/bioclim_rcp85_17.tif',
                      'GIS/bioclim_rcp85_18.tif',
                      'GIS/bioclim_rcp85_19.tif'
)

names(bioclim.rcp85) = c('bio1','bio2','bio3','bio4','bio5','bio6','bio7','bio8','bio9',
                      'bio10','bio11','bio12','bio13','bio14','bio15','bio16','bio17',
                      'bio18','bio19')

                      
plot(bioclim.rcp85$bio1)
plot(predictors$bio1)


# re-run prediction with this differet set of data

maxent.prediction.rcp85 = predict(bioclim.rcp85, maxent.model)
plot(maxent.prediction.rcp85)
points(training.locations, pch=10, col='blue')
points(occurences)

maxent.prediction.rcp26 = predict(bioclim.rcp26, maxent.model)


par(mfrow=c(1,3))
plot(maxent.prediction.current, main='Current')
plot(maxent.prediction.rcp26, main='2050, RCP26')
plot(maxent.prediction.rcp85, main='2050, RCP85')



