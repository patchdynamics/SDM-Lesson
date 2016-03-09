for(i in 1:19){
  bioclim.layer = raster(paste0('~/Downloads/he26bi50/he26bi50',i,'.tif'))
  
  bioclim.layer.local = crop(bioclim.layer, project.area)
  writeRaster(bioclim.layer.local, paste0('GIS/bioclim_rcp26_', i,'.tif'), overwrite = TRUE)
  rm(bioclim.layer)
  rm(bioclim.layer.local)
}


bioclim.rcp26 = stack('GIS/bioclim_rcp26_1.tif',
                      'GIS/bioclim_rcp26_2.tif',
                      'GIS/bioclim_rcp26_3.tif',
                      'GIS/bioclim_rcp26_4.tif',
                      'GIS/bioclim_rcp26_5.tif',
                      'GIS/bioclim_rcp26_6.tif',
                      'GIS/bioclim_rcp26_7.tif',
                      'GIS/bioclim_rcp26_8.tif',
                      'GIS/bioclim_rcp26_9.tif',
                      'GIS/bioclim_rcp26_10.tif',
                      'GIS/bioclim_rcp26_11.tif',
                      'GIS/bioclim_rcp26_12.tif',
                      'GIS/bioclim_rcp26_13.tif',
                      'GIS/bioclim_rcp26_14.tif',
                      'GIS/bioclim_rcp26_15.tif',
                      'GIS/bioclim_rcp26_16.tif',
                      'GIS/bioclim_rcp26_17.tif',
                      'GIS/bioclim_rcp26_18.tif',
                      'GIS/bioclim_rcp26_19.tif'
)

names(bioclim.rcp26) = c('bio1','bio2','bio3','bio4','bio5','bio6','bio7','bio8','bio9',
                         'bio10','bio11','bio12','bio13','bio14','bio15','bio16','bio17',
                         'bio18','bio19')
