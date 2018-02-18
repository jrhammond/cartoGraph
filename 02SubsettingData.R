rm(list=ls())
pacman::p_load(raster, rgdal, data.table, spatialEco, overpass)

# setwd('/media/jesse/Files/Dropbox/RoadNetworks')
setwd('C:/Users/Jesse/Dropbox/RoadNetworks')
# setwd('/Users/jesse/Dropbox/RoadNetworks')


#######
#
# gROADS substitution: Sudan, South Sudan, Tajikistan, Chad, Burundi, and Russia
#  have very poor road coverage via OSM and/or structural issues 
#  (Burundi has two big stripes of non-coverage) so instead I substitute 
#  data from the the GROADS data set.
#
#######


## Set projection info
groads_afr <- readOGR('./Data/Roads/groads-v1-africa-shp', 'gROADS-v1-africa')
groads_asia <- readOGR('./Data/Roads/groads-v1-asia-shp', 'gROADS-v1-asia')
aeaproj <- CRS('+proj=aea +lat_1=20 +lat_2=-23 +lat_0=0 +lon_0=25 +x_0=0 
               +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs')


## Burundi

burundi <- raster::getData('GADM', country = 'Burundi', level = 0)
bi_roads <- groads_afr[burundi, ]
bi_roads <- bi_roads[bi_roads$FCLASS != 5, ]
bi_roads <- spTransform(bi_roads, aeaproj)
bi_roads@data <- data.frame('osm_id' = 1:length(bi_roads))
writeOGR(bi_roads, './Data/Roads', 'BI_roads', driver = 'ESRI Shapefile', overwrite_layer = T)


## South Sudan

sosudan <- raster::getData('GADM', country = 'SSD', level = 0)
ss_roads <- groads_afr[sosudan, ]
ss_roads <- ss_roads[ss_roads$FCLASS != 5, ]
ss_roads <- spTransform(ss_roads, aeaproj)
ss_roads@data <- data.frame('osm_id' = 1:length(ss_roads))
writeOGR(ss_roads, './Data/Roads', 'SS_roads', driver = 'ESRI Shapefile', overwrite_layer = T)


## Sudan

sudan <- raster::getData('GADM', country = 'Sudan', level = 0)
sd_roads <- groads_afr[sudan, ]
sd_roads <- sd_roads[sd_roads$FCLASS != 5, ]
sd_roads <- spTransform(ss_roads, aeaproj)
sd_roads@data <- data.frame('osm_id' = 1:length(sd_roads))
writeOGR(sd_roads, './Data/Roads', 'SD_roads', driver = 'ESRI Shapefile', overwrite_layer = T)


## Chad

chad <- raster::getData('GADM', country = 'TCD', level = 0)
td_roads <- groads_afr[chad, ]
td_roads <- td_roads[td_roads$FCLASS != 5, ]
td_roads <- spTransform(td_roads, aeaproj)
td_roads@data <- data.frame('osm_id' = 1:length(td_roads))
writeOGR(td_roads, './Data/Roads', 'TD_roads', driver = 'ESRI Shapefile', overwrite_layer = T)


## Russia
## Note: Russia's OSM coverage is terrible, so I am substituting gROADS data,
## AND I am subsetting by province.

borders <- raster::getData('GADM', country = 'RU', level = 1)
borders$NAME_1 <- iconv(borders$NAME_1, 'UTF-8', 'ASCII//TRANSLIT')

states <- c('Chechnya', 'Dagestan', 'Ingush', 'Kabardin-Balkar', 'North Ossetia', 'Karachay-Cherkess')
borders <- borders[borders$NAME_1 %in% states, ]
towns <-  readOGR('./Data/PopulatedPlaces', 'RU_towns')
proj4string(towns) <- proj4string(borders)
towns <- spTransform(towns, CRS(proj4string(groads_asia)))
borders <- spTransform(borders, CRS(proj4string(groads_asia)))


state_roads <- groads_asia[borders, ]
state_roads <- spTransform(state_roads, aeaproj)
writeOGR(state_roads, './Data/Roads', 'RU_1-206_roads', driver = 'ESRI Shapefile', overwrite_layer = T)
writeOGR(state_roads, './Data/Roads', 'RU_1-257_roads', driver = 'ESRI Shapefile', overwrite_layer = T)


#######
#
# Morocco / Western Sahara: This is a pain because some data sets treat WS as a separate
#   country (GADM, GISgraphy) and others (UCDP, OSM) do not. Manually subset towns
#   in "Greater Morocco" to only include those in Western Sahara.
#
#######

eh_borders <- raster::getData('GADM', country = 'EH', level = 0)
eh_towns <- readOGR('./Data/PopulatedPlaces', 'EH_towns')
proj4string(eh_towns) <- CRS(proj4string(eh_borders))
eh_towns <- eh_towns[eh_borders,]
writeOGR(eh_towns, './Data/PopulatedPlaces', 'EH_towns', driver = 'ESRI Shapefile', overwrite_layer = T)


#######
#
# Towns subsetting: For conflicts fought over territory, subset by the first-level admin
#   districts in which the bulk of fighting occurred.
#
#######

terr_conflicts <- list(
  list('BD', '1-126', c('Chittagong'))
  , list('ET', '1-133', c('Somali'))
  , list('ET', '1-219', c('Oromia'))
  , list('IN', '1-152', c('Assam', 'Manipur', 'Nagaland'))
  , list('IN', '1-156', c('Punjab', 'Haryana'))
  , list('IN', '1-169', c('Jammu and Kashmir'))
  , list('IN', '1-170', c('Assam', 'Meghalaya', 'Arunachal Pradesh'))
  , list('IN', '1-54', c('Assam', 'Manipur', 'Nagaland'))
  , list('IN', '1-29', c('Jharkhand', 'Bihar', 'Chhattisgarh', 'Odisha', 'Andhra Pradesh'))
  , list('ID', '1-171', c('Aceh'))
  , list('IR', '1-6', c('West Azarbaijan', 'Kordestan', 'Kermanshah'))
  , list('IR', '1-143', c('West Azarbaijan', 'Kordestan', 'Kermanshah'))
  , list('IQ', '1-74', c('Arbil', 'Dihok', 'As-Sulaymaniyah'))
  , list('MM', '1-23', c('Kayin', 'Mon', 'Tanintharyi', 'Bago', 'Yangon', 'Ayeyarwady'))
  , list('MM', '1-34', c('Kachin', 'Shan'))
  , list('MM', '1-67', c('Shan', 'Kayah'))
  , list('PK', '1-129', c('Baluchistan'))
  , list('PK', '1-209', c('F.A.T.A.', 'N.W.F.P.'))
  , list('PH', '1-112', c('Dinagat Islands', 'Surigao del Norte', 'Surigao del Sur', 
                          'Agusan del Norte', 'Agusan del Sur', 'Davao del Norte', 
                          'Compostela Valley', 'Davao Oriental', 'Misamis Oriental', 
                          'Bukidnon', 'Davao del Sur', 'Lanao del Norte', 
                          'Lanao del Sur', 'North Cotabato', 'South Cotabato', 'Sarangani', 
                          'Sultan Kudarat', 'Maguindanao', 'Zamboanga del Norte', 'Zamboanga del Sur', 
                          'Zamboanga Sibugay', 'Misamis Occidental'))
  , list('PH', '1-10', c('Dinagat Islands', 'Surigao del Norte', 'Surigao del Sur', 
                         'Agusan del Norte', 'Agusan del Sur', 'Davao del Norte', 
                         'Compostela Valley', 'Davao Oriental', 'Misamis Oriental', 
                         'Bukidnon', 'Davao del Sur', 'Lanao del Norte', 
                         'Lanao del Sur', 'North Cotabato', 'South Cotabato', 'Sarangani', 
                         'Sultan Kudarat', 'Maguindanao', 'Zamboanga del Norte', 'Zamboanga del Sur', 
                         'Zamboanga Sibugay', 'Misamis Occidental'))
  , list('SN', '1-180', c('Ziguinchor', 'Sedhiou', 'Kolda'))
  , list('TH', '1-248', c('Narathiwat', 'Pattani', 'Yala', 'Songkhla'))
  , list('TR', '1-159', c('Adiyaman', 'Batman', 'Bingol', 'Bitlis', 'Diyarbakir', 'Elazig'
                          , 'Hakkari', 'Mardin', 'Mus', 'Siirt', 'Tunceli', 'Van'))
  , list('YE', '1-207', c('`Adan', 'Abyan', 'Al Dali\'', 'Al Mahrah', 'Hadramawt', 'Lahij', 'Shabwah'))
)


lonlatproj <- CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')


## Function: subset roads/towns by conflict regions

state_subsetting <- function(country_iso, conflictid, districts){
  borders <- raster::getData('GADM', country = country_iso, level = 1)
  borders$NAME_1 <- iconv(borders$NAME_1, 'UTF-8', 'ASCII//TRANSLIT')
  
  states <- borders[borders$NAME_1 %in% districts | borders$NAME_1 %in% 'Elaz?g', ]
  if(conflictid %in% '1-10'){
    states <- borders[!(borders$NAME_1 %in% districts), ] ## Everything BUT Mindanao Island
  }
  roads <-  readOGR('./Data/Roads', paste0(country_iso, '_roads'))
  towns <-  readOGR('./Data/PopulatedPlaces', paste0(country_iso, '_towns'))
  
  proj4string(towns) <- lonlatproj
  towns <- spTransform(towns, lonlatproj)
  towns <- spTransform(towns, CRS(proj4string(roads)))
  states <- spTransform(states, CRS(proj4string(roads)))
  
  state_roads <- roads[states, ]
  state_towns <- towns[states, ]
  
  towns <- spTransform(towns, lonlatproj)
  writeOGR(state_roads, './Data/Roads', paste0(country_iso, '_', conflictid, '_roads'), driver = 'ESRI Shapefile', overwrite_layer = T)
  writeOGR(state_towns, './Data/PopulatedPlaces', paste0(country_iso, '_', conflictid, '_towns'), driver = 'ESRI Shapefile', overwrite_layer = T)
  print(paste(sum(districts %in% borders$NAME_1) / length(districts)*100, '% of districts found.'))
  print(paste0(country_iso, ' subsetting completed for conflict ', conflictid))
}



## Load in key table and set up input data

state_data <- fread('./Data/Overview/conflict_data.csv', stringsAsFactors = F)
setkeyv(state_data, 'iso2_ref')
terr_states <- unlist(lapply(terr_conflicts, '[[', 1))
terr_confids <- unlist(lapply(terr_conflicts, '[[', 2))


## Subset state regions and append conflict ID to road files

i <- 1

while(i <= nrow(state_data)){
  
  this_state <- state_data$iso2_ref[i]
  this_conflict <- state_data$ConflictId[i]
  
  
  if(this_state %in% terr_states){
    
    this_conf <- terr_conflicts[[which(terr_states == this_state & terr_confids == this_conflict)]]
    country_iso <- this_conf[[1]]
    conflictid <- this_conf[[2]]
    districts <- this_conf[[3]]
    
    state_subsetting(country_iso, conflictid, districts)
    
  } else{ 
    
    ## Rename files with iso2_confid names
    
    roads <-  readOGR('./Data/Roads', paste0(country_iso, '_roads'))
    towns <-  readOGR('./Data/PopulatedPlaces', paste0(country_iso, '_towns'))
    
    writeOGR(state_roads, './Data/Roads', paste0(country_iso, '_', conflictid, '_roads'), driver = 'ESRI Shapefile', overwrite_layer = T)
    writeOGR(state_towns, './Data/PopulatedPlaces', paste0(country_iso, '_', conflictid, '_towns'), driver = 'ESRI Shapefile', overwrite_layer = T)
    
  }

  
  i <- i+1
}





