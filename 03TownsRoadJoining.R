rm(list=ls())
if (!require("pacman")) install.packages("pacman")
if (!require("rmapshaper")) devtools::install_github('ateucher/rmapshaper')
pacman::p_load(rgdal, maptools, raster, rgeos, sp
               , data.table, stringr, gsubfn, stringdist
               , foreach, doSNOW, igraph, Matrix, moments
               , geosphere, geojsonio, rmapshaper
               , countrycode)


os_detect <- Sys.info()['sysname']

if (os_detect == 'Windows'){
  setwd('C:/Users/Jesse/Dropbox/RoadNetworks')
} else if (os_detect == 'Darwin'){
  setwd('/Users/jesse/Dropbox/RoadNetworks')
} else if (os_detect == 'Linux'){
  setwd('/media/jesse/Files/Dropbox/RoadNetworks')
}




############################################################
#
# Defining primary functions
#
############################################################

### Town attribute assignment
TownDataJoining <- function(country_iso2, country_name
                            , latlonproj, aeaproj
                            , townsloc, townsfile
                            , capsloc, capsfile
                            , prio_forest, prio_mtn
                            , geecon_ppp
                            , gpw
                            , ttime
                            , cap_coords
                            , geoepr
                            , diadata){
  
  
  ############
  #
  # Subfunction: process town names
  #
  ############
  
  TownNameProcessing <- function(towns, name){
    
    towns[[name]] <- sapply(strsplit(towns[[name]], "-"), "[[", 1)
    ## Convert characters to closest English char
    towns[[name]] <- iconv(towns[[name]], 'UTF-8', 'ASCII//TRANSLIT')
    towns[[name]] <- tolower(towns[[name]])
    
    ## Drop duplicate names
    towns <- towns[!duplicated(name), ]
    
    ## Drop extraneous naming conventions
    dropstuff <- c(' city| town| village| hamlet| junction| farm| mine| community| 
                   north| south| east| west| iand| central|[0-9]|[[:punct:]]| ')
    towns[[name]] <- str_replace_all(towns[[name]], dropstuff, '')
    
    return(towns)
  }
  
  
  ############
  #
  # Load state capitals
  #
  ############
  
  ## Get state capitals
  capitals <- data.table(read.csv(paste0(capsloc, '/', capsfile, '.csv'), stringsAsFactors = F))
  capitals <- unique(capitals[State == country_iso2])
  
  ## Process capital names
  capitals <- TownNameProcessing(capitals, 'Capital')
  setkeyv(capitals, 'Capital')
  
  ############
  #
  # Load towns data
  #
  ############
  
  ## Get towns data
  towns <- readOGR(townsloc, townsfile)
  names(towns@data)[1] <- 'id'
  names(towns@data) <- tolower(names(towns@data))
  towns@data <- towns@data[, c('id', 'name')]
  towns$id <- 1:length(towns)
  towns$name <- as.character(towns$name)
  if(is.na(proj4string(towns))){
    proj4string(towns) <- lonlatproj
  }
  if(proj4string(towns) != lonlatproj@projargs){
    towns <- spTransform(towns, lonlatproj)
  }
  
  ## Subset towns data by names
  names(towns) <- c('id', 'name')
  
  ## Process town names
  towns@data <- TownNameProcessing(towns@data, 'name')
  fillin_idx <- which(is.na(towns@data$name) | towns@data$name %in% '')
  towns@data[fillin_idx, 'name'] <- paste0('unk', towns@data$id[fillin_idx])
  
  
  ############
  #
  # Load state borders
  #
  ############
  
  ## Load state borders
  borders <- raster::getData('GADM', country = country_iso2, level = 0)
  borders <- ms_simplify(borders, .005)
  borders <- slot(borders, 'polygons')
  borders <- lapply(borders, checkPolygonsHoles)
  borders <- sp::disaggregate(SpatialPolygons(borders, proj4string = lonlatproj))
  borders <- borders[towns, ]
  
  ## Find minimum distance from towns to borders
  if(length(towns) < 100){
    end_idx <- seq(10,length(towns),by = 10)
    start_idx <- c(seq(1, length(towns)-9, by = 10), last(end_idx)+1)
    end_idx <- c(end_idx, length(towns))
  } else{
    end_idx <- seq(100,length(towns),by = 100)
    start_idx <- c(seq(1, length(towns)-99, by = 100), last(end_idx)+1)
    end_idx <- c(end_idx, length(towns))
  }
  
  
  b_distances <- foreach(i = start_idx
                         , j = end_idx
                         , .combine = rbind
                         , .packages = 'geosphere') %dopar% {
                           cbind(towns$id[i:j], dist2Line(p = towns[i:j, ]
                                     , line = borders))}

  towns$bdist <- b_distances[,2]
  
  
  ############
  #
  # Match state towns with state capitals
  #
  ############
  
  towns$capital <- 0L
  cap_idx <- c(sapply(capitals$Capital, FUN = amatch
                      , table = towns$name
                      , method = 'osa'
                      , weight = c(d = 1, i = 1, s = .66, t = .66)
                      , maxDist = floor(sqrt(nchar(capitals$Capital)))
                      , nomatch = NA))
  cap_idx <- cap_idx[!is.na(cap_idx)]
  towns[cap_idx, 'capital'] <- 1L
  
  
  ############
  #
  # Merge economic data
  #
  ############
  
  ## Merge cell PPP-adjusted GDP into towns data
  towns$ppp2000 <- extract(geecon_ppp, towns)
  
  
  ############
  #
  # Merge PRIO-GRID data
  #
  ############
  
  ## Merge cell PPP-adjusted GDP into towns data
  towns$forest <- extract(prio_forest, towns)
  towns$mountain <- extract(prio_mtn, towns)
  
  ############
  #
  # Merge population data
  #
  ############
  
  towns$logpop2000 <- extract(gpw, towns, na.rm = F)
  towns$logpop2000 <- log(towns$logpop2000)
  
  
  ############
  #
  # Merge transit-time data
  #
  ############
  
  towns$ttime <- extract(ttime, towns, na.rm = F)
  
  
  ############
  #
  # Calculate distance to capital
  #
  ############
  
  this_cap <- cap_coords[iso2_ref %in% country_iso2, ]
  cap_loc <- SpatialPoints(coords = cbind(this_cap$lon, this_cap$lat))
  proj4string(cap_loc) <- lonlatproj
  towns$capdist <- c(spDists(x = towns, y = cap_loc))
  
  
  ############
  #
  # Merge geoEPR data
  #
  ############
  
  towns@data <- cbind(towns@data, over(towns, geoepr, FUN = 'last')[, c('GROUPID', 'COWGROUP', 'GROUPNAME')])
  names(towns@data) <- tolower(names(towns@data))
  
  
  ############
  #
  # Merge diamond data
  #
  ############
  
  ## Subset by country
  country_dia <- diadata[borders, ]
  
  ## Find minimum distance from towns to diamonds
  if(length(towns) < 100){
    end_idx <- seq(10,length(towns),by = 10)
    start_idx <- c(seq(1, length(towns)-9, by = 10), last(end_idx)+1)
    end_idx <- c(end_idx, length(towns))
  } else{
    end_idx <- seq(100,length(towns),by = 100)
    start_idx <- c(seq(1, length(towns)-99, by = 100), last(end_idx)+1)
    end_idx <- c(end_idx, length(towns))
  }
  
  
  dia_distances <- foreach(i = start_idx
                           , j = end_idx
                           , .combine = rbind
                           , .packages = 'sp') %dopar% {
                             spDists(x = towns[i:j, ]
                                     , y = country_dia)}
  towns$diadist <- apply(dia_distances, 1, 'min')
  
 
  

  #####
  #
  # Output modified town data
  #
  #####
  
  return(towns)
}




####################################################################



### Spatial road network generation
SpatialRoadNetwork <- function(country_iso2, country_name
                               , country_conflict
                               , roadsloc, roadsfile
                               , roadsbufloc
                               , townsobj
                               , ged
                               , lonlatproj, aeaproj
                               , roadsnapdist, townthindist, townbufferdist){
  
  
  ############
  #
  # Subfunctions: thinning town points
  #
  ############
  
  ### Primary thinning function
  thin2 <- function (loc.data
                     , lat.col = "LAT", long.col = "LONG"
                     , id.col = 'id', thin.par = 1, verbose = TRUE) 
  {
    cat(paste("**********************************************", 
              "\n", "Beginning Spatial Thinning.\n", "Script Started at:", 
              date(), sep = " "))
    
    locs.df <- loc.data
    lat <- which(names(locs.df) == lat.col)
    long <- which(names(locs.df) == long.col)
    id <- which(names(locs.df) == id.col)
    locs.long.lat <- as.data.frame(cbind(locs.df[[long]], locs.df[[lat]], locs.df[[id]]))
    
    locs.thinned <- thin.algorithm2(rec.df.orig = locs.long.lat, thin.par = thin.par)
    return(locs.thinned)
  }
  
  
  ### Thinning algorithm
  thin.algorithm2 <- function (rec.df.orig, thin.par) 
  {
    rec.df <- rec.df.orig
    DistMat <- spDists(x = rec.df, longlat = F)
    diag(DistMat) <- NA
    while (min(DistMat, na.rm = TRUE) < thin.par & nrow(rec.df) > 1) {
      CloseRecs <- which(DistMat < thin.par, arr.ind = TRUE)[, 1]
      RemoveRec <- as.numeric(names(which(table(CloseRecs) == max(table(CloseRecs)))))
      if (length(RemoveRec) > 1) {
        RemoveRec <- sample(RemoveRec, 1)
      }
      rec.df <- rec.df[-RemoveRec, ]
      DistMat <- DistMat[-RemoveRec, -RemoveRec]
      if (length(DistMat) == 1) {
        break
      }
    }
    colnames(rec.df) <- c("Longitude", "Latitude", 'id')
    return(rec.df)
  }
  
  
  ############
  #
  # Load and process roads data
  #
  ############
  
  ## Read in roads
  roads <- readOGR(roadsloc, roadsfile)
  
  
  ############
  #
  # Load towns data
  #
  ############
  
  towns <- townsobj
  
  
  ############
  #
  # Snap towns and capitals to road lines
  #
  ############
  
  ## Set CRS to equal-area projection
  roads <- spTransform(roads, aeaproj)
  towns <- spTransform(towns, aeaproj)
  
  ## Snap towns to roads
  if(length(towns) > 1000){
    end_idx <- seq(1000,length(towns),by = 1000)
    start_idx <- c(seq(1, length(towns)-999, by = 1000), last(end_idx)+1)
    end_idx <- c(end_idx, length(towns))
  } else if(length(towns) <= 1000 & length(towns) > 100){
    end_idx <- seq(100,length(towns),by = 100)
    start_idx <- c(seq(1, length(towns)-99, by = 100), last(end_idx)+1)
    end_idx <- c(end_idx, length(towns))
  } else if(length(towns) < 100){
    end_idx <- seq(10,length(towns),by = 10)
    start_idx <- c(seq(1, length(towns)-9, by = 10), last(end_idx)+1)
    end_idx <- c(end_idx, length(towns))
  }
  
  if(last(start_idx) > last(end_idx)){
    start_idx <- start_idx[-length(start_idx)]
    end_idx <- end_idx[-length(end_idx)]
  }
  
  snaptowns <- foreach(i = start_idx
                       , j = end_idx
                       , .combine = rbind
                       , .packages = 'maptools') %dopar% {
                         snapPointsToLines(points = towns[i:j, ]
                                           , lines = roads
                                           , maxDist = roadsnapdist)}
  snaptowns$nearest_line_id <- NULL
  
  ## Reset coordinates of towns
  snaptowns@data[, c('X', 'Y')] <- coordinates(snaptowns)
  towns@data[, c('X', 'Y')] <- coordinates(towns)
  
  ## Recombine snapped towns and original towns
  townsdata <- data.table(rbind(snaptowns@data
                                , towns@data[!towns$id %in% snaptowns$id, ]))
  towns <- SpatialPointsDataFrame(cbind(townsdata$X, townsdata$Y)
                                  , data = townsdata, proj4string = aeaproj)
  
  
  ############
  #
  # Split into cap and non-cap data sets
  #
  ############
  
  capitals <- towns[towns$capital %in% 1, ]
  towns <- towns[!towns$capital %in% 1, ]
  
  
  ############
  #
  # Thin out towns and capitals
  #
  ############
  
  ## Thin towns
  if(length(towns) > 1000){
    end_idx <- seq(1000,length(towns),by = 1000)
    start_idx <- c(seq(1, length(towns)-999, by = 1000), last(end_idx)+1)
    end_idx <- c(end_idx, length(towns))
  } else if(length(towns) <= 1000 & length(towns) > 100){
    end_idx <- seq(100,length(towns),by = 100)
    start_idx <- c(seq(1, length(towns)-99, by = 100), last(end_idx)+1)
    end_idx <- c(end_idx, length(towns))
  } else if(length(towns) < 100 & length(towns) > 10){
    end_idx <- seq(10,length(towns),by = 10)
    start_idx <- c(seq(1, length(towns)-9, by = 10), last(end_idx)+1)
    end_idx <- c(end_idx, length(towns))
  }else if(length(towns) <= 10){
    start_idx <- 1
    end_idx <- length(towns)
  }
  
  if(last(start_idx) > last(end_idx)){
    start_idx <- start_idx[-length(start_idx)]
    end_idx <- end_idx[-length(end_idx)]
  }
  
  if(length(towns) > 10){
    thinnedtowns <- foreach(i = start_idx
                            , j = end_idx
                            , .combine = rbind) %dopar% {
                              thin2(towns[i:j,]
                                    , lat.col = 'X'
                                    , long.col = 'Y'
                                    , thin.par = 2501)}
    towns <- towns[thinnedtowns$id %in% towns$id, ]
  } else {
    thinnedtowns <- thin2(towns
                          , lat.col = 'X'
                          , long.col = 'Y'
                          , thin.par = 2501)
    towns <- towns[thinnedtowns$id %in% towns$id, ]
  }
  
  ## Thin out towns NEAR CAPITALS (be sure to keep the capitals!)
  if(length(capitals) > 0){
    for(i in 1:nrow(coordinates(capitals))){
      captown_dist <- spDists(coordinates(capitals[i, ]), coordinates(towns))
      drop_towns <- which(captown_dist <= roadsnapdist)
      if(length(drop_towns) > 0){
        towns <- towns[-drop_towns,]
      }
    }
    ## Recombine thinned towns and capitals
    towns <- SpatialPointsDataFrame(coords = rbind(coordinates(towns), coordinates(capitals))
                                    , data = rbind(towns@data, capitals@data)
                                    , proj4string = aeaproj)
  }
  
  towns$id <- 1:nrow(towns@data)
  towns$name <- paste0(towns$name, towns$id)
  
  
  
  ############
  #
  # Load state borders
  #
  ############
  
  ## Load state borders
  # borders <- raster::getData('GADM', country = country_iso2, level = 0)
  
  borders <- raster::getData('GADM', country = country_iso2, level = 1)
  borders <- spTransform(borders, aeaproj)
  borders <- borders[towns, ]
  
  
  ############
  #
  # Merge GED conflict data
  #
  ############
  
  ## Transform to projected CRS
  
  ged <- spTransform(ged, aeaproj)
  
  
  ## Subset by country
  
  country_ged <- ged[borders, ]
  
  
  ## Subset by conflict
  
  country_ged <- country_ged[country_ged$conflict_d %in% conflict_id, ]
  
  
  ## Subset by actors (only mutual conflict events) NO
  
  #   country_ged <- country_ged[!country_ged$side_b == 'Civilians', ]
  
  ## Subset by geographic accuracy - keep only events referenced in/near towns
  
  country_ged <- country_ged[country_ged$where_prec <= 2, ]
  
  
  ## Find minimum distance from towns to GED events
  
  if(length(towns) > 1000){
    end_idx <- seq(1000,length(towns),by = 1000)
    start_idx <- c(seq(1, length(towns)-999, by = 1000), last(end_idx)+1)
    end_idx <- c(end_idx, length(towns))
  } else if(length(towns) <= 1000 & length(towns) > 100){
    end_idx <- seq(100,length(towns),by = 100)
    start_idx <- c(seq(1, length(towns)-99, by = 100), last(end_idx)+1)
    end_idx <- c(end_idx, length(towns))
  } else if(length(towns) < 100 & length(towns) > 10){
    end_idx <- seq(10,length(towns),by = 10)
    start_idx <- c(seq(1, length(towns)-9, by = 10), last(end_idx)+1)
    end_idx <- c(end_idx, length(towns))
  }else if(length(towns) <= 10){
    start_idx <- 1
    end_idx <- length(towns)
  }
  
  if(last(start_idx) > last(end_idx)){
    start_idx <- start_idx[-length(start_idx)]
    end_idx <- end_idx[-length(end_idx)]
  }
  
  ## Snap conflict events to towns, 5000m buffer
  
  ged_distances <- foreach(i = start_idx
                           , j = end_idx
                           , .combine = rbind
                           , .packages = 'sp') %dopar% {
                             spDists(x = towns[i:j, ]
                                     , y = country_ged)}
  
  snapged <- data.table(t(apply(ged_distances, 2, function(x){cbind(min(x), which.min(x))}))
                        , 'cas_high' = country_ged$high_est)
  snapged[, name := towns$name[snapged$V2]]
  setnames(snapged, c('dist', 'id', 'cas_high', 'name'))
  snapged <- snapged[dist < 5000]
  
  ## Write snapped GED data to table for robustness checking
  
  write.csv(snapged, file = paste0('./Data/UCDP-GED/', country_conflict, '_events.csv'), row.names = F)
  
  
  ## Snap events to towns
  
  snapged <- snapged[, list(.N, sum(cas_high)), by = id]
  setnames(snapged, c('id', 'numevents', 'cas_high'))
  snapged[, straycount := (length(country_ged) - sum(snapged$numevents)) / length(country_ged)]
  towns <- merge(towns, snapged
                 , by.x = 'id', by.y = 'id'
                 , all.x = T, sort = F)
  towns$numevents[is.na(towns$numevents)] <- 0
  towns$numevents[is.na(towns$cas_high)] <- 0
  
  
  ############
  #
  # Clip roads by towns/capitals
  #
  ############
  
  ## Create SINGLE BUFFER shape for all towns
  townbuffers <- gBuffer(towns, byid = F, width = townbufferdist)
  
  ## Read in road buffer data
  roadbuffers <- readOGR(roadsbufloc, 'temp5')
  roadbuffers <- spTransform(roadbuffers, proj4string(towns))
  
  ## Stamp out town shapes on road
  roadbuffers <- gDifference(roadbuffers, townbuffers)
  
  ## Break stamped buffers
  roadbuffers <- sp::disaggregate(roadbuffers)
  roadbuffers <- SpatialPolygonsDataFrame(roadbuffers, data = data.frame('id' = 1:length(roadbuffers)))
  
  ## Create INDIVIDUAL ID BUFFERS for all towns
  townbuffers <- gBuffer(towns, byid = T, width = townbufferdist + 10)
  
  ## Calculate town/road intersections and generate edgelist
  ints <- gIntersects(roadbuffers, townbuffers, byid = T)
  townroads <- data.table(which(ints == T, arr.ind = T))
  setnames(townroads, c('town', 'road'))
  townroads[, town := townbuffers$id[town]]
  townroads[, road := roadbuffers$id[road]]
  setkeyv(townroads, c('town', 'road'))
  
  
  ############
  #
  # Create towns connectivity network
  #
  ############
  
  ## Generate bipartite adjacency matrix
  bigtest <- Matrix(0, nrow = length(towns), ncol = length(roadbuffers), sparse = T)
  bigtest[as.matrix(townroads[, list(town, road)])] <- 1
  
  ## Convert to town-level sociomatrix
  townsmat <- bigtest %*% t(bigtest)
  diag(townsmat) <- 0
  townsmat[which(townsmat > 1)] <- 1
  townsmat[which(upper.tri(townsmat))] <- 0
  
  ## Create towns network
  townsnet <- graph.adjacency(townsmat, mode = 'undirected', diag = F)
  townsnet <- igraph::set.vertex.attribute(townsnet, 'name', value = towns$name)
  townsnet <- igraph::set.vertex.attribute(townsnet, 'x', value = coordinates(towns)[,1])
  townsnet <- igraph::set.vertex.attribute(townsnet, 'y', value = coordinates(towns)[,2])
  townsnet <- igraph::set.vertex.attribute(townsnet, 'ppp2000', value = towns$ppp2000)
  townsnet <- igraph::set.vertex.attribute(townsnet, 'capital', value = towns$capital)
  townsnet <- igraph::set.vertex.attribute(townsnet, 'capdist', value = towns$capdist)
  townsnet <- igraph::set.vertex.attribute(townsnet, 'forest', value = towns$forest)
  townsnet <- igraph::set.vertex.attribute(townsnet, 'mountain', value = towns$mountain)
  townsnet <- igraph::set.vertex.attribute(townsnet, 'logpop2000', value = towns$logpop2000)
  townsnet <- igraph::set.vertex.attribute(townsnet, 'bdist', value = towns$bdist)
  townsnet <- igraph::set.vertex.attribute(townsnet, 'ttime', value = towns$ttime)
  townsnet <- igraph::set.vertex.attribute(townsnet, 'groupid', value = towns$groupid)
  townsnet <- igraph::set.vertex.attribute(townsnet, 'cowgroup', value = towns$cowgroup)
  townsnet <- igraph::set.vertex.attribute(townsnet, 'groupname', value = towns$groupname)
  townsnet <- igraph::set.vertex.attribute(townsnet, 'diadist', value = towns$diadist)
  townsnet <- igraph::set.vertex.attribute(townsnet, 'numevents', value = towns$numevents)
  townsnet <- igraph::set.vertex.attribute(townsnet, 'cas_high', value = towns$cas_high)
  townsnet <- igraph::set.vertex.attribute(townsnet, 'straycount', value = towns$straycount)
  
  return(townsnet)
}



############################################################
#
# Loading and processing towns/roads data
#
############################################################


#####################
#
# Load in key table
#
#####################

state_data <- fread('./Data/Overview/conflict_data.csv', stringsAsFactors = F)


#####################
#
# Load GED conflict events data
#
#####################

ged <- readOGR('./Data/UCDP-GED', 'ged40')


#####################
#
# Load G-ECON 1.8 data
#
#####################

geecon <- fread('./Data/Economic/Gecon40_post_final.csv'
                , na.string = c('#DIV/0!', '#N/A'))
geecon$AREA <- as.numeric(geecon$AREA)
geecon$cellid <- 1:nrow(geecon)


## Transform coordinates to spatial points

coordinates(geecon)<-~LONGITUDE+LAT # Creates spatial object
proj4string(geecon)=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")


## Transform spatial points to grid

extent<-extent(geecon)
rast<-raster(extent,ncol=359,nrow=172,
             crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
geecon$PPP2000_40[is.na(geecon$PPP2000_40)] <- 0
geecon_ppp <- rasterize(geecon,rast,geecon$PPP2000_40, na.rm = T, fun="last") # Create grid



#####################
#
# Load PRIO-GRID 2.0 data
#
#####################

## Load PRIO-GRID variables

priodata <- fread('./Data/PRIO-GRID/PRIO-GRID Static Variables - 2016-04-15.csv')


# setkeyv(priodata)
priodata$xcoord <- as.numeric(priodata$xcoord)
priodata$ycoord <- as.numeric(priodata$ycoord)
priodata$forest_gc <- as.numeric(priodata$forest_gc)
priodata$mountains_mean <- as.numeric(priodata$mountains_mean)
priodata$mountains_mean[is.na(priodata$mountains_mean)] <- 0


## Transform coordinates to spatial points

coordinates(priodata)<-~xcoord+ycoord # Creates spatial object
proj4string(priodata)=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")


## Transform spatial points to grid

extent<-extent(priodata)
rast<-raster(extent,ncol=360,nrow=180,
             crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
prio_forest <- rasterize(priodata,rast,priodata$forest_gc, na.rm = T,fun="last") # Create grid
prio_mtn <- rasterize(priodata,rast,priodata$mountains_mean, na.rm = T,fun="last") # Create grid



#####################
#
# Load GPWv4 population data
#
#####################

gpw <- raster('./Data/Population/gpwv42000/gpwv4')


#####################
#
# Load transit-time data
#
#####################

ttime <- raster('./Data/Accessibility/acc_50k/w001001.adf')


############
#
# Load capitals data
#
############

cap_coords <- fread('./Data/Capitals/CapitalsLatLong.csv')
setnames(cap_coords, c('cown', 'iso3', 'lat_1', 'lat_2', 'lon_1', 'lon_2'))
cap_coords[, lat := as.numeric(paste0(lat_1, '.', lat_2))]
cap_coords[, lon := as.numeric(paste0(lon_1, '.', lon_2))]
cap_coords[, iso2_ref := countrycode(cown, origin = 'cown', destination = 'iso2c')]
cap_coords <- cap_coords[, list(iso2_ref, lat, lon)]
cap_coords <- rbind(cap_coords, data.frame(iso2_ref = 'SS', lat = 31.58, lon = 4.85))
cap_coords <- rbind(cap_coords, data.frame(iso2_ref = 'EH', lat = 27.13, lon = -13.17))


#####################
#
# Load ethnic settlement data
#
#####################

## Load EPR data

geoepr <- readOGR('./Data/GeoEPR', 'GeoEPR-ETH_2.0')
geoepr$GROUPNAME <- as.character(geoepr$GROUPNAME)
proj4string(geoepr) <- proj4string(geecon)


## Load ACD-EPR data

acdepr <- fread('./Data/EthnicConflict/ACD2EPR-2014.csv')


## Subset ACD-EPR data by date

acdepr <- acdepr[to >= 1989]


## Subset ACD-EPR by support level

acdepr <- acdepr[claim > 0 | recruitment > 0 | support > 0]


## Subset by date and type (no diffused groups)

geoepr <- geoepr[geoepr$TYPE %in% c(1, 3, 6), ]
geoepr <- geoepr[geoepr$ENDYEAR >= 1989, ]


## Subset by conflict involvement

geoepr <- geoepr[geoepr$COWGROUP %in% acdepr$gwgroupid, ]



#####################
#
# Load diamond data
#
#####################

diadata <- readOGR('./Data/LootableResources', 'DIADATA')



#####################
#
# Set non-changing parameters
#
#####################

capsloc <- './Data/Capitals'
capsfile <- 'capitals'
lonlatproj <- CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')
aeaproj <- CRS('+proj=aea +lat_1=20 +lat_2=-23 +lat_0=0 +lon_0=25 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs')
roadsnapdist <- 5001
townthindist <- 5001
townbufferdist <- 1001
outfolder <- './Data/Networks'

#####################
#
# Loop through states
#
#####################


state <- 1
while(state <= nrow(state_data)){
  ## Set state-specific parameters
  country_conflict <- state_data$full_ref[state]
  country_iso2 <- substr(state_data$full_ref[state], 1,2)
  country_name <- state_data$Location[state]
  conflict_id <- state_data$ConflictId[state]
  roadsloc <- './Data/Roads'
  roadsfile <- paste0(country_conflict, '_roads')
  townsloc <- './Data/PopulatedPlaces'
  townsfile <- paste0(country_conflict, '_towns')
  roadsbufloc <- gsub('-', '_', paste0('./Data/Roads/', country_conflict, '_buffer'))
  
  ## Open parallel session
  
#   registerDoSNOW(cl <- makeCluster(4, type = 'SOCK'))
#   
#   
#   ## Towns processing
#   
#   townsobj <- TownDataJoining(country_iso2, country_name
#                               , latlonproj, aeaproj
#                               , townsloc, townsfile
#                               , capsloc, capsfile
#                               , prio_forest, prio_mtn
#                               , geecon_ppp
#                               , gpw
#                               , ttime
#                               , cap_coords
#                               , geoepr
#                               , diadata)
#   
#   
#   ## Close parallel session
#   
#   stopCluster(cl)
#   
#   
#   ## Save backup data
#   
#   save(townsobj, file = paste0('./Data/PopulatedPlaces/', country_conflict, '_towndata.Rdata'))
  load(paste0('./Data/PopulatedPlaces/', country_conflict, '_towndata.Rdata'))
  
  
  ## Open parallel session
  
  registerDoSNOW(cl <- makeCluster(6, type = 'SOCK'))
  
  ### Town attribute assignment
  
  out_network <- SpatialRoadNetwork(country_iso2, country_name
                                    , country_conflict
                                 , roadsloc, roadsfile
                                 , roadsbufloc
                                 , townsobj
                                 , ged
                                 , lonlatproj, aeaproj
                                 , roadsnapdist, townthindist, townbufferdist)
    
  
  save(out_network, file = paste0('./Data/Networks/', country_conflict, '_network.Rdata'))
  
  ## Close parallel session
  
  stopCluster(cl)
  
  state <- state + 1
}