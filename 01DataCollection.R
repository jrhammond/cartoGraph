## Collecting data on towns, conflicts, and conflict events

rm(list = ls())
if (!require("pacman")) install.packages("pacman")
if(!'overpass' %in% rownames(installed.packages())) devtools::install_github('hrbrmstr/overpass')
pacman::p_load(rgdal, maptools, raster, rgeos, sp
               , data.table, stringr, gsubfn, stringdist
               , foreach, doSNOW, igraph, Matrix, moments
               , geosphere, purrr, stringi, overpass, countrycode)

os_detect <- Sys.info()['sysname']

if (os_detect == 'Windows'){
  setwd('C:/Users/Jesse/Dropbox/RoadNetworks')
} else if (os_detect == 'Darwin'){
  setwd('/Users/jesse/Dropbox/RoadNetworks')
} else if (os_detect == 'Linux'){
  setwd('/media/jesse/Files/Dropbox/RoadNetworks')
}


####################
##
## Process UCDP and UCDP-GED data sets
##
####################

## Load in UCDP data and subset for desired countries / conflicts / timespans

load('./Data/UCDP Conflict/124920_1ucdp-prio-2015.rdata')
ucdp <- data.table(ucdp.prio)
ucdp <- ucdp %>% map_if(is.factor, as.character) %>% as.data.table()
ucdp <- ucdp[((Year >= 1989 & Region %in% c(2,3,4)) | (Year >= 2005 & Region %in% c(1,5))) & TypeOfConflict %in% c(3)
             & CumulativeIntensity == 1]
setkeyv(ucdp, c('ConflictId'))
ucdp <- unique(ucdp)
ucdp[, GWNoA := as.integer(as.character(GWNoA))]
ucdp <- ucdp[, list(ConflictId, Location, SideA, SideB, Incompatibility, StartDate, StartDate2, EpEndDate, GWNoA)]
ucdp[Location == 'Yemen (North Yemen)', GWNoA := 679]


## Deal with East Timor, since UCDP still codes it as Indonesia

ucdp[ConflictId == '1-134', GWNoA := 860]
ucdp[ConflictId == '1-134', Location := 'East Timor']


## Manually correct Western Sahara because UCDP codes it as Morocco and COW doesn't have a code for WS

ucdp[ConflictId == '1-135', GWNoA := NA]


## Organize by name

setkeyv(ucdp, 'Location')


## Load in GED data and subset by UCDP country-conflicts

# ged <- readOGR('/media/jesse/Files/Dropbox/RoadNetworks/Data/UCDP-GED', 'UCDPGEDpoly')
ged <- readOGR('./Data/UCDP-GED', 'ged40')
ged <- ged[ged$conflict_d %in% ucdp$ConflictId, ]
ged$country <- factor(ged$country)


## Finish subsetting UCDP country-conflicts by GED

ucdp <- ucdp[ConflictId %in% unique(ged$conflict_d)]
ged <- ged[ged$country %in% unique(ucdp$Location),]
data.frame(sort(unique(ucdp$Location)))



####################
##
## State boundaries
##
####################

## Load country boundary data
countries <- raster::getData('countries', download = T
                             , path = './Data/WorldStates')


####################
##
## Capital cities
##
####################

## Get administrative capital data
capitals <- data.table(read.csv('./Data/Capitals/capitals.csv'))



####################
##
## Create master table of country references
##
####################

country_names <- data.table(
  ucdp_ref = sort(as.character(unique(ucdp$Location)))
  , cow_ref = unique(ucdp$GWNoA)
  , iso3_ref = countrycode(unique(ucdp$GWNoA), origin = 'cown', destination = 'iso3c')
  , iso2_ref = countrycode(unique(ucdp$GWNoA), origin = 'cown', destination = 'iso2c')
  , capital_ref = sort(as.character(unique(capitals$State)))
  , osm_ref = sort(as.character(unique(capitals$State)))
)


####################
##
## Create master table of country conflicts
##
####################

conflict_names <- ucdp[, list(ConflictId, Location, GWNoA)]
conflict_names[, iso2_ref := countrycode(ucdp$GWNoA, origin = 'cown', destination = 'iso2c')]



####################
##
## Cleaning and fixing format issues in country data
##
####################

#### Finish manually processing Western Sahara data

## Country table

country_names[ucdp_ref == 'Morocco', iso2_ref := 'EH']
country_names[ucdp_ref == 'Morocco', iso3_ref := 'ESH']


## Conflict table

conflict_names[Location == 'Morocco', iso2_ref := 'EH']


## Conflict table: create a unique ID for states
## where multiple conflicts occur

conflict_names[, full_ref := iso2_ref]
conflict_names[, full_ref := paste0(iso2_ref, '_', ConflictId)]


#### Finish manually processing Western Sahara data

## Write out master states table

write.csv(country_names, file = './Data/Overview/country_data.csv', row.names = F)


## Write out master states table

write.csv(conflict_names, file = './Data/Overview/conflict_data.csv', row.names = F)


## Making some adjustments for name retrieval:
# Dropping Afghanistan - using an alternate source of towns data

country_names <- country_names[osm_ref != 'Afghanistan',]



####################
##
## Gathering towns data from OSM Overpass API
##
####################

i <- 1

while(i <= nrow(country_names)){
  name <- country_names$ucdp_ref[i]
  iso_name <- country_names$iso2_ref[i]
  # Get towns
  townscsv <- paste0('area["name:en"=', '"', name, '"', '];(node[place~"city|town|village"](area););out;')
  towns <- overpass_query(townscsv)
  print(table(towns$place))
  ## Get english-language names where possible
  if('name.en' %in% names(towns)){
    towns$name[!is.na(towns$name.en)] <- towns$name.en[!is.na(towns$name.en)]
  }
  
  ## Convert accented characters to closest standard English chars
  iconv(towns$name[Encoding(towns$name) == 'UTF-8'], 'UTF-8', 'ASCII//TRANSLIT')
  towns <- towns[, c('id', 'lon', 'lat', 'name')]

  # Write data to shapefile
  writeOGR(towns, paste0(getwd(), '/Data/PopulatedPlaces'), paste0(iso_name, "_towns"), driver="ESRI Shapefile"
           , overwrite_layer = T)

  print(name)

  i <- i+1
  Sys.sleep(30)
}


####################
##
## Manually formatting one special case: Laos
##
####################

### For countries with non-english text (Laos)
## write.csv(towns$name
##           , file = '/media/jesse/Files/Dropbox/Reworking city-network paper/Data/Capitals/laos_towns.csv'
##           , row.names = F)
## Done via Google Translate: manually transliterate characters
#trans_towns <- data.table(read.csv('/media/jesse/Files/Dropbox/Reworking city-network paper/Data/Capitals/laos_towns.csv'))
#write.csv(towns, './Data/Capitals/capitals.csv', row.names = F)