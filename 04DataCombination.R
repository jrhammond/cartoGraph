rm(list=ls())
if (!require("pacman")) install.packages("pacman")
pacman::p_load(rgdal, maptools, raster, rgeos, sp
               , data.table, stringr, gsubfn
               , foreach, doSNOW, igraph, Matrix
               , pscl, corrplot, parallel, caret
               , stargazer, igraph, McSpatial)


os_detect <- Sys.info()['sysname']

if (os_detect == 'Darwin'){
  setwd('/Users/localadmin/Dropbox/Research/RoadNetworks')
} else if (os_detect == 'Linux'){
  setwd('/media/jesse/Files/Dropbox/RoadNetworks')
} else if (os_detect == 'Windows'){
  setwd('C:/Users/Jesse/Dropbox/RoadNetworks')
}


state_data <- fread('./Data/Overview/conflict_data.csv')

############################################################
#
# Useful functions
#
############################################################

std_dt <-  function(DT) {
  for (i in names(DT)){
    DT[, i := scale(DT[, i, with=F])]
  }
}

fillna <- function(DT, col) {
  set(DT,which(is.nan(DT[[col]])),col,median(DT[[col]], na.rm=T))
  set(DT,which(is.na(DT[[col]])),col,median(DT[[col]], na.rm=T))
  set(DT,which(!is.finite(DT[[col]])),col,median(DT[[col]], na.rm=T))
}

############################################################
#
# Loading road network data
#
############################################################


out_table <- data.table(
  name = character()
  , capital = integer()
  , ppp2000 = numeric()
  , forest = numeric()
  , mountain = numeric()
  , logpop2000 = numeric()
  , cowgroup = integer()
  , bdist = numeric()
  , capdist = numeric()
  , ttime = numeric()
  , numevents = integer()
  , straycount = numeric()
  , between = numeric()
  , degree = numeric()
  , eigen = numeric()
  , x = numeric()
  , y = numeric()
  , vdist = numeric()
  , log_vdist = numeric()
  , confdummy = factor())


for(i in 1:nrow(state_data)){
  
  conflict <- state_data$full_ref[i]
  
  load(paste0('./Data/Networks/', conflict, '_network.Rdata'))
  
  vnames <- igraph::list.vertex.attributes(out_network)
  
  out_network <- igraph::set.vertex.attribute(
    out_network, 'id', index=V(out_network), 1:vcount(out_network)
    )
  
  
  net_table <- data.table('id' = 1:vcount(out_network))
  
  for(col in vnames[-c(2:3)]){
    foo <- igraph::get.vertex.attribute(out_network, col)
    net_table[, paste(col) := foo]
  }
  
  
  net_table <- net_table[
    , list(
      name, capital, ppp2000, forest, mountain, logpop2000, cowgroup
      , bdist, capdist, ttime, numevents, straycount)]
  
  # std01 <- function(x){(x-min(x))/(max(x)-min(x))}
  net_table[, between := igraph::betweenness(out_network, normalized = T)]
  # net_table[, between := std01(between)]
  net_table[, degree := igraph::degree(out_network, normalized = T)]
  # net_table[, degree := std01(degree)]
  net_table[, eigen := igraph::eigen_centrality(out_network)$vector]
  
  net_table[is.na(numevents), numevents := 0]
  net_table[!is.na(cowgroup), cowgroup := 1]
  net_table[is.na(cowgroup), cowgroup := 0]
  
  
  
  for(col in c('x', 'y')){
    net_table[, paste(col) := igraph::get.vertex.attribute(out_network, col)]
  }
  
  
  v_dist <- spDists(x = as.matrix(net_table[numevents > 0, list(x,y)])
          , y = as.matrix(net_table[, list(x,y)]))
  
  v_dist[v_dist == 0] <- 9999999999
  v_dist <- data.table(t(apply(v_dist, 2, function(x){cbind(min(x), which.min(x))})))
  
  net_table[, vdist := v_dist$V1 / 1000]
  net_table[, log_vdist := log(vdist + 1)]
  
  net_table[, confdummy := conflict]
  
  for(col in c('ppp2000', 'forest', 'mountain', 'logpop2000', 'ttime', 'straycount')){
    fillna(net_table, col)
  }
  
  
  out_table <- rbind(out_table, net_table)
}



## Generate some transformed variables

# out_table[, degree := degree / max(degree)]

out_table[, mountain := mountain * 100]
out_table[, log_capdist := log(capdist + 1)]
out_table[, log_bdist := log(bdist + 1)]

out_table[, eventflag := (numevents > 0) * 1]
out_table[, confgroup := !is.na(cowgroup)]

out_table[, isolate := degree == 0]


## Set conflict dummy to factor

out_table[, confdummy := factor(confdummy)]


## Drop cases with very few events or that are duplicated

table(out_table[, confdummy], out_table[, eventflag])
table(out_table[, confdummy], out_table[, degree > 0])
out_table <- out_table[!confdummy %in% c(
  'ET_1-78', 'IQ_1-74', 'LA_1-65', 'LB_1-63'
  , 'EH_1-135', 'MM_1-67', 'NP_1-72'
  , 'BD_1-126', 'TH_1-248', 'YE_1-207'
#   , 'SD_1-113', 'SO_1-141', 'ID_1-171'
#   , 'EH_1-135'
  )]


## Drop Iraq because it has wildly OVERreported city data

out_table <- out_table[!confdummy %in% c('IQ_1-62')]

out_table[, confdummy := factor(confdummy)]


##### Summary statistics

## Generate summary stats

summary_stats <- out_table[
  , list(.N, sum(numevents), mean(capital), mean(ppp2000), mean(logpop2000)
         , mean(forest), mean(mountain), mean(log_capdist), mean(log_bdist)
         , mean(cowgroup), mean(between), mean(degree), mean(log_vdist)
         ), by = confdummy]

setnames(summary_stats, c(
  'Conflict Id', 'No. settlements', 'No. events', 'Capital status'
  , 'Economic activity', 'Population (logged)', '% Forest', '% Mountain'
  , 'Capital dist. (logged)', 'Border dist. (logged)'
  , 'Ethnic territory', 'Betweenness', 'Degree', 'Violence dist. (logged)'))

## Save for later table construction

save(summary_stats, file = './Data/Overview/summary_stats.Rdata')


## Scale values
# 
# cols <- names(out_table)
# cols <- cols[-which(cols %in% c(
#   'name', 'capital', 'numevents'
#   , 'cowgroup', 'pop2000', 'straycount'
#   , 'confdummy', 'isolate', 'confgroup', 'eventflag'
#   ))]
# 
# for(conf in unique(out_table$confdummy)){
#   out_table[
#     confdummy %in% conf
#     , (cols) := lapply(.SD, scale)
#     , .SDcols=cols
#     ]
# }


## Scale values

# cols <- names(out_table)
# cols <- cols[-which(cols %in% c(
#   'name', 'capital', 'numevents'
#   , 'cowgroup', 'pop2000', 'straycount'
#   , 'confdummy', 'isolate', 'confgroup', 'eventflag'
#   ))]

# 
# cols <- c('between', 'degree')
# 
# out_table[
#   , (cols) := lapply(.SD, scale)
#   , .SDcols=cols
#   ]



## Save file for analysis

save(out_table, file = './Data/Overview/road_analysis_data.Rdata')
