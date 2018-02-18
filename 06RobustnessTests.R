
###############################################################################
##
## Robustness check: rolling window of observations ordered by fatality
##
###############################################################################

## Load in country-level GED data 

events <- data.table(
  'dist' = numeric()
  , 'id' = integer()
  , 'cas_high' = integer()
  , 'name' = character()
)

for(conflict in state_data$full_ref){
  try(
    conf_data <- fread(paste0('./Data/UCDP-GED/', conflict, '_events.csv'))
    )
  
  if (
    'data.table' %in% class(conf_data)
  ) {
    events <- rbind(events, conf_data)
  }
}


## Order events by casualty

setkeyv(events, 'cas_high')


## Remove all event-observations from analysis data set

out_table[, numevents := NULL] 


## Set up indices: rolling window of 50% of observations, step = 100
##  for a total of 117 sub-models

start <- c(seq(1, 11588, by = 250), 11588)
end <- c(seq(11588, nrow(events), by = 250), nrow(events))
out_len <- length(start)


## Set up output data object
out_coefs <- data.table(
  'mean_cas' = numeric()
  , 'count_between_coef' = numeric()
  , 'count_between_se' = numeric()
  , 'count_degree_coef' = numeric()
  , 'count_degree_se' = numeric()
  , 'zero_between_coef' = numeric()
  , 'zero_between_se' = numeric()
  , 'zero_degree_coef' = numeric()
  , 'zero_degree_se' = numeric()
)


## Loop over window and for each step:
##  1. fit model
##  2. extract degree/between coefficients and SE's
##  3. store in output table along with mean casualties

for(i in c(1:out_len)){
  
  ## Initialize window indices
  
  idx_i <- start[i]
  idx_j <- end[i]
  
  
  ## Subset event data
  
  this_events <- events[idx_i:idx_j]
  
  
  ## Aggregate by settlement
  
  this_events[, numevents := .N, by = name]
  
  this_events <- this_events[!duplicated(name)]
  
  this_data <- merge(
    out_table, this_events
    , by = 'name'
    , all.x = T
    , sort = F
    )
  
  this_data[is.na(numevents), numevents := 0]
  
  
  ## Fit model to subsetted data
  
  out_model <- try(zeroinfl(
      numevents 
      ~ between 
      + degree 
      + ppp2000 
      + logpop2000
      + capital + cowgroup
      | forest + mountain + log_bdist + log_capdist 
      + between
      + degree
      + log_vdist 
      + isolate 
      + confdummy
      , data = this_data
      , dist = 'negbin'
      , link = 'logit'
    )
  )
  
  if(class(out_model) == 'try-error'){
    out_coefs <- rbind(out_coefs, list(NA, NA, NA, NA, NA, NA, NA, NA, NA))
  } else{
    
    ## Extract coefficient estimates for key variables
    
    count_coefs <- summary(out_model)$coefficients$count
    zero_coefs <- summary(out_model)$coefficients$zero
    
    out_coefs <- rbind(
      out_coefs
      , list(
        mean(this_events$cas_high)
        , count_coefs[2,1], count_coefs[2,2]
        , count_coefs[3,1], count_coefs[3,2]
        , zero_coefs[2,1], zero_coefs[2,2]
        , zero_coefs[3,1], zero_coefs[3,2]
      )
    )
  }
  
  
  ## Save periodically
  
  if(nrow(out_coefs) %% 5 == 0){
    write.csv(out_coefs, file = './Data/Overview/zinb_robustness_coefs.csv', row.names = F)
  }
  
}

write.csv(out_coefs, file = './Data/Overview/zinb_robustness_coefs.csv', row.names = F)






############
##
## Alternative modeling approaches
##
############

## Implement downsampling for 3:1 1/0 ratio, by state

rat <- 3

for(conf in unique(out_table$confdummy)){
  this_data <- out_table[confdummy %in% conf]
  obs_1 <- which(this_data$eventflag == 1)
  obs_0 <- which(this_data$eventflag == 0)
  n_1 <- length(obs_1)
  n_0 <- length(obs_0)
  if(n_0 > (n_1 * rat)){
    keep_idx <- sample(obs_0, (n_1 * rat), replace = F)
  } else {
    keep_idx <- obs_0
  }
  
  sampled_data <- this_data[c(obs_1, keep_idx)]
  
  out_table <- out_table[!confdummy %in% conf]
  out_table <- rbind(out_table, sampled_data)
  
}

table(out_table$confdummy, out_table$eventflag)



##### Appendix replication: SARP-MCMC models. These take a LONG TIME.

lonlatproj <- CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')
aeaproj <- CRS('+proj=aea +lat_1=20 +lat_2=-23 +lat_0=0 +lon_0=25 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs')

country_i <- integer()
country_j <- integer()
country_x <- numeric()


for(conf in unique(out_table$confdummy)){
  country_idx <- which(out_table$confdummy %in% conf)
  matrix_idx = min(country_idx) - 1
  
  
  towns <- SpatialPointsDataFrame(coords = as.matrix(out_table[confdummy %in% conf, list(x,y)])
                                  , data = out_table[confdummy %in% conf])
  proj4string(towns) <- aeaproj
  towns <- spTransform(towns, lonlatproj)
  
  # country_wmat <- makew(coormat = towns
  #                       , method = "knear")$wmat
  
  country_wmat <- makew(coormat = out_table[confdummy %in% conf, list(x,y)]
                        , method = "kernel"
                        , window = (15 / length(country_idx))
                        , kern = 'trwt')$wmat
  country_i <- c(country_i, which(country_wmat > 0, arr.ind = T)[,1] + matrix_idx)
  country_j <- c(country_j, which(country_wmat > 0, arr.ind = T)[,2] + matrix_idx)
  country_x <- c(country_x, country_wmat[which(country_wmat > 0)])
  
}

kmat <- sparseMatrix(i = country_i
                     , j = country_j
                     , x = country_x)

## Generate conflict dummies

inds <- as.character(unique(out_table$confdummy))
out_table[, (inds) := lapply(inds, function(x) confdummy == x)]


## Model 1: just controls

sarp_mcmc1 <- sar_probit_mcmc(y = out_table[, eventflag]
                              , X = as.matrix(
                                out_table[, c(2:7, 20:21, 25, 25:70), with = F]
                              )
                              , W = kmat
                              , showProgress = T
                              , ndraw = 1000
                              , burn.in = 500
                              , thinning = 10
)

save(sarp_mcmc1, file = 'sarp_mcmc_controls.Rdata')


## Model 2: network effects

sarp_mcmc2 <- sar_probit_mcmc(y = out_table[, eventflag]
                              , X = as.matrix(
                                out_table[, c(2:7, 12:13, 20:21, 24, 25:70), with = F]
                              )
                              , W = kmat
                              , showProgress = T
                              , ndraw = 1000
                              , burn.in = 500
                              , thinning = 10
)

save(sarp_mcmc2, file = 'sarp_mcmc_fullmodel.Rdata')

