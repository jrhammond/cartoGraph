rm(list=ls())
if (!require("pacman")) install.packages("pacman")
pacman::p_load(data.table, vcdExtra, caret, pscl, pROC)

os_detect <- Sys.info()['sysname']

if (os_detect == 'Darwin'){
  setwd('/Users/localadmin/Dropbox/Research/RoadNetworks')
} else if (os_detect == 'Linux'){
  setwd('/media/jesse/Files/Dropbox/RoadNetworks')
} else if (os_detect == 'Windows'){
  setwd('C:/Users/Jesse/Dropbox/RoadNetworks')
}

state_data <- fread('./Data/Overview/conflict_data.csv')
load('./Data/Overview/road_analysis_data.Rdata')

set.seed(88005)


############
##
## On-the-fly adjustments
##
############

out_table[, mod_between := between]
out_table[isolate == 1, mod_between := 0.000001]

out_table[, std_between := scale(between)]
out_table[, std_degree := scale(degree)]

out_table[, log_between := log(between+1)]
out_table[, log_degree := log(degree+1)]

out_table[, nonisolate := abs(isolate - 1)]
out_table[, logbet_isolate := log_between * abs(isolate - 1)]

############
##
## Subset into training/testing data
##
############

test_cases <- sample(unique(out_table$confdummy), 6)
train_data <- out_table[!confdummy %in% test_cases]
test_data <- out_table[confdummy %in% test_cases]


############
##
## Count models (Poisson and ZINB) with spatial controls
##
############


## Model 0: non-spatial controls, Poisson

m0 <- glm(
  numevents 
  ~ ppp2000 + logpop2000 + capital + cowgroup
  + forest + mountain + log_bdist + log_capdist 
  + confdummy
  , data = train_data
  , family = 'poisson'
  )



## Model 1: controls and distance from other events, Poisson

m1 <- glm(
  numevents 
  ~ ppp2000 + logpop2000 + capital + cowgroup
  + forest + mountain + log_bdist + log_capdist 
  + log_vdist
  + confdummy
  , data = train_data
  , family = 'poisson'
  )


## Model 2: controls and distance from other events, ZINB

m2 <- zeroinfl(
  numevents
  ~ ppp2000 + logpop2000 + capital + cowgroup
  | forest + mountain + log_bdist + log_capdist 
  + log_vdist 
  # + confdummy
  , data = train_data
  , dist = 'negbin'
  , link = 'logit'
  )


## Model 3: all controls and betweenness centrality, ZINB

m3 <- zeroinfl(
  numevents 
  ~ between + isolate
  + ppp2000 + logpop2000 + capital + cowgroup
  | forest + mountain + log_bdist + log_capdist 
  + between + isolate
  + log_vdist 
  # + confdummy
  , data = train_data
  , dist = 'negbin'
  , link = 'logit'
  )


## Model 4: all controls and degree centrality, ZINB

m4 <- zeroinfl(
  numevents 
  ~ degree + isolate
  + ppp2000 + logpop2000 + capital + cowgroup 
  | forest + mountain + log_bdist + log_capdist 
  + degree + isolate
  + log_vdist 
  # + confdummy
  , data = train_data
  , dist = 'negbin'
  , link = 'logit'
  )


## Model 6: all controls and between/degree metrics, ZINB

m6 <- zeroinfl(
  numevents 
  ~ between + degree + isolate
  + ppp2000 + logpop2000 + capital + cowgroup
  | forest + mountain + log_bdist + log_capdist
  + between + degree + isolate
  + log_vdist
  # + confdummy
  , data = train_data
  , dist = 'negbin'
  , link = 'logit'
)

save(m0, file = './Models/m0RR2.Rdata');AIC(m0);BIC(m0)
save(m1, file = './Models/m1RR2.Rdata');AIC(m1);BIC(m1)
save(m2, file = './Models/m2RR2.Rdata');AIC(m2)
save(m3, file = './Models/m3RR2.Rdata');AIC(m3)
save(m4, file = './Models/m4RR2.Rdata');AIC(m4)
