############################################
# Explorando dados climáticos da BHRD
# Bruno M. Carvalho - brunomc.eco@gmail.com
# Extraindo valores de GCMs nas estacoes
# meteorologicas da BHRD
############################################

library(raster)
library(rgdal)

# estacoes meteorologicas INMET
pontos <- read.csv("./data/estacoes_INMET_BDMEP_BHRD.csv", header = TRUE)
pontos <- pontos[pontos$dentro_BHRD.5km==1,]
coords <- data.frame(pontos$lon, pontos$lat)

# gcms cortados pra BHRD

load("./outputs/1_pr.RData")
load("./outputs/1_tasmax.RData")
load("./outputs/1_tasmin.RData")

# extrair valores dos gcms nas estacoes

pr_gcms <- list()
tasmax_gcms <- list()
tasmin_gcms <- list()
for(i in 1:8){
  pr_gcms[[i]] <- extract(pr[[i]], coords)
  tasmax_gcms[[i]] <- extract(tasmax[[i]], coords)
  tasmin_gcms[[i]] <- extract(tasmin[[i]], coords)
}

save(pr_gcms, file = "./outputs/2_pr_vals.RData")
save(tasmax_gcms, file = "./outputs/2_tasmax_vals.RData")
save(tasmin_gcms, file = "./outputs/2_tasmin_vals.RData")
