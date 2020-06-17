############################################
# Explorando dados climáticos da BHRD
# Bruno M. Carvalho - brunomc.eco@gmail.com
# Extraindo valores de GCMs nas estacoes
# meteorologicas da BHRD
############################################

library(raster)
library(rgdal)
library(data.table)
library(stringr)
library(tidyr)

# estacoes meteorologicas INMET
pontos <- read.csv("./data/estacoes_INMET_BDMEP_BHRD.csv", header = TRUE)
pontos <- pontos[pontos$dentro_BHRD.5km==1,]
coords <- data.frame(pontos$lon, pontos$lat)
coords <- data.frame(cod = pontos$ï..cod,
                     lon = coords$pontos.lon,
                     lat = coords$pontos.lat)


# extrair valores do worldclim nas estações

# prec
load("./outputs/1_worldclim_pr.RData")
pr <- raster::extract(pr, coords[ ,2:3], df = T) #extraindo valores em cada estação

pr <- data.frame(cod = coords$cod,
                 lon = coords$lon,
                 lat = coords$lat,
                 pr[ ,-1]) #criando data.frame com resultados

pr <- pivot_longer(pr,
                   cols = 4:15,
                   names_to = "mes",
                   values_to = "prec_wc") #transformando tabela (wide to long)

pr$mes <- as.integer(str_sub(pr$mes, -2, -1)) #convertendo nomes de meses em n


# tasmax
load("./outputs/1_worldclim_tasmax.RData")
tasmax <- raster::extract(tasmax, coords[ ,2:3], df = T) #extraindo valores em cada estação

tasmax <- data.frame(cod = coords$cod,
                 lon = coords$lon,
                 lat = coords$lat,
                 tasmax[ ,-1]) #criando data.frame com resultados

tasmax <- pivot_longer(tasmax,
                   cols = 4:15,
                   names_to = "mes",
                   values_to = "tasmax_wc") #transformando tabela (wide to long)

tasmax$mes <- as.integer(str_sub(tasmax$mes, -2, -1)) #convertendo nomes de meses em n



# tasmin
load("./outputs/1_worldclim_tasmin.RData")
tasmin <- raster::extract(tasmin, coords[ ,2:3], df = T) #extraindo valores em cada estação

tasmin <- data.frame(cod = coords$cod,
                     lon = coords$lon,
                     lat = coords$lat,
                     tasmin[ ,-1]) #criando data.frame com resultados

tasmin <- pivot_longer(tasmin,
                       cols = 4:15,
                       names_to = "mes",
                       values_to = "tasmin_wc") #transformando tabela (wide to long)

tasmin$mes <- as.integer(str_sub(tasmin$mes, -2, -1)) #convertendo nomes de meses em n



# salvando outputs
write.csv(pr, file = "./outputs/2_worldclim_pr_vals.csv", row.names = FALSE)
write.csv(tasmax, file = "./outputs/2_worldclim_tasmax_vals.csv", row.names = FALSE)
write.csv(tasmin, file = "./outputs/2_worldclim_tasmin_vals.csv", row.names = FALSE)

