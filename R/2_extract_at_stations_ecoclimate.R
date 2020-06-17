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

# carregar gcms cortados pra BHRD
load("./outputs/1_pr.RData")
load("./outputs/1_tasmax.RData")
load("./outputs/1_tasmin.RData")

# extrair valores dos gcms nas estacoes

gcm_names <- c("CCSM", "CNRM", "FGOALS", "GISS", "IPSL", "MIROC", "MPI", "MRI")
month_conv <- c("Jan" = "1", "Feb" = "2", "Mar" = "3", "Apr" = "4",
                "May" = "5", "Jun" = "6", "Jul" = "7", "Aug" = "8",
                "Sep" = "9", "Oct" = "10", "Nov" = "11", "Dec" = "12")

# precipitacao
pr_gcms <- list()
for(i in 1:8){
  pr_gcms[[i]] <- raster::extract(pr[[i]], coords[ ,2:3], df = T) #extraindo valores em cada estação

  pr_gcms[[i]] <- data.frame(cod = coords$cod,
                             lon = coords$lon,
                             lat = coords$lat,
                             gcm = gcm_names[[i]],
                             pr_gcms[[i]]) #criando data.frame com resultados

  pr_gcms[[i]] <- pivot_longer(pr_gcms[[i]],
                               cols = 6:17,
                               names_to = "month_name",
                               values_to = "prec") #transformando tabela (wide to long)

  month_n <- str_replace_all(str_sub(pr_gcms[[i]]$month_name, -3, -1),
                             month_conv) #convertendo nomes de meses em n

  pr_gcms[[i]] <- data.frame(pr_gcms[[i]],
                  month_n = month_n)
}

prec <- rbindlist(pr_gcms) #combinando resultados dos GCMs em um data.frame

prec_mensal <- prec %>%
  group_by(cod, lon, lat, gcm, month_n) %>%
  summarise(prec = mean(prec, na.rm = TRUE)) %>%
  pivot_wider(names_from = gcm, values_from = prec)

colnames(prec_mensal)[5:12] <- paste0(colnames(prec_mensal)[5:12], rep("_prec",8))


# temperatura maxima
tasmax_gcms <- list()
for(i in 1:8){
  tasmax_gcms[[i]] <- raster::extract(tasmax[[i]], coords[ ,2:3], df = T) #extraindo valores em cada estação

  tasmax_gcms[[i]] <- data.frame(cod = coords$cod,
                             lon = coords$lon,
                             lat = coords$lat,
                             gcm = gcm_names[[i]],
                             tasmax_gcms[[i]]) #criando data.frame com resultados

  tasmax_gcms[[i]] <- pivot_longer(tasmax_gcms[[i]],
                               cols = 6:17,
                               names_to = "month_name",
                               values_to = "tasmax") #transformando tabela (wide to long)

  month_n <- str_replace_all(str_sub(tasmax_gcms[[i]]$month_name, -3, -1),
                             month_conv) #convertendo nomes de meses em n

  tasmax_gcms[[i]] <- data.frame(tasmax_gcms[[i]],
                             month_n = month_n)
}
tasmax <- rbindlist(tasmax_gcms) #combinando resultados dos GCMs em um data.frame

tasmax_mensal <- tasmax %>%
  group_by(cod, lon, lat, gcm, month_n) %>%
  summarise(tasmax = mean(tasmax, na.rm = TRUE)) %>%
  pivot_wider(names_from = gcm, values_from = tasmax)

colnames(tasmax_mensal)[5:12] <- paste0(colnames(tasmax_mensal)[5:12], rep("_tasmax",8))


# temperatura minima
tasmin_gcms <- list()
for(i in 1:8){
  tasmin_gcms[[i]] <- raster::extract(tasmin[[i]], coords[ ,2:3], df = T) #extraindo valores em cada estação

  tasmin_gcms[[i]] <- data.frame(cod = coords$cod,
                                 lon = coords$lon,
                                 lat = coords$lat,
                                 gcm = gcm_names[[i]],
                                 tasmin_gcms[[i]]) #criando data.frame com resultados

  tasmin_gcms[[i]] <- pivot_longer(tasmin_gcms[[i]],
                                   cols = 6:17,
                                   names_to = "month_name",
                                   values_to = "tasmin") #transformando tabela (wide to long)

  month_n <- str_replace_all(str_sub(tasmin_gcms[[i]]$month_name, -3, -1),
                             month_conv) #convertendo nomes de meses em n

  tasmin_gcms[[i]] <- data.frame(tasmin_gcms[[i]],
                                 month_n = month_n)
}
tasmin <- rbindlist(tasmin_gcms) #combinando resultados dos GCMs em um data.frame

tasmin_mensal <- tasmin %>%
  group_by(cod, lon, lat, gcm, month_n) %>%
  summarise(tasmin = mean(tasmin, na.rm = TRUE)) %>%
  pivot_wider(names_from = gcm, values_from = tasmin)

colnames(tasmin_mensal)[5:12] <- paste0(colnames(tasmin_mensal)[5:12], rep("_tasmin",8))


# salvando outputs
write.csv(prec_mensal, file = "./outputs/2_pr_vals.csv", row.names = FALSE)
write.csv(tasmax_mensal, file = "./outputs/2_tasmax_vals.csv", row.names = FALSE)
write.csv(tasmin_mensal, file = "./outputs/2_tasmin_vals.csv", row.names = FALSE)

