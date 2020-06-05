############################################
# Explorando dados climáticos da BHRD
# Bruno M. Carvalho - brunomc.eco@gmail.com
# Cortando rasters de GCMs para a BHRD
############################################

library(raster)
library(rgdal)

# dados ecoclimate

# mask da regiao da BHRD (extensão do polígono BHRD + buffer de 5 km)
bhrd <- readOGR("./data/vector/extensao+5km_BHRD.shp")
bhrd <- buffer(bhrd, width=0.5) # só mais um pouquinho de buffer pra pegar as estações

gcm_names <- c("CCSM", "CNRM", "FGOALS", "GISS", "IPSL", "MIROC", "MPI", "MRI")

# carregar rasters por variável e cortar

# precipitacao
pr <- list()
for(i in 1:8){
  files <- list.files("./big/ecoclimate/pr", full.names = TRUE, pattern = ".bil$", recursive = TRUE)
  files <- grep(files, pattern = gcm_names[i], value = TRUE)
  pr[[i]] <- stack(files)
  pr[[i]] <- crop(pr[[i]], bhrd)
}

# temp maxima
tasmax <- list()
for(i in 1:8){
  files <- list.files("./big/ecoclimate/tasmax", full.names = TRUE, pattern = ".bil$", recursive = TRUE)
  files <- grep(files, pattern = gcm_names[i], value = TRUE)
  tasmax[[i]] <- stack(files)
  tasmax[[i]] <- crop(tasmax[[i]], bhrd)
}

# temp minima
tasmin <- list()
for(i in 1:8){
  files <- list.files("./big/ecoclimate/tasmin", full.names = TRUE, pattern = ".bil$", recursive = TRUE)
  files <- grep(files, pattern = gcm_names[i], value = TRUE)
  tasmin[[i]] <- stack(files)
  tasmin[[i]] <- crop(tasmin[[i]], bhrd)
}

# salvar listas

save(pr, file = "./outputs/1_pr.RData")
save(tasmax, file = "./outputs/1_tasmax.RData")
save(tasmin, file = "./outputs/1_tasmin.RData")
