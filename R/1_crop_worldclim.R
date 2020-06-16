############################################
# Explorando dados climáticos da BHRD
# Bruno M. Carvalho - brunomc.eco@gmail.com
# Cortando rasters de GCMs para a BHRD
############################################

library(raster)
library(rgdal)
library(stringr)

# dados worldclim

# mask da regiao da BHRD (extensão do polígono BHRD + buffer de 5 km)
bhrd <- readOGR("./data/vector/extensao+5km_BHRD.shp")
bhrd <- buffer(bhrd, width=0.5) # só mais um pouquinho de buffer pra pegar as estações


# carregar rasters por variável e cortar

pr <- list.files("./big/worldclim/wc2.1_10m_prec/", full.names = TRUE, pattern = ".tif$", recursive = TRUE) %>%
  stack() %>%
  crop(bhrd)

tasmax <- list.files("./big/worldclim/wc2.1_10m_tmax/", full.names = TRUE, pattern = ".tif$", recursive = TRUE) %>%
  stack() %>%
  crop(bhrd)

tasmin <- list.files("./big/worldclim/wc2.1_10m_tmin/", full.names = TRUE, pattern = ".tif$", recursive = TRUE) %>%
  stack() %>%
  crop(bhrd)

# salvar listas (ocupam menos espaço que rasters)

save(pr, file = "./outputs/1_pr.RData")
save(tasmax, file = "./outputs/1_tasmax.RData")
save(tasmin, file = "./outputs/1_tasmin.RData")
