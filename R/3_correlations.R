############################################
# Explorando dados climáticos da BHRD
# Bruno M. Carvalho - brunomc.eco@gmail.com
# Calculando correlacoes mensais
############################################

library(raster)
library(rgdal)
library(reshape)

## carregando dados do inmet
dados_inmet <- read.csv("./data/INMET_BHRD_prec_tmax_tmin_1970-2013.csv", header=TRUE)

# mantendo apenas anos compatíveis com ecoclimate
dados_inmet <- subset(dados_inmet, ano>=1950 & ano<=1999)

# dados mensais por estacao
inmet_mensal <- aggregate(dados_inmet, by = c(list(dados_inmet$mes), list(dados_inmet$cod)), mean, na.rm=TRUE)

inmet_mensal <- data.frame(inmet_mensal$cod,
                           inmet_mensal$lat,
                           inmet_mensal$lon,
                           inmet_mensal$mes,
                           inmet_mensal$PrecipitacaoTotal,
                           inmet_mensal$TempMaximaMedia,
                           inmet_mensal$TempMinimaMedia)

names(inmet_mensal) <- c("ponto", "lat", "lon", "mes", "pr", "tasmax", "tasmin")

pr_inmet <- cast(inmet_mensal, formula = ponto ~ mes, value = "pr")
tasmax_inmet <- cast(inmet_mensal, formula = ponto ~ mes, value = "tasmax")
tasmin_inmet <- cast(inmet_mensal, formula = ponto ~ mes, value = "tasmin")


## carregando dados do ecoclimate

load("./outputs/2_pr_vals.RData")
load("./outputs/2_tasmax_vals.RData")
load("./outputs/2_tasmin_vals.RData")
gcm_names <- c("CCSM", "CNRM", "FGOALS", "GISS", "IPSL", "MIROC", "MPI", "MRI")


## correlacoes mensais pr

results_r <- matrix(NA, nrow = 12, ncol = 8)
results_p <- matrix(NA, nrow = 12, ncol = 8)
for(j in 1:8){
  corValue <- vector(mode="numeric", length=12) # Correlation value here
  pVal <- vector(mode="numeric", length=12) # p-values here
  for(i in 1:12){
    corTest <- cor.test(pr_inmet[,i+1], pr_gcms[[j]][,i], method = "pearson")
    corValue[i] <- corTest$estimate
    pVal[i] <- corTest$p.value
  }

  results_r[,j] <- corValue
  results_p[,j] <- pVal

  # graficos
  pType <- c(1,16) # Point types (not-filled=1, filled=16 significant)
  indPtype <- as.integer(pVal <= 0.05)+1 # set alpha of the test here (in this case alpha=0.05)
  cols <- c("black","red") # Colors for points (if significant use red)

  tiff(filename = paste('./figs/3_correl_pr_', gcm_names[[j]], '.tif', sep = ""),
       width = 500, height = 400, units = "px")

  plot(1:length(corValue), corValue,
       type="n", xlab="Meses", ylab="Correlacao (Pearson)",
       main = paste("PREC - ", gcm_names[[j]], sep=""),
       ylim = c(-1,1), xlim = c(1, 12),
       axes = FALSE)
  axis(side=1, at=c(1:12))
  axis(side=2, at=seq(-1, 1, by=0.5))
  abline(h=0, lty=2, col="light grey")
  lines(1:length(corValue), corValue)
  points(1:length(corValue), corValue, pch=pType[indPtype], col=cols[indPtype], cex=1.5)
  dev.off()
}

colnames(results_r) <- gcm_names
colnames(results_p) <- gcm_names
rownames(results_r) <- c(1:12)
rownames(results_p) <- c(1:12)
write.csv(results_r, file = "./outputs/3_pr_ecoclimate_r.csv")
write.csv(results_p, file = "./outputs/3_pr_ecoclimate_p.csv")



#### correlacoes mensais tasmax ####

results_r <- matrix(NA, nrow = 12, ncol = 8)
results_p <- matrix(NA, nrow = 12, ncol = 8)
for(j in 1:8){
  corValue <- vector(mode="numeric", length=12) # Correlation value here
  pVal <- vector(mode="numeric", length=12) # p-values here
  for(i in 1:12){
    corTest <- cor.test(tasmax_inmet[,i+1], tasmax_gcms[[j]][,i], method = "pearson")
    corValue[i] <- corTest$estimate
    pVal[i] <- corTest$p.value
  }

  results_r[,j] <- corValue
  results_p[,j] <- pVal

  # graficos
  pType <- c(1,16) # Point types (not-filled=1, filled=16 significant)
  indPtype <- as.integer(pVal <= 0.05)+1 # set alpha of the test here (in this case alpha=0.05)
  cols <- c("black","red") # Colors for points (if significant use red)

  tiff(filename = paste('./figs/3_correl_tasmax_', gcm_names[[j]], '.tif', sep = ""),
       width = 500, height = 400, units = "px")

  plot(1:length(corValue), corValue,
       type="n", xlab="Meses", ylab="Correlacao (Pearson)",
       main = paste("TMAX - ", gcm_names[[j]], sep=""),
       ylim = c(-1,1), xlim = c(1, 12),
       axes = FALSE)
  axis(side=1, at=c(1:12))
  axis(side=2, at=seq(-1, 1, by=0.5))
  abline(h=0, lty=2, col="light grey")
  lines(1:length(corValue), corValue)
  points(1:length(corValue), corValue, pch=pType[indPtype], col=cols[indPtype], cex=1.5)
  dev.off()
}

colnames(results_r) <- gcm_names
colnames(results_p) <- gcm_names
rownames(results_r) <- c(1:12)
rownames(results_p) <- c(1:12)
write.csv(results_r, file = "./outputs/3_tasmax_ecoclimate_r.csv")
write.csv(results_p, file = "./outputs/3_tasmax_ecoclimate_p.csv")

#### correlacoes mensais tasmin ####

results_r <- matrix(NA, nrow = 12, ncol = 8)
results_p <- matrix(NA, nrow = 12, ncol = 8)
for(j in 1:8){
  corValue <- vector(mode="numeric", length=12) # Correlation value here
  pVal <- vector(mode="numeric", length=12) # p-values here
  for(i in 1:12){
    corTest <- cor.test(tasmin_inmet[,i+1], tasmin_gcms[[j]][,i], method = "pearson")
    corValue[i] <- corTest$estimate
    pVal[i] <- corTest$p.value
  }

  results_r[,j] <- corValue
  results_p[,j] <- pVal

  # graficos
  pType <- c(1,16) # Point types (not-filled=1, filled=16 significant)
  indPtype <- as.integer(pVal <= 0.05)+1 # set alpha of the test here (in this case alpha=0.05)
  cols <- c("black","red") # Colors for points (if significant use red)

  tiff(filename = paste('./figs/3_correl_tasmin_', gcm_names[[j]], '.tif', sep = ""),
       width = 500, height = 400, units = "px")

  plot(1:length(corValue), corValue,
       type="n", xlab="Meses", ylab="Correlacao (Pearson)",
       main = paste("TMIN - ", gcm_names[[j]], sep=""),
       ylim = c(-1,1), xlim = c(1, 12),
       axes = FALSE)
  axis(side=1, at=c(1:12))
  axis(side=2, at=seq(-1, 1, by=0.5))
  abline(h=0, lty=2, col="light grey")
  lines(1:length(corValue), corValue)
  points(1:length(corValue), corValue, pch=pType[indPtype], col=cols[indPtype], cex=1.5)
  dev.off()
}

colnames(results_r) <- gcm_names
colnames(results_p) <- gcm_names
rownames(results_r) <- c(1:12)
rownames(results_p) <- c(1:12)
write.csv(results_r, file = "./outputs/3_tasmin_ecoclimate_r.csv")
write.csv(results_p, file = "./outputs/3_tasmin_ecoclimate_p.csv")
