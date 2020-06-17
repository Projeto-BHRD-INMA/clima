############################################
# Explorando dados climáticos da BHRD
# Bruno M. Carvalho - brunomc.eco@gmail.com
# Calculando correlacoes mensais
############################################

library(dplyr)

## carregando dados do inmet
dados_inmet <- read.csv("./data/INMET_BHRD_prec_tmax_tmin_1970-2013.csv", header=TRUE)

# mantendo apenas anos compatíveis com ecoclimate, agregando por mês
inmet_mensal <- dados_inmet %>%
  filter(ano>=1950 & ano<=1999) %>%
  group_by(cod, lon, lat, mes) %>%
  summarise(prec = mean(PrecipitacaoTotal, na.rm = TRUE),
            tasmax = mean(TempMaximaMedia, na.rm = TRUE),
            tasmin = mean(TempMinimaMedia, na.rm = TRUE))

## carregando dados do ecoclimate

prec <- read.csv("./outputs/2_pr_vals.csv")
tasmax <- read.csv("./outputs/2_tasmax_vals.csv")
tasmin <- read.csv("./outputs/2_tasmin_vals.csv")

gcm_names <- c("CCSM", "CNRM", "FGOALS", "GISS", "IPSL", "MIROC", "MPI", "MRI")


## correlações geral inmet x gcms

inmet_mensal <- arrange(inmet_mensal, cod, mes)
prec <- arrange(prec, cod, month_n)
tasmax <- arrange(tasmax, cod, month_n)
tasmin <- arrange(tasmin, cod, month_n)

cor_results <- matrix(NA, nrow = 8, ncol = 6)
for(i in 1:8){
  cor_prec <- cor.test(inmet_mensal$prec,
                       prec[ ,i+4],
                       method = "pearson")
  cor_results[i,1] <- cor_prec$estimate
  cor_results[i,2] <- cor_prec$p.value
  cor_tasmax <- cor.test(inmet_mensal$tasmax,
                         tasmax[ ,i+4],
                         method = "pearson")
  cor_results[i,3] <- cor_tasmax$estimate
  cor_results[i,4] <- cor_tasmax$p.value
  cor_tasmin <- cor.test(inmet_mensal$tasmin,
                         tasmin[ ,i+4],
                         method = "pearson")
  cor_results[i,5] <- cor_tasmin$estimate
  cor_results[i,6] <- cor_tasmin$p.value
}
rownames(cor_results) <- gcm_names
colnames(cor_results) <- c("prec_r", "prec_p",
                           "tasmax_r", "tasmax_p",
                           "tasmin_r", "tasmin_p")

write.csv(cor_results, file = "./outputs/3_cor_results.csv", row.names = TRUE)


## correlacoes mensais pr
results_r <- matrix(NA, nrow = 12, ncol = 8)
results_p <- matrix(NA, nrow = 12, ncol = 8)
for(j in 1:8){
  corValue <- vector(mode="numeric", length=12) # valores da correlacao por mes
  pVal <- vector(mode="numeric", length=12) # p-valores da correlacao por mes
  for(i in 1:12){
    corTest <- cor.test(filter(inmet_mensal, mes == i)$prec,
                        filter(prec, month_n == i)[ ,j+4],
                        method = "pearson")
    corValue[i] <- corTest$estimate
    pVal[i] <- corTest$p.value
  }
  results_r[,j] <- corValue
  results_p[,j] <- pVal

  #graficos
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
write.csv(results_r, file = "./outputs/3_moncor_pr_r.csv")
write.csv(results_p, file = "./outputs/3_moncor_pr_p.csv")


## correlacoes mensais tasmax
results_r <- matrix(NA, nrow = 12, ncol = 8)
results_p <- matrix(NA, nrow = 12, ncol = 8)
for(j in 1:8){
  corValue <- vector(mode="numeric", length=12) # valores da correlacao por mes
  pVal <- vector(mode="numeric", length=12) # p-valores da correlacao por mes
  for(i in 1:12){
    corTest <- cor.test(filter(inmet_mensal, mes == i)$tasmax,
                        filter(tasmax, month_n == i)[ ,j+4],
                        method = "pearson")
    corValue[i] <- corTest$estimate
    pVal[i] <- corTest$p.value
  }
  results_r[,j] <- corValue
  results_p[,j] <- pVal

  #graficos
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
write.csv(results_r, file = "./outputs/3_moncor_tasmax_r.csv")
write.csv(results_p, file = "./outputs/3_moncor_tasmax_p.csv")


## correlacoes mensais tasmin
results_r <- matrix(NA, nrow = 12, ncol = 8)
results_p <- matrix(NA, nrow = 12, ncol = 8)
for(j in 1:8){
  corValue <- vector(mode="numeric", length=12) # valores da correlacao por mes
  pVal <- vector(mode="numeric", length=12) # p-valores da correlacao por mes
  for(i in 1:12){
    corTest <- cor.test(filter(inmet_mensal, mes == i)$tasmin,
                        filter(tasmin, month_n == i)[ ,j+4],
                        method = "pearson")
    corValue[i] <- corTest$estimate
    pVal[i] <- corTest$p.value
  }
  results_r[,j] <- corValue
  results_p[,j] <- pVal

  #graficos
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
write.csv(results_r, file = "./outputs/3_moncor_tasmin_r.csv")
write.csv(results_p, file = "./outputs/3_moncor_tasmin_p.csv")
