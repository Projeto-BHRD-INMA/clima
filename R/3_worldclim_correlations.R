############################################
# Explorando dados climáticos da BHRD
# Bruno M. Carvalho - brunomc.eco@gmail.com
# Calculando correlacoes mensais
############################################

library(dplyr)

## carregando dados do inmet
dados_inmet <- read.csv("./data/INMET_BHRD_prec_tmax_tmin_1970-2013.csv", header=TRUE)

# mantendo apenas anos compatíveis com worldclim, agregando por mês
inmet_mensal <- dados_inmet %>%
  filter(ano>=1970 & ano<=2000) %>%
  group_by(cod, lon, lat, mes) %>%
  summarise(prec = mean(PrecipitacaoTotal, na.rm = TRUE),
            tasmax = mean(TempMaximaMedia, na.rm = TRUE),
            tasmin = mean(TempMinimaMedia, na.rm = TRUE))

## carregando dados do worldclim extraídos nas estações

prec <- read.csv("./outputs/2_worldclim_pr_vals.csv")
tasmax <- read.csv("./outputs/2_worldclim_tasmax_vals.csv")
tasmin <- read.csv("./outputs/2_worldclim_tasmin_vals.csv")


## correlações geral inmet x worldclim

inmet_mensal <- arrange(inmet_mensal, cod, mes)
prec <- arrange(prec, cod, mes)
tasmax <- arrange(tasmax, cod, mes)
tasmin <- arrange(tasmin, cod, mes)


cor_results <- matrix(NA, nrow = 1, ncol = 6)

cor_prec <- cor.test(inmet_mensal$prec,
                       prec$prec_wc,
                       method = "pearson")
cor_results[1,1] <- cor_prec$estimate
cor_results[1,2] <- cor_prec$p.value
cor_tasmax <- cor.test(inmet_mensal$tasmax,
                         tasmax$tasmax_wc,
                         method = "pearson")
cor_results[1,3] <- cor_tasmax$estimate
cor_results[1,4] <- cor_tasmax$p.value
cor_tasmin <- cor.test(inmet_mensal$tasmin,
                         tasmin$tasmin_wc,
                         method = "pearson")
cor_results[1,5] <- cor_tasmin$estimate
cor_results[1,6] <- cor_tasmin$p.value


rownames(cor_results) <- c("worldclim")
colnames(cor_results) <- c("prec_r", "prec_p",
                           "tasmax_r", "tasmax_p",
                           "tasmin_r", "tasmin_p")

write.csv(cor_results, file = "./outputs/3_worldclim_cor_results.csv", row.names = TRUE)



## correlacoes mensais pr, tasmax e tasmin

wc <- list(prec, tasmax, tasmin)

results_r <- matrix(NA, nrow = 12, ncol = 3)
results_p <- matrix(NA, nrow = 12, ncol = 3)
for(j in 1:3){
  corValue <- vector(mode="numeric", length=12) # valores da correlacao por mes
  pVal <- vector(mode="numeric", length=12) # p-valores da correlacao por mes
  for(i in 1:12){
    corTest <- cor.test(filter(as.data.frame(inmet_mensal), mes == i)[ ,j+4], #$prec,
                        filter(wc[[j]], mes == i)[ ,5], #$prec_wc,
                        method = "pearson")
    corValue[i] <- corTest$estimate
    pVal[i] <- corTest$p.value
  }
  results_r[,j] <- corValue
  results_p[,j] <- pVal

}

colnames(results_r) <- c("prec", "tmax", "tmin")
colnames(results_p) <- c("prec", "tmax", "tmin")
rownames(results_r) <- c(1:12)
rownames(results_p) <- c(1:12)
write.csv(results_r, file = "./outputs/3_worldclim_moncor_r.csv")
write.csv(results_p, file = "./outputs/3_worldclim_moncor_p.csv")


#graficos

varname = c("PREC", "TMAX", "TMIN")
for(i in 1:3){
  pType <- c(1,16) # Point types (not-filled=1, filled=16 significant)
  indPtype <- as.integer(results_p[ ,i] <= 0.05)+1 # set alpha of the test here (in this case alpha=0.05)
  cols <- c("black","red") # Colors for points (if significant use red)

  tiff(filename = paste0('./figs/3_worldclim_correl_', varname[i], '.tif'),
       width = 500, height = 400, units = "px")

  plot(1:nrow(results_r), results_r[ ,i],
       type="n", xlab="Meses", ylab="Correlacao (Pearson)",
       main = paste(varname[i], "- Worldclim"),
       ylim = c(-1,1), xlim = c(1, 12),
       axes = FALSE)
  axis(side=1, at=c(1:12))
  axis(side=2, at=seq(-1, 1, by=0.5))
  abline(h=0, lty=2, col="light grey")
  lines(1:nrow(results_r), results_r[ ,i])
  points(1:nrow(results_r), results_r[ ,i], pch=pType[indPtype], col=cols[indPtype], cex=1.5)
  dev.off()
}
