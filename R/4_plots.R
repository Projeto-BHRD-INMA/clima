############################################
# Explorando dados climáticos da BHRD
# Bruno M. Carvalho - brunomc.eco@gmail.com
# Gráficos diversos
############################################

library(dplyr)
library(ggplot2)

cor1 <- read.csv("./outputs/3_worldclim_cor_results.csv")
cor2 <- read.csv("./outputs/3_cor_results.csv")

cors <- rbind(cor1, cor2)

model <- as.character(cors$X)
model[1] <- "Worldclim"

cors_prec <- data.frame(model = model, r = cors$prec_r) %>%
  arrange(desc(r)) %>%
  mutate(r3=round(r,3))

cors_tmax <- data.frame(model = model, r = cors$tasmax_r) %>%
  arrange(desc(r)) %>%
  mutate(r3=round(r,3))

cors_tmin <- data.frame(model = model, r = cors$tasmin_r) %>%
  arrange(desc(r)) %>%
  mutate(r3=round(r,3))

png("./figs/4_correl_all_prec.png", res = 300, width = 2000, height = 1100)
ggplot(data = cors_prec, aes(x = reorder(model, -r3), y = r3, width=.5)) +
  geom_bar(stat = "identity") +
  xlab("") +
  ylab("") +
  geom_text(aes(label=r3), vjust=-0.3, size=3.5) +
  theme_classic() +
  ggtitle("Precipitação")
dev.off()

png("./figs/4_correl_all_tmax.png", res = 300, width = 2000, height = 1100)
ggplot(data = cors_tmax, aes(x = reorder(model, -r3), y = r3, width=.5)) +
  geom_bar(stat = "identity") +
  xlab("") +
  ylab("") +
  geom_text(aes(label=r3), vjust=-0.3, size=3.5) +
  theme_classic() +
  ggtitle("Temperatura Máxima")
dev.off()

png("./figs/4_correl_all_tmin.png", res = 300, width = 2000, height = 1100)
ggplot(data = cors_tmin, aes(x = reorder(model, -r3), y = r3, width=.5)) +
  geom_bar(stat = "identity") +
  xlab("") +
  ylab("") +
  geom_text(aes(label=r3), vjust=-0.3, size=3.5) +
  theme_classic() +
  ggtitle("Temperatura Mínima")
dev.off()
