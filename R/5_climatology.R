############################################
# Explorando dados climáticos da BHRD
# Bruno M. Carvalho - brunomc.eco@gmail.com
# Gráficos de climatologia
############################################

library(dplyr)
library(ggplot2)

## carregando dados do inmet
dados_inmet <- read.csv("./data/INMET_BHRD_prec_tmax_tmin_1970-2013.csv", header=TRUE)

# mantendo série de 20 anos (compatível com worldclim), calculando temp média e agregando por mês
inmet_mensal <- dados_inmet %>%
  filter(ano>=1970 & ano<=2000) %>%
  mutate(TempMedia = (TempMaximaMedia+TempMinimaMedia)/2) %>%
  group_by(mes) %>%
  summarise(prec = mean(PrecipitacaoTotal, na.rm = TRUE),
            tasmax = mean(TempMaximaMedia, na.rm = TRUE),
            tasmin = mean(TempMinimaMedia, na.rm = TRUE),
            tasmed = mean(TempMedia, na.rm = TRUE))

inmet_mensal$mes <- as.factor(inmet_mensal$mes)

# plot

png("./figs/5_climatol_BHRD.png", res = 300, width = 2000, height = 1000)
inmet_mensal %>% ggplot() +
  geom_bar(mapping = aes(x = mes, y = prec * 30 / 300), stat = "identity", colour = gray(0.4), fill = gray(0.4), width = 0.5) +
  geom_line(mapping = aes(x = mes, y = tasmed, group=1), color = "red") +
  geom_point(mapping = aes(x = mes, y = tasmed), size = 3, shape = 21, fill = "red") +
  scale_x_discrete(labels = c("Jan", "Fev", "Mar", "Abr", "Mai", "Jun", "Jul", "Ago", "Set", "Out", "Nov", "Dez")) +
  scale_y_continuous(
    name = expression("Temperatura ("~degree~"C)"),
    sec.axis = sec_axis(~ . * 300 / 30 , name = "Precipitação (mm)"),
    limits = c(0, 30)) +
  xlab("") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )
dev.off()
