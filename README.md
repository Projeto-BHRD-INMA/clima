# BHRD - Clima

Análise exploratória dos dados climáticos da Bacia Hidrográfica do Rio Doce (BHRD)

Correlações entre dados observados em estações meteorológicas e dados modelados


Variáveis avaliadas (médias mensais):
- Precipitação
- Temperatura máxima
- Temperatura mínima


Dados de estações meteorológicas:  
INMET - BDMEP, www.inmet.gov.br  
Série Histórica - dados mensais (1970-2013)  
- PrecipitaçãoTotal: acumulada mensal (mm)  
- TempMaximaMedia: média das temperaturas máximas diárias (°C)  
- TempMinimaMedia: média das temperaturas mínimas diárias (°C)  

Dados de GCMs:  
Ecoclimate, www.ecoclimate.org  
Modern (1950-1999)  
- pr: precipitation flux (mm m-2 s-1) converted to total monthly precipitation (mm/month)  
- tasmax: maximum surface temperature, converted from Kelvin to Celsius  
- tasmin: minimum surface temperature, converted from Kelvin to Celsius  

Dados históricos interpolados:  
Worldclim v2.1, www.worldclim.org  
Historical (1970-2000)  
- prec: total precipitation (mm)  
- tmin: average minimum temperature (°C)  
- tmax: average maximum temperature (°C)  
