devtools::install_github("metronhomo/brostatistics")
library(brostatistics)
# ≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤

# Leo los datos

datos <- brostatistics::gaseosa(
  xfile = list.files("./datos",full.names = T)[1], 
  yfile = list.files("./datos",full.names = T)[2]
  )

##########################################################################################
# Modificaciones necesarias


##########################################################################################
# Saco variable "total
datos$Total <- factor(1,levels = 1,labels = "Total")


##########################################################################################

# Reporte



bandera<- c("Total","P15_Celular","P15_Tablet___Ipad","P15_Computadora__Lap_Top",
"P15_1_Celular","P15_1_Tablet___Ipad","P15_1_Computadora__Lap_Top",
"F1Genero","NSE","Edad_Rango","Plaza")

datos$P15_Celular

resultados <- list(
    genero = frecuentator(fTtabla = datos,fTvariables = 'F1Genero',fTlevels = T,fbanner = bandera),
    nse = frecuentator(fTtabla = datos,fTvariables = 'NSE',fTlevels = T,fbanner = bandera)
)

exportator(resultados,"resultados.csv")
