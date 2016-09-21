devtools::install_github("metronhomo/brostatistics2")
library(brostatistics2)
# ≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤

# Leo los datos

datos <- brostatistics2::gaseosa(
  xfile = list.files("./datos",full.names = T)[1], 
  yfile = list.files("./datos",full.names = T)[2]
  )

##########################################################################################
# Modificaciones necesarias


##########################################################################################
# Saco variable "total
datos$Total <- factor(1,levels = 1,labels = "Total")
<-0


datos[is.na(datos$P15_B_CELULAR_Hablar_por_tel_fono),"P15_B_CELULAR_Hablar_por_tel_fono"]<-0


##########################################################################################

# Reporte

frecuentator(fTtabla = datos,fTvariables = nombresR(datos, "15_B_CELULAR"),fTlevels = F)

frecuentator

nombresR(datos,"P15_C")
)
table(datos$P15_B_CELULAR_OTRO)
table(datos$P15_B_TABLET_OTRO)
table(datos$P15_B_COMPU_OTRO)

table(datos$P15_C_CELULAR_OTRO)
table(datos$P15_C_TABLET_OTRO)
table(datos$P15_C_COMPU_OTRO)

nombresR(datos,"P16")

table(datos$P16_CELULAR_OTRO)
table(datos$P16_TABLETA_Otro__Especificar)

