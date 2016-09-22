devtools::install_github("metronhomo/brostatistics")
library(brostatistics)
install.packages("readxl")
library(readxl)
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


datos[is.na(datos$P15_B_CELULAR_Hablar_por_tel_fono),"P15_B_CELULAR_Hablar_por_tel_fono"]<-0

##########################################################################################

# Reporte

frecuentator(fTtabla = datos,fTvariables = nombresR(datos, "15_B_CELULAR"),fTlevels = F)

# nombresR(datos,"P15_C")
# 
# table(datos$P15_B_CELULAR_OTRO)
# table(datos$P15_B_TABLET_OTRO)
# table(datos$P15_B_COMPU_OTRO)
# 
# table(datos$P15_C_CELULAR_OTRO)
# table(datos$P15_C_TABLET_OTRO)
# table(datos$P15_C_COMPU_OTRO)

nombresR(datos,"P16_C_")



# table(datos$P16_C_COMPU_Otra__Especificar)

nombresR(datos,"P16_J")

nombresR(datos,"P18_A")
table(datos$P18_A_COMPU_Otra__Especificar)

# .-------------------------------------------

exportarAbiertas(xpa = datos,xpb = "P16_CELULAR_OTRO",xpc = "./abiertas/P16_CELULAR_OTRO.csv")
# table(datos$P16_CELULAR_OTRO)
# table(datos$P16_COMPU_OTRO)
# 
# table(datos$P16_J_CELULAR_OTRO)
exportarAbiertas(xpa = datos,xpb = "P16_J_CELULAR_OTRO",xpc = "./abiertas/P16_J_CELULAR_OTRO.csv")

# table(datos$P17_B_CELULAR_OTRO) #Esta tiene catalogo....
exportarAbiertas(xpa = datos,xpb = "P17_B_CELULAR_OTRO",xpc = "./abiertas/P17_B_CELULAR_OTRO.csv")
# table(datos$P17_E_CELULAR_OTRO) #Esta tiene catalogo....
exportarAbiertas(xpa = datos,xpb = "P17_E_CELULAR_OTRO",xpc = "./abiertas/P17_E_CELULAR_OTRO.csv")

# table(datos$P17_F_CELULAR_OTRO) 
exportarAbiertas(xpa = datos,xpb = "P17_F_CELULAR_OTRO",xpc = "./abiertas/P17_F_CELULAR_OTRO.csv")

# table(datos$P18_A_CELULAR_OTRO) #Esta tiene catalogo....
exportarAbiertas(xpa = datos,xpb = "P18_A_CELULAR_OTRO",xpc = "./abiertas/P18_A_CELULAR_OTRO.csv")

# .-------------------------------------------


