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


##########################################################################################

# Reporte


