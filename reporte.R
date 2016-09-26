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

#Eliminar casos

eliminar<-c(75169079,75267892,75211789,75211784,75179295,75179289,75222476,75223311,75211791,75276387,
            75202149,75231633,75211794,75211786,75173876,75204845,75211656,75211768,75211775,
            75214002,75223811,75227008,75227285,75231628,75269760,75312126,75313838)


datos<-datos[which(!datos$ResponseID%in%eliminar),]



#Creo la variable para el Banner
datos$EdocivilHIjos <-5
datos[(datos$A4=="Casado(a)"| datos$A4=='Unión libre') & datos$A6=='Sí',"EdocivilHIjos"] <-1
datos[(datos$A4=="Casado(a)"| datos$A4=='Unión libre') & datos$A6=='No',"EdocivilHIjos"] <-2 
datos[(datos$A4=="Soltero(a)"|datos$A4=="Divorciado(a)"| datos$A4=="Viudo(a)") & datos$A6_2=='Sí',"EdocivilHIjos"] <-3
datos[(datos$A4=="Soltero(a)"|datos$A4=="Divorciado(a)"| datos$A4=="Viudo(a)") & datos$A6_2=='No',"EdocivilHIjos"] <-4

datos$EdocivilHIjos <- factor(datos$EdocivilHIjos,levels = c(1,2,3,4,5),labels = c("Casado con hijos","Casado sin hijos","Soltero con Hijos","Soltero sin HIjos","No contestó"))




##########################################################################################
# Saco variable "total
datos$Total <- factor(1,levels = 1,labels = "Total")


##########################################################################################

# Reporte



bandera<- c("Total","P15_Celular","P15_Tablet___Ipad","P15_Computadora__Lap_Top",
"P15_1_Celular","P15_1_Tablet___Ipad","P15_1_Computadora__Lap_Top",
"F1Genero","NSE","Edad_Rango","Plaza","EdocivilHIjos")


resultVarBanner <- list(
    genero = frecuentator(fTtabla = datos,fTvariables = 'F1Genero',fTlevels = T,fbanner = bandera),
    nse = frecuentator(fTtabla = datos,fTvariables = 'NSE',fTlevels = T,fbanner = bandera),
    edad = frecuentator(fTtabla = datos,fTvariables = 'Edad_Rango',fTlevels = T,fbanner = bandera),
    plaza = frecuentator(fTtabla = datos,fTvariables = 'Plaza',fTlevels = T,fbanner = bandera),
    edoCivilHijos = frecuentator(fTtabla = datos,fTvariables = 'EdocivilHIjos',fTlevels = T,fbanner = bandera),
    P15_CelularExc = frecuentator(fTtabla = datos,fTvariables = 'P15_Celular',fTlevels = T,fbanner = bandera),
    P15_TabletExc = frecuentator(fTtabla = datos,fTvariables = 'P15_Tablet___Ipad',fTlevels = T,fbanner = bandera),
    P15_ComputadoraExc = frecuentator(fTtabla = datos,fTvariables = 'P15_Computadora__Lap_Top',fTlevels = T,fbanner = bandera),
    P15_CelularComp = frecuentator(fTtabla = datos,fTvariables = 'P15_1_Celular',fTlevels = T,fbanner = bandera),
    P15_TabletComp = frecuentator(fTtabla = datos,fTvariables = 'P15_1_Tablet___Ipad',fTlevels = T,fbanner = bandera),
    P15_LapTopComp = frecuentator(fTtabla = datos,fTvariables = 'P15_1_Computadora__Lap_Top',fTlevels = T,fbanner = bandera)
  )

exportator(resultVarBanner,"resultadosVarBanner.csv")

resultVarBanner <- list(
    P9 = frecuentator(fTtabla = datos,fTvariables = 'P9',fTlevels = T,fbanner = bandera),
    P9_1 = frecuentator(fTtabla = datos,fTvariables = 'P9_1',fTlevels = T,fbanner = bandera),
    P10_CEL = frecuentator(fTtabla = datos,fTvariables = 'P10_Celular',fTlevels = T,fbanner = bandera),
    P10_TABLET = frecuentator(fTtabla = datos,fTvariables = 'P10_Tablet___Ipad',fTlevels = T,fbanner = bandera),
    P10_LAPTOP = frecuentator(fTtabla = datos,fTvariables = 'P10_Computadora___Lap_Top',fTlevels = T,fbanner = bandera),
    P10_1_CEL = frecuentator(fTtabla = datos[datos$P10_Celular==T,],fTvariables = 'P10_1_CELULAR',fTlevels = T,fbanner = bandera),
    P10_1_TABLET = frecuentator(fTtabla = datos[datos$P10_Tablet___Ipad==T,],fTvariables = 'P10_1_TABLET',fTlevels = T,fbanner = bandera),
    P10_1_LAPTOP = frecuentator(fTtabla = datos[datos$P10_Computadora___Lap_Top==T,],fTvariables = 'P10_1_COMPU',fTlevels = T,fbanner = bandera),
    #P10_quienCEL = frecuentator(fTtabla = datos[datos$P10_Celular==T,],fTvariables = c('P10_1_CELULAR_USO1','P10_1_CELULAR_USO2','P10_1_CELULAR_USO3','P10_1_CELULAR_USO4'),fTlevels = T,fbanner = bandera),
    P15a_CEL = frecuentator(fTtabla = datos[datos$P15_1_Celular==T,],fTvariables = 'P15_A_CELULAR',fTlevels = T,fbanner = bandera),
    P15a_TABLET = frecuentator(fTtabla = datos[datos$P15_1_Tablet___Ipad==T,],fTvariables = 'P15_A_TABLET',fTlevels = T,fbanner = bandera),
    P15a_LAPTOP = frecuentator(fTtabla = datos[datos$P15_1_Computadora__Lap_Top==T,],fTvariables = 'P15_A_COMPU',fTlevels = T,fbanner = bandera),
    
    P15b_CEL = frecuentator(fTtabla = datos[datos$P15_1_Celular==T,],fTvariables = c('P15_B_CELULAR_Hablar_por_tel_fono','P15_B_CELULAR_Mandar_mensajes',
                                                                                     'P15_B_CELULAR_Jugar','P15_B_CELULAR_O_r_m_sica',
                                                                                     'P15_B_CELULAR_Buscar__informaci_n','P15_B_CELULAR_Tomar_fotos',
                                                                                     'P15_B_CELULAR_Mandar_mails','P15_B_CELULAR_Hacer_pagos',
                                                                                     'P15_B_CELULAR_Ver__TV','P15_B_CELULAR_Ver_pel_culas',
                                                                                     'P15_B_CELULAR_Ver_videos','P15_B_CELULAR_Comprar_por_internet',
                                                                                     'P15_B_CELULAR_Banca_en_l_nea','P15_B_CELULAR_Entrar_a_redes_sociales',
                                                                                     'P15_B_CELULAR_Para_trabajar','P15_B_CELULAR_Para_leer',
                                                                                     'P15_B_CELULAR_Hacer_tareas___trabajos_____dd','P15_B_CELULAR_Otra__Especificar'),
                            fTlevels = F,fbanner = bandera),
    
    P15b_TABLET = frecuentator(fTtabla = datos[datos$P15_1_Tablet___Ipad==T,],fTvariables = c('P15_B_TABLET_Hablar_por_tel_fono','P15_B_TABLET_Mandar_mensajes',
                                                                                     'P15_B_TABLET_Jugar','P15_B_TABLET_O_r_m_sica',
                                                                                     'P15_B_TABLET_Buscar__informaci_n','P15_B_TABLET_Tomar_fotos',
                                                                                     'P15_B_TABLET_Mandar_mails','P15_B_TABLET_Hacer_pagos',
                                                                                     'P15_B_TABLET_Ver__TV','P15_B_TABLET_Ver_pel_culas',
                                                                                     'P15_B_TABLET_Ver_videos','P15_B_TABLET_Comprar_por_internet',
                                                                                     'P15_B_TABLET_Banca_en_l_nea','P15_B_TABLET_Entrar_a_redes_sociales',
                                                                                     'P15_B_TABLET_Para_trabajar','P15_B_TABLET_Para_leer',
                                                                                     'P15_B_TABLET_Hacer_tareas___trabajos_____dd','P15_B_CELULAR_Otra__Especificar'),
                               fTlevels = F,fbanner = bandera),
    
    P15b_LAPTOP = frecuentator(fTtabla = datos[datos$P15_1_Computadora__Lap_Top==T,],fTvariables = c('P15_B_COMPU_Hablar_por_tel_fono','P15_B_COMPU_Mandar_mensajes',
                                                                                              'P15_B_COMPU_Jugar','P15_B_COMPU_O_r_m_sica',
                                                                                              'P15_B_COMPU_Buscar__informaci_n','P15_B_COMPU_Tomar_fotos',
                                                                                              'P15_B_COMPU_Mandar_mails','P15_B_COMPU_Hacer_pagos',
                                                                                              'P15_B_COMPU_Ver__TV','P15_B_COMPU_Ver_pel_culas',
                                                                                              'P15_B_COMPU_Ver_videos','P15_B_COMPU_Comprar_por_internet',
                                                                                              'P15_B_COMPU_Banca_en_l_nea','P15_B_COMPU_Entrar_a_redes_sociales',
                                                                                              'P15_B_COMPU_Para_trabajar','P15_B_COMPU_Para_leer',
                                                                                              'P15_B_COMPU_Hacer_tareas___trabajos_____dd','P15_B_COMPU_Otra__Especificar'),
                               fTlevels = F,fbanner = bandera),
    
    P15c_CEL = frecuentator(fTtabla = datos[datos$P15_1_Celular==T,],fTvariables = c('P15_C_CELULAR_Por_las_tareas___trabajos',
                                                                                     'P15_C_CELULAR_Por_los_juegos','P15_C_CELULAR_Por_que_solo_tenemos_uno_en_ca',
                                                                                     'P15_C_CELULAR_Por_que_son_caros_para_tener_u','P15_C_CELULAR_Porque_no_tenemos_tel_fono_fij',
                                                                                     'P15_C_CELULAR_Es_donde_revisamos_las_redes_s','P15_C_CELULAR_No_es_necesario_tener_varios',
                                                                                     'P15_C_CELULAR_Otro__Especificar'),
                            fTlevels = F,fbanner = bandera),
    
    P15c_TABLET = frecuentator(fTtabla = datos[datos$P15_1_Tablet___Ipad==T,],fTvariables = c('P15_C_TABLET_Por_las_tareas___trabajos',
                                                                                     'P15_C_TABLET_Por_los_juegos','P15_C_TABLET_Por_que_solo_tenemos_uno_en_ca',
                                                                                     'P15_C_TABLET_Por_que_son_caros_para_tener_u','P15_C_TABLET_Porque_no_tenemos_tel_fono_fij',
                                                                                     'P15_C_TABLET_Es_donde_revisamos_las_redes_s','P15_C_TABLET_No_es_necesario_tener_varios',
                                                                                     'P15_C_TABLET_Otro__Especificar'),
                               fTlevels = F,fbanner = bandera),
    
    P15c_LAPTOP = frecuentator(fTtabla = datos[datos$P15_1_Computadora__Lap_Top==T,],fTvariables = c('P15_C_COMPU_Por_las_tareas___trabajos',
                                                                                              'P15_C_COMPU_Por_los_juegos','P15_C_COMPU_Por_que_solo_tenemos_uno_en_ca',
                                                                                              'P15_C_COMPU_Por_que_son_caros_para_tener_u','P15_C_COMPU_Porque_no_tenemos_tel_fono_fij',
                                                                                              'P15_C_COMPU_Es_donde_revisamos_las_redes_s','P15_C_COMPU_No_es_necesario_tener_varios',
                                                                                              'P15_C_COMPU_Otro__Especificar'),
                               fTlevels = F,fbanner = bandera),
    
    P15d_CEL = frecuentator(fTtabla = datos[datos$P15_1_Celular==T,],fTvariables = 'P15_D_CELULAR',fTlevels = T,fbanner = bandera),
    P15d_TABLET = frecuentator(fTtabla = datos[datos$P15_1_Tablet___Ipad==T,],fTvariables = 'P15_D_TABLET',fTlevels = T,fbanner = bandera),
    P15d_LAPTOP = frecuentator(fTtabla = datos[datos$P15_1_Computadora__Lap_Top==T,],fTvariables = 'P15_D_COMPU',fTlevels = T,fbanner = bandera),
    P15e_CEL = frecuentator(fTtabla = datos[datos$P15_1_Celular==T,],fTvariables = 'P15_E_CELULAR',fTlevels = T,fbanner = bandera),
    P15e_TABLET = frecuentator(fTtabla = datos[datos$P15_1_Tablet___Ipad==T,],fTvariables = 'P15_E_TABLET',fTlevels = T,fbanner = bandera),
    P15e_LAPTOP = frecuentator(fTtabla = datos[datos$P15_1_Computadora__Lap_Top==T,],fTvariables = 'P15_E_COMPU',fTlevels = T,fbanner = bandera),
    
    P16_CEL = frecuentator(fTtabla = datos[datos$P15_Celular==T,],fTvariables = c('P16_CELULAR_Es_del_trabajo',
                                                                                  'P16_CELULAR_Tengo_mis_secretos',
                                                                                  'P16_CELULAR_Tengo_mis_fotos',
                                                                                  'P16_CELULAR_Tengo_mis_mails',
                                                                                  'P16_CELULAR_Nadie_de_mi_casa_sabe_usarlo',
                                                                                  'P16_CELULAR_Es_personal',
                                                                                  'P16_CELULAR_Otro__Especificar'),
                           fTlevels = F,fbanner = bandera),
    
    P16_TABLET = frecuentator(fTtabla = datos[datos$P15_Tablet___Ipad==T,],fTvariables = c('P16_TABLETA_Es_del_trabajo',
                                                                                  'P16_TABLETA_Tengo_mis_secretos',
                                                                                  'P16_TABLETA_Tengo_mis_fotos',
                                                                                  'P16_TABLETA_Tengo_mis_mails',
                                                                                  'P16_TABLETA_Nadie_de_mi_casa_sabe_usarlo',
                                                                                  'P16_TABLETA_Es_personal',
                                                                                  'P16_TABLETA_Otro__Especificar'),
                           fTlevels = F,fbanner = bandera),
    
    P16_LAPTOP = frecuentator(fTtabla = datos[datos$P15_Computadora__Lap_Top==T,],fTvariables = c('P16_COMPU_Es_del_trabajo',
                                                                                           'P16_COMPU_Tengo_mis_secretos',
                                                                                           'P16_COMPU_Tengo_mis_fotos',
                                                                                           'P16_COMPU_Tengo_mis_mails',
                                                                                           'P16_COMPU_Nadie_de_mi_casa_sabe_usarlo',
                                                                                           'P16_COMPU_Es_personal',
                                                                                           'P16_COMPU_Otro__Especificar'),
                              fTlevels = F,fbanner = bandera),
    
    
    P16a_CEL = frecuentator(fTtabla = datos[datos$P15_Celular==T,],fTvariables = 'P16_A_CELULAR',fTlevels = T,fbanner = bandera),
    P16a_TABLET = frecuentator(fTtabla = datos[datos$P15_Tablet___Ipad==T,],fTvariables = 'P16_A_TABLET',fTlevels = T,fbanner = bandera),
    P16a_LAPTOP = frecuentator(fTtabla = datos[datos$P15_Computadora__Lap_Top==T,],fTvariables = 'P16_A_COMPU',fTlevels = T,fbanner = bandera),
    P16b_CEL = frecuentator(fTtabla = datos[datos$P15_Celular==T,],fTvariables = 'P16_B_CELULAR',fTlevels = T,fbanner = bandera),
    P16b_TABLET = frecuentator(fTtabla = datos[datos$P15_Tablet___Ipad==T,],fTvariables = 'P16_B_TABLET',fTlevels = T,fbanner = bandera),
    P16b_LAPTOP = frecuentator(fTtabla = datos[datos$P15_Computadora__Lap_Top==T,],fTvariables = 'P16_B_COMPU',fTlevels = T,fbanner = bandera),
    
    P16c_CEL = frecuentator(fTtabla = datos[datos$P15_Celular==T,],fTvariables = 'P16_B_COMPU',fTlevels = T,fbanner = bandera),
    
    P16g_CEL = frecuentator(fTtabla = datos[datos$P15_Celular==T,],fTvariables = 'P16_G_CELULAR',fTlevels = T,fbanner = bandera),
    P16g_TABLET = frecuentator(fTtabla = datos[datos$P15_Tablet___Ipad==T,],fTvariables = 'P16_G_TABLET',fTlevels = T,fbanner = bandera),
    P16g_COMPU = frecuentator(fTtabla = datos[datos$P15_Computadora__Lap_Top==T,],fTvariables = 'P16_G_COMPU',fTlevels = T,fbanner = bandera),
    P16h_CEL = frecuentator(fTtabla = datos[datos$P15_Celular==T,],fTvariables = 'P16_H_CELULAR',fTlevels = T,fbanner = bandera),
    P16h_TABLET = frecuentator(fTtabla = datos[datos$P15_Tablet___Ipad==T,],fTvariables = 'P16_H_TABLET',fTlevels = T,fbanner = bandera),
    P16h_COMPU = frecuentator(fTtabla = datos[datos$P15_Computadora__Lap_Top==T,],fTvariables = 'P16_H_COMPU',fTlevels = T,fbanner = bandera),
    P16i = frecuentator(fTtabla = datos,fTvariables = 'P16_I',fTlevels = T,fbanner = bandera),
    
    
    )

maxdiff<-datos%>%select(contains('CELULARES'))
unique(maxdiff$CELULARES_BLOQUE1_MAS)
summary(maxdiff[,1])
nrow(maxdiff[which(!is.na(maxdiff$CELULARES_BLOQUE1_MAS)),])
###PARA MAXDIFF QUITAR LOS NA'S
