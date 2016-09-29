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

#######Eliminamos los levels duplicados de las preguntas P17e_celular y P17e_tablet

datos$P17_E_CELULAR_REC <- datos$P17_E_CELULAR
datos$P17_E_CELULAR_REC <- droplevels(datos$P17_E_CELULAR_REC)

datos$P17_E_TABLET_REC <- datos$P17_E_TABLET
datos$P17_E_TABLET_REC <- droplevels(datos$P17_E_TABLET_REC)



#######Agrupacion de preguntas
##Uso de transporte
misP <- nombresR(datos,"P10_1")
misP <- misP[-c(1,6,11)]

misP <- matrix(data = misP,nrow = 6,ncol = 5)

listaNombres<-list()
for(i in 1:nrow(misP)){
    # i <- 1
    yoSoy <- misP[i,1]
    yoSoy <- paste("Rr_", yoSoy,sep="")
    listaNombres[[i]]<-paste(misP[i,])
    names(listaNombres)[[i]]<-yoSoy  
}

for(i in 1:length(listaNombres)){
    # i<-1
    mmm<- names(listaNombres)[i]
    mhijitos<- listaNombres[[i]]
    datos[,mmm]<-FALSE
    for(t in 1:length(mhijitos)){
        # t<-2
        logico<- datos[,mhijitos[t]]==1
        logico[is.na(logico)]<-FALSE
        datos[logico,mmm]<-TRUE
    }
}





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
    P15_CELExc = frecuentator(fTtabla = datos,fTvariables = 'P15_Celular',fTlevels = T,fbanner = bandera),
    P15_TABLETExc = frecuentator(fTtabla = datos,fTvariables = 'P15_Tablet___Ipad',fTlevels = T,fbanner = bandera),
    P15_COMPUExc = frecuentator(fTtabla = datos,fTvariables = 'P15_Computadora__Lap_Top',fTlevels = T,fbanner = bandera),
    P15_CELComp = frecuentator(fTtabla = datos,fTvariables = 'P15_1_Celular',fTlevels = T,fbanner = bandera),
    P15_TABLETComp = frecuentator(fTtabla = datos,fTvariables = 'P15_1_Tablet___Ipad',fTlevels = T,fbanner = bandera),
    P15_COMPUComp = frecuentator(fTtabla = datos,fTvariables = 'P15_1_Computadora__Lap_Top',fTlevels = T,fbanner = bandera)
  )

exportator(resultVarBanner,"resultadosVarBanner.csv")

resultBanner1 <- list(
    P9 = frecuentator(fTtabla = datos,fTvariables = 'P9',fTlevels = T,fbanner = bandera),
    P9_1 = frecuentator(fTtabla = datos,fTvariables = 'P9_1',fTlevels = T,fbanner = bandera),
    P10_CEL = frecuentator(fTtabla = datos,fTvariables = 'P10_Celular',fTlevels = T,fbanner = bandera),
    P10_TABLET = frecuentator(fTtabla = datos,fTvariables = 'P10_Tablet___Ipad',fTlevels = T,fbanner = bandera),
    P10_COMPU = frecuentator(fTtabla = datos,fTvariables = 'P10_Computadora___Lap_Top',fTlevels = T,fbanner = bandera),
    P10_1_CEL = frecuentator(fTtabla = datos[datos$P10_Celular==T,],fTvariables = 'P10_1_CELULAR',fTlevels = T,fbanner = bandera),
    P10_1_TABLET = frecuentator(fTtabla = datos[datos$P10_Tablet___Ipad==T,],fTvariables = 'P10_1_TABLET',fTlevels = T,fbanner = bandera),
    P10_1_COMPU = frecuentator(fTtabla = datos[datos$P10_Computadora___Lap_Top==T,],fTvariables = 'P10_1_COMPU',fTlevels = T,fbanner = bandera),
    
    P10_quienCEL = frecuentator(fTtabla = datos[datos$P10_Celular==T,],fTvariables = c('P10_1_CELULAR_USO1',
                                                                                       'P10_1_CELULAR_USO2',
                                                                                       'P10_1_CELULAR_USO3',
                                                                                       'P10_1_CELULAR_USO4'),
                                fTlevels = T,fbanner = bandera),
    
    P15a_CEL = frecuentator(fTtabla = datos[datos$P15_1_Celular==T,],fTvariables = 'P15_A_CELULAR',fTlevels = T,fbanner = bandera),
    P15a_TABLET = frecuentator(fTtabla = datos[datos$P15_1_Tablet___Ipad==T,],fTvariables = 'P15_A_TABLET',fTlevels = T,fbanner = bandera),
    P15a_COMPU = frecuentator(fTtabla = datos[datos$P15_1_Computadora__Lap_Top==T,],fTvariables = 'P15_A_COMPU',fTlevels = T,fbanner = bandera),
    
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
    
    P15b_COMPU = frecuentator(fTtabla = datos[datos$P15_1_Computadora__Lap_Top==T,],fTvariables = c('P15_B_COMPU_Hablar_por_tel_fono','P15_B_COMPU_Mandar_mensajes',
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
    
    P15c_COMPU = frecuentator(fTtabla = datos[datos$P15_1_Computadora__Lap_Top==T,],fTvariables = c('P15_C_COMPU_Por_las_tareas___trabajos',
                                                                                              'P15_C_COMPU_Por_los_juegos','P15_C_COMPU_Por_que_solo_tenemos_uno_en_ca',
                                                                                              'P15_C_COMPU_Por_que_son_caros_para_tener_u','P15_C_COMPU_Porque_no_tenemos_tel_fono_fij',
                                                                                              'P15_C_COMPU_Es_donde_revisamos_las_redes_s','P15_C_COMPU_No_es_necesario_tener_varios',
                                                                                              'P15_C_COMPU_Otro__Especificar'),
                               fTlevels = F,fbanner = bandera),
    
    P15d_CEL = frecuentator(fTtabla = datos[datos$P15_1_Celular==T,],fTvariables = 'P15_D_CELULAR',fTlevels = T,fbanner = bandera),
    P15d_TABLET = frecuentator(fTtabla = datos[datos$P15_1_Tablet___Ipad==T,],fTvariables = 'P15_D_TABLET',fTlevels = T,fbanner = bandera),
    P15d_COMPU = frecuentator(fTtabla = datos[datos$P15_1_Computadora__Lap_Top==T,],fTvariables = 'P15_D_COMPU',fTlevels = T,fbanner = bandera),
    P15e_CEL = frecuentator(fTtabla = datos[datos$P15_1_Celular==T,],fTvariables = 'P15_E_CELULAR',fTlevels = T,fbanner = bandera),
    P15e_TABLET = frecuentator(fTtabla = datos[datos$P15_1_Tablet___Ipad==T,],fTvariables = 'P15_E_TABLET',fTlevels = T,fbanner = bandera),
    P15e_COMPU = frecuentator(fTtabla = datos[datos$P15_1_Computadora__Lap_Top==T,],fTvariables = 'P15_E_COMPU',fTlevels = T,fbanner = bandera),
    
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
    
    P16_COMPU = frecuentator(fTtabla = datos[datos$P15_Computadora__Lap_Top==T,],fTvariables = c('P16_COMPU_Es_del_trabajo',
                                                                                           'P16_COMPU_Tengo_mis_secretos',
                                                                                           'P16_COMPU_Tengo_mis_fotos',
                                                                                           'P16_COMPU_Tengo_mis_mails',
                                                                                           'P16_COMPU_Nadie_de_mi_casa_sabe_usarlo',
                                                                                           'P16_COMPU_Es_personal',
                                                                                           'P16_COMPU_Otro__Especificar'),
                              fTlevels = F,fbanner = bandera),
    
    
    P16a_CEL = frecuentator(fTtabla = datos[datos$P15_Celular==T,],fTvariables = 'P16_A_CELULAR',fTlevels = T,fbanner = bandera),
    P16a_TABLET = frecuentator(fTtabla = datos[datos$P15_Tablet___Ipad==T,],fTvariables = 'P16_A_TABLET',fTlevels = T,fbanner = bandera),
    P16a_COMPU = frecuentator(fTtabla = datos[datos$P15_Computadora__Lap_Top==T,],fTvariables = 'P16_A_COMPU',fTlevels = T,fbanner = bandera),
    P16b_CEL = frecuentator(fTtabla = datos[datos$P15_Celular==T,],fTvariables = 'P16_B_CELULAR',fTlevels = T,fbanner = bandera),
    P16b_TABLET = frecuentator(fTtabla = datos[datos$P15_Tablet___Ipad==T,],fTvariables = 'P16_B_TABLET',fTlevels = T,fbanner = bandera),
    P16b_COMPU = frecuentator(fTtabla = datos[datos$P15_Computadora__Lap_Top==T,],fTvariables = 'P16_B_COMPU',fTlevels = T,fbanner = bandera),
    
    P16c_CEL = frecuentator(fTtabla = datos[datos$P15_Celular==T,],fTvariables = c('P16_C_CELULAR_Hablar_por_tel_fono',
                                                                                   'P16_C_CELULAR_Mandar_mensajes',
                                                                                   'P16_C_CELULAR_Jugar',
                                                                                   'P16_C_CELULAR_O_r_m_sica',
                                                                                   'P16_C_CELULAR_Buscar__informaci_n',
                                                                                   'P16_C_CELULAR_Tomar_fotos',
                                                                                   'P16_C_CELULAR_Mandar_mails',
                                                                                   'P16_C_CELULAR_Hacer_pagos',
                                                                                   'P16_C_CELULAR_Ver__TV',
                                                                                   'P16_C_CELULAR_Ver_pel_culas',
                                                                                   'P16_C_CELULAR_Ver_videos',
                                                                                   'P16_C_CELULAR_Comprar_por_internet',
                                                                                   'P16_C_CELULAR_Banca_en_l_nea',
                                                                                   'P16_C_CELULAR_Entrar_a_redes_sociales',
                                                                                   'P16_C_CELULAR_Para_trabajar',
                                                                                   'P16_C_CELULAR_Para_leer',
                                                                                   'P16_C_CELULAR_Hacer_tareas___trabajos_____dd',
                                                                                   'P16_C_CELULAR_Otra__Especificar'),
                            fTlevels = F,fbanner = bandera),
    
    P16c_TABLET = frecuentator(fTtabla = datos[datos$P15_Tablet___Ipad==T,],fTvariables = c('P16_C_TABLET_Hablar_por_tel_fono',
                                                                                   'P16_C_TABLET_Mandar_mensajes',
                                                                                   'P16_C_TABLET_Jugar',
                                                                                   'P16_C_TABLET_O_r_m_sica',
                                                                                   'P16_C_TABLET_Buscar__informaci_n',
                                                                                   'P16_C_TABLET_Tomar_fotos',
                                                                                   'P16_C_TABLET_Mandar_mails',
                                                                                   'P16_C_TABLET_Hacer_pagos',
                                                                                   'P16_C_TABLET_Ver__TV',
                                                                                   'P16_C_TABLET_Ver_pel_culas',
                                                                                   'P16_C_TABLET_Ver_videos',
                                                                                   'P16_C_TABLET_Comprar_por_internet',
                                                                                   'P16_C_TABLET_Banca_en_l_nea',
                                                                                   'P16_C_TABLET_Entrar_a_redes_sociales',
                                                                                   'P16_C_TABLET_Para_trabajar',
                                                                                   'P16_C_TABLET_Para_leer',
                                                                                   'P16_C_TABLET_Hacer_tareas___trabajos_____dd',
                                                                                   'P16_C_TABLET_Otra__Especificar'),
                            fTlevels = F,fbanner = bandera),
    
    P16c_COMPU = frecuentator(fTtabla = datos[datos$P15_Computadora__Lap_Top==T,],fTvariables = c('P16_C_COMPU_Hablar_por_tel_fono',
                                                                                            'P16_C_COMPU_Mandar_mensajes',
                                                                                            'P16_C_COMPU_Jugar',
                                                                                            'P16_C_COMPU_O_r_m_sica',
                                                                                            'P16_C_COMPU_Buscar__informaci_n',
                                                                                            'P16_C_COMPU_Tomar_fotos',
                                                                                            'P16_C_COMPU_Mandar_mails',
                                                                                            'P16_C_COMPU_Hacer_pagos',
                                                                                            'P16_C_COMPU_Ver__TV',
                                                                                            'P16_C_COMPU_Ver_pel_culas',
                                                                                            'P16_C_COMPU_Ver_videos',
                                                                                            'P16_C_COMPU_Comprar_por_internet',
                                                                                            'P16_C_COMPU_Banca_en_l_nea',
                                                                                            'P16_C_COMPU_Entrar_a_redes_sociales',
                                                                                            'P16_C_COMPU_Para_trabajar',
                                                                                            'P16_C_COMPU_Para_leer',
                                                                                            'P16_C_COMPU_Hacer_tareas___trabajos_____dd',
                                                                                            'P16_C_COMPU_Otra__Especificar'),
                               fTlevels = F,fbanner = bandera),
    
    
    P16g_CEL = frecuentator(fTtabla = datos[datos$P15_Celular==T,],fTvariables = 'P16_G_CELULAR',fTlevels = T,fbanner = bandera),
    P16g_TABLET = frecuentator(fTtabla = datos[datos$P15_Tablet___Ipad==T,],fTvariables = 'P16_G_TABLET',fTlevels = T,fbanner = bandera),
    P16g_COMPU = frecuentator(fTtabla = datos[datos$P15_Computadora__Lap_Top==T,],fTvariables = 'P16_G_COMPU',fTlevels = T,fbanner = bandera),
    P16h_CEL = frecuentator(fTtabla = datos[datos$P15_Celular==T,],fTvariables = 'P16_H_CELULAR',fTlevels = T,fbanner = bandera),
    P16h_TABLET = frecuentator(fTtabla = datos[datos$P15_Tablet___Ipad==T,],fTvariables = 'P16_H_TABLET',fTlevels = T,fbanner = bandera),
    P16h_COMPU = frecuentator(fTtabla = datos[datos$P15_Computadora__Lap_Top==T,],fTvariables = 'P16_H_COMPU',fTlevels = T,fbanner = bandera),
    P16i = frecuentator(fTtabla = datos,fTvariables = 'P16_I',fTlevels = T,fbanner = bandera),
    
    
    P16j_CEL = frecuentator(fTtabla = datos[datos$P15_Celular==T & datos$P16_I=="Sí" ,],fTvariables = c('P16_J_CELULAR_Hablar_por_tel_fono',
                                                                                   'P16_J_CELULAR_Mandar_mensajes',
                                                                                   'P16_J_CELULAR_Jugar',
                                                                                   'P16_J_CELULAR_O_r_m_sica',
                                                                                   'P16_J_CELULAR_Buscar__informaci_n',
                                                                                   'P16_J_CELULAR_Tomar_fotos',
                                                                                   'P16_J_CELULAR_Mandar_mails',
                                                                                   'P16_J_CELULAR_Ver__TV',
                                                                                   'P16_J_CELULAR_Ver_pel_culas',
                                                                                   'P16_J_CELULAR_Ver_videos',
                                                                                   'P16_J_CELULAR_Entrar_a_redes_sociales',
                                                                                   'P16_J_CELULAR_Para_trabajar',
                                                                                   'P16_J_CELULAR_Para_leer',
                                                                                   'P16_J_CELULAR_Hacer_tareas___trabajos_____dd',
                                                                                   'P16_J_CELULAR_Otra__Especificar'),
                            fTlevels = F,fbanner = bandera),
    
    P16j_TABLET = frecuentator(fTtabla = datos[datos$P15_Tablet___Ipad==T & datos$P16_I=="Sí" ,],fTvariables = c('P16_J_TABLET_Hablar_por_tel_fono',
                                                                                                        'P16_J_TABLET_Mandar_mensajes',
                                                                                                        'P16_J_TABLET_Jugar',
                                                                                                        'P16_J_TABLET_O_r_m_sica',
                                                                                                        'P16_J_TABLET_Buscar__informaci_n',
                                                                                                        'P16_J_TABLET_Tomar_fotos',
                                                                                                        'P16_J_TABLET_Mandar_mails',
                                                                                                        'P16_J_TABLET_Ver__TV',
                                                                                                        'P16_J_TABLET_Ver_pel_culas',
                                                                                                        'P16_J_TABLET_Ver_videos',
                                                                                                        'P16_J_TABLET_Entrar_a_redes_sociales',
                                                                                                        'P16_J_TABLET_Para_trabajar',
                                                                                                        'P16_J_TABLET_Para_leer',
                                                                                                        'P16_J_TABLET_Hacer_tareas___trabajos_____dd',
                                                                                                        'P16_J_TABLET_Otra__Especificar'),
                            fTlevels = F,fbanner = bandera),
    
    P16j_COMPU = frecuentator(fTtabla = datos[datos$P15_Computadora__Lap_Top==T & datos$P16_I=="Sí" ,],fTvariables = c('P16_J_COMPU_Hablar_por_tel_fono',
                                                                                                                 'P16_J_COMPU_Mandar_mensajes',
                                                                                                                 'P16_J_COMPU_Jugar',
                                                                                                                 'P16_J_COMPU_O_r_m_sica',
                                                                                                                 'P16_J_COMPU_Buscar__informaci_n',
                                                                                                                 'P16_J_COMPU_Tomar_fotos',
                                                                                                                 'P16_J_COMPU_Mandar_mails',
                                                                                                                 'P16_J_COMPU_Ver__TV',
                                                                                                                 'P16_J_COMPU_Ver_pel_culas',
                                                                                                                 'P16_J_COMPU_Ver_videos',
                                                                                                                 'P16_J_COMPU_Entrar_a_redes_sociales',
                                                                                                                 'P16_J_COMPU_Para_trabajar',
                                                                                                                 'P16_J_COMPU_Para_leer',
                                                                                                                 'P16_J_COMPU_Hacer_tareas___trabajos_____dd',
                                                                                                                 'P16_J_COMPU_Otra__Especificar'),
                               fTlevels = F,fbanner = bandera),
    
    P17_CEL = frecuentator(fTtabla = datos[datos$P15_Celular==T,],fTvariables = 'P17_CELULAR',fTlevels = T,fbanner = bandera),
    
    P17_TABLET = frecuentator(fTtabla = datos[datos$P15_Tablet___Ipad==T,],fTvariables = 'P17_TABLET',fTlevels = T,fbanner = bandera),
    
    P17_COMPU = frecuentator(fTtabla = datos[datos$P15_Computadora__Lap_Top==T,],fTvariables = 'P17_COMPU',fTlevels = T,fbanner = bandera),
    
    
    P17a_CEL = frecuentator(fTtabla = datos[datos$P17_CELULAR=='Yo lo compré',],fTvariables = 'P17_A_CELULAR',fTlevels = T,fbanner = bandera),
    
    P17a_TABLET = frecuentator(fTtabla = datos[datos$P17_TABLET=='Yo lo compré',],fTvariables = 'P17_A_TABLET',fTlevels = T,fbanner = bandera),
    
    P17a_COMPU = frecuentator(fTtabla = datos[datos$P17_COMPU=='Yo lo compré',],fTvariables = 'P17_A_COMPU',fTlevels = T,fbanner = bandera),
    
    P17b_CEL = frecuentator(fTtabla = datos[datos$P17_A_CELULAR=='Nuevo',],fTvariables = 'P17_B_CELULAR',fTlevels = T,fbanner = bandera),
    
    P17b_TABLET = frecuentator(fTtabla = datos[datos$P17_A_TABLET=='Nuevo',],fTvariables = 'P17_B_TABLET',fTlevels = T,fbanner = bandera),
    
    P17b_COMPU = frecuentator(fTtabla = datos[datos$P17_A_COMPU=='Nuevo',],fTvariables = 'P17_B_COMPU',fTlevels = T,fbanner = bandera),
    
    P17c_CEL = frecuentator(fTtabla = datos[datos$P17_A_CELULAR=='Usado',],fTvariables = 'P17_C_CELULAR',fTlevels = T,fbanner = bandera),
    
    P17c_TABLET = frecuentator(fTtabla = datos[datos$P17_A_TABLET=='Usado',],fTvariables = 'P17_C_TABLET',fTlevels = T,fbanner = bandera),
    
    P17c_COMPU = frecuentator(fTtabla = datos[datos$P17_A_COMPU=='Usado',],fTvariables = 'P17_C_COMPU',fTlevels = T,fbanner = bandera),
    
    P17d_CEL = frecuentator(fTtabla = datos[datos$P17_CELULAR=='Yo lo compré',],fTvariables = 'P17_D_CELULAR',fTlevels = T,fbanner = bandera),
    
    P17d_TABLET = frecuentator(fTtabla = datos[datos$P17_TABLET=='Yo lo compré',],fTvariables = 'P17_D_TABLET',fTlevels = T,fbanner = bandera),
    
    P17d_COMPU = frecuentator(fTtabla = datos[datos$P17_COMPU=='Yo lo compré',],fTvariables = 'P17_D_COMPU',fTlevels = T,fbanner = bandera),
    
    P17e_CEL = frecuentator(fTtabla = datos[datos$P15_Celular==T,],fTvariables = 'P17_E_CELULAR_REC',fTlevels = T,fbanner = bandera),
    
    P17e_TABLET = frecuentator(fTtabla = datos[datos$P15_Tablet___Ipad==T,],fTvariables = 'P17_E_TABLET_REC',fTlevels = T,fbanner = bandera),
    
    P17e_COMPU = frecuentator(fTtabla = datos[datos$P15_Computadora__Lap_Top==T,],fTvariables = 'P17_E_COMPU',fTlevels = T,fbanner = bandera),
    
    P17f_CEL = frecuentator(fTtabla = datos[datos$P15_Celular==T,],fTvariables = c('P17_F_CELULAR_Es_resistente',
                                                                                   'P17_F_CELULAR_Barata',
                                                                                   'P17_F_CELULAR_Tiene_buena_resoluci_n_la_pant',
                                                                                   'P17_F_CELULAR_Toma_buenas_fotos',
                                                                                   'P17_F_CELULAR_Capacidad_de_almacenamiento',
                                                                                   'P17_F_CELULAR_Dise_o___color',
                                                                                   'P17_F_CELULAR_Tama_o_c_modo',
                                                                                   'P17_F_CELULAR_Otra__especificar'),
                            fTlevels = F,fbanner = bandera),
    
    P17f_TABLET = frecuentator(fTtabla = datos[datos$P15_Tablet___Ipad==T,],fTvariables = c('P17_F_TABLET_Es_resistente',
                                                                                   'P17_F_TABLET_Barata',
                                                                                   'P17_F_TABLET_Tiene_buena_resoluci_n_la_pant',
                                                                                   'P17_F_TABLET_Toma_buenas_fotos',
                                                                                   'P17_F_TABLET_Capacidad_de_almacenamiento',
                                                                                   'P17_F_TABLET_Dise_o___color',
                                                                                   'P17_F_TABLET_Tama_o_c_modo',
                                                                                   'P17_F_TABLET_Otra__especificar'),
                            fTlevels = F,fbanner = bandera),
    
    P17f_COMPU = frecuentator(fTtabla = datos[datos$P15_Computadora__Lap_Top==T,],fTvariables = c('P17_F_COMPU_Es_resistente',
                                                                                            'P17_F_COMPU_Barata',
                                                                                            'P17_F_COMPU_Tiene_buena_resoluci_n_la_pant',
                                                                                            'P17_F_COMPU_Capacidad_de_almacenamiento',
                                                                                            'P17_F_COMPU_Dise_o___color',
                                                                                            'P17_F_COMPU_Tama_o_c_modo',
                                                                                            'P17_F_COMPU_Otra__especificar'),
                               fTlevels = F,fbanner = bandera),
    
    P18_CEL = frecuentator(fTtabla = datos[datos$P10_Celular==T,],fTvariables = c('P18_CELULAR_Por_moda',
                                                                                  'P18_CELULAR_Ya_no_sirve',
                                                                                  'P18_CELULAR_Por_deterioro__Es_muy_viejo__a',
                                                                                  'P18_CELULAR_La_pantalla_no_se_ve_bien_est_',
                                                                                  'P18_CELULAR_No_tiene_tanta_capacidad_de_al',
                                                                                  'P18_CELULAR_Quiero_un_modelo_m_s_reciente_',
                                                                                  'P18_CELULAR_Otra__Especificar',
                                                                                  'P18_CELULAR_No_sabe___no_contest'),
                           fTlevels = F,fbanner = bandera),
    
    P18_TABLET = frecuentator(fTtabla = datos[datos$P10_Tablet___Ipad==T,],fTvariables = c('P18_TABLET_Por_moda',
                                                                                  'P18_TABLET_Ya_no_sirve',
                                                                                  'P18_TABLET_Por_deterioro__Es_muy_viejo__a',
                                                                                  'P18_TABLET_La_pantalla_no_se_ve_bien_est_',
                                                                                  'P18_TABLET_No_tiene_tanta_capacidad_de_al',
                                                                                  'P18_TABLET_Quiero_un_modelo_m_s_reciente_',
                                                                                  'P18_TABLET_Otra__Especificar',
                                                                                  'P18_TABLET_No_sabe___no_contest'),
                           fTlevels = F,fbanner = bandera),
    
    
    P18_COMPU = frecuentator(fTtabla = datos[datos$P10_Computadora___Lap_Top==T,],fTvariables = c('P18_COMPU_Por_moda',
                                                                                           'P18_COMPU_Ya_no_sirve',
                                                                                           'P18_COMPU_Por_deterioro__Es_muy_viejo__a',
                                                                                           'P18_COMPU_La_pantalla_no_se_ve_bien_est_',
                                                                                           'P18_COMPU_No_tiene_tanta_capacidad_de_al',
                                                                                           'P18_COMPU_Quiero_un_modelo_m_s_reciente_',
                                                                                           'P18_COMPU_Otra__Especificar',
                                                                                           'P18_COMPU_No_sabe___no_contest'),
                              fTlevels = F,fbanner = bandera),
    
    P18a_CEL = frecuentator(fTtabla = datos[datos$P10_Celular==T,],fTvariables = c('P18_A_CELULAR_El_precio',
                                                                                  'P18_A_CELULAR_Suficiente_capacidad_de_almace',
                                                                                  'P18_A_CELULAR_Que_tome_buenas_fotos',
                                                                                  'P18_A_CELULAR_Resoluci_n_de_la_pantalla',
                                                                                  'P18_A_CELULAR_Que_sea_c_modo_para_cargar___n',
                                                                                  'P18_A_CELULAR_En_el_color___dise_o',
                                                                                  'P18_A_CELULAR_Que_sea_f_cil_de_usar',
                                                                                  'P18_A_CELULAR_Otra__Especificar',
                                                                                  'P18_A_CELULAR_No_sabe___no_contest'),
                           fTlevels = F,fbanner = bandera),
    
    
    P18a_TABLET = frecuentator(fTtabla = datos[datos$P10_Tablet___Ipad==T,],fTvariables = c('P18_A_TABLET_El_precio',
                                                                                   'P18_A_TABLET_Suficiente_capacidad_de_almace',
                                                                                   'P18_A_TABLET_Que_tome_buenas_fotos',
                                                                                   'P18_A_TABLET_Resoluci_n_de_la_pantalla',
                                                                                   'P18_A_TABLET_Que_sea_c_modo_para_cargar___n',
                                                                                   'P18_A_TABLET_En_el_color___dise_o',
                                                                                   'P18_A_TABLET_Que_sea_f_cil_de_usar',
                                                                                   'P18_A_TABLET_Otra__Especificar',
                                                                                   'P18_A_TABLET_No_sabe___no_contest'),
                            fTlevels = F,fbanner = bandera),
    
    
    P18a_COMPU = frecuentator(fTtabla = datos[datos$P10_Computadora___Lap_Top==T,],fTvariables = c('P18_A_COMPU_El_precio',
                                                                                            'P18_A_COMPU_Suficiente_capacidad_de_almace',
                                                                                            'P18_A_COMPU_Que_tome_buenas_fotos',
                                                                                            'P18_A_COMPU_Resoluci_n_de_la_pantalla',
                                                                                            'P18_A_COMPU_Que_sea_c_modo_para_cargar___n',
                                                                                            'P18_A_COMPU_En_el_color___dise_o',
                                                                                            'P18_A_COMPU_Que_sea_f_cil_de_usar',
                                                                                            'P18_A_COMPU_Otra__Especificar',
                                                                                            'P18_A_COMPU_No_sabe___no_contest'),
                               fTlevels = F,fbanner = bandera),
    
    P18b_CEL = frecuentator(fTtabla = datos[datos$P10_Celular==T,],fTvariables = 'P18_B_CELULAR',fTlevels = T,fbanner = bandera),
    
    P18b2_CEL = frecuentator(fTtabla = datos[datos$P10_Celular==T,],fTvariables = 'P18_B_2_CELULAR',fTlevels = T,fbanner = bandera),
    
    P18b_TABLET = frecuentator(fTtabla = datos[datos$P10_Tablet___Ipad==T,],fTvariables = 'P18_B_TABLET',fTlevels = T,fbanner = bandera),
    
    P18b2_TABLET = frecuentator(fTtabla = datos[datos$P10_Tablet___Ipad==T,],fTvariables = 'P18_B_2_TABLET',fTlevels = T,fbanner = bandera),
    
    P18b_COMPU = frecuentator(fTtabla = datos[datos$P10_Computadora___Lap_Top==T,],fTvariables = 'P18_B_COMPU',fTlevels = T,fbanner = bandera),

    P18b2_COMPU = frecuentator(fTtabla = datos[datos$P10_Computadora___Lap_Top==T,],fTvariables = 'P18_B_2_COMPU',fTlevels = T,fbanner = bandera)
    
    
    )

exportator(resultBanner1,"resultadosBanner_1.csv")




maxdiff<-datos%>%select(contains('CELULARES'))
unique(maxdiff$CELULARES_BLOQUE1_MAS)
summary(maxdiff[,1])
nrow(maxdiff[which(!is.na(maxdiff$CELULARES_BLOQUE1_MAS)),])
###PARA MAXDIFF QUITAR LOS NA'S
