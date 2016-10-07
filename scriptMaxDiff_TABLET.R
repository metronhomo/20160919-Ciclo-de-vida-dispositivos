
library(dplyr)
library(mlogit)
library(ggplot2)
#library(ggrepel)

####LEEMOS LOS DATOS

base <- brostatistics::gaseosa(
    xfile = list.files("./datos",full.names = T)[1], 
    yfile = list.files("./datos",full.names = T)[2]
)

base <- base%>%arrange(ResponseID)
#Eliminar casos

eliminar<-c(75169079,75267892,75211789,75211784,75179295,75179289,75222476,75223311,75211791,75276387,
            75202149,75231633,75211794,75211786,75173876,75204845,75211656,75211768,75211775,
            75214002,75223811,75227008,75227285,75231628,75269760,75312126,75313838)

base <- base[which(!base$ResponseID%in%eliminar),]

#Seleccionamos los casos que dijeron tener CELULAR EN CASA

base <- base[base$P10_Tablet___Ipad==T,]

#Obtenemos el subconjunto de la base con las evaluaciones de los atributos
z<-base %>% select(contains('TABLET_BLOQUE'))
z<-z[complete.cases(z),] #verifico que no haya NA's
z<-as.data.frame(z)

names(z)
head(z)
str(z)

summary(z)

#Convertimos a numericas las variables que nos interesan
#La base como la tenemos está en FORMATO AMPLIO
for(i in 1:ncol(z)) {
    z[,i] <- as.numeric((z[,i]))
}

###Cambio los niveles en cada bloque

for(j in c(3:4)){#bloque2
    for(i in 1:nrow(z)){
        if(z[i,j]==1)z[i,j]=7
        if(z[i,j]==2)z[i,j]=8
        if(z[i,j]==3)z[i,j]=9
        if(z[i,j]==4)z[i,j]=10
        if(z[i,j]==5)z[i,j]=5
        if(z[i,j]==6)z[i,j]=11
    }
}
table(z[,3])
table(z[,4])
unique(z[,3])

for(j in c(5:6)){#bloque3
    for(i in 1:nrow(z)){
        if(z[i,j]==1)z[i,j]=12
        if(z[i,j]==2)z[i,j]=1
        if(z[i,j]==3)z[i,j]=9
        if(z[i,j]==4)z[i,j]=13
        if(z[i,j]==5)z[i,j]=11
        if(z[i,j]==6)z[i,j]=14
    }
}
table(z[,5])
table(z[,6])

for(j in c(7:8)){#bloque4
    for(i in 1:nrow(z)){
        if(z[i,j]==1)z[i,j]=7
        if(z[i,j]==3)z[i,j]=15
        if(z[i,j]==4)z[i,j]=16
        if(z[i,j]==5)z[i,j]=9
        if(z[i,j]==6)z[i,j]=6
        if(z[i,j]==2)z[i,j]=3
    }
}
table(z[,7])
table(z[,8])

for(j in c(9:10)){#bloque5 
    for(i in 1:nrow(z)){
        if(z[i,j]==1)z[i,j]=12
        if(z[i,j]==2)z[i,j]=8
        if(z[i,j]==3)z[i,j]=17
        if(z[i,j]==4)z[i,j]=3
        if(z[i,j]==5)z[i,j]=5
        if(z[i,j]==6)z[i,j]=18
    }
}
table(z[,9])
table(z[,10])

for(j in c(11:12)){#bloque6
    for(i in 1:nrow(z)){
        if(z[i,j]==1)z[i,j]=8
        if(z[i,j]==2)z[i,j]=19
        if(z[i,j]==3)z[i,j]=20
        if(z[i,j]==4)z[i,j]=16
        if(z[i,j]==5)z[i,j]=4
        if(z[i,j]==6)z[i,j]=11
    }
}
table(z[,11])
table(z[,12])

for(j in c(13:14)){#bloque7
    for(i in 1:nrow(z)){
        if(z[i,j]==1)z[i,j]=19
        if(z[i,j]==2)z[i,j]=2
        if(z[i,j]==3)z[i,j]=15
        if(z[i,j]==4)z[i,j]=10
        if(z[i,j]==5)z[i,j]=18
        if(z[i,j]==6)z[i,j]=14
    }
}
table(z[,13])
table(z[,14])

for(j in c(15:16)){#bloque8
    for(i in 1:nrow(z)){
        if(z[i,j]==1)z[i,j]=7
        if(z[i,j]==2)z[i,j]=19
        if(z[i,j]==3)z[i,j]=2
        if(z[i,j]==4)z[i,j]=20
        if(z[i,j]==5)z[i,j]=5
        if(z[i,j]==6)z[i,j]=13
    }
}
table(z[,15])
table(z[,16])

for(j in c(17:18)){#bloque9
    for(i in 1:nrow(z)){
        if(z[i,j]==1)z[i,j]=1
        if(z[i,j]==2)z[i,j]=17
        if(z[i,j]==3)z[i,j]=20
        if(z[i,j]==4)z[i,j]=10
        if(z[i,j]==5)z[i,j]=13
        if(z[i,j]==6)z[i,j]=6
    }
}
table(z[,17])
table(z[,18])

for(j in c(19:20)){#bloque10
    for(i in 1:nrow(z)){
        if(z[i,j]==1)z[i,j]=19
        if(z[i,j]==2)z[i,j]=1
        if(z[i,j]==3)z[i,j]=17
        if(z[i,j]==4)z[i,j]=16
        if(z[i,j]==5)z[i,j]=9
        if(z[i,j]==6)z[i,j]=18
    }
}
table(z[,19])
table(z[,20])

for(j in c(21:22)){#bloque11
    for(i in 1:nrow(z)){
        if(z[i,j]==1)z[i,j]=12
        if(z[i,j]==2)z[i,j]=8
        if(z[i,j]==3)z[i,j]=2
        if(z[i,j]==4)z[i,j]=16
        if(z[i,j]==6)z[i,j]=14
        if(z[i,j]==5)z[i,j]=6
    }
}
table(z[,21])
table(z[,22])

for(j in c(23:24)){#bloque12
    for(i in 1:nrow(z)){
        if(z[i,j]==1)z[i,j]=17
        if(z[i,j]==2)z[i,j]=20
        if(z[i,j]==3)z[i,j]=3
        if(z[i,j]==4)z[i,j]=15
        if(z[i,j]==5)z[i,j]=11
        if(z[i,j]==6)z[i,j]=14
    }
}
table(z[,23])
table(z[,24])

for(j in c(25:26)){#bloque13
    for(i in 1:nrow(z)){
        if(z[i,j]==1)z[i,j]=7
        if(z[i,j]==2)z[i,j]=12
        if(z[i,j]==3)z[i,j]=15
        if(z[i,j]==4)z[i,j]=4
        if(z[i,j]==5)z[i,j]=13
        if(z[i,j]==6)z[i,j]=18
    }
}
table(z[,25])
table(z[,26])

#######################################################################################
#######################################################################################

#Para realizar el análisis requerimos que la base esté en formato "apilado"
# Creamos un dataframe con NA's de tamaño 134*13 renglones, donde 134 es el no. de 
# personas y 13 corresponden al no. de bloques aplicados a cada persona. El numero 20
# corresponde al numero total de atributos evaluados, y estos formarán las columnas.
# Lo que haremos será "APILAR LA BASE", es decir, tendremos 13 renglones por cada persona,
# porque se aplicaron 13 sets, y tendremos 20 columnas porque se evaluaron 20 atributos.

temp<-as.data.frame(matrix(NA,nrow=489*12,ncol=23))         
names(temp)<-c(1:23)



# Creamos la base apilada
# k recorre el no. de sets
# j recorre columnas
# i recorre el no. de renglones
# 
# La matriz temp será llenada con:
#-1: Worst (las columnas son las pares)
# 0: Mostrado pero no seleccionado
# 1: Best (columnas impares)

impar<-seq(1,2000,2)
par<-seq(2,2000,2)
for(k in 1:12){
    for(j in 1:2+(2*(k-1))){
        for(i in 1:nrow(z)){
            w<-z[i,j] #obtenemos el valor de cada celda
            u<-unique(z[,j]) #obtengo atributos que se mostraron en el set  
            if(w%in%u){ #llenamos con ceros los atributos que se mostraron en cada set
                temp[i+(nrow(z)*(k-1)),u]=0
            }
            
            if(j%in%par){ # en las columnas par está el "worst"
                temp[i+(nrow(z)*(k-1)),w]= -1 #indicamos con un -1
            }                                
        }        
    }  
}


for(k in 1:12){
    for(j in 1:2+(2*(k-1))){
        for(i in 1:nrow(z)){
            w<-z[i,j]
            if(j%in%impar){ #en las columnas impar está el "best"
                temp[i+(nrow(z)*(k-1)),w]= 1
            } 
        }       
    }   
}


#juntamos los renglones para cada individuo
temp2<-temp

#k 
#j recorre columnas (sets:12)
#i recorre el no. de renglones (personas:489) 

k<-1
for(i in 1:489){
    for(j in 1:12){
        temp2[k,]<-temp[(i+489*(j-1)),]
        k<-k+1
    }
}


#Indicamos las variables que utilizaremos para crear el rank

#declaramos los 23 atributos que se estan evaluando

alternativeNames = c("Que desea viajar constantemente",
                     "Que prefiere marcas de calidad",
                     "Que le gusta vivir/disfrutar el momento sin pensar en el futuro",
                     "Que gusta de usar la nueva tecnología y enseñar a otras personas a usarla",
                     "Que disfruta de convivir con la familia",
                     "Que prefiere usar productos de marca libre",
                     "Que siempre busca ofertas/descuentos",
                     "Que gusta de tener productos/servicios que reflejen lujo",
                     "Que gusta salir con amigos",
                     "Que se siente realizado en la vida",
                     "Que gusta de usar artículos modernos",
                     "Que prefiere comprar ropa en tiendas de prestigio",
                     "Que ahorra pensando tener una vejez tranquila",
                     "Que usa todos sus ingresos en hijos/familia",
                     "Que disfruta de vestir de manera sencilla",
                     "Que prefiere usar productos/servicios que no dañen el medio ambiente",
                     "Que gusta vestir de manera conservadora",
                     "Que se previene ahorrando en caso de enfermedad grave", 
                     "Que prefiere usar ropa cómoda sin importar la marca", 
                     "Que gusta usar marcas de prestigio",
                     "Que prefiere usar productos/servicios que anuncia la gente famosa",
                     "Que gusta de usar productos/servicios que lo hacen sobresalir",
                     "Que gusta de la tecnología nueva, sólo si los demás la están usando"
)

nAlternatives = length(alternativeNames) #23 atributos a total
nBlocks = 12 #no. de bloques de preguntas, 12 BLOQUES
nAltsPerSet = 6   # Se preguntan 5 atributos en cada bloque
n = nrow(z) #no. de personas que contestaron la encuesta (997)
nObservations = n * nBlocks #total observaciones 489*12=5868

names(temp2)<-alternativeNames

####Calculo de los conteos para la muestra completa TOMANDO PROMEDIOS
#Se obtiene la media por columna(en este caso es por atributo)
counts = apply(temp2, 2, mean, na.rm = TRUE)

#nAlternatives es 23, rank "ordena" ascendente (de menor a mayor), 
#en nuestro caso el primer lugar es el de mayor promedio, por lo que 
#necesitamos invertir el orden
ranks = nAlternatives + 1 - rank(counts)

#unimos por columnas Counts y Ranks
rank_total<-cbind(Counts = counts, Ranks = ranks)
rank_total<-as.data.frame(rank_total)
#rownames(rank_total)<-alternativeNames
#rank_total$atrib<-rownames(rank_total)
#rank_total<-rank_total[,c(3,1,2)]
rank_total<-rank_total[order(rank_total$Ranks),]
rank_total

####Calculo de los conteos por persona------------------------------------------
#El siguiente codigo realiza el analisis por persona
#nBlocks=12 a modo de recordatorio
#n=489
#Creamos la variable id, el código resulta en una secuencia del 1 hasta 489, donde
#cada numero se repite 12 veces (no. de bloques)
id = rep(1:n,rep(nBlocks,n))

#Con la funcion "aggregate",obtenemos la media POR PERSONA Y POR COLUMNA,
#excepto la 1 que corresponde a los id's.
#Lo que significa del total de veces que le fue mostrado el atributo, en promedio
#cuantas lo eligio como el "mejor"
individualCounts = aggregate(temp2,list(id),mean, na.rm = TRUE)[,-1]
round(individualCounts[1:10,],1) #muestra las primeras 10 personas

####Calculo de los ranks sobre los conteos a nivel persona

#A modo de recordatorio
#n=489 
#nAlternatives=23 no. de atributos

set.seed(0) #fijamos la semilla para generar los numeros aleatorios 
#Se suman numeros aleatorios a la matriz de conteos para romper los empates
individualCountsNoTies = individualCounts + matrix(runif(n * nAlternatives)/100000, n) 
#rankeamos por renglon, es decir,obtenemos el rankeo de los atributos por cada persona
ranks = nAlternatives + 1 - apply(individualCountsNoTies,1,rank) #ranks OK

#########
#La siguiente linea de codigo se utilizara solo cuando ranks sea una matriz cuadrada
rankProportions = t(apply(ranks,1,table) / n*100)
#rankProportions <- as.data.frame(rankProportions)
#########

# La siguiente tabla muestra los ranks resultantes.
# La primer columna de números muestra la proporción de personas que tuvieron 
# el conteo más alto para cada atributo. 
#redondeamos a un decimal rankProportions y visualizamos
round(rankProportions,1) 

#Calculamos el promedio de los rangos:
aveRank = rankProportions %*% (1:23)/100  #23 son los atributos a evaluar
aveRank<-cbind(aveRank, Rank = rank(aveRank))
aveRank<-as.data.frame(aveRank)

#Agregué la siguiente línea para que aparezcan las etiquetas completas
aveRank$atributos<-alternativeNames

aveRank<-aveRank[order(aveRank$Rank),]

#COMO SON RANKS NOS INTERESA EL MENOR, PUES ESO INDICA QUE ESTÁ EN PRIMER LUGAR
aveRank<-aveRank[,c(2,1,3)]
aveRank

####Modelo Logit Total--------------------------------------------------------
#Previo a ejecutar el modelo logit es necesario construir una base con "truco"
#Creamos la variable "CHOICE", es una variable indicadora de la marca seleccionada
#por el respondente

#nRows = 70416 #no. de alternativas por bloque* 2*no. de bloques*no. de personas
nRows=nAltsPerSet*2*nBlocks*n
longDataDE = matrix(0, nRows,nAlternatives + 3)
counter = 0
setCounter = 0
for (rr in 1:nObservations){
    nAlts = 0
    alternatives = NULL
    respondent = floor(rr/nBlocks) + 1
    for (cc in 1:nAlternatives){
        v = temp2[rr,cc] #MODIFICAR LA BASE
        if (!is.na(v)){
            nAlts = nAlts + 1
            alternatives[nAlts] = cc
            if (v == 1)
                best = cc
            if (v == -1)
                worst = cc
        }
    }
    setCounter = setCounter + 1
    for (a in 1:nAlts){
        counter = counter + 1
        this_a = alternatives[a]
        if (this_a == best)
            longDataDE[counter,3] = 1
        else if (this_a == worst)
            longDataDE[counter + nAlts,3] = 1
        longDataDE[counter, 1] = respondent 
        longDataDE[counter + nAlts,1] = respondent 
        longDataDE[counter, 2] = setCounter 
        longDataDE[counter + nAlts, 2] = setCounter + 1
        longDataDE[counter,3 + this_a] = 1
        longDataDE[counter + nAlts,3 + this_a] = -1
    }
    setCounter = setCounter + 1
    counter = counter + nAlts
}
longDataDE[1:20,]
longDataDE = as.data.frame(longDataDE)

nombres<-LETTERS[seq(from=1,to=23)]
names(longDataDE)<-c("ID","Set","Choice",nombres)
names(longDataDE)[9]<-"FF"
names(longDataDE)[23]<-"TT"


#####Modelo LOGIT

#Los coeficientes indican el rank ordenado de las preferencias
################
#El coeficiente más grande es el que ocupa el 1er. lugar 
################

nAltsPerSet = 6
logitModel = mlogit(Choice ~ B+C+D+E+FF+G+H+I+J+K+L+M+N+O+P+Q+R+S+TT+U+V+W | 0, 
                    data = longDataDE, alt.levels = paste(1:nAltsPerSet), 
                    shape = "long")
summary(logitModel)


#######Coeficientes Modelo Logit Global--------------------------

#Extraigo los coeficientes para hacer un df y ordenar
x<-logitModel$coefficients
x<-as.numeric(x)

#Extraigo nuevamente los coeficientes y sus etiquetas
x2<-logitModel$coefficients
df_coef<-matrix(0,22,2) #el 22 es el no. de atributos (23) menos 1,porque
#en el modelo logit se consideran solo n-1 atributos

#Coloco en la matriz los coeficientes y sus etiquetas
for(i in 1:22){ #i es el no. de atributos menos 1
    df_coef[i,2]=x[[i]]
    df_coef[i,1]=names(x2)[[i]]
}

#Convierto a df la matriz df_coef
df_coef<-as.data.frame(df_coef,stringsAsFactors=F)
df_coef$V2 <- as.numeric(df_coef$V2)

#Agrego el coeficiente "A" en el último renglón
df_coef[23,'V1']<-'A'
df_coef[23,'V2']<-0

#ordenamos ascendente de acuerdo a atributos 
df_coef <- df_coef%>%arrange(V1)

#Agrego las "etiquetas"
df_coef$atributos<-alternativeNames
names(df_coef)[c(1,2)] <- c("atr1","coeficiente")

df_coef$coef_neg<-df_coef$coeficiente*(-1)
df_coef<-df_coef[order(-df_coef$coeficiente),]
df_coef$rank<-rev(1:23)# crea una secuencia en orden inverso

#Guardo los resultados

setwd("~/Documents/TRABAJO/20_IKE")
write.table(aveRank,"aveRankHOMB.txt", sep = "|")
write.table(df_coef,"coeficientesLogitHOMBRES.txt" , sep = "|")


#############


png("graImportanciasHOMBRES.png",width=1200,height=700,units="px",pointsize=8,res=110)
ggplot(df_coef,aes(x=-rank,y=coeficiente))+
    geom_point()+
    geom_text_repel(aes(-rank,coeficiente,label =atributos,vjust=-1.2, hjust =0.05))+
    ggtitle("Coeficientes MLogit")
dev.off()


df_coef$id<-letters[1:23]

pdf('impP27HOMBRES.pdf',width=22,height=11)
ggplot(df_coef, aes(x=-rank,y=coeficiente))+
    geom_point(size=8,alpha=.15)+
    geom_point(size=7,color='white',alpha=.15)+
    geom_point(size=5,aes(shape=id))+
    theme_bw()+
    theme(legend.position="bottom")+
    scale_shape_manual(values=c(65:93),labels=df_coef$atributos)+
    xlab('Importancia')

dev.off()
