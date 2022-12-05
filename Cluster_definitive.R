library(tidyverse)
library(ggplot2)
library(purrr)
library(cluster)
library(factoextra)
library(clustMixType)
library(wesanderson)
library(GGally)
library(showtext)
library(dplyr)
library(RColorBrewer)
library(ggsci)
library(ggpubr)
library(extrafont)
font_import()
fonts()

font_add_google("Caladea", "caladea")
showtext_auto()

# Paleta ColorBlind en Gris

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

#Paleta ColorBlind en Negro

cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

hist(CLUSTER_CRIBADO$Volumen_m3, freq=FALSE,ylim=c(0,0.006),col="lightcyan", main = "Volumen", xlab="", ylab="Densidad")
#Estimaci?n de la funci?n de densidad de la variable mediante un estimador de nucleo
lines(density(CLUSTER_CRIBADO$Volumen_m3), col="red", lwd=2)
#Ajuste de una funci?n de densidad normal con media y desviaci?n est?ndar estimadas a partir de los datos
curve(dnorm(x,mean=mean(CLUSTER_CRIBADO$Volumen_m3), sd=sd(CLUSTER_CRIBADO$Volumen_m3)),from=0, to=1500,
      add=TRUE, col="blue", lwd=2)
#Leyenda

legend("topleft",col=c("blue","red"),legend =c("Densidad normal estimada","Estimador de n?cleo de la densidad"),lwd=2, bty = "n")


CLUSTER <- read.csv2("MENORCA_DEF.csv", header = TRUE, sep = ";", na.strings = "")



# Eliminar los valores sin datos NA.OMIT

CLUSTER_CRIBADO <-na.omit(CLUSTER)


# Identificador - Poner campo CÓDIGO como identificador
row.names(CLUSTER_CRIBADO)=CLUSTER_CRIBADO$CODIGO
#Eliminar campo "CÓDIGO" (ya lo hemos puesto como identificador de filas)
CLUSTER_CRIBADO$CODIGO=NULL
row.names(CLUSTER_CRIBADO)
str(CLUSTER_CRIBADO)
# Conversión variables categóricas a FACTORES
CLUSTER_CRIBADO$Clasificacion=as.factor(CLUSTER_CRIBADO$Clasificacion)
CLUSTER_CRIBADO$PAT=as.factor(CLUSTER_CRIBADO$PAT)
CLUSTER_CRIBADO$HE=as.factor(CLUSTER_CRIBADO$HE)
CLUSTER_CRIBADO$HIN=as.factor(CLUSTER_CRIBADO$HIN)
CLUSTER_CRIBADO$CA=as.factor(CLUSTER_CRIBADO$CA)
CLUSTER_CRIBADO$X2PUERT=as.factor(CLUSTER_CRIBADO$X2PUERT)
CLUSTER_CRIBADO$PLA=as.factor(CLUSTER_CRIBADO$PLA)
CLUSTER_CRIBADO$ESC=as.factor(CLUSTER_CRIBADO$ESC)
CLUSTER_CRIBADO$F=as.factor(CLUSTER_CRIBADO$F)
CLUSTER_CRIBADO$FO=as.factor(CLUSTER_CRIBADO$FO)
CLUSTER_CRIBADO$EL_DECO=as.factor(CLUSTER_CRIBADO$EL_DECO)
CLUSTER_CRIBADO$ESP_EL=as.factor(CLUSTER_CRIBADO$ESP_EL)
CLUSTER_CRIBADO$PLAT_EL=as.factor(CLUSTER_CRIBADO$PLAT_EL)
CLUSTER_CRIBADO$P=as.factor(CLUSTER_CRIBADO$P)
CLUSTER_CRIBADO$PI=as.factor(CLUSTER_CRIBADO$PI)
CLUSTER_CRIBADO$CO=as.factor(CLUSTER_CRIBADO$CO)
CLUSTER_CRIBADO$VENT=as.factor(CLUSTER_CRIBADO$VENT)
CLUSTER_CRIBADO$ENT=as.factor(CLUSTER_CRIBADO$ENT)
CLUSTER_CRIBADO$ï..Nombre =as.factor(CLUSTER_CRIBADO$ï..Nombre)
CLUSTER_CRIBADO$SI=as.factor(CLUSTER_CRIBADO$SI)

#Conversión variables numéricas a NUMÉRICAS (el csv las había interpretado como texto)
CLUSTER_CRIBADO$Ã.rea_m2=as.numeric(CLUSTER_CRIBADO$Ã.rea_m2)

CLUSTER_CRIBADO$Volumen_m3=as.numeric(CLUSTER_CRIBADO$Volumen_m3)
str(CLUSTER_CRIBADO)
colSums(is.na(CLUSTER_CRIBADO))

BASE_CLUSTER <- CLUSTER_CRIBADO %>% select(- ï..Nombre)
str(BASE_CLUSTER)

#Estandarizar variables num�ricas

#BASE_CLUSTER$Ã.rea_m2=scale(BASE_CLUSTER$Ã.rea_m2)[,1]

#BASE_CLUSTER$Volumen_m3=scale(BASE_CLUSTER$Volumen_m3)[,1]

summary(BASE_CLUSTER)
str(BASE_CLUSTER)
# Matriz de distancias 

dis_gower=daisy(BASE_CLUSTER,metric = "gower")

# Determinar cuántos grupos se van a crear - N. ÓPTIMO = métrica silueta baja

silhouette <- c()
for(i in 2:9){
  pam_clusters = pam(as.matrix(dis_gower),diss=T,k = i)
  silhouette = c(silhouette ,pam_clusters$silinfo$avg.width)
}
plot(2:9, silhouette,
     xlab = "Clusters",
     ylab = "Silhouette Width")
lines(2:9, silhouette)

# Método codo - gráfico en forma brazo, cluster óptimo donde caiga el codo- en este caso 4 (menos ruido y mayor concentraciónen cada uno de los grupos)

# Prototipo cluster - crear segmentos a partir de variables mixtas

# kprototipo - funci�n kproto

summary(BASE_CLUSTER)
Grupos_clusters <- kproto(BASE_CLUSTER,k = 4)

Grupos_clusters
# kproto - n. clusters= 4, cluster sizes = tamaño de los cluster (observaciones totales), within cluster error = error dentro de cada uno de los grupos
## (p. ej.: consolidación cluster 2 menos error que el 4)

# Porcentaje de tipos

summary(Grupos_clusters)

#Asignar grupos a base de datos original

BASE_CLUSTERIZADA <- data.frame(BASE_CLUSTER,Grupos_clusters$cluster)

# MÉTODO KMEDOIDES - DISTANCIA DE GOWER

clust=pam(dis_gower,diss = T,k = 4)
clust$medoids # RepresentaciÃ³n lider de cada grupo
BASE_CLUSTERIZADA[clust$medoids,]
BASE_CLUSTERIZADA$Grupo_medoide=clust$clustering

table(BASE_CLUSTERIZADA$Grupo_medoide)

View(BASE_CLUSTERIZADA)

table(BASE_CLUSTERIZADA$Clasificacion,BASE_CLUSTERIZADA$Grupo_medoide)
table(BASE_CLUSTERIZADA$Clasificacion,BASE_CLUSTERIZADA$Grupos_clusters.cluster)
names(BASE_CLUSTERIZADA)

ggplot(BASE_CLUSTERIZADA, aes(x = Ã.rea_m2, y = Volumen_m3)) +
  geom_point(aes(colour=factor(Grupos_clusters.cluster)), 
            size=2.4) +
  labs(title = "Clasificaci�n general",
       x = "Área (m2)", y = "Volumen (m3)")  +
  scale_colour_manual(values=cbPalette) +
  theme(plot.title=element_text(family='Caladea', face="bold", size=20),
        axis.title.x = element_text(family='Caladea', face='bold', size= 15),
        axis.title.y = element_text(family='Caladea', face='bold', size= 15),
        legend.title = element_text(family = "caladea", face= "bold", size = 15)) +
  labs(color='Tipo')



#  Cluster cuevas artificiales (SÓLO CUEVAS ARTIFICIALES)


CLUSTER_CUEVAS_ARTIFICIALES <- CLUSTER_CRIBADO %>% filter(CLUSTER_CRIBADO$Clasificacion=="Cueva artificial")

summary(CLUSTER_CUEVAS_ARTIFICIALES)

dis_gower_cuevas_artificiales=daisy(CLUSTER_CUEVAS_ARTIFICIALES,metric = "gower")

silhouette <- c()
for(i in 2:9){
  pam_clusters = pam(as.matrix(dis_gower_cuevas_artificiales),diss=T,k = i)
  silhouette = c(silhouette ,pam_clusters$silinfo$avg.width)
}
plot(2:9, silhouette,
     xlab = "Clusters",
     ylab = "Silhouette Width")
lines(2:9, silhouette)


BASE_CLUSTER_CUEVAS_ARTIFICIALES <- CLUSTER_CUEVAS_ARTIFICIALES %>% select(- ï..Nombre)

Grupos_clusters_cuevas_artificiales <- kproto(BASE_CLUSTER_CUEVAS_ARTIFICIALES,k = 5)

Grupos_clusters_cuevas_artificiales

summary(Grupos_clusters_cuevas_artificiales)

BASE_CLUSTERIZADA_CUEVAS_ARTIFICIALES <- data.frame(BASE_CLUSTER_CUEVAS_ARTIFICIALES,Grupos_clusters_cuevas_artificiales$cluster)


ggplot(BASE_CLUSTERIZADA_CUEVAS_ARTIFICIALES, aes(x = Ã.rea_m2, y = Volumen_m3)) +
  geom_point(aes(colour=factor(Grupos_clusters_cuevas_artificiales.cluster)),
             size=2.4) +
  labs(title = "Clasificación cuevas artificiales",
       x = "Área (m2)", y = "Volumen (m3)")  +
  scale_colour_manual(values=cbPalette) +
  theme(plot.title=element_text(family='Caladea', face="bold", size=20),
        axis.title.x = element_text(family='Caladea', face='bold', size= 15),
        axis.title.y = element_text(family='Caladea', face='bold', size= 15),
        legend.title = element_text(family = "caladea", face= "bold", size = 15)) +
  labs(color='Tipo')


# Cluster Cales Coves 


CLUSTER_CRIBADO_CCO <- CLUSTER_CRIBADO %>% filter(CLUSTER_CRIBADO$ï..Nombre=="Cales Coves")

BASE_CLUSTER_CCO <- CLUSTER_CRIBADO_CCO %>% select(- ï..Nombre)

dis_gower_cco=daisy(BASE_CLUSTER_CCO,metric = "gower")


silhouette <- c()
for(i in 2:9){
  pam_clusters = pam(as.matrix(dis_gower_cco),diss=T,k = i)
  silhouette = c(silhouette ,pam_clusters$silinfo$avg.width)
}
plot(2:9, silhouette,
     xlab = "Clusters",
     ylab = "Silhouette Width")
lines(2:9, silhouette)


Grupos_clusters_CCO <- kproto(BASE_CLUSTER_CCO,k = 3)

summary(Grupos_clusters_CCO)

Grupos_clusters_CCO

BASE_CLUSTERIZADA_CCO <- data.frame(BASE_CLUSTER_CCO,Grupos_clusters_CCO$cluster)


ggplot(BASE_CLUSTERIZADA_CCO, aes(x = Ã.rea_m2, y = Volumen_m3)) +
  geom_point(aes(colour=factor(Grupos_clusters_CCO.cluster)),
             size=2.4) +
  labs(title = "Tipos Cales Coves",
       x = "Área (m2)", y = "Volumen (m3)")  +
  scale_colour_manual(values=cbPalette) +
  theme(plot.title=element_text(family='Caladea', face="bold", size=20),
        axis.title.x = element_text(family='Caladea', face='bold', size= 15),
        axis.title.y = element_text(family='Caladea', face='bold', size= 15),
        legend.title = element_text(family = "caladea", face= "bold", size = 15)) +
  labs(color='Tipo')

# Cluster Forma Nou


CLUSTER_CRIBADO_FON <- CLUSTER_CRIBADO %>% filter(CLUSTER_CRIBADO$ï..Nombre=="Forma Nou")

BASE_CLUSTER_FON <- CLUSTER_CRIBADO_FON %>% select(- ï..Nombre)

dis_gower_fon=daisy(BASE_CLUSTER_FON,metric = "gower")


silhouette <- c()
for(i in 2:9){
  pam_clusters = pam(as.matrix(dis_gower_fon),diss=T,k = i)
  silhouette = c(silhouette ,pam_clusters$silinfo$avg.width)
}
plot(2:9, silhouette,
     xlab = "Clusters",
     ylab = "Silhouette Width")
lines(2:9, silhouette)


Grupos_clusters_FON <- kproto(BASE_CLUSTER_FON,k = 4)

summary(Grupos_clusters_FON)
Grupos_clusters_FON

BASE_CLUSTERIZADA_FON <- data.frame(BASE_CLUSTER_FON,Grupos_clusters_FON$cluster)


ggplot(BASE_CLUSTERIZADA_FON, aes(x = Ã.rea_m2, y = Volumen_m3)) +
  geom_point(aes(colour=factor(Grupos_clusters_FON.cluster)),
             size=2.4) +
  labs(title = "Tipos Forma Nou",
       x = "Área (m2)", y = "Volumen (m3)")  +
  scale_colour_manual(values=cbPalette) +
  theme(plot.title=element_text(family='Caladea', face="bold", size=20),
        axis.title.x = element_text(family='Caladea', face='bold', size= 15),
        axis.title.y = element_text(family='Caladea', face='bold', size= 15),
        legend.title = element_text(family = "caladea", face= "bold", size = 15)) +
  labs(color='Tipo')

# Cluster Torrevella

CLUSTER_CRIBADO_TVE <- CLUSTER_CRIBADO %>% filter(CLUSTER_CRIBADO$ï..Nombre=="Torrevella")

BASE_CLUSTER_TVE <- CLUSTER_CRIBADO_TVE %>% select(- ï..Nombre)

dis_gower_tve=daisy(BASE_CLUSTER_TVE,metric = "gower")


silhouette <- c()
for(i in 2:9){
  pam_clusters = pam(as.matrix(dis_gower_tve),diss=T,k = i)
  silhouette = c(silhouette ,pam_clusters$silinfo$avg.width)
}
plot(2:9, silhouette,
     xlab = "Clusters",
     ylab = "Silhouette Width")
lines(2:9, silhouette)


Grupos_clusters_TVE <- kproto(BASE_CLUSTER_TVE,k = 6)

summary(Grupos_clusters_TVE)

Grupos_clusters_TVE

BASE_CLUSTERIZADA_TVE <- data.frame(BASE_CLUSTER_TVE,Grupos_clusters_TVE$cluster)


ggplot(BASE_CLUSTERIZADA_TVE, aes(x = Ã.rea_m2, y = Volumen_m3)) +
  geom_point(aes(colour=factor(Grupos_clusters_TVE.cluster)),
             size=2.4) +
  labs(title = "Tipos Torrevella",
       x = "Área (m2)", y = "Volumen (m3)")  +
  scale_colour_manual(values=cbPalette) +
  theme(plot.title=element_text(family='Caladea', face="bold", size=20),
        axis.title.x = element_text(family='Caladea', face='bold', size= 15),
        axis.title.y = element_text(family='Caladea', face='bold', size= 15),
        legend.title = element_text(family = "caladea", face= "bold", size = 15)) +
  labs(color='Tipo')



clust_tve=pam(dis_gower_tve,diss = T,k = 6)
clust_tve$medoids # RepresentaciÃ³n lider de cada grupo
BASE_CLUSTERIZADA_TVE[clust$medoids,]
BASE_CLUSTERIZADA_TVE$Grupo_medoide=clust_tve$clustering

table(BASE_CLUSTERIZADA$Grupo_medoide)

View(BASE_CLUSTERIZADA_TVE)

table(BASE_CLUSTERIZADA_TVE$Clasificacion,BASE_CLUSTERIZADA_TVE$Grupo_medoide)
table(BASE_CLUSTERIZADA_TVE$Clasificacion,BASE_CLUSTERIZADA_TVE$Grupos_clusters_TVE.cluster)
names(BASE_CLUSTERIZADA)


ggplot(BASE_CLUSTERIZADA_TVE, aes(x = Ã.rea_m2, y = Volumen_m3)) +
  geom_point(aes(colour=factor(Grupo_medoide)),
             size=2.4) +
  labs(title = "Tipos Torrevella",
       x = "Área (m2)", y = "Volumen (m3)")  +
  scale_colour_manual(values=cbPalette) +
  theme(plot.title=element_text(family='Caladea', face="bold", size=20),
        axis.title.x = element_text(family='Caladea', face='bold', size= 15),
        axis.title.y = element_text(family='Caladea', face='bold', size= 15),
        legend.title = element_text(family = "caladea", face= "bold", size = 15)) +
  labs(color='Tipo')

CONTEO_PATIO <- CLUSTER_CRIBADO %>% count(PAT)

CONTEO_HE <- CLUSTER_CRIBADO %>% count(HE)


CONTEO_HIN <- CLUSTER_CRIBADO %>% count(HIN)

CONTEO_ENT <- CLUSTER_CRIBADO %>% count(ENT)

CONTEO_F <- CLUSTER_CRIBADO %>% count(F)

CONTEO_FO <- CLUSTER_CRIBADO %>% count(FO)

CONTEO_SI <- CLUSTER_CRIBADO %>% count(SI)

CONTEO_VENT <- CLUSTER_CRIBADO %>% count(VENT)

CONTEO_P <- CLUSTER_CRIBADO %>% count(P)

CONTEO_PI <- CLUSTER_CRIBADO %>% count(PI)

CONTEO_CO <- CLUSTER_CRIBADO %>% count(CO)

CONTEO_CA <- CLUSTER_CRIBADO %>% count(CA)

CONTEO_X2PUERT <- CLUSTER_CRIBADO %>% count(X2PUERT)

CONTEO_PLA <- CLUSTER_CRIBADO %>% count(PLA)

CONTEO_ESC <- CLUSTER_CRIBADO %>% count(ESC)

CONTEO_EL_DECO <- CLUSTER_CRIBADO %>% count(EL_DECO)

CONTEO_ESP_EL <- CLUSTER_CRIBADO %>% count(ESP_EL)

CONTEO_PLAT_EL <- CLUSTER_CRIBADO %>% count(PLAT_EL)


#Barplot (ggplot) para ausencia y presencia de fosas y sitjots

dataframe_silos_fosas <- data.frame(Elemento=c("SITJOTS","SITJOTS", "FOSAS", "FOSAS"),
                        Tipo=c("Ausencia","Presencia","Ausencia","Presencia"),
                        Valor=c(273,23,279,17))


ggplot(dataframe_silos_fosas,aes(fill=Tipo,x=Elemento,y=Valor))+
  geom_bar(position = "dodge", stat = "identity")+
  geom_text(mapping=aes(label=Valor),position=position_dodge(width=0.9),
            cex=3,vjust=-1)+
  theme(axis.title.x = element_text(family = "caladea", face = "bold", size = 16),
        axis.title.y = element_text(family = "caladea", face = "bold", size = 16),
        legend.title = element_text(family = "caladea", face= "bold", size = 18))+
  labs(x="Tipos de elementos",y="Cuantificación",fill="")+
  theme_pubr()

# Conteo global elementos

CUANTIFICACION_ELEMENTOS <- read.csv("Conteo_elementos.csv",header = TRUE,sep = ";",na.strings = "")


ggplot(CUANTIFICACION_ELEMENTOS,aes(fill=TIPO,x=ï..ELEMENTO,y=VALOR))+
  geom_bar(position = "dodge", stat = "identity")+
  geom_text(mapping=aes(label=VALOR),position=position_dodge(width=0.9),#Para indicar el conteo en las barras
            cex=3,vjust=-1)+
  theme(axis.title.x = element_text(family = "caladea", face = "bold", size = 8),
        axis.title.y = element_text(family = "caladea", face = "bold", size = 8),
        legend.title = element_text(family = "caladea", face= "bold", size = 18))+
  labs(x="Tipos de elementos",y="Cuantificación",fill="")+
  theme_pubr()

# Gr�fico representativo de elementos NO determinantes en la configuraci�n de los clusters
GRÁFICO_EL_INSIG <- CUANTIFICACION_ELEMENTOS %>% filter (!ï..ELEMENTO=="CO",
                                                         !ï..ELEMENTO=="HIN",
                                                         !ï..ELEMENTO=="PAT",
                                                         !ï..ELEMENTO=="PI")


ggplot(GRÁFICO_EL_INSIG,aes(fill=TIPO,x=ï..ELEMENTO,y=VALOR))+
  geom_bar(position = "dodge", stat = "identity")+
  geom_text(mapping=aes(label=VALOR),position=position_dodge(width=0.9),#Para indicar el conteo en las barras
            cex=3,vjust=-1)+
  theme(axis.title.x = element_text(family = "caladea", face = "bold", size = 8),
        axis.title.y = element_text(family = "caladea", face = "bold", size = 8),
        legend.title = element_text(family = "caladea", face= "bold", size = 18))+
  labs(x="TIPOS DE ELEMENTOS",y="CUANTIFICACIÓN",fill="")+
  theme_pubr(base_size = 12, base_family = "Caladea")+
  labs_pubr(base_size = 11, base_family = "Caladea")
