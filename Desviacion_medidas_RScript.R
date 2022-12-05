library(ggplot2)
library(magrittr)
library(tidyverse)
library(showtext)
library(dplyr)
library(extrafont)
font_import()
fonts()
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
library(fdth)

#Importar Fuentes



font_add_google("Caladea", "caladea")
showtext_auto()



# Paleta de colores en Colourblind

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
scale_colour_manual(values=cbPalette)


#Cargar datos


Pilastras <-read.csv("CSV/Pilastras.csv", header= TRUE, sep =";", na.strings = "") 
Pilares <- read.csv("CSV/Pilares.csv", header= TRUE, sep= ";", na.strings = "")
Coco <- read.csv("CSV/Coco.csv", header= TRUE, sep= ";", na.strings = "")
Entrada <- read.csv("CSV/Entrada.csv", header= TRUE, sep= ";", na.strings = "")
Fosa<- read.csv("CSV/Fosas.csv", header= TRUE, sep= ";", na.strings = "")
Hornacinas<- read.csv("CSV/Hornacinas.csv", header= TRUE, sep= ";", na.strings = "")
Sitjot<- read.csv("CSV/Sitjot.csv", header= TRUE, sep= ";", na.strings = "")
Plataforma <- read.csv("CSV/Plataformas.csv", header= TRUE, sep= ";", na.strings = "")
Ventana <- read.csv("CSV/Ventanas.csv", header= TRUE, sep= ";", na.strings = "")
Entrada_copia <- read.csv("CSV/Entrada - copia.csv", header= TRUE, sep= ";", na.strings = "")
Fosa_copia<- read.csv("CSV/Fosas - copia.csv", header= TRUE, sep= ";", na.strings = "")
Hornacinas_copia<- read.csv("CSV/Hornacinas - copia.csv", header= TRUE, sep= ";", na.strings = "")
Pilastras_copia <-read.csv("CSV/Pilastras - copia.csv", header= TRUE, sep =";", na.strings = "") 
Pilares_copia <- read.csv("CSV/Pilares - copia.csv", header= TRUE, sep= ";", na.strings = "")
Sitjots_copia <-read.csv("CSV/Sitjot - copia.csv", header = TRUE, sep = ",", na.strings = "")
Pilastras_sitios <- read.csv("CSV/Pilastras - sitios.csv", header = TRUE, sep = ",", na.strings = "")




# Representacion variable altura x tipo de pilastra - colores siendo sitios 

Representacion_pilastras <- Pilastras |>
    dplyr::select(SITIO,
            ALTO,
            TIPO)


Representacion_pilastras |>
    ggplot2::ggplot() +
    geom_point(mapping = aes(x=TIPO,
                             y=ALTO,
                             color = SITIO), size =3.5) +
    scale_color_hue(c=45) +
    ggtitle("ALTURA PILASTRAS POR TIPOS Y NECRÓPOLIS") +
    theme(plot.title = element_text(family = "caladea", face = "bold", size = 20),
          axis.title.x = element_text(family = "caladea", face = "bold", size = 16),
          axis.title.y = element_text(family = "caladea", face = "bold", size = 16),
          legend.title = element_text(family = "caladea", face= "bold", size = 18)) +
    labs (color = "Necrópolis", x = "Tipo", y= "Altura")

# Boxplot

Representacion_pilastras <- Pilastras |>
  dplyr::select(ALTO,
                ANCHO,
                TIPO)

boxplot(Pilastras$ALTO ~ Pilastras$TIPO,
        main = "ALTURA PILASTRAS POR TIPO",
        cex.main = 2,
        xlab = "Tipos",family="caladea",
        ylab = "Altura", family ="caladea",
        cex.lab=1.6)

stripchart(Pilastras$ALTO ~ Pilastras$TIPO, vertical = TRUE, method = "jitter",
           pch = 16, add = TRUE, col = 45:length(levels(Pilastras$TIPO)))

# Boxplot con ggplot

ggplot(data=Representacions_pilastras_menos_NA,
       mapping = aes(x=TIPO,
                     y=ALTO,
                     fill=TIPO)) +
  geom_boxplot() +
  coord_flip() +
  scale_color_hue(c=45) +
  ggtitle("ALTURA DE PILASTRAS POR TIPOS") +
  theme(plot.title = element_text(family ="caladea", face="bold", size=20),
        axis.title.x = element_text(family = "caladea", face = "bold", size = 16),
        axis.title.y = element_text(family = "caladea", face = "bold", size = 16),
        legend.title = element_text(family = "caladea", face= "bold", size = 18)) +
  labs(color="TIPO",x="Altura",y="Tipo")

# Representación pilastras anchura x altura x tipo

Representacion_pilastras |>
  ggplot2::ggplot() +
  geom_point(mapping = aes(x=ANCHO,
                           y=ALTO,
                           color =TIPO), size=3.5) +
  scale_color_hue(c=45) +
  ggtitle("ALTURA Y ANCHURA DE PILASTRAS POR TIPOS") +
  theme(plot.title = element_text(family ="caladea", face="bold", size=20),
        axis.title.x = element_text(family = "caladea", face = "bold", size = 16),
        axis.title.y = element_text(family = "caladea", face = "bold", size = 16),
        legend.title = element_text(family = "caladea", face= "bold", size = 18)) +
  labs(color="Tipo",x="Anchura",y="Altura")
  

# Ggplot con distribucion de la variable altura según los diferentes tipos morfológicos de pilastras-> NO SIRVE

Representacions_pilastras_menos_NA <- Representacion_pilastras %>%
  filter(!TIPO == "NA")

ggplot(data=Representacions_pilastras_menos_NA,
       mapping = aes(x=rep(1:142),
                     y= ALTO,
                     color=TIPO)) +
  
geom_line() +
scale_color_hue(c=45) +
ggtitle("ALTURA DE PILASTRAS POR TIPOS") +
theme(plot.title = element_text(family ="caladea", face="bold", size=20),
        axis.title.x = element_text(family = "caladea", face = "bold", size = 16),
        axis.title.y = element_text(family = "caladea", face = "bold", size = 16),
        legend.title = element_text(family = "caladea", face= "bold", size = 18)) +
labs(color="Tipo",x="Casos",y="Altura")



# Representacion medidas sitjots por sitios arqueológicos

Representacion_sitjots <- Sitjot |>
    dplyr::select(DIAMETRO_LARGO,
                  DIAMETRO_CORTO,
                  SITIO)

Representacion_sitjots |>
    ggplot2::ggplot() +
    geom_point(mapping = aes(x=DIAMETRO_LARGO,
                             y=DIAMETRO_CORTO,
                             color=SITIO), size= 3.5) +
    scale_colour_hue(c=45) +
    ggtitle("SITJOTS") +
    theme(plot.title = element_text(family ="caladea", face="bold", size=20),
          axis.title.x = element_text(family = "caladea", face = "bold", size = 16),
          axis.title.y = element_text(family = "caladea", face = "bold", size = 16),
          legend.title = element_text(family = "caladea", face= "bold", size = 18)) +
  labs(color="Necrópolis",x="Diámetro largo",y="Diámetro corto") 



#Representación medidas de cocons por sitios arqueológicos

Representacion_cocons <- Coco |>
    dplyr::select(DIAMETRO_LARGO, 
                  DIAMETRO_CORTO,
                  SITIO)

Representacion_cocons |>
    ggplot2::ggplot()+
    geom_point(mapping = aes(x=DIAMETRO_LARGO,
                             y=DIAMETRO_CORTO,
                             color=SITIO), size = 3.5) +
    scale_colour_hue(c=45) +
    ggtitle("COCONS") +
  theme(plot.title = element_text(family ="caladea", face="bold", size=20),
        axis.title.x = element_text(family = "caladea", face = "bold", size = 16),
        axis.title.y = element_text(family = "caladea", face = "bold", size = 16),
        legend.title = element_text(family = "caladea", face= "bold", size = 18)) +
  labs(color="Necrópolis",x="Diámetro largo",y="Diámetro corto")

#Convertir ggplot en un objeto--> primero creamos el objeto (p)  
Representacion_cocons |>
    ggplot2::ggplot() -> p

#Ahora vamos a representar densidades en función del diámetro largo--> mayor concentración entre 0,20-0,25 m

p +
  geom_density(mapping = aes(x = DIAMETRO_LARGO)) +
  geom_point(mapping   = aes(x = DIAMETRO_LARGO,
                             y = DIAMETRO_CORTO,
                             color = SITIO)) +
  geom_dotplot(mapping = aes(x = DIAMETRO_LARGO), size=3.5) +
  ggtitle("MEDIDAS COCONS") +
  theme(plot.title = element_text(family ="caladea", face="bold",size=20),
                axis.title.x = element_text(family = "caladea", face = "bold", size = 16),
                axis.title.y = element_text(family = "caladea", face = "bold", size = 16),
                legend.title = element_text(family = "caladea", face= "bold", size = 18)) +
  labs(color="Necrópolis",x="Diámetro largo",y="Diámetro corto")

#Cocons por SITIO y diámetro

Representacion_cocons <- na.omit(Representacion_cocons)
Representacion_cocons |>
  ggplot2::ggplot()+
  geom_point(mapping = aes(x=DIAMETRO_LARGO,
                           y=SITIO,
                           color=SITIO), size = 3.5) +
  scale_colour_hue(c=45) +
  ggtitle("COCONS") +
  theme(plot.title = element_text(family ="caladea", face="bold", size=20),
        axis.title.x = element_text(family = "caladea", face = "bold", size = 16),
        axis.title.y = element_text(family = "caladea", face = "bold", size = 16),
        legend.title = element_text(family = "caladea", face= "bold", size = 18)) +
  labs(color="Necrópolis",x="Diámetro",y="Necrópolis")

# Matriz de dispersion (Scatter Plot Matrix) con package de tidyverse y funcion "pairs"

Variables_entrada <- Entrada [,c("ALTO", "ANCHO", "PROFUNDIDAD")]
pairs(Variables_entrada, col=factor(Entrada$TIPO), pch=19, data = Variables_entrada,
      lower.panel = NULL ,labels=c("ALTO","ANCHO","PROFUNDIDAD"), font.labels=2, cex.labels=2.7)

# Matriz de dispersión (Scatter Plot Matrix) con package de GGally y funcion "ggpairs"

Variables_entrada_tipo <- Entrada[,2:5]

Variables_entrada_tipo %>% ggpairs(columns = c("ALTO", "ANCHO", "PROFUNDIDAD"),
                    upper=list(continous=wrap('cor', size =8)))
                    
# Matriz de dispersion (Scatter Plot Matrix) pilastras con package tidyverse y función "pairs"

Variables_pilastras <- Pilastras [,c("ALTO", "ANCHO", "PROFUNDIDAD_IZDA", "PROFUNDIDAD_DCHA")]

pairs(Variables_pilastras , col=factor(Pilastras$TIPO), pch=19, data=Variables_pilastras,
      lower.panel = NULL,labels=c("ALTO","ANCHO","PROFUNDIDAD IZDA","PROFUNDIDAD DCHA"), font.labels=2, cex.labels=1.5)


# Matriz de dispersion (Scatter Plot Matrix) con package de GGally y funcion "ggpairs"

Variables_pilastra_tipo <- Pilastras [, c("ALTO", "ANCHO","PROFUNDIDAD_IZDA", "PROFUNDIDAD_DCHA", "TIPO")]

Variables_pilastra_tipo %>% ggpairs(columns = c("ALTO", "ANCHO", "PROFUNDIDAD_IZDA", "PROFUNDIDAD_DCHA"),
                                     upper = list(continous=wrap('cor', size = 8)))



#Análisis componentes principales - 1) ver si hay una relación de dependencia entre variables (tenemos en cuenta dos variables: anchura y altura)

cor(Entrada_copia [,2:3])
colSums(is.na (Entrada [,2:3]))
cor.test(Entrada_copia$ALTO,Entrada_copia$ANCHO)
cor(Fosa_copia [, 2:3])
cor.test(Fosa_copia$ALTO..m.,Fosa_copia$LARGO..m.)
cor(Hornacinas_copia [, 2:3])
cor.test(Hornacinas_copia$ALTO,Hornacinas_copia$ANCHO)

cor(Pilares_copia [, 3:4])

cor.test(Pilares_copia$ALTO,Pilares_copia$ANCHO,method = "spearman")

cor(Sitjots_copia [, 2:3])

cor(Pilastras_copia[,2:3])
cor.test(Pilastras_copia$ALTO,Pilastras_copia$ANCHO)$p.value

# Primero agrupamos por TIPO a las pilastras y luego hacemos un cálculo de correlación entre el alto y el ancho
Pilastras_copia %>% group_by(TIPO) %>% summarise(CORRELACION=cor(ALTO,ANCHO))

#Ahora ejecutamos un t.test para determinar la significación entre el ancho y alto por tipo de pilastra

Pilastras_copia %>% group_by(TIPO) %>% summarise(CORRELACION_test=cor.test(ALTO,ANCHO)$p.value)

# t.test para determinar significación entre ancho y alto de pilastras según la necrópolis a la que pertenecen


Pilastras_sitios %>% group_by(SITIO) %>% summarise(CORRELACION_test=cor.test(ALTO,ANCHO)$p.value)


# Distribución de frecuencias de altura de las pilastras


hist(Pilastras_copia$ALTO, freq=FALSE,col="lightcyan", ylim=c(0,2), main = "Altura de pilastras", xlab="", ylab="Densidad")
#Estimación de la función de densidad de la variable mediante un estimador de nucleo
lines(density(Pilastras_copia$ALTO), col="red", lwd=2)
#Ajuste de una función de densidad normal con media y desviación estándar estimadas a partir de los datos
curve(dnorm(x,mean=mean(Pilastras_copia$ALTO), sd=sd(Pilastras_copia$ALTO)),from=1.0, to=3.0,
      add=TRUE, col="blue", lwd=2)
#Leyenda

legend("topleft",col=c("blue","red"),legend =c("Densidad normal estimada","Estimador de núcleo de la densidad"),lwd=2, bty = "n")


# Gráfico representativo del tipo de pilastra PIR02 (pilastra recta con capitel) al ser el único con una correlación significativa entre alto y ancho
Pilastras_copia %>%filter(TIPO=="PIR02") %>%
  ggplot2::ggplot() +
  geom_point(mapping = aes(x=ANCHO,
                           y=ALTO), size=3.5,color="deepskyblue") +
  ggtitle("ALTURA Y ANCHURA DE PILASTRAS RECTAS CON CAPITEL") +
  theme_bw() +
  theme(plot.title = element_text(family ="caladea", face="bold", size=18, hjust = 0.5),
        axis.title.x = element_text(family = "caladea", face = "bold", size = 16),
        axis.title.y = element_text(family = "caladea", face = "bold", size = 16),
        legend.title = element_text(family = "caladea", face= "bold", size = 18)) +
  labs(x="Anchura",y="Altura")

table(Pilastras_copia$TIPO)
str(Pilastras_copia)

#Como tratamos con una variables categórica (TIPO) la tenemos que convertir en factor con la función as.factor

Pilastras_copia$TIPO=as.factor(Pilastras_copia$TIPO)

#agrupamos pilastras por TIPO y pedimos que nos saque el promedio de la altura

Pilastras_copia %>% group_by(TIPO)%>%summarise(PROM_ALTURA=mean(ALTO))

#Creamos 4 subconjuntos (uno por cada tipo) ya que el test estadístico aov y el anova nos daban error
Pilastras_PIC01 = Pilastras_copia %>% filter(TIPO=="PIC01")
Pilastras_PIC02 = Pilastras_copia %>% filter(TIPO=="PIC02")
Pilastras_PIR01 = Pilastras_copia %>% filter(TIPO=="PIR01")
Pilastras_PIR02 = Pilastras_copia %>% filter(TIPO=="PIR02")

#Ejecutamos el t.test para determinar la existencia de correlación entre la altura de los diferentes tipo
# para ello hacemos PIC01 con PIC02, PIC01 con PIR01, PIC01 con PIR02/PIC02 con PIR01, PIC02 con PIR02/PIR01 con PIR02)

t.test(Pilastras_PIC01$ALTO,Pilastras_PIC02$ALTO)

t.test(Pilastras_PIC01$ALTO,Pilastras_PIR01$ALTO)

t.test(Pilastras_PIC01$ALTO,Pilastras_PIR02$ALTO)

t.test(Pilastras_PIC02$ALTO,Pilastras_PIR01$ALTO)

t.test(Pilastras_PIC02$ALTO,Pilastras_PIR02$ALTO)

t.test(Pilastras_PIR01$ALTO,Pilastras_PIR02$ALTO)



Pilastras_copia %>% group_by(TIPO)%>%summarise(MEDIANA=median(ALTO))

# Agrupar por TIPOS y sacar el promedio del ANCHO de cada TIPO 
Pilastras_copia %>% group_by(TIPO)%>%summarise(PROM_ANCHO=mean(ANCHO))

#Repetimos mismo proceso que con el ALTO - t.test para determinar la existencia o no de una correlación por 
#la anchura de las pilastras --> resultado = PIR02 diferente, hay una diferencia significativa de este tipo por la variable anchura

t.test(Pilastras_PIC01$ANCHO,Pilastras_PIC02$ANCHO)

t.test(Pilastras_PIC01$ANCHO,Pilastras_PIR01$ANCHO)

t.test(Pilastras_PIC01$ANCHO,Pilastras_PIR02$ANCHO)

t.test(Pilastras_PIC02$ANCHO,Pilastras_PIR01$ANCHO)

t.test(Pilastras_PIC02$ANCHO,Pilastras_PIR02$ANCHO)

t.test(Pilastras_PIR01$ANCHO,Pilastras_PIR02$ANCHO)

# COCONS

Cocons_copia <- read.csv2("CSV/Coco - copia.csv", header=TRUE, sep = ",", na.strings = "")

#Convertimos a factor la variable categórica de "SITIO" y a número las variables numéricas de las medidas porque las ha leído como texto

Cocons_copia$SITIO=as.factor(Cocons_copia$SITIO)

Cocons_copia$DIAMETRO_LARGO=as.numeric(Cocons_copia$DIAMETRO_LARGO)

Cocons_copia$DIAMETRO_CORTO=as.numeric(Cocons_copia$DIAMETRO_CORTO)

#Correlación entre DIAMETRO LARGO Y DIAMETRO CORTO

(CORRELATION_TEST=cor.test(Cocons_copia$DIAMETRO_LARGO,Cocons_copia$DIAMETRO_CORTO)$p.value)

#el resultado muestra un p-value de 1.167197e-16,valores más altos de 0.05, por lo tanto se rechaza la hipótesis nula que refiere que no hay diferencias significativas entre ambas variables
## En este caso además la estimación es muy significativa y que existe una relación entre las dos variables

#Para ver diferencias por sitios, si la correlación es la misma según el conjunto de necrópolis (primero hemos copiado el archivo y eleminado los casos que tuviesen menos de dos observaciones)

Cocons_sitios <- read.csv("CSV/Coco - sitios.csv", header = TRUE, sep = ",",na.strings = "")
Cocons_sitios %>% summarise(CORRELATION_TEST=cor.test(DIAMETRO_LARGO,DIAMETRO_CORTO)$p.value)

Cocons_sitios %>% group_by(SITIO) %>% summarise(CORRELACION_test=cor.test(DIAMETRO_LARGO,DIAMETRO_CORTO)$p.value)

# En los resultados de correlación de medidas vemos que el conjunto de Alcaidús presenta valores muy por encima del 0.05, lo cual
#significa que aceptamos la hipótesis nula de que ambas variables son iguales al no presentar disonancia entre las mismas. Únicamente se rechaza la hipótesis nula
### en los casos de Algendar_Pas_revull, Cales_Coves, Torrepetxina y Torreta_Saura al presentar un p.value por debajo del 0.05 --> puede deberse a la degradación y factores erosivos

#Como creemos que una ligera variación en las medidas pudo haber sido significativa en los t.test decidimos escoger únicamente una varible y comparar su comportamiento por tipos


#PILARES

str(Pilares_copia)
table(Pilares_copia$TIPO)

#Como tratamos con una variables categórica (TIPO) la tenemos que convertir en factor con la función as.factor

Pilares_copia$TIPO=as.factor(Pilares_copia$TIPO)

#Agrupamos pilastras por TIPO y pedimos que nos saque el promedio de la altura

Pilares_copia %>% group_by(TIPO)%>%summarise(PROM_ALTURA=mean(ALTO))

#Agrupamos pilastras por TIPO y pedimos que nos saque el promedio del ancho

Pilares_copia %>% group_by(TIPO)%>%summarise(PROM_ANCHO=mean(ANCHO))

#Creamos 4 subconjuntos (uno por cada tipo) ya que el test estadístico aov y el anova nos daban error

Pilares_PC001 = Pilares_copia %>% filter(TIPO=="PC001")
Pilares_PC002 = Pilares_copia %>% filter(TIPO=="PC002")
Pilares_PR001 = Pilares_copia %>% filter(TIPO=="PR001")
Pilares_PR002 = Pilares_copia %>% filter(TIPO=="PR002")
Pilares_PR003 = Pilares_copia %>% filter(TIPO=="PR003")

#Ejecutamos el t.test para determinar la existencia de correlación entre la altura de los diferentes tipo


t.test(Pilares_PC001$ALTO,Pilares_PC002$ALTO)

t.test(Pilares_PC001$ALTO,Pilares_PR001$ALTO)

t.test(Pilares_PC001$ALTO,Pilares_PR002$ALTO)

t.test(Pilares_PC001$ALTO,Pilares_PR003$ALTO)

t.test(Pilares_PC002$ALTO,Pilares_PR001$ALTO)

t.test(Pilares_PC002$ALTO,Pilares_PR002$ALTO)

t.test(Pilares_PC002$ALTO,Pilares_PR003$ALTO)

t.test(Pilares_PR001$ALTO,Pilares_PR002$ALTO)

t.test(Pilares_PR001$ALTO,Pilares_PR003$ALTO)

t.test(Pilares_PR002$ALTO,Pilares_PR003$ALTO)

#Ejecutamos el t.test para determinar la existencia de correlación entre la anchura de los diferentes tipos


t.test(Pilares_PC001$ANCHO,Pilares_PC002$ANCHO)

t.test(Pilares_PC001$ANCHO,Pilares_PR001$ANCHO)

t.test(Pilares_PC001$ANCHO,Pilares_PR002$ANCHO)

t.test(Pilares_PC001$ANCHO,Pilares_PR003$ANCHO)

t.test(Pilares_PC002$ANCHO,Pilares_PR001$ANCHO)

t.test(Pilares_PC002$ANCHO,Pilares_PR002$ANCHO)

t.test(Pilares_PC002$ANCHO,Pilares_PR003$ANCHO)

t.test(Pilares_PR001$ANCHO,Pilares_PR002$ANCHO)

t.test(Pilares_PR001$ANCHO,Pilares_PR003$ANCHO)

t.test(Pilares_PR002$ANCHO,Pilares_PR003$ANCHO)


# SITJOTS

(CORRELATION_TEST=cor.test(Sitjot$DIAMETRO_LARGO,Sitjot$DIAMETRO_CORTO))

(CORRELATION_TEST=cor.test(Sitjot$DIAMETRO_LARGO,Sitjot$DIAMETRO_CORTO)$p.value)

Sitjots_copia %>% group_by(SITIO) %>% summarise(CORRELACION_test=cor.test(DIAMETRO_LARGO,DIAMETRO_CORTO)$p.value)



#el resultado muestra un p-value de 5.413412e-12,valores más altos de 0.05, por lo tanto se rechaza la hipótesis nula que refiere que no hay diferencias significativas entre ambas variables
## En este caso además la estimación es muy significativa y que existe una relación entre las dos variables

#FOSAS

Fosas_sitio <- read.csv("CSV/Fosas - sitio.csv", header=TRUE, sep = ",", na.strings = "")

Fosas_sitio$SITIO=as.factor(Fosas_sitio$SITIO)

(CORRELATION_TEST=cor.test(Fosa_copia$ALTO,Fosa_copia$LARGO)$p.value)

#agrupamos fosas por SITIO y pedimos que nos saque el promedio de la altura
Fosas_sitio %>% group_by(SITIO)%>%summarise(PROM_ANCHO=mean(ALTO))
#agrupamos pilastras por SITIO y pedimos que nos saque el promedio del ancho
Fosas_sitio %>% group_by(SITIO)%>%summarise(PROM_LARGO=mean(LARGO))

# BOXPLOT CON GGPLOT DE PILARES (ALTURAS X TIPOS)

Representacion_pilares <- Pilares_copia |>
  dplyr::select(ALTO,
                ANCHO,
                TIPO)


ggplot(data=Representacion_pilares,
       mapping = aes(x=TIPO,
                     y=ALTO,
                     fill=TIPO)) +
  geom_boxplot() +
  scale_color_hue(c=45) +
  ggtitle("ALTURA DE PILARES POR TIPOS") +
  theme(plot.title = element_text(family ="caladea", face="bold", size=20),
        axis.title.x = element_text(family = "caladea", face = "bold", size = 16),
        axis.title.y = element_text(family = "caladea", face = "bold", size = 16),
        legend.title = element_text(family = "caladea", face = "bold", size = 16))+
  labs(color="TIPO",x="Altura",y="Tipo")
      
# Boxplot con distribución de casos de ALTURA PILARES X TIPOS
boxplot(Pilares$ALTO ~ Pilares$TIPO,
        main = "ALTURA PILARES POR TIPO",
        cex.main = 2,
        xlab = "Tipos",family= "caladea", face= "bold", size = 16,
        ylab = "Altura", family = "caladea",face= "bold", size = 16,
        cex.lab=1.6)

stripchart(Pilares$ALTO ~ Pilares$TIPO, vertical = TRUE, method = "jitter",
           pch = 16, add = TRUE, col = 45:length(levels(Pilares$TIPO)))

# Boxplot con distribución de casos de ANCHURA PILARES X TIPOS

boxplot(Pilares$ANCHO ~ Pilares$TIPO,
        main = "ANCHURA PILARES POR TIPO",
        cex.main = 2,
        xlab = "Tipos",family= "caladea", face= "bold", size = 16,
        ylab = "Anchura", family = "caladea",face= "bold", size = 16,
        cex.lab=1.6)

stripchart(Pilares$ANCHO ~ Pilares$TIPO, vertical = TRUE, method = "jitter",
           pch = 16, add = TRUE, col = 45:length(levels(Pilares$TIPO)))

# Correlación alto x ancho pilares

Pilares_sitios <- read.csv("CSV/Pilares - sitios.csv", header = TRUE, sep = ",", na.strings = "")
Pilares_sitios %>% group_by(TIPO) %>% summarise(CORRELACION_test=cor.test(ALTO,ANCHO)$p.value)

# Gráfico represnetativo del tipo de pilar PC002 (pilar curvo sin capitel) 
Pilares_copia %>%filter(TIPO=="PC002") %>%
  ggplot2::ggplot() +
  geom_point(mapping = aes(x=ANCHO,
                           y=ALTO), size=3.5,color="deepskyblue") +
  ggtitle("ALTURA Y ANCHURA DE PILARES CURVOS SIN CAPITEL") +
  theme_bw() +
  theme(plot.title = element_text(family ="caladea", face="bold", size=18, hjust = 0.5),
        axis.title.x = element_text(family = "caladea", face = "bold", size = 16),
        axis.title.y = element_text(family = "caladea", face = "bold", size = 16),
        legend.title = element_text(family = "caladea", face= "bold", size = 18)) +
  labs(x="Anchura",y="Altura")

#Histograma Volumenes - años --> necrópolis de Cales Coves

VOLUMENES <- read.csv("CSV/Calculo_volumenes.csv", header = TRUE, sep = ";", na.strings = "")

VOLUMENES <- VOLUMENES %>%select(, 1:7)

str(VOLUMENES)

VOLUMENES$ ï..Nombre=as.factor(VOLUMENES$ ï..Nombre)
VOLUMENES$CODIGO=as.factor(VOLUMENES$CODIGO)
VOLUMENES$Volumen_m3=as.numeric(VOLUMENES$Volumen_m3)
VOLUMENES$Area_m2=as.numeric(VOLUMENES$Area_m2)
VOLUMENES$Dias=as.numeric(VOLUMENES$Dias)
VOLUMENES$Meses=as.numeric(VOLUMENES$Meses)
VOLUMENES$AÃ.os=as.numeric(VOLUMENES$AÃ.os)



Volumenes_CCO <- VOLUMENES %>% filter(VOLUMENES$ï..Nombre=="Cales Coves")


ggplot(Volumenes_CCO,aes(x=Volumen_m3,y=AÃ.os))+
  geom_bar(width = 2, stat ="identity",
           position = position_dodge(),
          )+
  geom_text(aes(label=CODIGO),    
            vjust=0.5,                         
            color="black",                     
            hjust=-0.5,                         
            #position = position_dodge(1.9),   
            angle=90,                           
            size=2                            
            ) + 
  theme(axis.title.x = element_text(family = "caladea", face = "bold", size = 8),
        axis.title.y = element_text(family = "caladea", face = "bold", size = 8),
        legend.title = element_text(family = "caladea", face= "bold", size = 18))+
  scale_y_continuous(limits=c(0,20))+ # Cortes del eje Y del 0 al 20 cada 2.5 puntos de distancia
  scale_x_continuous(limits=c(0,400))+ # Cortes del eje X del 0 al 400 cada 10 puntos de distancia
  labs(x="Volúmenes",y="Años",fill="CODIGO")+
  theme_pubr(base_size = 12, base_family = "Caladea")+
  labs_pubr(base_size = 11, base_family = "Caladea")


ggplot(Volumenes_CCO,aes(x=Volumen_m3,y=AÃ.os, fill= "Color"))+
  geom_bar(width = 2, stat ="identity",
           position = position_dodge(),
          )+
  guides(fill = "none")+ # Para quitar la leyenda
  scale_y_continuous(limits=c(0,20))+ # Cortes del eje Y del 0 al 20 cada 2.5 puntos de distancia
  scale_x_continuous(limits=c(0,400))+ # Cortes del eje X del 0 al 400 cada 10 puntos de distancia
  labs(x="Volúmenes",y="Años")+
  theme_pubr(base_size = 12, base_family = "Caladea")+
  labs_pubr(base_size = 11, base_family = "Caladea")


VOLUMENES_general <- read.csv("CSV/Calculo_volumenes.csv", header = TRUE, sep = ";", na.strings = "")

Volumenes_simplificado <- VOLUMENES_general %>% select(VOLUMENES, 
                                               DIAS,
                                               MESES,
                                               AÃ.OS,
                                               NUMERO.TRABAJADORES)

Volumenes_simplificado <- na.omit(Volumenes_simplificado)

Volumenes_simplificado %>%
  arrange(VOLUMENES)



 ggplot(Volumenes_simplificado,aes(x=VOLUMENES, y=AÃ.OS))+
  geom_bar(position = "dodge", stat = "identity")+
  theme(axis.title.x = element_text(family = "caladea", face = "bold", size = 8),
        axis.title.y = element_text(family = "caladea", face = "bold", size = 8),
        legend.title = element_text(family = "caladea", face= "bold", size = 18))+
  scale_y_continuous(breaks=seq(0, 20, 2.5))+ # Cortes del eje Y del 0 al 20 cada 2.5 puntos de distancia
  scale_x_continuous(breaks=seq(0, 400, 10))+ # Cortes del eje X del 0 al 400 cada 10 puntos de distancia
  theme(legend.position = "none") + 
  labs(x="Volúmenes (m3)",y="Años",fill="")+
  theme_pubr(base_size = 12, base_family = "Caladea")+
  labs_pubr(base_size = 11, base_family = "Caladea")+
  coord_flip()



