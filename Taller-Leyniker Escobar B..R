#Leyniker Escobar
#Asignatura:  EStadística computacional

library(tidyverse)

#Punto 1----
#Genere un vector aleatorio de distribuci´on normal 
#con n´umero de observaciones 50, una media de 5 y una
#desviaci´on est´andar de 1. Use set.seed(2025). Realice los siguientes:

#i)Determine el rango de los valores en el vector.
set.seed(2025)
x<-rnorm(n=50, mean=5,sd=1)
x

#ii) Extraiga los valores que est´an por debajo de la mediana.
mediana<-x[x<median(x)]
mediana

#iii)Calcule el porcentaje de valores que est´an por encima de 6.
porcentaje<-((x/sum(x))*100)
porcentaje
sum(porcentaje)

#iv) Ord´enelos de menor a mayor.
x[order(x)]

#Punto 2----
#(a) swiss
data(swiss)
view(swiss)
head(swiss)

#b) InsectSprays
data("InsectSprays")
view(InsectSprays)
head(InsectSprays)

#El estudiante necesita ayuda para recuperarlos y analizarlos.

#(i) Identifique en qu´e paquete se encuentran estos conjuntos.
#swiss y # insectSpray: Son dataset que vienen instalados en R por defecto. 

#(ii) Proporcione una descripci´on breve de cada conjunto de datos.
#swiss: es un dataset  de R que sirve para medir, comparar o inferir alguna 
#información de la población aanlizar, puesto que son
#indicadores socieconomicos y demograficos de Suiza en el año 1888. El cual 
# contiene 47 observaciones con 6 variables. Con esto se puedo hacer modelos de 
#regresión lineal múltiple y análisis de correlación.

#InsectSprays: Es un dataset con 72 observaciones con 2 variables, que hace
#parte de R. Trata de un varios tipos de insecticidas aplicados para el control
#de insectos. Esto es usado para hacer comparaciones entre grupos


#(iii)
library(dplyr)
summary(swiss)
is.na(swiss$Fertility)
na_fertility<-sum(is.na(swiss$Fertility))
na_fertility

is.na(swiss$Education)
na_education<-sum(is.na(swiss$Education))
na_education

#Según el vector logico y el conteo no hay datos faltatnes en la variable
#fertility o education.

swiss %>%
  group_by(Education) %>%
  summarise (mea_fertility=mean(Fertility),
             sd_fertility=sd(Fertility),
             median_fertility=median(Fertility),
             min_fertility=min(Fertility),
             max_fertility=max(Fertility))->swiss_orden
arrange(swiss_orden)



#punto 3----
#Se define una variable statement <- "big data is transforming industries 
#worldwide". Esta variable se ha dividido en un vector que contiene 
#letras separadas y se ha almacenado en un vector chars con la funci´on
#strsplit().
#¿Puedes escribir un c´odigo que cuente el n´umero de d’s que vienen antes de 
#la primera w en statement?


statement<-"big data is transforming industries worldwide"

chars<-strsplit(statement, split = "")[[1]]
chars
contar_1raw<-which(chars=="w")
contar_1raw

rcount<-0
for(char in chars){
    if(char =="w"){
     break
         }
  
else {
      if(char=="d"){
      rcount<-rcount+1
      }
}
}

#Punto 4----
#Suponga que se lanza un dado sesgado, donde la probabilidad de obtener el 
#n´umero 6 es el doble que la de cualquier otro n´umero. Realice un 
#script que simule lanzamientos hasta que el n´umero 6 salga 12 veces. Debe
#mostrar el historial de lanzamientos.

# Definir los valores del dado
dado <- 1:6
probabilidades <- c(1, 1, 1, 1, 1, 2) / sum(c(1, 1, 1, 1, 1, 2))
historial <- NULL  
conteo_seis <- 0  

while (conteo_seis < 12) {
  lanzamiento <- sample(dado, size = 1, replace = TRUE)
  historial <- c(historial, lanzamiento)
  if (lanzamiento == 6) {
    conteo_seis <- conteo_seis + 1
  }
}

print(historial)



#punto 5----
#Escriba una funci´on (ll´amela r i c) que imprima el rango intercuart´ılico 
#de una variable aleatoria con distribuci´on binomial de longitud 40, 
#con 10 ensayos y probabilidad de ´exito igual a 0.4.
#Debe calcular el rango intercuart´ılico desde su definici´on,
#sin usar la funci´on quantile dentro del cuerpo de la funci´on. 
#Dentro de la funci´on, debe utilizar paste en su programa.

set.seed(100)
  ric<-rbinom(40, 10, 0.4) 
  ric
  orden_ric<-sort(ric)
  n<-length(orden_ric)
  n
  posicion_Q1<-(0.25*n)
  posicion_Q1

  posicion_Q3<-(0.75*n)
  posicion_Q3

  Q1<-orden_ric[posicion_Q1]
  Q1
  Q3<-orden_ric[posicion_Q3]
  Q3
  IQR<-Q3-Q1
  IQR

# se verificará con la función IQR
  IQR(ric)

#punto 6----
#El conjunto de datos llamado tree growth, que se encuentra en la carpeta de 
#datos de Teams, se refiere al crecimiento de dos tipos de ´arboles frutales 
#(a51 = manzano y a52 = peral) en dos tipos de suelos (arenoso y arcilloso). 
#La variable altura indica la altura (en cm) que alcanzaron los ´arboles
#despu´es de 6 meses decultivo.
  
  #(i) Cargue los datos tree growth y muestre las 5 primeras filas. [0.2]

  tree_growth
  tree_growth$suelo<-factor(tree_growth$suelo)
  tree_growth$arbol<-factor(tree_growth$arbol)
  str(tree_growth)
  view(tree_growth)
  
  head(tree_growth, 5)
    
  #(ii) Cambie el nombre codificado de los dos tipos de ´arboles al nombre 
  #com´un correspondiente. Use dplyr.
  
library(dplyr)

tree_growth %>% 
    mutate(arbol=recode(arbol, "a51"="manzano", "a52"="peral")) %>% 
   

# (iii) Extraiga todas las observaciones en las que la altura de los ´arboles 
#sea mayor a 50 cm y muestre las 5observaciones m´as peque˜nas en t´erminos 
#de altura.

tree_growth %>% 
  mutate(arbol=recode(arbol, "a51"="manzano", "a52"="peral")) %>% 
  filter(altura>50) %>% 
  arrange(altura)


#(iv) Hay diversos fertilizantes que pueden mejorar el crecimiento de los 
#´arboles. La siguiente tabla muestra el tipo de suelo y el fertilizante 
#recomendado:

suelo=c("arenoso", "arcilloso", "arenoso", "arcilloso")
fertilizante = c("Fertilizante-A", "Fertilizante-B", "Fertilizante-C", "Fertilizante-D")
recomendacion = data.frame(suelo, fertilizante)
head(recomendacion)
View(recomendacion)
head(tree_growth)

#A partir de lo anterior, cree una nueva tabla llamada fertilized growth 
#niendo la informaci´on de los fertilizantes relevantes con la tabla de 
#datos original. Use left join.

tree_growth %>% 
  mutate(arbol=recode(arbol, "a51"="manzano", "a52"="peral"))%>% 
  left_join(recomendacion, by="suelo")->fertilized_growth
  fertilized_growth

#De los datos del ´ıtem anterior, seg´un cada tipo de fertilizante, 
#¿cu´al fue la altura promedio de los ´arboles?

fertilized_growth %>% 
  group_by(fertilizante) %>% 
  summarise(mean_altura=mean(altura))
  
