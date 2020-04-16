rm(list=ls())

library(lubridate)
library(ggplot2)
library(dplyr)
library('rmarkdown')
library(pwr)
library(boot)

setwd('C:/Users/elosasso/OneDrive - Universidad Torcuato Di Tella/Metodos estadisticos aplicados a negocios/TP1')

df <- read.csv('recorridos-realizados-2018.csv')
users_2018 <- read.csv('usuarios-ecobici-2018.csv')
users_2017 <- read.csv('usuarios-ecobici-2017.csv')
users_2016 <- read.csv('usuarios-ecobici-2016.csv')
users_2015 <- read.csv('usuarios-ecobici-2015.csv')

users <- rbind(users_2018,users_2017,users_2016,users_2015)

head(df)
colnames(df)
str(df)

# Se observa que algunas variables est谩n mal declaradas en sus tipos. 
# fecha_origen_recorrido es un factor y debe ser un tipo date
# duracion_recorrido es un factor y debe ser un tipo number
# fecha_destino_recorrido es un factor y debe ser un tipo date


df$fecha_origen_recorrido <- as.POSIXct(strptime(df$fecha_origen_recorrido, format = "%Y-%m-%d %H:%M:%OS",
                                      tz = "America/Argentina/Buenos_Aires"))

df$duracion_recorrido <- as.numeric(df$duracion_recorrido, units = 'secs') #Hay que verificar si estan bien calculadas!

df$fecha_destino_recorrido <- as.POSIXct(strptime(df$fecha_destino_recorrido, format = "%Y-%m-%d %H:%M:%OS",
                                      tz = "America/Argentina/Buenos_Aires"))

#summary(df)

####################################################################################################
# Obtenci贸n de los d铆as de la semana en los que se retiraron y devolvieron bicicletas, al igual que
# la hora del d铆a en la que se extrajeron y devolvieron

df$dia_de_semana_origen   <- wday(df$fecha_origen_recorrido, label = TRUE,
                              abbr = FALSE)

df$hora_del_dia_origen <- hour(df$fecha_origen_recorrido)

df$mes_de_origen <- month(df$fecha_origen_recorrido, label = TRUE,
                          abbr = FALSE)

####################################################################################################
# Realizaci贸n de algunos gr谩ficos descriptivos del conjunto de datos

jpeg("distribucion_uso.jpg") 
boxplot(df$duracion_recorrido, xlab = 'Duracion del recorrido (seg)', main = 'Distribucion del tiempo de uso', horizontal = TRUE)
dev.off()


ggplot(data = df) + geom_bar(mapping = aes(x = hora_del_dia_origen)) + facet_wrap(~ dia_de_semana_origen)

#######################################################################################################
# Creo una columna para identificar si empezo y termino el viaje en la misma estacion

df$misma_estacion <- df$nombre_estacion_origen == df$nombre_estacion_destino

bicis = as.data.frame(df)

##############################################################################################################
# Esta parte unicamente se hace para ver que tan "comun" es el recorrido que propone la consigna

# Este groupby me da un top de los recorridos mas populares

bicis %>% filter((!is.na(id_estacion_origen)) & (!is.na(id_estacion_destino)) & (misma_estacion == FALSE)) %>% 
  group_by(nombre_estacion_origen,nombre_estacion_destino) %>%  
  summarize(count = n()) %>% arrange(desc(count)) %>% ungroup()

# Este groupby me devuelve la cantidad de viajes hechos entre las estaciones 009 y 066

bicis %>% filter((id_estacion_origen == 009) & (id_estacion_destino == 066)) %>% select(nombre_estacion_origen,nombre_estacion_destino) %>% 
  group_by(nombre_estacion_origen,nombre_estacion_destino) %>% summarize(count = n()) %>% ungroup()

#############################################################################################################

# Creo una columna con la distancia recorrida, para verificar la informacion de distancia promedio entre estaciones

df$lat_estacion_origen <- df$lat_estacion_origen * pi / 180
df$lat_estacion_destino <- df$lat_estacion_destino * pi / 180
df$long_estacion_origen <- df$long_estacion_origen * pi / 180
df$long_estacion_destino <- df$long_estacion_destino * pi / 180

# Al haber valores en NA, el calculo de la distancia dara error, creo un dataframe sin Na

df_filtered <- filter(df, (complete.cases(df$id_estacion_destino)) & (complete.cases(df$id_estacion_origen)))

df_filtered$Distance <- acos(sin(df_filtered$lat_estacion_origen) * sin(df_filtered$lat_estacion_destino) + 
                      cos(df_filtered$lat_estacion_origen)*cos(df_filtered$lat_estacion_destino) * cos(df_filtered$long_estacion_destino - df_filtered$long_estacion_origen)) * 6371


mean(filter(df_filtered,(id_estacion_origen == 009) & (id_estacion_destino == 066))[,'Distance'])

# La distancia promedio entre las dos estaciones es de ~1.35 km, similar a los 1.5 km que informaba la consigna
# El error puede deberse a que la distancia medida en la consigna era en l铆nea recta, y la formula usada no usa exactamente
# la misma forma de calculo

######################################################################################################################
# Evaluo el tiempo del recorrido, si bien hay una columna con la informaci贸n, se analiza que tan acertada es

df$duracion_recorrido_manual <- as.numeric((df$fecha_destino_recorrido - df$fecha_origen_recorrido), units = 'secs')


ggplot(filter(df,(id_estacion_origen == 009) & (id_estacion_destino == 066))) + 
         geom_density(aes(x = duracion_recorrido_manual),fill = 'red', alpha = 0.3) +
         geom_density(aes(x = duracion_recorrido),fill = 'green', alpha = 0.3) +
         ggtitle('Distribuci贸n de la duraci贸n de los recorridos')


df$delta_duracion <- df$duracion_recorrido - df$duracion_recorrido_manual

ggplot(filter(df,(id_estacion_origen == 009) & (id_estacion_destino == 066))) + 
  geom_density(aes(x = delta_duracion), na.rm = TRUE, fill = 'red', alpha = 0.3)+
  ggtitle('Distribucion de la diferencia entre la duracion dada y la calculada') + xlab('Diferencia de tiempo (s)')

# Consistentemente, el tiempo dado de duraci贸n de uso de la bicicleta, tiene 298 segundos ~ 5minutos de menos que el tiempo
# calculado. Recordar que el tiempo calculado se estimo haciendo la diferencia entre la hora de extraci贸n y devoluci贸n
# Se decide hacer el test de hipotesis con ambas variables, para poder ver cuanto variar谩 el resultado (se refutara o no la hipotesis)

######################################################################################################################

# Hago la prueba de hipotesis
# Creo una columna que indique si tardo mas de 15 minutos

df$mas_15_min <- df$duracion_recorrido_manual > 15*60

# Hip髏esis nula H0:        La proporci髇 de usuarios que tardan mas de 15 minutos en hacer el recorrido de la estacion 009 a la 066
#                           es mayor o igual al 20%
# Hip髏esis alternativa H1: La proporci髇 de usuarios que tardan mas de 15 minutos en hacer el recorrido de la estacion 009 a la 066
#                           es menor al 20%

# Para hacer el test se aplica un binom.test con un nivel de significacion del 0.05

binom.test( x = nrow(filter(df,id_estacion_origen == 009, id_estacion_destino == 066,mas_15_min == TRUE)), 
            n = nrow(filter(df,id_estacion_origen == 009, id_estacion_destino == 066)), 
            p = 0.2, alternative = 'less')

# El p-value da 0.06669 > a 0.05 del alpha, por lo que no puede rechazarse la hipotesis nula
  
######################################################################################################################
# Estudio la potencia del test

n = nrow(filter(df,id_estacion_origen == 009, id_estacion_destino == 066))

mustar <- seq(from = 0.1, to=0.2, by=0.001)

xcrit <- qnorm(0.05)*sqrt((0.2*(1-0.2))/n)+0.2

beta <- 1-pnorm((xcrit-mustar)/sqrt((mustar*(1-mustar))/n))

potencia <- 1-beta


plot(mustar,potencia, ylim=c(0,1),type = "l", xlab= "mu*", ylab= "potencia", lwd=2)
#abline(h = potencia, lty = 2)
#text(0.15, potencia,  potencia,
  #   cex=1, pos=3,col="red") 

######################################################################################################################

# Boostrap

dim(users)

# Chequeo que no haya usuarios duplicados 
length(unique(users$usuario_id))

# No pareciera haber

dim(df);colnames(users)

names(users)[1] <- 'id_usuario'
df_with_users <- merge(x = df, y = users,by = 'id_usuario')
df_with_users <- select(df_with_users, -c(fecha_alta,hora_alta))

# Defino funcion de correlacion

fc_cor <- function(d,i){
  d <- df_with_users[i,]
  return(cor(d$duracion_recorrido,d$usuario_edad)) # Uso duracion de recorrido y no la manual porque tiene missings
}

#chequeando que funciona

fc_cor(df_with_users)

#usamos el comando boot:

set.seed(123) #si lo suprimo no puedo replicar los mismos resultados (variar韆n por la simulaci髇)

boot_cor <- boot(data = filter(df_with_users,id_estacion_origen == 009, id_estacion_destino == 066), statistic = fc_cor, R = 10000)
boot_cor
boot_cor$t #para ver las 1000 replicaciones
sd(boot_cor$t) #el error est醤dar
mean(boot_cor$t)-boot_cor$t0 #el sesgo
#histograma y qqplot
plot(boot_cor)
#intervalo de confianza:
boot.ci(boot.out = boot_cor, type = c("norm", "basic", "perc", "bca"))