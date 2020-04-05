rm(list=ls())

library(lubridate)
library(ggplot2)
library(dplyr)

setwd('C:/Users/elosasso/OneDrive - Universidad Torcuato Di Tella/Metodos estadisticos aplicados a negocios/TP1')

df <- read.csv('recorridos-realizados-2018.csv')

head(df)
colnames(df)
str(df)

# Se observa que algunas variables están mal declaradas en sus tipos. 
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
# Obtención de los días de la semana en los que se retiraron y devolvieron bicicletas, al igual que
# la hora del día en la que se extrajeron y devolvieron

df$dia_de_semana_origen   <- wday(df$fecha_origen_recorrido, label = TRUE,
                              abbr = FALSE)

df$hora_del_dia_origen <- hour(df$fecha_origen_recorrido)

df$mes_de_origen <- month(df$fecha_origen_recorrido, label = TRUE,
                          abbr = FALSE)

####################################################################################################
# Realización de algunos gráficos descriptivos del conjunto de datos

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
# El error puede deberse a que la distancia medida en la consigna era en línea recta, y la formula usada no usa exactamente
# la misma forma de calculo

######################################################################################################################
# Evaluo el tiempo del recorrido, si bien hay una columna con la información, se analiza que tan acertada es

df$duracion_recorrido_manual <- as.numeric((df$fecha_destino_recorrido - df$fecha_origen_recorrido), units = 'secs')


ggplot(filter(df,(id_estacion_origen == 009) & (id_estacion_destino == 066))) + 
         geom_density(aes(x = duracion_recorrido_manual),fill = 'red', alpha = 0.3) +
         geom_density(aes(x = duracion_recorrido),fill = 'green', alpha = 0.3) +
         ggtitle('Distribución de la duración de los recorridos')


df$delta_duracion <- df$duracion_recorrido - df$duracion_recorrido_manual

ggplot(filter(df,(id_estacion_origen == 009) & (id_estacion_destino == 066))) + 
  geom_density(aes(x = delta_duracion), na.rm = TRUE, fill = 'red', alpha = 0.3)+
  ggtitle('Distribucion de la diferencia entre la duracion dada y la calculada') + xlab('Diferencia de tiempo (s)')

# Consistentemente, el tiempo dado de duración de uso de la bicicleta, tiene 298 segundos ~ 5minutos de menos que el tiempo
# calculado. Recordar que el tiempo calculado se estimo haciendo la diferencia entre la hora de extración y devolución
# Se decide hacer el test de hipotesis con ambas variables, para poder ver cuanto variará el resultado (se refutara o no la hipotesis)

######################################################################################################################

# Hago la prueba de hipotesis
# Creo una columna que indique si tardo mas de 15 minutos

df$mas_15_min <- df$duracion_recorrido_manual > 15*60

# Calculo la proporcion para aquellos que realizaron el viaje entre las estaciones 009 y 006

phat <- as.numeric(prop.table(table(select(filter(df,(id_estacion_origen == 009) & (id_estacion_destino == 066)), mas_15_min)))[2])


n <- nrow(filter(df,(id_estacion_origen == 009) & (id_estacion_destino == 066)))
#phat <- 0.53

zstat <- (phat-0.2)/sqrt(0.2*(1-0.2)/n); zstat

zcrit <- qnorm(0.95); zcrit

# Chequeo si el z estadistico es menor al critico (lo que indicaría que no puede refutarse la hipotesis nula)

if (zstat < zcrit){
  
  print('No se refuta la hipótesis nula: La proporción de usuarios que tardan más de 15 minutos no supera el 20%')
}else{
  
  print('Se refuta la hipótesis nula: La proporción de usuarios que tardan más de 15 minutos no supera el 20%')
}
  
######################################################################################################################
# Estudio la potencia del test










