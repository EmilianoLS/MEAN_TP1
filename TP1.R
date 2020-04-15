# Metodos estadisticos aplicados a negocios
# Trabajo practico 1
# Eloy Chang

# ----> librerias <----

library(plotly)
library(dplyr)
library(boot)

# ----> Leer Datos <----

leerDatosRecorridos<- function(dir){
    recorridos<- read.csv(paste(dir,"recorridos-realizados-2018.csv", sep = ""))
    recorridos$fecha_origen_recorrido<- as.POSIXct(as.character(recorridos$fecha_origen_recorrido))
    #recorridos$fecha_destino_recorrido<- as.POSIXct(as.character(recorridos$fecha_destino_recorrido))
    return(recorridos)
}

direccion<- "datasets/" # Direccion en la que se encuentran los datasets
recorridos<- leerDatosRecorridos(direccion)
save(recorridos, file = "datasets/recorridos.RData")
load("datasets/recorridos.RData")

# ----> Analisis <----

# ----> Parte 1 <----

distGenero<- as.data.frame(recorridos %>% group_by(genero_usuario) %>%
                               summarise(cantidad = length(genero_usuario)))
plot_ly(distGenero, x = ~genero_usuario, y = ~cantidad, type = "bar") %>%
    layout(title = "Distribucion de viajes por genero",
           xaxis = list(title = "Genero"), yaxis = list(title = "Numero de viajes"))

distGenero<- as.data.frame(recorridos %>% group_by(genero_usuario) %>%
                               summarise(cantidad = length(unique(id_usuario))))
plot_ly(distGenero, x = ~genero_usuario, y = ~cantidad, type = "bar") %>%
    layout(title = "Distribucion de usuarios activos por genero",
           xaxis = list(title = "Genero"), yaxis = list(title = "Numero de usuarios activos"))


evoViajes<- as.data.frame(recorridos %>%
                              group_by(fecha = as.Date(fecha_origen_recorrido)) %>%
                              summarise(cantidad = length(fecha_origen_recorrido)))
plot_ly(evoViajes, x = ~fecha, y = ~cantidad, type = "scatter", mode = "line") %>%
    layout(title = "Evolucion de numero de viajes",
           xaxis = list(title = "Fecha"), yaxis = list(title = "Numero de viajes"))

plot_ly(recorridos, x = ~round(as.numeric(recorridos$duracion_recorrido)/60),
        type = "histogram", histnorm = "probability") %>%
    layout(title = "Histograma de duracion del recorrido",
           yaxis = list(title = "Distribucion"),
           xaxis = list(title = "Minutos"))

# ----> Parte 4 <----

posUsuario<- recorridos$id_usuario == 606320
recorridosCiclista<- recorridos[posUsuario,] %>%
    group_by(id_estacion_origen,lat_estacion_origen,long_estacion_origen,
             id_estacion_destino,lat_estacion_destino,long_estacion_destino) %>%
    summarise(recorridos = length(id_estacion_origen),
              tiempoRecorrido = median(as.numeric(duracion_recorrido)))
recorridosCiclista<- as.data.frame(recorridosCiclista)
recorridosCiclista<- recorridosCiclista[complete.cases(recorridosCiclista),]

# > recorridosCiclista$kilometros<- 0
# > recorridosCiclista$kilometros[1] <- 3.5
# > recorridosCiclista$kilometros[2] <- 2.6
# > recorridosCiclista$kilometros[3] <- 2.1
# > recorridosCiclista$kilometros[4] <- 2.1
# > recorridosCiclista$kilometros[5] <- 0.9
# > recorridosCiclista$kilometros[6] <- 1.5
# > recorridosCiclista$kilometros[7] <- 3.7
# > recorridosCiclista$kilometros[8] <- 2.9
# > recorridosCiclista$kilometros[9] <- 0.9
# > recorridosCiclista$kilometros[11] <- 2.1
# > recorridosCiclista$kilometros[12] <- 1.8
# > recorridosCiclista$kilometros[13] <- 0.65
# > recorridosCiclista$kilometros[14] <- 3.9
# > recorridosCiclista$kilometros[15] <- 0.9
# > recorridosCiclista$kilometros[16] <- 3.0
# > recorridosCiclista$kilometros[17] <- 1.3
# > recorridosCiclista$kilometros[18] <- 3.7
# > recorridosCiclista$kilometros[19] <- 3
# > recorridosCiclista$kilometros[20] <- 2.9
# > recorridosCiclista$kilometros[21] <- 2.8
# > recorridosCiclista$kilometros[22] <- 2.4
# > recorridosCiclista$kilometros[23] <- 1.1
# > recorridosCiclista$kilometros[24] <- 3.1
# > recorridosCiclista$kilometros[25] <- 2.9
# > recorridosCiclista$kilometros[26] <- 2.9
# > recorridosCiclista$kilometros[27] <- 2

recorridos$velocidad<- 0
for(pos in which(posUsuario)){
    if(is.na(recorridos$id_estacion_origen[pos]) | is.na(recorridos$id_estacion_destino[pos])){
        next
    }
    duracion<- as.numeric(recorridos$duracion_recorrido[pos])/3600
    posRuta<- recorridosCiclista$id_estacion_origen == recorridos$id_estacion_origen[pos] &
        recorridosCiclista$id_estacion_destino == recorridos$id_estacion_destino[pos]
    kilometros<- recorridosCiclista$kilometros[posRuta]
    recorridos$velocidad[pos]<- kilometros / duracion
}
posUsuario<- posUsuario & recorridos$velocidad > 0
mediaArmonica<- 1/mean(1/recorridos$velocidad[posUsuario])
mediaAritmetica<- mean(recorridos$velocidad[posUsuario])

# ----> Parte 7 <----

recorridos<- recorridos[!(is.na(recorridos$id_estacion_origen) |
                              is.na(recorridos$id_estacion_destino)),]
recorridos$mes<- as.POSIXlt(recorridos$fecha_origen_recorrido)$mon
recorridos$fecha_origen<- as.Date(recorridos$fecha_origen_recorrido)
recorridos$fecha_destino<- as.Date(recorridos$fecha_destino_recorrido)
recorridos$facultad_derecho<- recorridos$id_estacion_destino == 1 |
    recorridos$id_estacion_origen == 1
recorridosAntes<- recorridos[recorridos$facultad_derecho &
                                 recorridos$mes < 5,]
recorridosDespues<- recorridos[recorridos$facultad_derecho &
                                   recorridos$mes > 4,]

viajesDiariosAntes<- recorridosAntes %>% group_by(fecha_origen) %>%
    summarise(viajes = length(fecha_origen))
viajesDiariosAntes<- as.data.frame(viajesDiariosAntes)
viajesDiariosDespues<- recorridosDespues %>% group_by(fecha_origen) %>%
    summarise(viajes = length(fecha_origen))
viajesDiariosDespues<- as.data.frame(viajesDiariosDespues)

t.test(viajesDiariosAntes$viajes,viajesDiariosDespues$viajes)
t.test(viajesDiariosAntes$viajes,viajesDiariosDespues$viajes[1:151])
t.test(viajesDiariosAntes$viajes[92:151],viajesDiariosDespues$viajes[1:60])

plot_ly(viajesDiariosDespues, type = "scatter", mode = "line",
        x = ~ fecha_origen, y = ~viajes)

# ----> Parte 10 <----

usuarios<- read.csv("datasets/usuarios-ecobici-2018.csv")

cat(paste("Catidad de usuarios en dataframe:",length(unique(recorridos$id_usuario)),
          "\nCatidad de usuarios:",nrow(usuarios),
          "\nCantidad de usuarios en ambos DF:",sum(unique(recorridos$id_usuario) %in% usuarios$usuario_id)))

archivos<- c("datasets/usuarios-ecobici-2017.csv",
             "datasets/usuarios-ecobici-2016.csv",
             "datasets/usuarios-ecobici-2015.csv")
for(archivo in archivos){
    aux<- read.csv(archivo)
    usuarios<- rbind(usuarios,aux)
}

cat(paste("Catidad de usuarios en dataframe:",length(unique(recorridos$id_usuario)),
          "\nCatidad de usuarios:",nrow(usuarios),
          "\nCantidad de usuarios en ambos DF:",sum(unique(recorridos$id_usuario) %in% usuarios$usuario_id)))
rm(aux)
gc()

datosEdad<- merge(recorridos[recorridos$id_estacion_origen == 9 & recorridos$id_estacion_destino == 66,
                             c("id_usuario","duracion_recorrido")],
                  usuarios[,c("usuario_id","usuario_edad")],
                  by.x = "id_usuario", by.y = "usuario_id")

datosEdad$duracion_recorrido<- as.numeric(datosEdad$duracion_recorrido)

fc_cor <- function(d,i){
    d <- datosEdad[i,]
    return(cor(d$duracion_recorrido,d$usuario_edad))
}

boot_cor <- boot(data = datosEdad, statistic = fc_cor, R = 1000)

plot(boot_cor)

boot.ci(boot.out = boot_cor, type = c("norm", "basic", "perc","bca"))

