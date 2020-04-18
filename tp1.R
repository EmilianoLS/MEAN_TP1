#(1)
altas <- read.csv("C:/Users/User three/Desktop/Fede/MiM+Analytics/Materias/Modulo 1/Metodos Estadisticos Aplicados a Negocios/TP/usuarios-ecobici-2018.csv",
                  header = TRUE, sep = ",")

summary(altas)
dim(altas)
prop.table(table(altas$usuario_sexo))
#Vemos que en el 2018 se dieron de alta 56182 usuarios,
#de los cuales aproximadamente el 46% es femenino y el 54% masculino.
#El dia donde mas altas se dieron fue el 08/10/2018.
#La hora en la cual más usuarios se dan de alta es a las 5:57:45.
#La edad promedio de los usuarios es de 33 años (ojo los extremos aca, ver).

recorridos <- read.csv("C:/Users/User three/Desktop/Fede/MiM+Analytics/Materias/Modulo 1/Metodos Estadisticos Aplicados a Negocios/TP/recorridos-realizados-2018.csv",
                       header = TRUE, sep = ",")

#Creo una columna que indique el dia de la semana en el que se retiro la bici.
recorridos$dia_semana <- as.factor(strftime(recorridos$fecha_origen_recorrido, "%A"))

summary(recorridos)
dim(recorridos)
prop.table(table(recorridos$genero_usuario))
prop.table(table(recorridos$dia_semana))
#Vemos que en el 2018 se hicieron 2619968 recorridos, que tanto el origen
#como el destino más comun se da en la Facultad de Medicina y que las mujeres
#hicieron aproximadamente el 28% de los recorridos, mientras que los hombres
#el 72%. El día de la semana en el que mas bicis son retiradas es el miercoles.

#(2)
recorridos$fecha_origen_auxiliar <- as.factor(strptime(recorridos$fecha_origen_recorrido,
                                                       format = "%Y-%m-%d",
                                                       tz = "America/Argentina/Buenos_Aires"))
library("dplyr")
usuarios_diarios_año <- recorridos %>% group_by(fecha_origen_auxiliar) %>% summarise(total = length(dia_semana)) %>% ungroup()
usuarios_diarios_año$total <- as.numeric(usuarios_diarios_año$total)
usuarios_diarios_año$dia_semana <- as.factor(strftime(usuarios_diarios_año$fecha_origen_auxiliar, "%A"))
tab <- usuarios_diarios_año %>% group_by(dia_semana) %>% summarise(mu = mean(total)) %>% ungroup()
tab$z <- usuarios_diarios_año %>% group_by(dia_semana) %>% summarise(mu = qt(0.975, length(total) - 1)) %>% ungroup()
tab$std <- usuarios_diarios_año %>% group_by(dia_semana) %>% summarise(mu = sd(total)/sqrt(length(total))) %>% ungroup()
tab$lim_inf <- tab$mu - (tab$z$mu*tab$std$mu)
tab$lim_sup <- tab$mu + (tab$z$mu*tab$std$mu)

library("ggplot2")
tab$dia_semana <- factor(tab$dia_semana, levels = c("lunes","martes","miércoles","jueves","viernes","sábado","domingo"))
grafIC <- ggplot(data = tab) + geom_pointrange(mapping = aes(x = dia_semana, y = mu, ymin = lim_inf, ymax = lim_sup)) + geom_errorbar(mapping = aes(x = dia_semana, ymin = lim_inf, ymax = lim_sup))

grafIC
#En el gráfico que muestra los intervalos de confianza de la cantidad media de
#usuarios por cada dia de la semana encontramos evidencia estadistica de una
#estacionalidad en el uso de las bicis los dias de semana (lunes a viernes),
#donde vemos que los IC se solapan y son superiores a los de los dias de fin de
#semana (sabado y domingo).

#(3)
feriados <- c("2018-01-01", "2018-02-12", "2018-02-13", "2018-03-24", "2018-03-29", "2018-03-30",
              "2018-04-02", "2018-04-30", "2018-05-01", "2018-05-25", "2018-06-17", "2018-06-20",
              "2018-07-09", "2018-08-20", "2018-09-15", "2018-10-19", "2018-12-08", "2018-12-24",
              "2018-12-25", "2018-12-31")
usuarios_diarios_año$es_feriado <- factor(ifelse(usuarios_diarios_año$fecha_origen_auxiliar %in% feriados,
                                                 1, 0))

tab <- usuarios_diarios_año %>% filter(es_feriado == 0) %>% group_by(dia_semana) %>% summarise(mu = mean(total)) %>% ungroup()
tab$z <- usuarios_diarios_año %>% filter(es_feriado == 0) %>% group_by(dia_semana) %>% summarise(mu = qt(0.975, length(total) - 1)) %>% ungroup()
tab$std <- usuarios_diarios_año %>% filter(es_feriado == 0) %>% group_by(dia_semana) %>% summarise(mu = sd(total)/sqrt(length(total))) %>% ungroup()
tab$lim_inf <- tab$mu - (tab$z$mu*tab$std$mu)
tab$lim_sup <- tab$mu + (tab$z$mu*tab$std$mu)

tab$dia_semana <- factor(tab$dia_semana, levels = c("lunes","martes","miércoles","jueves","viernes","sábado","domingo"))
grafIC_sin_feriados <- ggplot(data = tab) + geom_pointrange(mapping = aes(x = dia_semana, y = mu, ymin = lim_inf, ymax = lim_sup)) + geom_errorbar(mapping = aes(x = dia_semana, ymin = lim_inf, ymax = lim_sup))

grafIC_sin_feriados
#Si descontamos del analisis los dias que fueron feriados vemos que la media de uso
#diario de bicis aumenta en general pero se mantiene la estacionalidad los dias de
#semana (lunes a viernes).

#(10)
altas2018 <- altas
altas2017 <- read.csv("C:/Users/User three/Desktop/Fede/MiM+Analytics/Materias/Modulo 1/Metodos Estadisticos Aplicados a Negocios/TP/usuarios-ecobici-2017.csv",
                      header = TRUE, sep = ",")
altas2016 <- read.csv("C:/Users/User three/Desktop/Fede/MiM+Analytics/Materias/Modulo 1/Metodos Estadisticos Aplicados a Negocios/TP/usuarios-ecobici-2016.csv",
                      header = TRUE, sep = ",")
altas2015 <- read.csv("C:/Users/User three/Desktop/Fede/MiM+Analytics/Materias/Modulo 1/Metodos Estadisticos Aplicados a Negocios/TP/usuarios-ecobici-2015.csv",
                      header = TRUE, sep = ",")

altas <- rbind(altas2018, altas2017, altas2016, altas2015)

recorridos$usuario_edad <- with(altas, usuario_edad[match(recorridos$id_usuario,
                                                          altas$usuario_id)])

library(boot)
recorridos$usuario_edad <- as.numeric(recorridos$usuario_edad)
recorridos$duracion_recorrido <- as.numeric(recorridos$duracion_recorrido)
recorridos <- recorridos[!is.na(recorridos$usuario_edad),]
recorridos <- recorridos[!is.na(recorridos$duracion_recorrido),]
fc_cor <- function(d,i){
  d <- recorridos[i,]
  return(cor(recorridos$usuario_edad,recorridos$duracion_recorrido))
}

fc_cor(recorridos)

recorridos <- recorridos %>% filter(id_estacion_origen == 9) %>% filter(id_estacion_destino == 66)
set.seed(123) #si lo suprimo no puedo replicar los mismos resultados (variarían por la simulación)
boot_cor <- boot(data = recorridos, statistic = fc_cor, R = 100)
boot_cor
boot_cor$t #para ver las 1000 replicaciones
sd(boot_cor$t) #el error estándar
mean(boot_cor$t)-boot_cor$t0 #el sesgo
#histograma y qqplot
plot(boot_cor)
#intervalo de confianza:
boot.ci(boot.out = boot_cor, type = c("norm", "basic", "perc", "bca"))
