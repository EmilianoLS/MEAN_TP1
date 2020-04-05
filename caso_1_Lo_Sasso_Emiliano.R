# Solucion 1: Tomando las probabilidades de x numero de aciertos
# Se hace un sampleo de las distintas cantidades de aciertos, con las probabilidades correspondientes a cada una, repitiendo 
# el sample 100000 veces

sol1_hist <- sample(c(0,1,2,3,4), size=100000
            , prob=c(1/70, 16/70, 36/70,16/70,1/70), replace=TRUE)


# Solucion 2: Recreacion del experimento de las tazas.

# La siguiente función hace un sample de las 8 tazas (4 con milk-first y 4 con tea-first), tomando una muestra de 4 tazas
# y devuelve la cantidad de casos exitosos.

simulacion <- function(){
  
  tazas = c(1,1,1,1,0,0,0,0)
  X = sample(tazas ,4, replace=FALSE)
  aciertos = sum(X)
  return(aciertos)
}


sol2_hist <- replicate(n = 100000,simulacion())


# Ploteamos ambas soluciones

par(mfrow=c(2,1))

hist(sol1_hist, main = 'Solucion 1')
hist(sol2_hist, main = 'Solucion 2')

par(mfrow=c(1,1))

# Se agrega las proporciones de cada cantidad de aciertos para mostrar que son muy similares a las calculadas en la solucion 1

prop.table(table(replicate(n = 100000,simulacion())))
