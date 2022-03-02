### Comentatio AAE

#Soluciones Tarea 1 | Primavera 2022
# Creada por Arturo Aguilar y Miguel Ángel Negrete Flores

# Para ejecutar este script desde su computadora pueden guardar
# las bases de datos que emplea en un folder en su computadora
# y establecer dicho folder como el working directory cambiando
# la siguiente linea [ojo: utilicen las diagonales igual que 
# abajo sin importar si usan PC o Mac]

#Cargamos las librerías que vamos a utilizar
library(tidyverse)
library(ggplot2)
library(stats)
library(haven)
library(stringr)
library(broom)
library(dgof)
library(corrplot)
library(stargazer)
library(lmtest)
library(moments)

#Cargamos las Base de Datos SpotifyFeatures

SpotifyFeatures<-read.csv("SpotifyFeatures.csv")
View(SpotifyFeatures)

# ====// Pregunta 2 \\====

#Inciso ( a )

#Creamos una función para calcular la moda:

get_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

#Calculamos la media, mediana, moda, desviación estándar, el máximo y el mínimo para Valence
mean(SpotifyFeatures$valence)    #Media
median(SpotifyFeatures$valence)  #Mediana
get_mode(SpotifyFeatures$valence)#Moda
sd(SpotifyFeatures$valence)      #Desviación estándar
min(SpotifyFeatures$valence)     #Mínimo
max(SpotifyFeatures$valence)     #Máximo

#Calculamos la media, mediana, moda, desviación estándar, el máximo y el mínimo para Energy
mean(SpotifyFeatures$energy)    #Media
median(SpotifyFeatures$energy)  #Mediana
get_mode(SpotifyFeatures$energy)#Moda
sd(SpotifyFeatures$energy)      #Desviación estándar
min(SpotifyFeatures$energy)     #Mínimo
max(SpotifyFeatures$energy)     #Máximo

#Histogramas

#Valence
ggplot(SpotifyFeatures, aes(x = valence, y = ..count../sum(..count..))) + geom_histogram() +
  labs(x = "Valence", y = "Densidad") + 
  theme_minimal()

#Energy
ggplot(SpotifyFeatures, aes(x = energy, y = ..count../sum(..count..))) + geom_histogram() +
  labs (x = "Energy", y = "Densidad") + 
  theme_minimal()

# ====// Pregunta 3  \\====

#Creamos la muestra que contiene las canciones de Pop y Rock
SpotifyPR <- SpotifyFeatures[SpotifyFeatures$genre == "Pop"| SpotifyFeatures$genre == "Rock",]
View(SpotifyPR)

# ====// Inciso ( a )

#Calculamos media, varianza, número de observaciones y desviación estándar para las variables popularity, danceability, energy y valence en la base SpotifyPR

#Cálculos Popularity
(media_popularity_<PR <- mean(SpotifyPR$popularity, na.rm = T))
varianza_popularity_PR <- var(SpotifyPR$popularity, na.rm = T)
n_popularity_PR <- sum(!is.na(SpotifyPR$popularity))
(sd_popularity_PR <- sqrt(varianza_popularity_PR/n_popularity_PR))

#Cálculos Danceability
media_danceability_PR <- mean(SpotifyPR$danceability, na.rm = T)
varianza_danceability_PR <- var(SpotifyPR$danceability, na.rm = T)
n_danceability_PR <- sum(!is.na(SpotifyPR$danceability))
sd_danceability_PR <- sqrt(varianza_danceability_PR/n_danceability_PR)

#Cálculos Energy
media_energy_PR <- mean(SpotifyPR$energy, na.rm = T)
varianza_energy_PR <- var(SpotifyPR$energy, na.rm = T)
n_energy_PR <- sum(!is.na(SpotifyPR$energy))
sd_energy_PR <- sqrt(varianza_energy_PR/n_energy_PR) 

#Cálculos Valence
media_valence_PR <- mean(SpotifyPR$valence, na.rm = T)
varianza_valence_PR <- var(SpotifyPR$valence, na.rm = T)
n_valence_PR <- sum(!is.na(SpotifyPR$valence))
sd_valence_PR <- sqrt(varianza_valence_PR/n_valence_PR) 

#Calculamos las medias para popularity, danceability, energy y valence en la base SpotifyFeatures (SF)
(media_popularity_SF <- mean(SpotifyFeatures$popularity, na.rm = T))
(media_danceability_SF <- mean(SpotifyFeatures$danceability, na.rm = T))
(media_energy_SF <- mean(SpotifyFeatures$energy, na.rm = T))
(media_valence_SF <- mean(SpotifyFeatures$valence, na.rm = T)) 

#Mediante el estadístico t, analizamos si las muestras son representativas
(t_popularity <- (media_popularity_PR-media_popularity_SF)/(sd_popularity_PR))
(t_danceability <- (media_danceability_PR-media_danceability_SF )/(sd_danceability_PR))
(t_energy <- (media_energy_PR-media_energy_SF)/(sd_energy_PR))
(t_valence <- (media_valence_PR-media_valence_SF)/(sd_valence_PR))

#De misma forma, podríamos obtener el valor-p y os intervalos de confianza
#P-value
(pvalue_popularity <- 2*(1-pnorm(abs(t_popularity),0,1)))
(pvalue_danceability <- 2*(1-pnorm(abs(t_danceability),0,1)))
(pvalue_energy <- 2*(1-pnorm(abs(t_energy),0,1)))
(pvalue_valence <- 2*(1-pnorm(abs(t_valence),0,1)))

#Intervalo de Confianza al 95%
(IC_popularity <- c(media_popularity_PR - (1.96)*sd_popularity_PR, 
                    media_popularity_PR + (1.96)*sd_popularity_PR))
(IC_danceability <- c(media_danceability_PR - (1.96)*sd_danceability_PR, 
                    media_danceability_PR + (1.96)*sd_danceability_PR))
(IC_energy <- c(media_energy_PR - (1.96)*sd_energy_PR, 
                    media_energy_PR + (1.96)*sd_energy_PR))
(IC_valence <- c(media_valence_PR - (1.96)*sd_valence_PR, 
                    media_valence_PR + (1.96)*sd_valence_PR))

# ====// Inciso ( b )

#Calculamos la varianza y el número de observaciones para la variable danceability de SpotifyFeatures
(varianza_danceability_SF <- var(SpotifyFeatures$danceability, na.rm = T))
(n_danceability_SF <- sum(!is.na(SpotifyFeatures$danceability)))

#Calculamos los valores de Eta
(valores_eta <- c(((media_danceability_SF - media_danceability_PR)-(+1.96*sqrt((varianza_danceability_SF/n_danceability_SF) + (varianza_danceability_PR/n_danceability_PR)))),((media_danceability_SF - media_danceability_PR)-(-1.96*sqrt((varianza_danceability_SF/n_danceability_SF) + (varianza_danceability_PR/n_danceability_PR))))))

#Comprobación
(t1_dance<-((media_danceability_PR - media_danceability_SF+valores_eta[1])/(sqrt((varianza_danceability_PR/ n_danceability_PR)+(varianza_danceability_SF/ n_danceability_SF)))))
(pvalue_dance <- 2*(1-pnorm(abs(t1_dance),0,1)))

(t2_dance<-((media_danceability_PR - media_danceability_SF+valores_eta[2])/(sqrt((varianza_danceability_PR/ n_danceability_PR)+(varianza_danceability_SF/ n_danceability_SF)))))
(pvalue_dance <- 2*(1-pnorm(abs(t2_dance),0,1)))

# ====// Inciso ( c )

#Creamos variables a las que les asignamos un valor RGB para nuestras gráficas
col_1 <- rgb(173,216,230,max = 255, alpha = 80, names = "lt.blue")
col_2 <- rgb(255,192,203, max = 255, alpha = 80, names = "lt.pink")

#Creamos los intervalos para nuestras gráficas
b <- min(c(SpotifyFeatures$valence,SpotifyPR$valence))
e <- max(c(SpotifyFeatures$valence,SpotifyPR$valence))
ax <- pretty(b:e, n = 50) 

#Creamos las gráficas
hgA <- hist(SpotifyFeatures$valence, breaks = ax, plot = FALSE)
hgB <- hist(SpotifyPR$valence, breaks = ax, plot = FALSE)

#Unimos las gráficas en un solo plano y le añadimos las propiedades que queremos
plot(hgA, col = col_1, xlab="Valence",ylab = "Frecuencia") #main = "Distribución de Valence para SpotifyPR y SpotifyFeatures "
plot(hgB, col = col_2, add = TRUE)
abline(v = media_valence_SF,
       col = "blue",
       lwd = 3)
text(x = 0.25,
     y = 8000,
     paste("Media SpotifyFeatures =", round(media_valence_SF,4)),
     col = "blue",
     cex = 1)
abline(v = media_valence_PR,
       col = "red",
       lwd = 3)
text(x = 0.7,
     y = 8000,
     paste("Media SpotifyPR =", round(media_valence_PR,4)),
     col = "red",
     cex = 1)
legend("topright", c("Valence SF", "Valence PR"), col=c(col_1, col_2), lwd=10)

# ====// Inciso ( d )

#Obtenemos el estadístico de Kolmogorov-Smirnov (KS) y su valor-p

ks.test(SpotifyFeatures$valence,SpotifyPR$valence)
test$statistic

#Otra forma de calcularlo podría ser:
valence_SF_ecdf <- ecdf(SpotifyFeatures$valence)
valence_PR_ecdf <- ecdf(SpotifyPR$valence)
n_valence_SF <- length(SpotifyFeatures$valence)
n_valence_PR <- length(SpotifyPR$valence)
n_tot <- n_valence_PR*n_valence_SF / ( n_valence_PR + n_valence_SF)
w <- c(SpotifyFeatures$valence,SpotifyPR$valence)
z<- cumsum(ifelse(order(w) <= n_valence_SF, 1/n_valence_SF, -1/n_valence_PR))
max(abs(z))
(max.at <- sort(w)[which(abs(z) == max(abs(z)))])

#Gráfica de KS

conjunto <- data.frame(x=c(SpotifyFeatures$valence,SpotifyPR$valence), group = gl (2,100000))
ggplot(conjunto, aes(x = x))+ stat_ecdf()
ggplot(SpotifyFeatures, aes(valence)) +
  stat_ecdf(geom = "step")
ggplot(SpotifyPR, aes(valence)) +stat_ecdf(geom = "step")
plot(ecdf(SpotifyFeatures$valence),
     xlab="Valence",
     ylab = "Probabilidad",
     main="",
     xlim = range(c(SpotifyFeatures$valence, SpotifyPR$valence)),
     col = "skyblue4")
plot(ecdf(SpotifyPR$valence),
     add = TRUE,
     lty = "dashed",
     col = "red")
abline(v=test$statistic, lty=2)
text(x = .23, 
     y = .7, 
     labels = paste("KS =", 
                    round(test$statistic, 6)),
     pos = 3)
lines(abs(z)~sort(w), col="purple", lwd=2)
legend("right", legend=c("SpotifyFeatures$valence", "SpotifyPR$valence", "|Distancia|"), col=c("skyblue4", "red", "purple"), lwd=c(2,2,2), bty="n")

# ====// Inciso ( e )

# ==== Parte I

#Creamos un vector para guardar los resultados de las simulaciones
boot_valence_ks <- c() 

# Realizamos las simulaciones
for (i in 1:5000){
  SpotifyFeatures_boot <- sample_n(SpotifyFeatures, size = 100000, replace = T)
  SpotifyPR_boot <- SpotifyFeatures_boot[SpotifyFeatures_boot$genre == "Pop"| SpotifyFeatures_boot$genre == "Rock",]
  boot_valence_ks <- c(boot_valence_ks,ks.test(SpotifyFeatures_boot$valence,SpotifyPR_boot$valence)$statistic)
}

#Guardamos nuestro vector de resultados como un DataFrame
ks_boot <- as.data.frame(boot_valence_ks)
view(ks_boot)

#Graficamos nuestros resultados
ggplot(ks_boot, aes(x = boot_valence_ks, y = ..count../sum(..count..))) + geom_histogram() +
  labs( x = "KS Bootstrap", y = "Densidad") + 
  theme_minimal() 

#Test adicionales de normalidad
skewness(ks_boot$boot_valence_ks)
kurtosis(ks_boot$boot_valence_ks)
shapiro.test(ks_boot$boot_valence_ks)

# ==== Parte II

#Obtenemos la media, varianza, número de observaciones y desviación estándar para calcular el estadístico-t
(ks_boot_mean <- mean(ks_boot$boot_valence_ks, na.rm = T))
(ks_boot_var <- var(ks_boot$boot_valence_ks, na.rm = T))
(ks_n_boot <- sum(!is.na(ks_boot$boot_valence_ks)))
(ks_sd_boot <- sqrt(ks_boot_var/ks_n_boot))

(t_ks_boot <- (ks_boot_mean-0)/(ks_sd_boot))

# ==== Parte III

#Cortamos el 5% de cada extremo de nuestra distribución para calcular un intervalo de confianza de 90% para KS 
(quantile(ks_boot$boot_valence_ks,.05))
(quantile(ks_boot$boot_valence_ks,.95))


# ====// Pregunta 4 \\====

# ====// Inciso ( a )

#Cálculo de correlaciones

#Acousticness
(cor_acousticness <- cor(x = SpotifyFeatures$popularity, y = SpotifyFeatures$acousticness, use = "pairwise.complete.obs"))
#Danceability
(cor_danceability <- cor(x = SpotifyFeatures$popularity, y = SpotifyFeatures$danceability, use = "pairwise.complete.obs"))
#Duration
(cor_duration_s <- cor(x = SpotifyFeatures$popularity, y = SpotifyFeatures$duration_s, use = "pairwise.complete.obs"))
#Energy
(cor_energy <- cor(x = SpotifyFeatures$popularity, y = SpotifyFeatures$energy, use = "pairwise.complete.obs"))
#Instrumentalness
(cor_instrumentalness <- cor(x = SpotifyFeatures$popularity, y = SpotifyFeatures$instrumentalness, use = "pairwise.complete.obs"))
#Liveness
(cor_liveness <- cor(x = SpotifyFeatures$popularity, y = SpotifyFeatures$liveness, use = "pairwise.complete.obs"))
#Speechiness
(cor_speechiness <- cor(x = SpotifyFeatures$popularity, y = SpotifyFeatures$speechiness, use = "pairwise.complete.obs"))
#Tempo
(cor_tempo <- cor(x = SpotifyFeatures$popularity, y = SpotifyFeatures$tempo, use = "pairwise.complete.obs"))
#Valence
(cor_valence <- cor(x = SpotifyFeatures$popularity, y = SpotifyFeatures$valence, use = "pairwise.complete.obs"))

#Podríamos visualizar nuestras correlaciones de la siguiente forma
vars1<-data.frame(SpotifyFeatures$popularity,SpotifyFeatures$acousticness,SpotifyFeatures$danceability,SpotifyFeatures$duration_s,SpotifyFeatures$energy,SpotifyFeatures$instrumentalness,SpotifyFeatures$liveness,SpotifyFeatures$speechiness,SpotifyFeatures$tempo,SpotifyFeatures$valence)
(V1<-round(vars1,4))
corrplot(cor(V1), method= "number")

#Diagramas de dispersión

#Acousticness
ggplot(SpotifyFeatures, aes(x= acousticness, y = popularity)) + 
  geom_point(alpha=0.2,color="navy") + geom_smooth(method = "lm", se=FALSE, color="red") +
  labs(
    x="Acousticness",
    y="Popularity") +
  theme_minimal()

#Danceability
ggplot(SpotifyFeatures, aes(x= danceability, y = popularity)) + 
  geom_point(alpha=0.2,color="navy") + geom_smooth(method = "lm", se=FALSE, color="red") +
  labs(
    x="Danceability",
    y="Popularity") +
  theme_minimal()
#Duration_s
ggplot(SpotifyFeatures, aes(x= duration_s, y = popularity)) + 
  geom_point(alpha=0.2,color="navy") + geom_smooth(method = "lm", se=FALSE, color="red") +
  labs(
    x="Duration_s",
    y="Popularity") +
  theme_minimal()

#Energy
ggplot(SpotifyFeatures, aes(x= energy, y = popularity)) + 
  geom_point(alpha=0.2,color="navy") + geom_smooth(method = "lm", se=FALSE, color="red") +
  labs(
    x="Energy",
    y="Popularity") +
  theme_minimal()

#Instrumentalness
ggplot(SpotifyFeatures, aes(x= instrumentalness, y = popularity)) + 
  geom_point(alpha=0.2,color="navy") + geom_smooth(method = "lm", se=FALSE, color="red") +
  labs(
    x="Instrumentalness",
    y="Popularity") +
  theme_minimal()

#Liveness
ggplot(SpotifyFeatures, aes(x= liveness, y = popularity)) + 
  geom_point(alpha=0.2,color="navy") + geom_smooth(method = "lm", se=FALSE, color="red") +
  labs(
    x="Liveness",
    y="Popularity") +
  theme_minimal()

#Speechiness
ggplot(SpotifyFeatures, aes(x= speechiness, y = popularity)) + 
  geom_point(alpha=0.2,color="navy") + geom_smooth(method = "lm", se=FALSE, color="red") +
  labs(
    x="Speechiness",
    y="Popularity") +
  theme_minimal()

#Tempo
ggplot(SpotifyFeatures, aes(x= tempo, y = popularity)) + 
  geom_point(alpha=0.2,color="navy") + geom_smooth(method = "lm", se=FALSE, color="red") +
  labs(
    x="Tempo",
    y="Popularity") +
  theme_minimal()

#Valence
ggplot(SpotifyFeatures, aes(x= valence, y = popularity)) + 
  geom_point(alpha=0.2,color="navy") + geom_smooth(method = "lm", se=FALSE, color="red") +
  labs(
    x="Valence",
    y="Popularity") +
  theme_minimal()

# ====// Inciso ( b )

#Dado que la variable con mayor correlación con respecto a Popularity es Acousticness, calculamos la regresión:

modelo_1<-lm (popularity ~ acousticness, data=SpotifyFeatures)
stargazer(modelo_1, type = "text")

(cor_B1<-cor(SpotifyFeatures$acousticness,SpotifyFeatures$popularity))
(sd_pop<-sqrt(var(SpotifyFeatures$popularity)))
(sd_acousticness<-sqrt(var(SpotifyFeatures$acousticness)))
(B1<-cor_B1*(sd_pop/sd_acousticness))

# ====// Inciso ( c )

#Creamos la variable PR

SpotifyFeatures <- SpotifyFeatures %>%
  mutate(PR = ifelse((genre == "Pop" | genre == "Rock"),1,0))

modelo_2<-lm (popularity ~ acousticness + PR, data=SpotifyFeatures)
stargazer(modelo_2, type = "text")

# ====// Inciso ( d )

#Creamos una base de datos que incluya a las canciones que no pertenecen al género Pop o Rock
Spotify_Not_PR <- SpotifyFeatures[SpotifyFeatures$genre != "Pop" & SpotifyFeatures$genre != "Rock",]
View(Spotify_Not_PR) 

#Calculamos los datos necesarios para obtener B1 de la regresión lineal simple ( popularity = B0 + B1 acousticness)
(cov_B1_PR<-cov(SpotifyPR$acousticness,SpotifyPR$popularity))
(var_acousticness_PR<-var(SpotifyPR$acousticness))
(B1_PR<-(cov_B1_PR/var_acousticness_PR))

#Podríamos obtener el mismo resultado calculando la regresión completa
modelo_3<-lm (popularity ~ acousticness, data=SpotifyPR)
stargazer(modelo_3, type = "text")

#Calculamos el ponderador de las canciones que no pertenecen al género Pop o Rock
(weight_PR<-sum(!is.na(SpotifyPR$popularity))/sum(!is.na(SpotifyFeatures$popularity)))

#Repetimos los pasos anteriores para la base de datos que solo contiene a las canciones que pertenecen al género pop o rock (esta base la calculamos en la pregunta 3)

#Cov, Var y B1
(cov_B1_Not_PR<-cov(Spotify_Not_PR$acousticness,Spotify_Not_PR$popularity))
(var_acousticness_Not_PR<-var(Spotify_Not_PR$acousticness))
(B1_Not_PR<-(cov_B1_Not_PR/var_acousticness_Not_PR))

#Regresión completa
modelo_4<-lm (popularity ~ acousticness, data=Spotify_Not_PR)
stargazer(modelo_4, type = "text")

#Ponderador
(weight_Not_PR<-sum(!is.na(Spotify_Not_PR$popularity))/sum(!is.na(SpotifyFeatures$popularity)))

#Finalmente, calculamos la B1 de nuestra regresión con una variable dicotómica (popularity = B0 + B1 acousticness + B2 PR)
(B1_Reg<-((B1_PR)*weight_PR+(B1_Not_PR)*weight_Not_PR))

