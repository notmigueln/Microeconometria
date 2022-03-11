#Soluciones Tarea 1 | Primavera 2022
# Creada por Miguel Negrete
# Revisada y editada por Arturo Aguilar

# Para ejecutar este script desde su computadora pueden guardar
# las bases de datos que emplea en un folder en su computadora
# y establecer dicho folder como el working directory cambiando
# la siguiente linea [ojo: utilicen las diagonales igual que 
# abajo sin importar si usan PC o Mac]. Alternativamente pueden
# crear un proyecto referenciado a dicho folder como vimos en 
# clase

#setwd("C:/Users/aaeag/Econometria/Repaso estadistica")

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

SpotifyFeatures<-read_csv("SpotifyFeatures.csv")

# ====// Pregunta 2 \\====

#Inciso ( a )

# Todas las estadisticas descriptivas excepto la moda se pueden obtener con stargazer
# Abajo pueden ver como se puede combinar tidyverse con un comando
# El output se produce en LaTex y a dicho output le agregamos la moda de forma manual
stargazer(SpotifyFeatures %>% select(valence,energy), nobs = F, median = T, title="Estadisticas Descriptivas", digits=3, out="preg2.tex")

#Creamos una función para calcular la moda:
get_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

#Calculamos la moda para las variables 
get_mode(SpotifyFeatures$valence)#Moda
get_mode(SpotifyFeatures$energy)#Moda


#Histogramas

#Valence
ggplot(SpotifyFeatures, aes(x = valence, y = ..count../sum(..count..))) + geom_histogram() +
  labs(x = "Valence", y = "Densidad") + 
  theme_minimal()
ggsave("Hist_valence.jpeg",  width = 5.54, height = 4.95)

#Energy
ggplot(SpotifyFeatures, aes(x = energy, y = ..count../sum(..count..))) + geom_histogram() +
  labs (x = "Energy", y = "Densidad") + 
  theme_minimal()
ggsave("Hist_energy.jpeg",  width = 5.54, height = 4.95)

# ====// Pregunta 3  \\====

#Creamos la muestra que contiene las canciones de Pop y Rock
SpotifyFeatures <- SpotifyFeatures %>% mutate(base=0)

SpotifyPR <- SpotifyFeatures %>% filter(str_detect(genre,'Pop|Rock')) %>% mutate(base = replace(base,base==0,1))

# Pego una base encima de la otra
SpotifyAgg <- rbind(SpotifyFeatures,SpotifyPR) %>% mutate(base = factor(base))

# ====// Inciso ( a )

#Calculamos media, varianza, número de observaciones y desviación estándar para las variables popularity, danceability, energy y valence en la base SpotifyPR

# Con la base completa hacemos un t test que es equivalente a la diferencia de medias
t.test(popularity~base, data = SpotifyAgg, var.equal = F)
t.test(danceability~base, data = SpotifyAgg, var.equal = F)
t.test(energy~base, data = SpotifyAgg, var.equal = F)
t.test(valence~base, data = SpotifyAgg, var.equal = F)

# Para comprobar que el test es correcto, vean como se haria por pasos
#Cálculos Valence
media_valence_PR <- mean(SpotifyPR$valence, na.rm = T)
varianza_valence_PR <- var(SpotifyPR$valence, na.rm = T)
n_valence_PR <- sum(!is.na(SpotifyPR$valence))

media_valence_SF <- mean(SpotifyFeatures$valence, na.rm = T)
varianza_valence_SF <- var(SpotifyFeatures$valence, na.rm = T)
n_valence_SF <- sum(!is.na(SpotifyFeatures$valence))

(t_valence <- (media_valence_SF - media_valence_PR)/(sqrt(varianza_valence_PR/n_valence_PR + varianza_valence_SF/n_valence_SF)))
(pval_valence <- 2*(1-pnorm(abs(t_valence),0,1)))

# ====// Inciso ( b )

#Calculamos la diferencia de medias y varianza para la variable danceabilty
media_dance_PR <- mean(SpotifyPR$danceability, na.rm = T)
varianza_dance_PR <- var(SpotifyPR$danceability, na.rm = T)
n_dance_PR <- sum(!is.na(SpotifyPR$danceability))

media_dance_SF <- mean(SpotifyFeatures$danceability, na.rm = T)
varianza_dance_SF <- var(SpotifyFeatures$danceability, na.rm = T)
n_dance_SF <- sum(!is.na(SpotifyFeatures$danceability))

(eta_dif_medias <- media_dance_SF - media_dance_PR)
(eta_var_dif_medias <- varianza_dance_PR/n_dance_PR + varianza_dance_SF/n_dance_SF)
(eta <- c(eta_dif_medias - 1.96*sqrt(eta_var_dif_medias),
          eta_dif_medias + 1.96*sqrt(eta_var_dif_medias)))

#Comprobación
t1_dance<-(media_dance_SF - media_dance_PR-eta[1])/sqrt(eta_var_dif_medias)
(pvalue1_dance <- 2*(1-pnorm(abs(t1_dance),0,1)))

t2_dance<-(media_dance_SF - media_dance_PR-eta[2])/sqrt(eta_var_dif_medias)
(pvalue2_dance <- 2*(1-pnorm(abs(t2_dance),0,1)))

# ====// Inciso ( c )

label_group <- c("Features", "PopRock")
names(label_group) <- c("0","1")

#Create means
SpotifyMeans <- SpotifyAgg %>% group_by(base) %>%
  summarize(mean_val = mean(valence))

ggplot(data = SpotifyAgg, aes(x = valence, y= ..count../sum(..count..))) + geom_histogram() +
  facet_grid(base~., scales = "free", labeller = labeller(base = label_group)) +
  geom_vline(data = SpotifyMeans, aes(xintercept = mean_val, colour = base)) +
  theme_classic() + theme(legend.position = "none") + ylab("density")

ggsave("Hist_preg3c.jpeg",  width = 5.54, height = 4.95)


# ====// Inciso ( d )

#Obtenemos el estadístico de Kolmogorov-Smirnov (KS) y su valor-p

KS <- ks.test(SpotifyFeatures$valence,SpotifyPR$valence)
(KS$statistic)

#Otra forma de calcularlo podría ser:
SpotifyCDF <- SpotifyAgg %>% group_by(base) %>% mutate(n = n()) %>%
  mutate(iSF = ifelse(base==0,1,0), iPR = ifelse(base==0,0,1)) %>% 
  ungroup() %>% group_by(valence) %>% arrange(valence) %>% 
  summarize(iSF = sum(iSF), iPR = sum(iPR), Ntot_SF = max(n), Ntot_PR = min(n)) %>% 
  ungroup() %>% mutate(nSF = cumsum(iSF), nPR = cumsum(iPR), Ntot_PR = min(Ntot_PR),
                       CDF_SF = nSF/Ntot_SF, CDF_PR = nPR / Ntot_PR,
                       CDF_dif = abs(CDF_SF - CDF_PR)) 
  
(KS_stat <- max(SpotifyCDF$CDF_dif))
(KS_valence <- SpotifyCDF$valence[SpotifyCDF$CDF_dif==KS_stat])

#Gráfica de KS

ggplot() + stat_ecdf(data = SpotifyAgg,aes(x = valence, color = base)) + 
  geom_line(data = SpotifyCDF, aes(x = valence, y = CDF_dif)) +
  scale_color_hue(labels = c("Features","PopRock")) +
  geom_vline(xintercept = KS_valence, linetype = "longdash") + 
  theme_classic()

ggsave("KS_preg3d.jpeg",  width = 5.54, height = 4.95)


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

