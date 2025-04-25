#### DATASET ADAPTATION ####

options(scipen=999)
setwd("/Users/nicolobachiorri/Desktop/PROGETTODM")
r_logit <- read.csv("dataset_completo.csv", header = T, sep = ",", stringsAsFactors = T, na.strings=c("NA","NaN", "","Unknown"))

# Cambio delle variabili di rating in fattoriali
r_logit$Anxiety <- as.factor(r_logit$Anxiety)
r_logit$Depression <- as.factor(r_logit$Depression)
r_logit$Insomnia <- as.factor(r_logit$Insomnia)
r_logit$OCD <- as.factor(r_logit$OCD)
r_logit$While.working <- as.factor(r_logit$While.working)
r_logit$Instrumentalist <- as.factor(r_logit$Instrumentalist)
r_logit$Composer <- as.factor(r_logit$Composer)
r_logit$Exploratory <- as.factor(r_logit$Exploratory)
r_logit$Foreign.languages <- as.factor(r_logit$Foreign.languages)


str(r_logit) 

#il modello importato è frutto di una lavorazione pregressa svolta per il modello lineare, 
#in particolare sono già state effettuate le seguenti operazioni:

#rese fattoriali le variabili da rendere fattoriali;
#errori nel dataset eliminati (BPM < 20, BPM > 600), ma vi sono ancora Hours.per.day == 0 (da togliere, poiché non ci interessano)
#le variabili costanti, zero variance, già eliminate; 

which(r_logit$Hours.per.day == 0 )
r_logit <- r_logit[, -c(145 , 179 , 201 , 231)]#stessi indici di r_completo quindi l'importazione effettuata è corretta!


#### STARTING LOGIT MODEL #### 
#install.packages("ResourceSelection")
# Carica il pacchetto

FormulaModel <- paste(colnames(r_logit), collapse="','")
FormulaModel

list=c( 'Age','Primary.streaming.service','Hours.per.day','While.working','Instrumentalist',
                 'Composer','Fav.genre','Exploratory','Foreign.languages','BPM','Frequency..Classical.',
                 'Frequency..Country.','Frequency..EDM.','Frequency..Folk.','Frequency..Gospel.','Frequency..Hip.hop.',
                 'Frequency..Jazz.','Frequency..K.pop.','Frequency..Latin.','Frequency..Lofi.','Frequency..Metal.',
                 'Frequency..Pop.','Frequency..R.B.','Frequency..Rap.','Frequency..Rock.','Frequency..Video.game.music.',
                 'Anxiety','Depression','Insomnia','OCD')

good_covariates=r_logit[,list]
FormulaModel <- paste(colnames(good_covariates), collapse=" + ")
FormulaModel       

logit <- glm(Music.effects ~ Age + Primary.streaming.service + Hours.per.day + 
                While.working + Instrumentalist + Composer + Fav.genre + Exploratory + 
                Foreign.languages + BPM + Frequency..Classical. + Frequency..Country. + 
                Frequency..EDM. + Frequency..Folk. + Frequency..Gospel. + Frequency..Hip.hop. + 
                Frequency..Jazz. + Frequency..K.pop. + Frequency..Latin. + Frequency..Lofi. + 
                Frequency..Metal. + Frequency..Pop. + Frequency..R.B. + Frequency..Rap. +
                Frequency..Rock. + Frequency..Video.game.music. + Anxiety + Depression + Insomnia + OCD , data = r_logit , family = "binomial")

#error -> glm.fit: fitted probabilities numerically 0 or 1 occurred, typically indicates 
#that the logistic regression model is encountering perfect separation or near-perfect separation in the data.                 

summary(logit)  #aic = 826.42 --> 
R2 = 1- (592.42 / 782.7) #0.24 (Abbastanza basso considerando che ci sono tutti i predittori) 
vif(logit)  #VIF troppo elevato per Fav. genre  + Frequency..Hip.hop. + Frequency..Rap. + Anxiety + Depression + Insomnia + OCD
drop1(logit, test = "LRT") 
plot(logit)




#outlier riga 112 


#### COLLINEARITY #### 
library(mctest)
library(lmtest)
library(car)   

logit_a<- glm(Music.effects ~ . - Fav.genre, r_logit, family = "binomial") #ancora problemi 
vif(logit_a) #Frequency..Hip.hop. + Frequency..Rap. + Anxiety + Depression + Insomnia + OCD [GIA' LE CONOSCIAMO DA LINEAR MODEL]
drop1(logit_a, test="LRT")  

table(r_logit$Frequency..Hip.hop., r_logit$Frequency..Rap.)   # Hip.hop. e Rap. appaiono linearmente correlate, rimuoviamo Hip.hop.
mosaicplot(table(r_logit$Frequency..Hip.hop., r_logit$Frequency..Rap.) , main = "FREQUENCY_HIPHOP-FREQUENCY_RAP", shade = TRUE)

logit_b<- glm(Music.effects ~ . - Fav.genre - Frequency..Hip.hop. , r_logit, family = "binomial") 
vif(logit_b) #rimane da droppare Depression 

logit_c <- glm(Music.effects ~ .  - Fav.genre - Frequency..Hip.hop. - Depression, r_logit, family = "binomial" )
vif(logit_c) #vif sistemati - ma abbiamo quasi-separation (che proviamo a risolvere al punto seguente)
drop1(logit_c, test="LRT") 
plot(logit_c) #ci sono ancora dei possibili punti influenti 

#                              Df Deviance    AIC     LRT  Pr(>Chi) 
#While.working                 1   656.67 832.67 14.6689 0.0001281 ***
#Exploratory                   1   654.22 830.22 12.2265 0.0004712 *** 

#### INDIVIDUAZIONE SEPARATION #### 

# Carica pacchetti necessari
library(dplyr)
library(ggplot2)


# Definisci la variabile target e dataset
target <- "Music.effects"   # Nome della variabile target binaria (0/1)
dataset <- r_logit          # Sostituisci con il nome del tuo dataset

# Funzione per verificare la separation e quasi-separation
detect_separation <- function(dataset, target) {
  
  # Per ogni variabile esplicativa
  for (var in setdiff(names(dataset), target)) {
    
    # Identifica se la variabile è categorica o continua
    if (is.factor(dataset[[var]]) | is.character(dataset[[var]])) {
      # Caso Variabile Categorica
      
      # Crea tabella di contingenza
      table_2x2 <- table(dataset[[target]], dataset[[var]])
      
      # Mostra il grafico della distribuzione per controllo visivo
      ggplot(as.data.frame(table_2x2), aes(x = Var2, y = Freq, fill = factor(Var1))) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(title = paste("Distribuzione di", var, "per ciascuna classe di", target),
             x = var, y = "Conteggio") +
        scale_fill_manual(values = c("blue", "red"), labels = c("Class 0", "Class 1")) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
      # Rileva casi di separation
      if (any(table_2x2 == 0)) {
        cat("SEPARATION: La variabile", var, "mostra una separazione perfetta tra le classi target.\n")
      } else if (any(table_2x2 < 5)) {
        cat("QUASI-SEPARATION: La variabile", var, "mostra una quasi-separazione con alcune celle inferiori a 5.\n")
      }
      
    } else {
      # Caso Variabile Continua
      
      # Grafico di densità per individuare sovrapposizione delle classi
      ggplot(dataset, aes(x = dataset[[var]], fill = factor(dataset[[target]]))) +
        geom_density(alpha = 0.5) +
        labs(title = paste("Distribuzione di", var, "per ciascuna classe di", target),
             x = var, y = "Densità") +
        scale_fill_manual(values = c("blue", "red"), labels = c("Class 0", "Class 1")) +
        theme_minimal()
      
      # Individua quasi-separazione in modo semplificato
      # Verifica se i valori della variabile sono concentrati su una sola classe
      mean_class0 <- mean(dataset[[var]][dataset[[target]] == 0], na.rm = TRUE)
      mean_class1 <- mean(dataset[[var]][dataset[[target]] == 1], na.rm = TRUE)
      sd_class0 <- sd(dataset[[var]][dataset[[target]] == 0], na.rm = TRUE)
      sd_class1 <- sd(dataset[[var]][dataset[[target]] == 1], na.rm = TRUE)
      
      # Rileva quasi-separazione se la sovrapposizione tra le distribuzioni è minima
      if (abs(mean_class0 - mean_class1) > (sd_class0 + sd_class1)) {
        cat("QUASI-SEPARATION: La variabile continua", var, 
            "mostra una bassa sovrapposizione tra le classi target.\n")
      }
    }
  }
}

# Esegui la funzione per rilevare la separation e quasi-separation
detect_separation(dataset, target) 

#SEPARATION: La variabile Primary.streaming.service mostra una separazione perfetta tra le classi target.
#SEPARATION: La variabile Fav.genre mostra una separazione perfetta tra le classi target.
#SEPARATION: La variabile Frequency..Gospel. mostra una separazione perfetta tra le classi target.
#QUASI-SEPARATION: La variabile OCD mostra una quasi-separazione con alcune celle inferiori a 5.

#verifichiamo: 
table(r_logit$Primary.streaming.service, r_logit$Music.effects) #Chi usa Pandora ha solo effetti positivi
table(r_logit$Fav.genre, r_logit$Music.effects) #Gospel e Lofi hanno associati solo effetti positivi 
table(r_logit$Frequency..Gospel., r_logit$Music.effects) #chi ascolta Gospel very frequently ha associati solo effetti positivi 
table(r_logit$OCD, r_logit$Music.effects) #livello 9 di OCD a rischio separation 

#dal momento che abbiamo individuato in queste variabili casi di separation con la variabile target le andremo ad esludere
#dagli adattamenti seguenti in quanto porterebbero a conseguenze drammatiche per i parametri del modello. 


#### TRASFORMAZIONI PER X - risolviamo collinearità e separation ####

library(factorMerger)
library(dplyr)
library(ResourceSelection) 
library(ggplot2) 
#### mental health #### 
reduce_levels_Anxiety<-mergeFactors(response=r_logit$Music.effects, factor=r_logit$Anxiety, family = "binomial")
plot(reduce_levels_Anxiety, panelGrid = FALSE)   #suddivisone in gruppi non interpretabile

reduce_levels_Depression<-mergeFactors(response=r_logit$Music.effects, factor=r_logit$Depression, family = "binomial" )
plot(reduce_levels_Depression, panelGrid = FALSE)   #suddivisone in gruppi non interpretabile

reduce_levels_Insomnia<-mergeFactors(response=r_logit$Music.effects, factor=r_logit$Insomnia, family = "binomial")
plot(reduce_levels_Insomnia, panelGrid = FALSE)   #suddivisone in gruppi non interpretabile

reduce_levels_OCD<-mergeFactors(response=r_logit$Music.effects, factor=r_logit$OCD, family = "binomial")
plot(reduce_levels_OCD, panelGrid = FALSE)   #suddivisone in gruppi non interpretabile

#per questi riproviamo poi la pca come su modello lineare! 

#### Primary Streaming Service #### 

reduce_levels_Primary.streaming.service<-mergeFactors(response=r_logit$Music.effects, factor=r_logit$Primary.streaming.service, family = "binomial")
plot(reduce_levels_Primary.streaming.service, panelGrid = FALSE)   #suddivisione poco interpretabile e poco sensata  (Pandora)- (spotift-apple music-yt music) - (Otss-Idnuass) 
#lascia da solo Pandora (che è quello che crea problemi di separation) contro (streaming più usati) e contro (altri streaming - e nessuno streaming) --> NON INTERPRETABILE 
#poiché il grouping non è interpretabile e la variabile così come è crea problemi di separation allora DROP
summary(r_logit$Primary.streaming.service)

#### Frequency Classical #### 
library(dplyr)
library(MASS)

reduce_levels_Frequency..Classical.<-mergeFactors(response=r_logit$Music.effects, factor=r_logit$Frequency..Classical. , family = "binomial")
plot(reduce_levels_Frequency..Classical., panelGrid = FALSE)   #gruppo unico - quindi non modifichiamo nulla!

#### Frequency Country | model #### 
reduce_levels_Frequency..Country.<-mergeFactors(response= r_logit$Music.effects, factor=r_logit$Frequency..Country. , family = "binomial")
plot(reduce_levels_Frequency..Country., panelGrid = FALSE) #due gruppi sensati 
#gruppo1 --> (Nevr - Rrly) 
#gruppo2 --> (Smtm - Vryf)

r_logit$Frequency..Country.grouped <- as.factor(as.numeric(cutTree(reduce_levels_Frequency..Country.)))
table(r_logit$Frequency..Country.grouped, cutTree(reduce_levels_Frequency..Country.))

# Creazione del grafico con media e intervalli di confidenza
ggplot(r_logit, aes(x = Frequency..Country.grouped, y = Music.effects)) +
  stat_summary(fun = mean, geom = "bar", fill = "skyblue") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
  labs(title = "Media di Music.effects con intervalli di confidenza",
       x = "Frequency..Country.grouped",
       y = "Media di Music.effects") +
  theme_minimal() 


a <- glm(Music.effects ~ Frequency..Country., r_logit, family = "binomial") ; summary(a) ; anova(a) #Frequency..Country. non significativa
b <- glm(Music.effects ~ Frequency..Country.grouped, r_logit, family = "binomial") ; summary(b) ; anova(b)  #sembra più significativo con p(Chi) = 0.02532 

logit1_a <- glm(Music.effects ~ Age  + Hours.per.day + 
                  While.working + Instrumentalist + Composer  + Exploratory + 
                  Foreign.languages + BPM + Frequency..Classical. + Frequency..Country.grouped + 
                  Frequency..EDM. + Frequency..Folk. + Frequency..Gospel. + 
                  Frequency..Jazz. + Frequency..K.pop. + Frequency..Latin. + Frequency..Lofi. + 
                  Frequency..Metal. + Frequency..Pop. + Frequency..R.B. + Frequency..Rap. +
                  Frequency..Rock. + Frequency..Video.game.music. + Anxiety  + Insomnia + OCD , data = r_logit , family = "binomial")

summary(logit1_a) #AIC = 810
#R2 = 1 - (646.0 / 782.7) # 0.1746 
drop1(logit1_a, test = "LRT") 
hoslem.test(r_logit$Music.effects, fitted(logit1_a), g = 10) #p = 0.2341 

#### Frequency EDM #### 
reduce_levels_Frequency..EDM.<-mergeFactors(response=r_logit$Music.effects, factor=r_logit$Frequency..EDM.) 
plot(reduce_levels_Frequency..EDM., panelGrid = FALSE) #distinzione in due gruppi non interpretabile 
#### Frequency Folk ####
reduce_levels_Frequency..Folk.<-mergeFactors(response=r_logit$Music.effects, factor=r_logit$Frequency..Folk.) 
plot(reduce_levels_Frequency..Folk., panelGrid = FALSE) #gruppo unico 
#### Frequency Gospel ####
reduce_levels_Frequency..Gospel.<-mergeFactors(response=r_logit$Music.effects, factor=r_logit$Frequency..Gospel.) 
plot(reduce_levels_Frequency..Gospel., panelGrid = FALSE) #distinzione in due gruppi non interpretabile

#### Frequency Jazz | model ####
reduce_levels_Frequency..Jazz.<-mergeFactors(response=r_logit$Music.effects, factor=r_logit$Frequency..Jazz.) 
plot(reduce_levels_Frequency..Jazz., panelGrid = FALSE) #distinzione interessante, distingue fra chi Si e chi No 
#gruppo1 --> (Nevr) 
#gruppo2 --> (Smtm - Rrly - Vryf)


r_logit$Frequency..Jazz.grouped <- as.factor(as.numeric(cutTree(reduce_levels_Frequency..Jazz.)))
table(r_logit$Frequency..Jazz.grouped, cutTree(reduce_levels_Frequency..Jazz.))

# Creazione del grafico con media e intervalli di confidenza
ggplot(r_logit, aes(x = Frequency..Jazz.grouped, y = Music.effects)) +
  stat_summary(fun = mean, geom = "bar", fill = "skyblue") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
  labs(title = "Media di Music.effects con intervalli di confidenza",
       x = "Frequency..Jazz.grouped",
       y = "Media di Music.effects") +
  theme_minimal() 

r_logit$Frequency..Jazz.grouped <- ifelse(r_logit$Frequency..Jazz.grouped == 1, 0, 1) 
r_logit$Frequency..Jazz.grouped <- as.factor(r_logit$Frequency..Jazz.grouped) 
colnames(r_logit)[colnames(r_logit)=="Frequency..Jazz.grouped"] <- "Listens.to.Jazz"
#str(r_logit) 

a <- glm(Music.effects ~ Frequency..Jazz., r_logit, family = "binomial") ; summary(a) ; anova(a) #Frequency..Jazz. non significativa
b <- glm(Music.effects ~ Listens.to.Jazz, r_logit, family = "binomial") ; summary(b) ; anova(b)  #Listens.to.Jazz significativa p(Chi) = 0.0277 
 
logit1_b <- glm(Music.effects ~ Age  + Hours.per.day + 
                  While.working + Instrumentalist + Composer  + Exploratory + 
                  Foreign.languages + BPM + Frequency..Classical. + Frequency..Country.grouped + 
                  Frequency..EDM. + Frequency..Folk. + Frequency..Gospel. + 
                  Listens.to.Jazz + Frequency..K.pop. + Frequency..Latin. + Frequency..Lofi. + 
                  Frequency..Metal. + Frequency..Pop. + Frequency..R.B. + Frequency..Rap. +
                  Frequency..Rock. + Frequency..Video.game.music. + Anxiety  + Insomnia + OCD , data = r_logit , family = "binomial")

summary(logit1_b) #AIC = 808.6 
#R2 =  1- (648.6 / 782.7) #0.1733 
hoslem.test(r_logit$Music.effects, fitted(logit1_b), g = 10) #p = 0.8556 

#### Frequency k Pop #### 
reduce_levels_Frequency..K.pop.<-mergeFactors(response=r_logit$Music.effects, factor=r_logit$Frequency..K.pop.) 
plot(reduce_levels_Frequency..K.pop., panelGrid = FALSE) #gruppo unico 

#### Frequency Latin | model ####
reduce_levels_Frequency..Latin.<-mergeFactors(response=r_logit$Music.effects, factor=r_logit$Frequency..Latin.) 
plot(reduce_levels_Frequency..Latin., panelGrid = FALSE) #due gruppi (Smtm-Vryf) - (Rrly-Nevr)
#gruppo1 --> (Nevr - Rrly) 
#gruppo2 --> (vryf - Smtm) 

r_logit$Frequency..Latin.grouped <- as.factor(as.numeric(cutTree(reduce_levels_Frequency..Latin.)))
table(r_logit$Frequency..Latin.grouped , cutTree(reduce_levels_Frequency..Latin.))

# Creazione del grafico con media e intervalli di confidenza
ggplot(r_logit, aes(x = Frequency..Latin.grouped, y = Music.effects)) +
  stat_summary(fun = mean, geom = "bar", fill = "skyblue") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
  labs(title = "Media di Music.effects con intervalli di confidenza",
       x = "Frequency..Latin.grouped",
       y = "Media di Music.effects") +
  theme_minimal() 

a <- glm(Music.effects ~ Frequency..Latin., r_logit, family = "binomial") ; summary(a) ; anova(a) #Frequency..Latin. non significativa
b <- glm(Music.effects ~ Frequency..Latin.grouped, r_logit, family = "binomial") ; summary(b) ; anova(b) #Frequency..Latin.grouped più significativa con p(Chi)= 0.07503

logit1_c <- glm(Music.effects ~ Age  + Hours.per.day + 
                  While.working + Instrumentalist + Composer  + Exploratory + 
                  Foreign.languages + BPM + Frequency..Classical. + Frequency..Country.grouped + 
                  Frequency..EDM. + Frequency..Folk. + Frequency..Gospel. + 
                  Listens.to.Jazz + Frequency..K.pop. + Frequency..Latin.grouped + Frequency..Lofi. + 
                  Frequency..Metal. + Frequency..Pop. + Frequency..R.B. + Frequency..Rap. +
                  Frequency..Rock. + Frequency..Video.game.music. + Anxiety  + Insomnia + OCD , data = r_logit , family = "binomial")

summary(logit1_c) #AIC = 805.23
#R2 = 1 - (649.23 / 782.3) #0.1701
drop1(logit1_c ,  test = "LRT")
anova(logit1_c)
hoslem.test(r_logit$Music.effects, fitted(logit1_c), g = 10) #p = 0.5202 (sceso! ma buono!)

#### Frequency Lofi | model ####
reduce_levels_Frequency..Lofi.<-mergeFactors(response=r_logit$Music.effects, factor=r_logit$Frequency..Lofi.) 
plot(reduce_levels_Frequency..Lofi., panelGrid = FALSE) #due gruppi sensati: (Smtm - Vryf) -(Rrly - Nevr) 

#gruppo1 --> (Nevr - Rrly)
#gruppo2 --> (Vryf-Smtm) 

r_logit$Frequency..Lofi.grouped <- as.factor(as.numeric(cutTree(reduce_levels_Frequency..Lofi.)))
table(r_logit$Frequency..Lofi.grouped, cutTree(reduce_levels_Frequency..Lofi.))

# Creazione del grafico con media e intervalli di confidenza
ggplot(r_logit, aes(x = Frequency..Lofi.grouped, y = Music.effects)) +
  stat_summary(fun = mean, geom = "bar", fill = "skyblue") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
  labs(title = "Media di Music.effects con intervalli di confidenza",
       x = "Frequency..Lofi.grouped",
       y = "Media di Music.effects") +
  theme_minimal() 

a <- glm(Music.effects ~ Frequency..Lofi., r_logit, family = "binomial") ; summary(a) ; anova(a) #Frequency..Lofi. non significativa 
b <- glm(Music.effects ~ Frequency..Lofi.grouped, r_logit, family = "binomial") ; summary(b) ; anova(b) #più significativa con p = 0.06405 

logit1_d <- glm(Music.effects ~ Age  + Hours.per.day + 
                  While.working + Instrumentalist + Composer  + Exploratory + 
                  Foreign.languages + BPM + Frequency..Classical. + Frequency..Country.grouped + 
                  Frequency..EDM. + Frequency..Folk. + Frequency..Gospel. + 
                  Listens.to.Jazz + Frequency..K.pop. + Frequency..Latin.grouped + Frequency..Lofi.grouped + 
                  Frequency..Metal. + Frequency..Pop. + Frequency..R.B. + Frequency..Rap. +
                  Frequency..Rock. + Frequency..Video.game.music. + Anxiety  + Insomnia + OCD , data = r_logit , family = "binomial")

summary(logit1_d) #AIC = 801.04
R2 = 1 - (649.64 / 782.70) #0.1700
hoslem.test(r_logit$Music.effects, fitted(logit1_d), g = 10) # p = 0.1998 

#### Frequency Metal #### 
reduce_levels_Frequency..Metal.<-mergeFactors(response=r_logit$Music.effects, factor=r_logit$Frequency..Metal.) 
plot(reduce_levels_Frequency..Metal., panelGrid = FALSE) #gruppo unico 
#### Frequency Pop. ####
reduce_levels_Frequency..Pop.<-mergeFactors(response=r_logit$Music.effects, factor=r_logit$Frequency..Pop.) 
plot(reduce_levels_Frequency..Pop., panelGrid = FALSE) #non interpretabile 

#### Frequency RB | model #### 
reduce_levels_Frequency..R.B.<-mergeFactors(response=r_logit$Music.effects, factor=r_logit$Frequency..R.B.) 
plot(reduce_levels_Frequency..R.B., panelGrid = FALSE) 
#gruppo1 --> (Nevr - Rrly)
#gruppo2 --> (Vryf-Smtm) 

r_logit$Frequency..R.B.grouped <- as.factor(as.numeric(cutTree(reduce_levels_Frequency..R.B.)))
table(r_logit$Frequency..R.B.grouped, cutTree(reduce_levels_Frequency..R.B.))

# Creazione del grafico con media e intervalli di confidenza
ggplot(r_logit, aes(x = Frequency..R.B.grouped, y = Music.effects)) +
  stat_summary(fun = mean, geom = "bar", fill = "skyblue") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
  labs(title = "Media di Music.effects con intervalli di confidenza",
       x = "Frequency..R.B.grouped",
       y = "Media di Music.effects") +
  theme_minimal() 

a <- glm(Music.effects ~ Frequency..R.B., r_logit, family = "binomial") ; summary(a) ; anova(a) #Pr(>Chi) = 0.02715 *
b <- glm(Music.effects ~ Frequency..R.B.grouped, r_logit, family = "binomial") ; summary(b) ; anova(b) #Pr(>Chi) = 0.00303 **

logit1_e <- glm(Music.effects ~ Age  + Hours.per.day + 
                  While.working + Instrumentalist + Composer  + Exploratory + 
                  Foreign.languages + BPM + Frequency..Classical. + Frequency..Country.grouped + 
                  Frequency..EDM. + Frequency..Folk. + Frequency..Gospel. + 
                  Listens.to.Jazz + Frequency..K.pop. + Frequency..Latin.grouped + Frequency..Lofi.grouped + 
                  Frequency..Metal. + Frequency..Pop. + Frequency..R.B.grouped + Frequency..Rap. +
                  Frequency..Rock. + Frequency..Video.game.music. + Anxiety  + Insomnia + OCD , data = r_logit , family = "binomial")

summary(logit1_e) #AIC = 798.26 
#R2 = 1 - (650.26 / 782.7) #0.1692 
drop1(logit1_e, test = "LRT") 
hoslem.test(r_logit$Music.effects, fitted(logit1_e), g = 10) #p = 0.2212

#### Frequency Rap | model #### 
reduce_levels_Frequency..Rap.<-mergeFactors(response=r_logit$Music.effects, factor=r_logit$Frequency..Rap.) 
plot(reduce_levels_Frequency..Rap., panelGrid = FALSE) 
#gruppo1 --> (Nevr - Rrly)
#gruppo2 --> (Vryf-Smtm) 

r_logit$Frequency..Rap.grouped <- as.factor(as.numeric(cutTree(reduce_levels_Frequency..Rap.)))
table(r_logit$Frequency..Rap.grouped , cutTree(reduce_levels_Frequency..Rap.))

# Creazione del grafico con media e intervalli di confidenza
ggplot(r_logit, aes(x = Frequency..Rap.grouped, y = Music.effects)) +
  stat_summary(fun = mean, geom = "bar", fill = "skyblue") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
  labs(title = "Media di Music.effects con intervalli di confidenza",
       x = "Frequency..Rap.grouped",
       y = "Media di Music.effects") +
  theme_minimal() 

a <- glm(Music.effects ~ Frequency..Rap., r_logit, family = "binomial") ; summary(a) ; anova(a) #Pr(>Chi) = 0.3503 
b <- glm(Music.effects ~ Frequency..Rap.grouped, r_logit, family = "binomial") ; summary(b) ; anova(b) #Pr(>Chi) = 0.09955 * 

logit1_f <- glm(Music.effects ~ Age  + Hours.per.day + 
                  While.working + Instrumentalist + Composer  + Exploratory + 
                  Foreign.languages + BPM + Frequency..Classical. + Frequency..Country.grouped + 
                  Frequency..EDM. + Frequency..Folk. + Frequency..Gospel. + 
                  Listens.to.Jazz + Frequency..K.pop. + Frequency..Latin.grouped + Frequency..Lofi.grouped + 
                  Frequency..Metal. + Frequency..Pop. + Frequency..R.B.grouped + Frequency..Rap.grouped +
                  Frequency..Rock. + Frequency..Video.game.music. + Anxiety  + Insomnia + OCD , data = r_logit , family = "binomial")


summary(logit1_f)
#R2 = 1- (652.08 / 782.7) #0.1668 
drop1(logit1_f , test = "LRT")
plot(logit1_f) 
hoslem.test(r_logit$Music.effects, fitted(logit1_f), g = 10) #p = 0.3781 

#### Frequency Rock #### 
reduce_levels_Frequency..Rock.<-mergeFactors(response=r_logit$Music.effects, factor=r_logit$Frequency..Rock.) 
plot(reduce_levels_Frequency..Rock., panelGrid = FALSE) #gruppo unico 
#### Frequency VideoGame Music #### 
reduce_levels_Frequency..Video.game.music.<-mergeFactors(response=r_logit$Music.effects, factor=r_logit$Frequency..Video.game.music.) 
plot(reduce_levels_Frequency..Video.game.music., panelGrid = FALSE) #non interpretabile




#### PCA mental health #### 


pca_data=r_logit[,c('Anxiety','Depression','Insomnia','OCD')]
#View(pca_data)

# Converti i fattori in numerici (se utile) 
pca_data$Anxiety <- (as.numeric(pca_data$Anxiety))-1
pca_data$Depression <- (as.numeric(pca_data$Depression))-1
pca_data$Insomnia <- (as.numeric(pca_data$Insomnia))-1
pca_data$OCD <- (as.numeric(pca_data$OCD))-1 

# Esegui la PCA sui dati convertiti
pca <- prcomp(pca_data, scale = TRUE, center = TRUE)   #scale divide per sigma e ceter centra quidni sono la standardizzazione
par(mfrow=c(1,1))
#biplot(pca, scale = 0)
library(ggplot2)
library(ggfortify)
autoplot(pca, data = pca_data, 
         colour = 'blue', 
         loadings = TRUE, 
         loadings.label = TRUE,
         loadings.label.size = 3)   # Autoplot delle prime due pc

library(factoextra)
tt<-data.frame(get_eig(pca))
tt$pca<-1:nrow(tt)
head(tt)   #1 pc spiega il 50% della varianza, 2 ne spiegano il 70%

library(psych)
fitV <- principal(pca_data, nfactors=2, rotate="varimax",covar=FALSE)
fitV
biplot(fitV, cex=0.5,pc.biplot=TRUE)   #Autoplot ruotato per interpretare meglio la seconda pc
fa.diagram(fitV) 

#inserico i risultati della PCA nel dataset completo
scores=fitV$scores
plot(scores)
pc=data.frame(fitV$scores)
#View(pc)
cor(pc)
r_logit=cbind(r_logit,pc)

logit1_g <- glm(Music.effects ~ Age  + Hours.per.day + 
                  While.working + Instrumentalist + Composer  + Exploratory + 
                  Foreign.languages + BPM + Frequency..Classical. + Frequency..Country.grouped + 
                  Frequency..EDM. + Frequency..Folk. + Frequency..Gospel. + 
                  Listens.to.Jazz + Frequency..K.pop. + Frequency..Latin.grouped + Frequency..Lofi.grouped + 
                  Frequency..Metal. + Frequency..Pop. + Frequency..R.B.grouped + Frequency..Rap.grouped +
                  Frequency..Rock. + Frequency..Video.game.music. + RC1 + RC2 , data = r_logit , family = "binomial")

summary(logit1_g) 
#R2 = 1 - (692.29 / 782.70) #0.1155 
vif(logit1_g)
drop1(logit1_g , test = "LRT") #var significative: While.working + Exploratory + Frequency..Country.grouped + Frequency..Gospel. + Listens.to.Jazz + RC1  
plot(logit1_g)
hoslem.test(r_logit$Music.effects, fitted(logit1_g), g = 10) #p = 0.3832 (BUONO!)  


#### TRASFORMAZIONI PER X QUANTITATIVE | Age e Hours.per.day  #### 

install.packages("mgcv")
library(mgcv)

logit2_a <- gam(Music.effects ~ s(Age)  + s(Hours.per.day) + 
                  While.working + Instrumentalist + Composer  + Exploratory + 
                  Foreign.languages + BPM + Frequency..Classical. + Frequency..Country.grouped + 
                  Frequency..EDM. + Frequency..Folk. + Frequency..Gospel. + 
                  Listens.to.Jazz + Frequency..K.pop. + Frequency..Latin.grouped + Frequency..Lofi.grouped + 
                  Frequency..Metal. + Frequency..Pop. + Frequency..R.B.grouped + Frequency..Rap.grouped +
                  Frequency..Rock. + Frequency..Video.game.music. + RC1 + RC2 , data = r_logit , family = binomial(link = "logit"))

#sia s(Age) che s(Hours.per.day) sono molto significative 
plot(logit2_a, pages = 1) 
#Il grafico suggerisce che una trasformazione quadratica potrebbe adattarsi bene per "Age", 
#mentre "Hours per day" sembra essere ben rappresentata in forma lineare.
gam.check(logit2_a)

logit2_b <- glm(Music.effects ~ Age + I(Age^2) + Hours.per.day + While.working + 
                  Instrumentalist + Composer + Exploratory + Foreign.languages + BPM + 
                  Frequency..Classical. + Frequency..Country.grouped + Frequency..EDM. + 
                  Frequency..Folk. + Frequency..Gospel. + Listens.to.Jazz + 
                  Frequency..K.pop. + Frequency..Latin.grouped + Frequency..Lofi.grouped + 
                  Frequency..Metal. + Frequency..Pop. + Frequency..R.B.grouped + 
                  Frequency..Rap.grouped + Frequency..Rock. + Frequency..Video.game.music. + 
                  RC1 + RC2, data = r_logit, family = binomial(link = "logit"))
summary(logit2_b) 
drop1(logit2_b, test = "LRT") 
plot(logit2_b) 
vif(logit2_b) #unici vif alti tra age e age^2 (normale) 
hoslem.test(r_logit$Music.effects, fitted(logit2_b), g = 10)  #p = 0.2289 (Bene, ma non benissimo!) 

# Calcola i residui deviance
residui_deviance <- residuals(logit2_b, type = "deviance")
# Grafico Residuals vs Fitted
plot(logit2_b$fitted.values, residui_deviance, 
     xlab = "Valori Predetti (Fitted)", 
     ylab = "Residui di Deviance", 
     main = "Grafico Residuals vs Fitted")
abline(h = 0, col = "red", lty = 2) 


#### VALORI INFLUENTI  #### 

# Calcola le misure di influenza
influence_measures <- influence.measures(logit2_b)

# Visualizza un riepilogo delle osservazioni influenti
summary(influence_measures)

# Estrai Cook's Distance, DFBETAS, e leverage
cook_distance <- cooks.distance(logit2_b)
dfbetas <- dfbetas(logit2_b)
leverage <- hatvalues(logit2_b) 

# Identifica osservazioni con alto Cook's Distance
threshold_cook <- 4 / nrow(r_logit)  # Soglia comune
influential_obs <- which(cook_distance > threshold_cook)

# Stampa le osservazioni influenti
cat("Osservazioni influenti (Cook's Distance > soglia):", influential_obs, "\n")

#Osservazioni influenti (Cook's Distance > soglia): 112 175 182 188 189 191 197 202
#204 205 209 210 214 215 229 233 235 237 240 299 310 314 326 328 331 332 414 421 422 
#426 479 481 482 484 485 486 487 488 500 530 532 533 581 582 583 585 586 618 619 633 
#640 641 644 647 648 649 650 651 709 710 


#### RIMOZIONE PUNTI INFLUENTI  #### 

# Crea un dataset senza le osservazioni influenti

r_logit_clean <- r_logit[-influential_obs, ]

# Ricrea il modello senza i punti influenti

logit2_c <- glm(Music.effects ~ Age + I(Age^2) + Hours.per.day + While.working + 
                  Instrumentalist + Composer + Exploratory + Foreign.languages + BPM + 
                  Frequency..Classical. + Frequency..Country.grouped + Frequency..EDM. + 
                  Frequency..Folk. + Frequency..Gospel. + Listens.to.Jazz + 
                  Frequency..K.pop. + Frequency..Latin.grouped + Frequency..Lofi.grouped + 
                  Frequency..Metal. + Frequency..Pop. + Frequency..R.B.grouped + 
                  Frequency..Rap.grouped + Frequency..Rock. + Frequency..Video.game.music. + 
                  RC1 + RC2, data = r_logit_clean, family = binomial(link = "logit"))

summary(logit2_c) #AIC = 478.66 #Gospel mi sembra problematica 
R2 = 1- (388.66 / 604.39) #0.35693 
drop1(logit2_c, test = "LRT") 
plot(logit2_c) 
vif(logit2_c) #unici vif alti tra age e age^2 (normale) 
hoslem.test(r_logit_clean$Music.effects, fitted(logit2_c), g = 10) #p = 0.0538 (AAAAAAAA)

#### MODEL SELECTION #### 
step(logit2_c, direction = "both", trace = 1) 

logit3_a <- glm(formula = Music.effects ~ Age + I(Age^2) + Hours.per.day + 
                  While.working + Instrumentalist + Composer + Exploratory + 
                  Foreign.languages + BPM + Frequency..Country.grouped + Frequency..EDM. + 
                  Frequency..Latin.grouped + Frequency..Pop. + Frequency..R.B.grouped + 
                  Frequency..Rock. + RC1 + RC2, family = binomial(link = "logit"), 
                data = r_logit_clean) 

summary(logit3_a) #AIC = 451.94 
#R2 = 1 - (403.94 / 604.39) #0.3316 
drop1(logit3_a, test = "LRT") 
hoslem.test(r_logit_clean$Music.effects, fitted(logit3_a), g = 10) #p = 0.1476 


#### PUNTI INFLUENTI CON DIFCHISQ  #### 

library(MASS)

# Calcolare il chi-quadro di Pearson iniziale per il modello completo
pearson_full <- sum(residuals(logit3_a, type = "pearson")^2)

# Inizializzare un vettore per salvare i DIFCHISQ per ogni osservazione
n <- nrow(r_logit1)
DIFCHISQ <- numeric(n)

# Loop attraverso ogni osservazione, rimuoverla e rifittare il modello
for (i in 1:n) {
  # Creare un sottoinsieme dei dati senza l'osservazione i-esima
  data_minus_i <- r_logit_clean[-i, ]
  
  # Rifittare il modello logit sui dati senza l'osservazione i-esima
  model_minus_i <- glm(formula = Music.effects ~ Age + I(Age^2) + Hours.per.day + 
                         While.working + Instrumentalist + Composer + Exploratory + 
                         Foreign.languages + BPM + Frequency..Country.grouped + Frequency..EDM. + 
                         Frequency..Latin.grouped + Frequency..Pop. + Frequency..R.B.grouped + 
                         Frequency..Rock. + RC1 + RC2, family = binomial(link = "logit"), 
                       data = r_logit_clean)
  
  # Calcolare il chi-quadro di Pearson per il modello senza l'osservazione i-esima
  pearson_minus_i <- sum(residuals(model_minus_i, type = "pearson")^2)
  
  # Calcolare DIFCHISQ come differenza nei chi-quadro di Pearson
  DIFCHISQ[i] <- pearson_full - pearson_minus_i
}

# Identificare le osservazioni influenti in base a una soglia per DIFCHISQ
# Per esempio, possiamo considerare influenti quelle con DIFCHISQ > 4 (threshold arbitrario)

influential_points <- which(DIFCHISQ > 4) 
print(influential_points) #non visualizzo nulla 
length(influential_points) #lunghezza 0 


#### INFLUENCE PLOT #### 

#install.packages("broom")
#install.packages("ggrepel")

library(broom)
library(ggrepel)

# Fit del modello logit (sostituire con il tuo modello)
logit_model <- logit3_a 

# Calcola le statistiche diagnostiche per il modello
diagnostics <- augment(logit_model)  # broom::augment() dà statistiche diagnostiche
diagnostics$cooksd <- cooks.distance(logit_model)   # Aggiunge la Cook's Distance
diagnostics$leverage <- hatvalues(logit_model)   # Aggiunge il leverage
diagnostics$residuals_std <- rstandard(logit_model)   # Residui standardizzati

# Definisci soglie per Cook's Distance, leverage e residui
cook_threshold <- 4 / nrow(diagnostics)   # Soglia di Cook's Distance
leverage_threshold <- 2 * mean(diagnostics$leverage)   # Soglia di leverage
residuals_threshold <- 2   # Soglie per residui standardizzati a ±2

# Aggiungi un identificativo per ogni osservazione
diagnostics$index <- seq_len(nrow(diagnostics))

# Creazione del grafico di influenza con ggplot2
ggplot(diagnostics, aes(x = leverage, y = residuals_std)) +
  geom_point(aes(size = cooksd, color = cooksd), alpha = 0.6) +   # Punti dimensionati e colorati da Cook's Distance
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +   # Linea orizzontale per residui = 0
  geom_hline(yintercept = residuals_threshold, linetype = "dashed", color = "green") +   # Soglia +2 per residui
  geom_hline(yintercept = -residuals_threshold, linetype = "dashed", color = "green") +   # Soglia -2 per residui
  geom_vline(xintercept = leverage_threshold, linetype = "dashed", color = "blue") +   # Linea per leverage
  scale_size_continuous(range = c(2, 10), name = "Cook's Distance") +   # Dimensione dei punti in base a Cook's Distance
  scale_color_gradient(low = "blue", high = "red", name = "Cook's Distance") +   # Gradient di colore in base a Cook's Distance
  labs(title = "Influence Plot",
       x = "Leverage",
       y = "Standardized Residuals") +
  theme_minimal() +
  geom_text_repel(data = diagnostics[diagnostics$cooksd > cook_threshold | 
                                       diagnostics$leverage > leverage_threshold | 
                                       abs(diagnostics$residuals_std) > residuals_threshold, ], 
                  aes(label = index),   # Usa l'indice pre-calcolato per le etichette
                  size = 4, 
                  color = "black", 
                  nudge_y = 0.5, 
                  show.legend = FALSE)   # Aggiunge etichette ai punti che superano le soglie

#### ROBUST REGRESSION | BOOTSTRAP  #### 

#al momento siamo arrivati a questo modello che segue (logit3_a) 
logit3_a <- glm(formula = Music.effects ~ Age + I(Age^2) + Hours.per.day + 
                  While.working + Instrumentalist + Composer + Exploratory + 
                  Foreign.languages + BPM + Frequency..Country.grouped + Frequency..EDM. + 
                  Frequency..Latin.grouped + Frequency..Pop. + Frequency..R.B.grouped + 
                  Frequency..Rock. + RC1 + RC2, family = binomial(link = "logit"), 
                data = r_logit_clean) 

#summary(logit3_a) #AIC = 451.94 
#R2 = 1 - (403.94 / 604.39) #0.3316 
#drop1(logit3_a, test = "LRT") 
#hoslem.test(r_logit_clean$Music.effects, fitted(logit3_a), g = 10) #p = 0.1476 

# Carica i pacchetti necessari
library("car")  
library("boot")   

# Applica il bootstrap al modello logit
BOOT.MOD <- Boot(logit3_a, R = 1000)

# Visualizza il sommario del bootstrap (con momenti elevati per analisi avanzata)
summary(BOOT.MOD, high.moments = TRUE)

# Calcola gli intervalli di confidenza (per esempio, al 95%) 
Confint(BOOT.MOD, level = c(0.95), type = "perc")
library(coefplot)
coefplot(logit3_a, intercept=FALSE)
# Crea un istogramma dei coefficienti bootstrap
hist(BOOT.MOD, legend = "separate")


#### GRAFICO ODDS RATIO ED IC95 ####
# Carica il pacchetto sjPlot
#install.packages("sjPlot")
library(sjPlot)

# Crea il grafico degli odds ratio con intervalli di confidenza direttamente dal modello
plot_model(logit3_a, 
           type = "est", 
           transform = "exp",  # Trasforma i coefficienti in odds ratio
           show.values = TRUE, 
           show.p = TRUE,
           title = "Odds Ratio e Intervalli di Confidenza")


#### ACCURACY ####

r_logit_clean$predicted_p <- predict(logit3_a, r_logit_clean, type="response") 
tail(r_logit_clean)

# predicted target
r_logit_clean$predicted_y <- ifelse(r_logit_clean$predicted_p > 0.5,1,0) 

table(observed=r_logit_clean$Music.effects, predicted=r_logit_clean$predicted_y)/nrow(good_covariates)
accuracy = 0.06741 + 0.7233 #0.79 di accuracy , buono! 


# Crea una colonna per il colore basata sulla correttezza della previsione
r_logit_clean$color <- ifelse(r_logit_clean$predicted_y == r_logit_clean$Music.effects, "green", "red")

# Carica ggplot2 per creare il grafico
library(ggplot2)

# Crea lo scatter plot
ggplot(r_logit_clean, aes(x = 1:nrow(r_logit_clean), y = predicted_p)) +
  geom_point(aes(color = color)) +
  scale_color_identity() +  # Usa i colori specificati in 'color' senza mappare ad una scala
  labs(title = "Scatter Plot delle Probabilità Predette",
       x = "Osservazioni",
       y = "Probabilità Predetta",
       color = "Correttezza") +
  theme_minimal()





#### VALUTAZIONE CAPACITA' DISCRIMINATIVA CON AUC  #### 

# Caricare i pacchetti necessari
library(pROC)
library(boot)

# Calcola l'AUC del modello logit3_a sul dataset r_logit_clean
roc_curve <- roc(r_logit_clean$Music.effects, predict(logit3_a, type = "response"))
auc_initial <- auc(roc_curve)
print(paste("AUC iniziale del modello:", round(auc_initial, 3)))

# Definisce una funzione per calcolare l'AUC, che verrà usata nella procedura bootstrap
calc_auc <- function(data, indices) {
  # Sottocampiona il dataset in base agli indici bootstrap
  data_bootstrap <- data[indices, ]
  
  # Adatta il modello sui dati campionati
  model <- glm(formula = Music.effects ~ Age + I(Age^2) + Hours.per.day + 
                 While.working + Instrumentalist + Composer + Exploratory + 
                 Foreign.languages + BPM + Frequency..Country.grouped + Frequency..EDM. + 
                 Frequency..Latin.grouped + Frequency..Pop. + Frequency..R.B.grouped + 
                 Frequency..Rock. + RC1 + RC2, 
               family = binomial(link = "logit"), data = data_bootstrap)
  
  # Calcola l'AUC sul modello bootstrap usando la predizione del modello
  roc_boot <- roc(data_bootstrap$Music.effects, predict(model, type = "response"))
  return(auc(roc_boot))
}

# Esegue il bootstrap per stimare la distribuzione dell'AUC
set.seed(123)  # Fissa il seme per la riproducibilità
auc_results <- boot(data = r_logit_clean, statistic = calc_auc, R = 1000)

# Calcola l'intervallo di confidenza al 95% per l'AUC
ci_auc <- boot.ci(auc_results, type = "perc")
print(ci_auc)

# Mostra la media dell'AUC dai risultati bootstrap
mean_auc <- mean(auc_results$t)
print(paste("Media dell'AUC (bootstrap):", round(mean_auc, 3))) #"Media dell'AUC (bootstrap): 0.895"

# Visualizza la distribuzione dell'AUC calcolata con il bootstrap
hist(auc_results$t, breaks = 30, main = "Distribuzione AUC bootstrap", xlab = "AUC")
abline(v = auc_initial, col = "red", lwd = 2, lty = 2)  # AUC iniziale

#Dato che l'intervallo è relativamente stretto e i valori di AUC sono ben al 
#di sopra di 0.8, il modello mostra buona stabilità e capacità discriminativa. 
#In altre parole, è in grado di distinguere efficacemente tra le classi della 
#variabile target.


