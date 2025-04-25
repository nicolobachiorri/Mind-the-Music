rm(list=ls())
options(scipen=999)
setwd("/Users/nicolobachiorri/Desktop/PROGETTODM")


#### PREPARAZIONE INIZIALE DATASET ####
r <- read.csv("dataset.csv", header = T, sep = ";", stringsAsFactors = T, na.strings=c("NA","NaN", "","Unknown"))
r<-r[,-1] #per eliminare la prima colonna di identificazione (indica data e ora in cui viene compilato il questionario)
#r <- r[, -ncol(r)] #per eliminare l'ultima colonna (è una factor a un livello che indica l'approvazione alla pubblicazione dei dati)
# Controlla se ci sono colonne con valori costanti 
apply(r, 2, function(x) length(unique(x)) == 1)
# Rimozione delle variabili costanti
r <- r[, apply(r, 2, function(x) length(unique(x)) > 1)]
#visualizzo le prime 5 righe
#head(r)
#vedo le caratteristiche delle variabili
str(r)



#### MODIFICHE DELLE VARIABILI ####

#cambio le variabili di rating in fattoriali
r$Anxiety <- as.factor(r$Anxiety)
r$Depression <- as.factor(r$Depression)
r$Insomnia <- as.factor(r$Insomnia)
r$OCD <- as.factor(r$OCD)

#rendo [0,1] le variabili binarie e le tratto come fattoriali
r$Music.effects <- ifelse(r$Music.effects == "No effect", 0, 1)
r$Music.effects <- as.factor(r$Music.effects)

r$While.working <- ifelse(r$While.working == "No", 0, 1)
r$While.working <- as.factor(r$While.working)

r$Instrumentalist <- ifelse(r$Instrumentalist == "No", 0, 1)
r$Instrumentalist <- as.factor(r$Instrumentalist)

r$Composer <- ifelse(r$Composer == "No", 0, 1)
r$Composer <- as.factor(r$Composer)

r$Exploratory <- ifelse(r$Exploratory == "No", 0, 1)
r$Exploratory <- as.factor(r$Exploratory)

r$Foreign.languages <- ifelse(r$Foreign.languages == "No", 0, 1)
r$Foreign.languages <- as.factor(r$Foreign.languages)

#vedo le caratteristiche delle variabili dopo i cambiamenti
str(r)

#variabili di tipo factor
isfactor <- sapply(r, function(x) is.factor(x))
factordata <- r[, isfactor]
sapply(factordata, function(x) length(levels(x)))

#variabili di tipon umerico
num <- sapply(r, function(x) is.numeric(x))
numdata <- r[, num]
#sapply(numdata, function(x) var(x))
# var=na if there are NA.......
sapply(numdata, function(x) var(x,na.rm=TRUE)) 

length(numdata) + length(factordata) == length(r)   #confermi che siano tutte le variabili



#### MISSING VALUES ####

#conto i missing
sapply(r, function(x)(sum(is.na(x))))

#install.packages("mice")    # Installazione del pacchetto mice
library(mice)    # Caricamento della libreria
imputed_data <- mice(r, m = 5, method = 'pmm', maxit = 50, seed = 123)   #restituisce errore: cerchiamo il problema

#osservo la distrubuzione dei missing values
library(VIM)
missingness<- aggr(r, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE,labels=names(r), cex.axis=.7,gap=3)

#creo un modello per osservare la relazione tra BPM (variabile con più missing) e Fav.genre che dovrebbe essere molto alta
modello_BPM <- lm(BPM~Fav.genre, data=r, na.action = na.exclude)
summary(modello_BPM)

#utilizzo un box plot per visualizzare graficamente il modello 
par(mfrow = c(1, 1))
boxplot(BPM~Fav.genre, data=r, na.action = na.exclude)   #individuo un errore: risulta che una persona ascolta musica da 1000000000 BPM!

#cerco di quale osservazione si tratta e la rimuovo
which(r$BPM > 1000)  
r_no535 <- r[-535,]

#rimuovo anche le osservazioni con BPM < 20 perchè soglia minima sotto cui non è credibile
which(r_no535$BPM < 20)
r_no_error <- r_no535[-c(136, 285, 393, 682, 694),] 

#riprovo ad applicare il modello
modello_BPM <- lm(BPM~Fav.genre, data=r_no_error, na.action = na.exclude)
summary(modello_BPM)

boxplot(BPM~Fav.genre, data=r_no_error, na.action = na.exclude)
which(r_no_error$BPM > 600)   #individuo un'altro valore anomalo a riga 474 che rimuoviamo
r_no_error <- r_no_error[-474,]

r_no_error$bpm_missing<-ifelse(sapply(r_no_error$BPM, function(x)(is.na(x)))==T, 0,1)   #creo una dummy in cui 0 è missimg e 1 è un valore presente
modello_NA<-glm(bpm_missing~Fav.genre, data=r_no_error, family="binomial")
summary(modello_NA)
drop1(modello_NA, test="F")
#distribuzione di Fav.genre è significatica per spiegare la presenza o l'assenza del dato della variabile BPM.
#possiamo imputare usando l'algoritmo mice: i missing values sono MISSING AT RANDOM

#riprovimo a imputare i dati con mice (togliendo dal dataset la dummy creata per la verifica della significatività)
imputed_data <- mice(r_no_error[,-32], m = 5, method = 'pmm', maxit = 50, seed = 123)
r_completo <- complete(imputed_data,1)

sapply(r_completo, function(x)(sum(is.na(x))))


#### da qui per non RIfare la mice ####

#per non rifare sempre la mice salvo il dataser completo nella cartella di lavoro
#write.csv(r_completo, file = "dataset_completo.csv", row.names = FALSE)
r_completo <- read.csv("dataset_completo.csv", header = T, sep = ",", stringsAsFactors = T, na.strings=c("NA","NaN", "","Unknown"))
#cambio di nuovo le variabili di rating in fattoriali
r_completo$Anxiety <- as.factor(r_completo$Anxiety)
r_completo$Depression <- as.factor(r_completo$Depression)
r_completo$Insomnia <- as.factor(r_completo$Insomnia)
r_completo$OCD <- as.factor(r_completo$OCD)
r_completo$Music.effects <- as.factor(r_completo$Music.effects)
r_completo$While.working <- as.factor(r_completo$While.working)
r_completo$Instrumentalist <- as.factor(r_completo$Instrumentalist)
r_completo$Composer <- as.factor(r_completo$Composer)
r_completo$Exploratory <- as.factor(r_completo$Exploratory)
r_completo$Foreign.languages <- as.factor(r_completo$Foreign.languages)



#### MODELLO INIZIALE ####

modello_iniziale <- lm(Hours.per.day ~ ., r_completo)
summary(modello_iniziale)   #0.2045
drop1(modello_iniziale, test="F")

#applihiamo il resettest al modello iniziale
library(lmtest)
library(zo)
resettest(modello_iniziale, power = 2, type = "fitted",  data = r_completo)   #0.00000000000000022  E NON VA BENE !!! Abbiamo (FORSE) problemi di linearità. 
par(mfrow = c(2, 2))
plot(modello_iniziale) #i grafici evidenziano ETEROSCHEDASTICITA'  E NON NORMALITA' DEI RESIDUI 



#### MULTICOLLINEARITA' ####

library(mctest)
library(car)  
vif(modello_iniziale)  #VIF troppo elevato per Fav. genre 

m1_a<-lm(Hours.per.day ~ . - Fav.genre , r_completo)
vif(m1_a)   #VIF elevato per Anxiety
table(r_completo$Anxiety, r_completo$Depression) #anxiety e depression appaiono linearmente correlate, rimuoviamo anxiety
par(mfrow = c(1, 1))
mosaicplot(table(r_completo$Anxiety, r_completo$Depression), main="ANXIETY-DEPRESSION", shade=TRUE)

m1_b<-lm(Hours.per.day ~ . - Fav.genre - Anxiety , r_completo)
vif(m1_b)  #VIF elevato per Frequency..Hip.hop. e Rap.
table(r_completo$Frequency..Hip.hop., r_completo$Frequency..Rap.)   # Hip.hop. e Rap. appaiono linearmente correlate, rimuoviamo Hip.hop.
mosaicplot(table(r_completo$Frequency..Hip.hop., r_completo$Frequency..Rap.), main = "FREQUENCY_HIPHOP-FREQUENCY_RAP", shade = TRUE)

m1_c<-lm(Hours.per.day~.-Fav.genre -Anxiety -Frequency..Hip.hop. , r_completo)
vif(m1_c)   #nessun VIF sopra al 5: ci riteniamo soddisfatti per quanto riguarda la collinearità tra variabili

summary(m1_c)   #0.1954
drop1(m1_c, test="F")

#applichiamo il resettest al modello risultante
resettest(m1_c, power = 2, type = "fitted",  data = r_completo)   #0.00000000000000022
par(mfrow = c(2, 2))
plot(m1_c) 



#### LINEARITA' Y - TRASFORMAZIONE BOX COX ####

#la box cox cerca di trovare una trasformazione ottimale dei dati che migliori l'assunzione di normalità dei residui
library(MASS)
#install.packages("lmtest")  # Run this line if you haven't installed the package yet
library(lmtest)
boxcox_m1_c<-boxcox(m1_c)   #Error in boxcox.default(m1_c) : response variable must be positive

#cerco quali osservazioni hanno variabile risposta nulla e le rimuovo perchè non rilevanti per la nostra analisi in quanto non ascoltano musica
which(r_completo$Hours.per.day == 0)   #145 179 201 231
r_completo<-r_completo[-c(145, 179, 201, 231),]
any(rownames(r_completo) %in% c(145, 179, 201, 231))   #confermiamo di averle tolte

boxcox_m1_c<-boxcox(m1_c)
lambda=boxcox_m1_c$x[which.max(boxcox_m1_c$y)]
lambda   #0.06060606 trasformazione logaritmica è consigliata 

m2_a<-lm(log(Hours.per.day)~.-Fav.genre-Anxiety-Frequency..Hip.hop. , r_completo) #i modelli da m2 saranno POST TRASFORMAZIONE BOX-COX
summary(m2_a)   #0.2467
drop1(m2_a, test="F")

#applichiamo il resettest al modello con trasformazione logaritmica
resettest(m2_a, power = 2, type = "fitted",  data = r_completo)   # 0.383
par(mfrow = c(2, 2))
plot(m2_a)



#### LINEARITA' X QUANTITATIVE ####

library(gam) 
m2_a_gam<- gam(log(Hours.per.day) ~ s(Age) + Primary.streaming.service + While.working + 
                 Instrumentalist + Composer + Exploratory + Foreign.languages + 
                 s(BPM) + Frequency..Classical. + Frequency..Country. + Frequency..EDM. + 
                 Frequency..Folk. + Frequency..Gospel. + Frequency..Jazz. + 
                 Frequency..K.pop. + Frequency..Latin. + Frequency..Lofi. + 
                 Frequency..Metal. + Frequency..Pop. + Frequency..R.B. + 
                 Frequency..Rap. + Frequency..Rock. + Frequency..Video.game.music. + 
                 Depression + Insomnia + OCD + Music.effects
               , data=r_completo)
anova(m2_a,m2_a_gam, test="F")   #confrontando i modelli noto la presenza di una relazione non lineare --> mod2 più significativo
summary(m2_a_gam)   # s(age) SIGNIFICATIVA DUNQUE AGE PUO' NON ESSERE LINEARE --> identifico la presenza di correlazione non lineare tra age e log(hours.per.day)

#guardiamo i grafici per dedurre quale funzione potrebbe essere appropriata per descriverle
par(mfrow=c(1,1))
plot(m2_a_gam)   #posso ipotizzare relazione quadratica su Age 

m2_b<-lm(log(Hours.per.day)~ Age + I(Age^2) + Primary.streaming.service + While.working + 
           Instrumentalist + Composer + Exploratory + Foreign.languages + 
           BPM + Frequency..Classical. + Frequency..Country. + Frequency..EDM. + 
           Frequency..Folk. + Frequency..Gospel. + Frequency..Jazz. + 
           Frequency..K.pop. + Frequency..Latin. + Frequency..Lofi. + 
           Frequency..Metal. + Frequency..Pop. + Frequency..R.B. + 
           Frequency..Rap. + Frequency..Rock. + Frequency..Video.game.music. + 
           Depression + Insomnia + OCD + Music.effects
         , data=r_completo)
summary(m2_b)   #0.2569 manteniamo la trasformazione quadratica 

drop1(m2_b, test="F")
resettest(m2_b, power = 2, type = "fitted",  data = r_completo[,-1])   # 0.3644 
par(mfrow = c(2, 2))
plot(m2_b)



#### LINEARITA' X CATEGORICHE ####

#install.packages("factorMerger")

library(factorMerger)
library(dplyr)

r_completo$Log.Hours.per.day = log(r_completo$Hours.per.day)
#plot(r_completo$Log.Hours.per.day, r_completo$Hours.per.day)

#### mental health ####

reduce_levels_Anxiety<-mergeFactors(response=r_completo$Log.Hours.per.day, factor=r_completo$Anxiety)
plot(reduce_levels_Anxiety, panelGrid = FALSE)   #suddivisone in gruppi non interpretabile

reduce_levels_Depression<-mergeFactors(response=r_completo$Log.Hours.per.day, factor=r_completo$Depression)
plot(reduce_levels_Depression, panelGrid = FALSE)   #suddivisone in gruppi non interpretabile

reduce_levels_Insomnia<-mergeFactors(response=r_completo$Log.Hours.per.day, factor=r_completo$Insomnia)
plot(reduce_levels_Insomnia, panelGrid = FALSE)   #suddivisone in gruppi non interpretabile

reduce_levels_OCD<-mergeFactors(response=r_completo$Log.Hours.per.day, factor=r_completo$OCD)
plot(reduce_levels_OCD, panelGrid = FALSE)   #suddivisone in gruppi non interpretabile

#### primary streaming service ####

reduce_levels_Primary.streaming.service<-mergeFactors(response=r_completo$Log.Hours.per.day, factor=r_completo$Primary.streaming.service)
plot(reduce_levels_Primary.streaming.service, panelGrid = FALSE)   #suddivisione accettabile (Sptf e AppM sono le più diffuse)

r_completo$Primary.streaming.service.grouped <- as.factor(as.numeric(cutTree(reduce_levels_Primary.streaming.service))) #questo mi serve per riportare il cutTree prima a numeric e poi a fattoriale
table(cutTree(reduce_levels_Primary.streaming.service), r_completo$Primary.streaming.service.grouped) 

str(r_completo) #$ Primary.streaming.service.grouped: Factor w/ 2 levels "1","2"

par(mfrow = c(1, 1))
plot(r_completo$Primary.streaming.service.grouped, r_completo$Log.Hours.per.day) 

a <- lm(Log.Hours.per.day ~ Primary.streaming.service, r_completo) ; summary(a)
b <- lm(Log.Hours.per.day ~ Primary.streaming.service.grouped, r_completo) ; summary(b)   #sembra molto più significativa !!! 
m2_c<-lm(log(Hours.per.day)~ Age + I(Age^2) + Primary.streaming.service.grouped + While.working + 
           Instrumentalist + Composer + Exploratory + Foreign.languages + 
           BPM + Frequency..Classical. + Frequency..Country. + Frequency..EDM. + 
           Frequency..Folk. + Frequency..Gospel. + Frequency..Jazz. + 
           Frequency..K.pop. + Frequency..Latin. + Frequency..Lofi. + 
           Frequency..Metal. + Frequency..Pop. + Frequency..R.B. + 
           Frequency..Rap. + Frequency..Rock. + Frequency..Video.game.music. + 
           Depression + Insomnia + OCD + Music.effects , data=r_completo)
summary(m2_b)   #0.2569
summary(m2_c) #0.2596 manteniamo il raggruppamento perchè aumenta la significatività e l'adattamento

#### frequency ####

reduce_levels_Frequency..Classical.<-mergeFactors(response=r_completo$Log.Hours.per.day, factor=r_completo$Frequency..Classical.)
plot(reduce_levels_Frequency..Classical., panelGrid = FALSE)   #gruppo unico - quindi non modifichiamo nulla!

reduce_levels_Frequency..Country.<-mergeFactors(response=r_completo$Log.Hours.per.day, factor=r_completo$Frequency..Country.)
plot(reduce_levels_Frequency..Country., panelGrid = FALSE) #due gruppi 

r_completo$Frequency..Country.grouped <- as.factor(as.numeric(cutTree(reduce_levels_Frequency..Country.)))
table(r_completo$Frequency..Country.grouped, cutTree(reduce_levels_Frequency..Country.))
plot(r_completo$Frequency..Country.grouped, r_completo$Log.Hours.per.day)
m2_d<-lm(log(Hours.per.day)~ Age + I(Age^2) + Primary.streaming.service.grouped + While.working + 
           Instrumentalist + Composer + Exploratory + Foreign.languages + 
           BPM + Frequency..Classical. + Frequency..Country.grouped + Frequency..EDM. + 
           Frequency..Folk. + Frequency..Gospel. + Frequency..Jazz. + 
           Frequency..K.pop. + Frequency..Latin. + Frequency..Lofi. + 
           Frequency..Metal. + Frequency..Pop. + Frequency..R.B. + 
           Frequency..Rap. + Frequency..Rock. + Frequency..Video.game.music. + 
           Depression + Insomnia + OCD + Music.effects, r_completo)
summary(m2_c)   #0.2596
summary(m2_d)   #0.2599

reduce_levels_Frequency..EDM.<-mergeFactors(response=r_completo$Log.Hours.per.day, factor=r_completo$Frequency..EDM.) #2 gruppi (distingue fra chi sì e chi no)
plot(reduce_levels_Frequency..EDM., panelGrid = FALSE)
r_completo$Frequency..EDM.grouped <- as.factor(as.numeric(cutTree(reduce_levels_Frequency..EDM.))) 
table(r_completo$Frequency..EDM.grouped, cutTree(reduce_levels_Frequency..EDM.))
plot(r_completo$Frequency..EDM.grouped, r_completo$Log.Hours.per.day)



r_completo$Frequency..EDM.grouped <- ifelse(r_completo$Frequency..EDM.grouped == 1, 0, 1) 
r_completo$Frequency..EDM.grouped <- as.factor(r_completo$Frequency..EDM.grouped)

#head(r_completo)

colnames(r_completo)[colnames(r_completo)=="Frequency..EDM.grouped"] <- "Listens.to.EDM"
m2_e<-lm(log(Hours.per.day)~ Age + I(Age^2) + Primary.streaming.service.grouped + While.working + 
           Instrumentalist + Composer + Exploratory + Foreign.languages + 
           BPM + Frequency..Classical. + Frequency..Country.grouped + Listens.to.EDM + 
           Frequency..Folk. + Frequency..Gospel. + Frequency..Jazz. + 
           Frequency..K.pop. + Frequency..Latin. + Frequency..Lofi. + 
           Frequency..Metal. + Frequency..Pop. + Frequency..R.B. + 
           Frequency..Rap. + Frequency..Rock. + Frequency..Video.game.music. + 
           Depression + Insomnia + OCD + Music.effects, r_completo)
summary(m2_d)   #0.2599
summary(m2_e)   #0.2615 

reduce_levels_Frequency..Folk.<-mergeFactors(response=r_completo$Log.Hours.per.day, factor=r_completo$Frequency..Folk.)
plot(reduce_levels_Frequency..Folk., panelGrid = FALSE)
r_completo$Frequency..Folk.grouped <- as.factor(as.numeric(cutTree(reduce_levels_Frequency..Folk.)))
table(r_completo$Frequency..Folk.grouped, cutTree(reduce_levels_Frequency..Folk.))
plot(r_completo$Frequency..Folk.grouped, r_completo$Log.Hours.per.day)
m2_f<-lm(log(Hours.per.day)~ Age + I(Age^2) + Primary.streaming.service.grouped + While.working + 
           Instrumentalist + Composer + Exploratory + Foreign.languages + 
           BPM + Frequency..Classical. + Frequency..Country.grouped + Listens.to.EDM + 
           Frequency..Folk.grouped + Frequency..Gospel. + Frequency..Jazz. + 
           Frequency..K.pop. + Frequency..Latin. + Frequency..Lofi. + 
           Frequency..Metal. + Frequency..Pop. + Frequency..R.B. + 
           Frequency..Rap. + Frequency..Rock. + Frequency..Video.game.music. + 
           Depression + Insomnia + OCD + Music.effects, r_completo)
summary(m2_e)   #0.2615 
summary(m2_f)   #0.2634

reduce_levels_Frequency..Gospel.<-mergeFactors(response=r_completo$Log.Hours.per.day, factor=r_completo$Frequency..Gospel.)
plot(reduce_levels_Frequency..Gospel., panelGrid = FALSE)   #gruppo unico

reduce_levels_Frequency..Jazz.<-mergeFactors(response=r_completo$Log.Hours.per.day, factor=r_completo$Frequency..Jazz.) #forma 3 gruppi:  (Vryf),  (Smtm) e (Rrly - Nevr)
plot(reduce_levels_Frequency..Jazz., panelGrid = FALSE)
r_completo$Frequency..Jazz.grouped <- as.factor(as.numeric(cutTree(reduce_levels_Frequency..Jazz.)))
table(r_completo$Frequency..Jazz.grouped, cutTree(reduce_levels_Frequency..Jazz.))
plot(r_completo$Frequency..Jazz.grouped, r_completo$Log.Hours.per.day)
m2_g<-lm(log(Hours.per.day)~ Age + I(Age^2) + Primary.streaming.service.grouped + While.working + 
           Instrumentalist + Composer + Exploratory + Foreign.languages + 
           BPM + Frequency..Classical. + Frequency..Country.grouped + Listens.to.EDM + 
           Frequency..Folk.grouped + Frequency..Gospel. + Frequency..Jazz.grouped + 
           Frequency..K.pop. + Frequency..Latin. + Frequency..Lofi. + 
           Frequency..Metal. + Frequency..Pop. + Frequency..R.B. + 
           Frequency..Rap. + Frequency..Rock. + Frequency..Video.game.music. + 
           Depression + Insomnia + OCD + Music.effects, r_completo)
summary(m2_f)   #0.2634
summary(m2_g)   #0.2645

reduce_levels_Frequency..K.pop.<-mergeFactors(response=r_completo$Log.Hours.per.day, factor=r_completo$Frequency..K.pop.)
plot(reduce_levels_Frequency..K.pop., panelGrid = FALSE)
r_completo$Frequency..K.pop.grouped <- as.factor(as.numeric(cutTree(reduce_levels_Frequency..K.pop.)))
table(r_completo$Frequency..K.pop.grouped, cutTree(reduce_levels_Frequency..K.pop.))
plot(r_completo$Frequency..K.pop.grouped, r_completo$Log.Hours.per.day)
r_completo$Frequency..K.pop.grouped <- ifelse(r_completo$Frequency..K.pop.grouped == 1, 0, 1)
r_completo$Frequency..K.pop.grouped <- as.factor(r_completo$Frequency..K.pop.grouped)
colnames(r_completo)[colnames(r_completo)=="Frequency..K.pop.grouped"] <- "Listens.to.K.pop"
m2_h<-lm(log(Hours.per.day)~ Age + I(Age^2) + Primary.streaming.service.grouped + While.working + 
           Instrumentalist + Composer + Exploratory + Foreign.languages + 
           BPM + Frequency..Classical. + Frequency..Country.grouped + Listens.to.EDM + 
           Frequency..Folk.grouped + Frequency..Gospel. + Frequency..Jazz.grouped + 
           Listens.to.K.pop + Frequency..Latin. + Frequency..Lofi. + 
           Frequency..Metal. + Frequency..Pop. + Frequency..R.B. + 
           Frequency..Rap. + Frequency..Rock. + Frequency..Video.game.music. + 
           Depression + Insomnia + OCD + Music.effects, r_completo)
summary(m2_g)   #0.2645
summary(m2_h)   #0.2663

reduce_levels_Frequency..Latin.<-mergeFactors(response=r_completo$Log.Hours.per.day, factor=r_completo$Frequency..Latin.) #3 gruppi 
plot(reduce_levels_Frequency..Latin., panelGrid = FALSE)
r_completo$Frequency..Latin.grouped <- as.factor(as.numeric(cutTree(reduce_levels_Frequency..Latin.)))
table(r_completo$Frequency..Latin.grouped, cutTree(reduce_levels_Frequency..Latin.))
plot(r_completo$Frequency..Latin.grouped, r_completo$Log.Hours.per.day)
m2_i<-lm(log(Hours.per.day)~ Age + I(Age^2) + Primary.streaming.service.grouped + While.working + 
           Instrumentalist + Composer + Exploratory + Foreign.languages + 
           BPM + Frequency..Classical. + Frequency..Country.grouped + Listens.to.EDM + 
           Frequency..Folk.grouped + Frequency..Gospel. + Frequency..Jazz.grouped + 
           Listens.to.K.pop + Frequency..Latin.grouped + Frequency..Lofi. + 
           Frequency..Metal. + Frequency..Pop. + Frequency..R.B. + 
           Frequency..Rap. + Frequency..Rock. + Frequency..Video.game.music. + 
           Depression + Insomnia + OCD + Music.effects, r_completo)
summary(m2_h)   #0.2663
summary(m2_i)   #0.2668

reduce_levels_Frequency..Lofi.<-mergeFactors(response=r_completo$Log.Hours.per.day, factor=r_completo$Frequency..Lofi.) #3 gruppi
plot(reduce_levels_Frequency..Lofi., panelGrid = FALSE)
r_completo$Frequency..Lofi.grouped <- as.factor(as.numeric(cutTree(reduce_levels_Frequency..Lofi.)))
table(r_completo$Frequency..Lofi.grouped, cutTree(reduce_levels_Frequency..Lofi.))
plot(r_completo$Frequency..Lofi.grouped, r_completo$Log.Hours.per.day)
m2_l<-lm(log(Hours.per.day)~ Age + I(Age^2) + Primary.streaming.service.grouped + While.working + 
           Instrumentalist + Composer + Exploratory + Foreign.languages + 
           BPM + Frequency..Classical. + Frequency..Country.grouped + Listens.to.EDM + 
           Frequency..Folk.grouped + Frequency..Gospel. + Frequency..Jazz.grouped + 
           Listens.to.K.pop + Frequency..Latin.grouped + Frequency..Lofi.grouped + 
           Frequency..Metal. + Frequency..Pop. + Frequency..R.B. + 
           Frequency..Rap. + Frequency..Rock. + Frequency..Video.game.music. + 
           Depression + Insomnia + OCD + Music.effects, r_completo)
summary(m2_i)   #0.2668
summary(m2_l)   #0.2678

reduce_levels_Frequency..Metal.<-mergeFactors(response=r_completo$Log.Hours.per.day, factor=r_completo$Frequency..Metal.)
plot(reduce_levels_Frequency..Metal., panelGrid = FALSE)
r_completo$Frequency..Metal.grouped <- as.factor(as.numeric(cutTree(reduce_levels_Frequency..Metal.)))
table(r_completo$Frequency..Metal.grouped, cutTree(reduce_levels_Frequency..Metal.))
plot(r_completo$Frequency..Metal.grouped, r_completo$Log.Hours.per.day)
m2_m<-lm(log(Hours.per.day)~ Age + I(Age^2) + Primary.streaming.service.grouped + While.working + 
           Instrumentalist + Composer + Exploratory + Foreign.languages + 
           BPM + Frequency..Classical. + Frequency..Country.grouped + Listens.to.EDM + 
           Frequency..Folk.grouped + Frequency..Gospel. + Frequency..Jazz.grouped + 
           Listens.to.K.pop + Frequency..Latin.grouped + Frequency..Lofi.grouped + 
           Frequency..Metal.grouped + Frequency..Pop. + Frequency..R.B. + 
           Frequency..Rap. + Frequency..Rock. + Frequency..Video.game.music. + 
           Depression + Insomnia + OCD + Music.effects, r_completo)
summary(m2_l)   #0.2678
summary(m2_m)   #0.2688

reduce_levels_Frequency..Pop.<-mergeFactors(response=r_completo$Log.Hours.per.day, factor=r_completo$Frequency..Pop.)
plot(reduce_levels_Frequency..Pop., panelGrid = FALSE)   #gruppo unico

reduce_levels_Frequency..R.B.<-mergeFactors(response=r_completo$Log.Hours.per.day, factor=r_completo$Frequency..R.B.) 
plot(reduce_levels_Frequency..R.B., panelGrid = FALSE) #due gruppi 
r_completo$Frequency..R.B.grouped <- as.factor(as.numeric(cutTree(reduce_levels_Frequency..R.B.)))
table(r_completo$Frequency..R.B.grouped, cutTree(reduce_levels_Frequency..R.B.))
plot(r_completo$Frequency..R.B.grouped, r_completo$Log.Hours.per.day)
m2_n<-lm(log(Hours.per.day)~ Age + I(Age^2) + Primary.streaming.service.grouped + While.working + 
           Instrumentalist + Composer + Exploratory + Foreign.languages + 
           BPM + Frequency..Classical. + Frequency..Country.grouped + Listens.to.EDM + 
           Frequency..Folk.grouped + Frequency..Gospel. + Frequency..Jazz.grouped + 
           Listens.to.K.pop + Frequency..Latin.grouped + Frequency..Lofi.grouped + 
           Frequency..Metal.grouped + Frequency..Pop. + Frequency..R.B.grouped + 
           Frequency..Rap. + Frequency..Rock. + Frequency..Video.game.music. + 
           Depression + Insomnia + OCD + Music.effects, r_completo)
summary(m2_m)   #0.2688
summary(m2_n)   #0.2702

reduce_levels_Frequency..Rap.<-mergeFactors(response=r_completo$Log.Hours.per.day, factor=r_completo$Frequency..Rap.)
plot(reduce_levels_Frequency..Rap., panelGrid = FALSE) #3 gruppi 
r_completo$Frequency..Rap.grouped <- as.factor(as.numeric(cutTree(reduce_levels_Frequency..Rap.)))
table(r_completo$Frequency..Rap.grouped, cutTree(reduce_levels_Frequency..Rap.))
plot(r_completo$Frequency..Rap.grouped, r_completo$Log.Hours.per.day)
m2_o<-lm(log(Hours.per.day)~ Age + I(Age^2) + Primary.streaming.service.grouped + While.working + 
           Instrumentalist + Composer + Exploratory + Foreign.languages + 
           BPM + Frequency..Classical. + Frequency..Country.grouped + Listens.to.EDM + 
           Frequency..Folk.grouped + Frequency..Gospel. + Frequency..Jazz.grouped + 
           Listens.to.K.pop + Frequency..Latin.grouped + Frequency..Lofi.grouped + 
           Frequency..Metal.grouped + Frequency..Pop. + Frequency..R.B.grouped + 
           Frequency..Rap.grouped + Frequency..Rock. + Frequency..Video.game.music. + 
           Depression + Insomnia + OCD + Music.effects, r_completo)
summary(m2_n)   #0.2702
summary(m2_o)   #0.2711 

reduce_levels_Frequency..Rock.<-mergeFactors(response=r_completo$Log.Hours.per.day, factor=r_completo$Frequency..Rock.)
plot(reduce_levels_Frequency..Rock., panelGrid = FALSE)   #suddivisione non interpretabile 

reduce_levels_Frequency..Video.game.music.<-mergeFactors(response=r_completo$Log.Hours.per.day, factor=r_completo$Frequency..Video.game.music.)
plot(reduce_levels_Frequency..Video.game.music., panelGrid = FALSE)   #suddivisione non interpretabile

summary(m2_o)   ##0.2711
drop1(m2_o, test="F")   
resettest(m2_o, power = 2, type = "fitted",  data = r_completo)   #0.4057 aumento notevole del p value dopo le grouping (prima 0.3644)
par(mfrow = c(2, 2))
plot(m2_o)



#### PCA mental health ####

pca_data=r_completo[,c('Anxiety','Depression','Insomnia','OCD')]
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
r_completo=cbind(r_completo,pc)
#View(r_completo)

m2_p<-lm(log(Hours.per.day)~ Age + I(Age^2) + Primary.streaming.service.grouped + While.working + 
            Instrumentalist + Composer + Exploratory + Foreign.languages + 
            BPM + Frequency..Classical. + Frequency..Country.grouped + Listens.to.EDM + 
            Frequency..Folk.grouped + Frequency..Gospel. + Frequency..Jazz.grouped + 
            Listens.to.K.pop + Frequency..Latin.grouped + Frequency..Lofi.grouped + 
            Frequency..Metal.grouped + Frequency..Pop. + Frequency..R.B.grouped + 
            Frequency..Rap.grouped + Frequency..Rock. + Frequency..Video.game.music. + 
            RC1 + RC2 + Music.effects, r_completo) 
summary(m2_p)   #0.2573 diminuzione dell'adattamento (prima 0.2711) - ce lo aspettavamo in quanto le pca hanno meno informazioni (30% in meno) - ma dovremmo risolvere problemi di collinearità fra variabili. 
resettest(m2_p, power = 2, type = "fitted",  data = r_completo)   #0.5814 aumento del p value (prima 0.4057)
drop1(m2_p, test="F")



#### RICONTROLLO COLLINEARITA' DOPO TRASFORMAZIONI ####

vif(m2_p)  #collinearità forte tra Age e Age al quadrato, come è ragionevole che sia, non è un problema



#### MODEL SELECTION TRAMITE STEPAIC E STEPWISE ####

stepAIC(m2_p)
step(m2_p, direction="both")
#arrivano allo stesso modello
m3_a<-lm(formula = log(Hours.per.day) ~ Age + I(Age^2) + Primary.streaming.service.grouped + 
            While.working + Instrumentalist + Composer + Exploratory + 
            Frequency..Country.grouped + Frequency..Folk.grouped + Frequency..Gospel. + 
            Listens.to.K.pop + Frequency..Metal.grouped + Frequency..Pop. + 
            Frequency..R.B.grouped + Frequency..Rap.grouped + RC1 + RC2, 
            data = r_completo)
summary(m3_a)   #0.2582 
resettest(m3_a, power = 2, type = "fitted",  data = r_completo)   #0.6146 aumento del p value (prima 0.5814)
par(mfrow = c(2, 2))
plot(m3_a)



#### INFLUENCE PLOT MODELLO SELEZIONATO #### 

library(ggplot2)
library(broom)   #per ordinare il modello in un formato tidied
library(ggrepel)   #per etichette non sovrapposte

# Calcola le statistiche diagnostiche per il modello
diagnostics <- augment(m3_a)  # broom::augment() dà statistiche diagnostiche
diagnostics$cooksd <- cooks.distance(m3_a)   #Aggiunge la Cook's Distance
diagnostics$leverage <- hatvalues(m3_a)   #Aggiunge il leverage
diagnostics$residuals_std <- rstandard(m3_a)   #Residui standardizzati

# Definisci soglie per Cook's Distance, leverage e residui
cook_threshold <- 4 / nrow(diagnostics)   #Soglia di Cook's Distance
leverage_threshold <- 2 * mean(diagnostics$leverage)   #Soglia di leverage
residuals_threshold <- 2   #Soglie per residui standardizzati a ±2

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


leverage_points <- which(hatvalues(m3_a) > leverage_threshold)
leverage_points
length(leverage_points)   #abbiamo 20 punti di leva (nettamente meno del 10%)

rstudent_vals_abs <- abs(rstudent(m3_a))
rstudent_points <- which(rstudent_vals_abs > residuals_threshold)
rstudent_points   
length(rstudent_points)   #abbiamo 36 valori oltre le soglie

#grafici più semplici con medisimi risultati
#grafico R-student e Leverage
#par(mfrow = c(1, 1))
#plot(x=hatvalues(m3_a), y=rstudent(m3_a), ylab="R Student", xlab="Leverage", ylim=c(-3,3), pch=20 )
#abline(v=2*mean(hatvalues(m3_a)), col="red")   #soglia dei leverage
#abline(h=c(-2,2), col=c("blue","blue"))   #soglia degli r-student



#### MODELLO ROBUSTO E OUTLIER ####

#siccome i laverage sono molto meno del 10% posso usare il seguente metodo per trattare gli outlier
library(robustbase)
control <- lmrob.control(k.max = 500)  
m4_a <- lmrob(formula = log(Hours.per.day) ~ Age + I(Age^2) + Primary.streaming.service.grouped + 
                While.working + Instrumentalist + Composer + Exploratory + 
                Frequency..Country.grouped + Frequency..Folk.grouped + Frequency..Gospel. + 
                Listens.to.K.pop + Frequency..Metal.grouped + Frequency..Pop. + 
                Frequency..R.B.grouped + Frequency..Rap.grouped + RC1 + RC2, 
                data = r_completo, control=control)
summary(m4_a)    #0.2699

resettest(m4_a, power = 2, type = "fitted",  data = r_completo)   #0.6146 
par(mfrow = c(2, 3))
plot(m4_a)

#cerco quali osservazioni hanno peso attribuito basso e sono quindi probabili outlier
robust_weights <- weights(m4_a, type = "robustness")
low_weight_obs <- which(robust_weights < 0.5)
r_completo[low_weight_obs, ]
data.frame(Observation = low_weight_obs, Weight = robust_weights[low_weight_obs])  #112, 155, 210, 213, 279, 330, 378, 565

#creiamo un modello lineare in cui rimuviamo le osservazioni a cui il modello robusto attribuisce un peso minore di 0,5
r_completo2 <- r_completo[!(rownames(r_completo) %in% c("112", "155", "210", "213", "279", "330", "378", "565")), ]
any(rownames(r_completo2) %in% c(112, 155, 210, 213, 279, 330, 378, 565))

m4_b <- lm(formula = log(Hours.per.day) ~ Age + I(Age^2) + Primary.streaming.service.grouped + 
             While.working + Instrumentalist + Composer + Exploratory + 
             Frequency..Country.grouped + Frequency..Folk.grouped + Frequency..Gospel. + 
             Listens.to.K.pop + Frequency..Metal.grouped + Frequency..Pop. + 
             Frequency..R.B.grouped + Frequency..Rap.grouped + RC1 + RC2, 
             data = r_completo2)
summary(m4_b)  #0.2832   
drop1(m4_b, test="F")

resettest(m4_b, power = 2, type = "fitted",  data = r_completo2)  #0.5134 è più basso rispetto al modello robusto (0.6146)
dev.off()
par(mfrow = c(2, 2))
plot(m4_b)



#### ETEROSCHEDASTICITà ####

#test test Breusch-Pagan per eteroschedasticità
bptest(m4_a)  #0.2213
bptest(m4_b)  #0.1949

#TEST White 
#ncvTest(m4_a)  #non è possibile applicare i test di White ai modelli lmrob
ncvTest(m4_b)  #0.97172



#### BOOTSTRAP INFERENCE ####

library("car")
#install.packages("boot")
library("boot")

BOOT.MOD=Boot(m4_a, R=1000)
summary(BOOT.MOD, high.moments=TRUE)

Confint(BOOT.MOD, level=c(.95), type="perc")
hist(BOOT.MOD, legend="separate")

m5_a <- lmrob(formula = log(Hours.per.day) ~ Age + I(Age^2) + 
                While.working + Instrumentalist + Composer + 
                Frequency..Country.grouped + Frequency..Gospel. +
                Frequency..Metal.grouped + Frequency..Pop. + 
                Frequency..R.B.grouped + Frequency..Rap.grouped + RC1, 
              data = r_completo, control=control) #+ Frequency..Folk.grouped (p-value 0.8772) + RC2 (p-value 0.8464)
summary(m5_a)    #0.2587  <- 0.2699
resettest(m5_a, power = 2, type = "fitted",  data = r_completo)   #0.9987 <- 0.6146 
par(mfrow = c(2, 3))
plot(m5_a)


BOOT.MOD2=Boot(m4_b, R=1000)
summary(BOOT.MOD2, high.moments=TRUE)

Confint(BOOT.MOD2, level=c(.95), type="perc")
hist(BOOT.MOD2, legend="separate")

summary(m4_b)
drop1(m4_b, test="F")

m5_b <- lm(formula = log(Hours.per.day) ~ Age + I(Age^2) + 
             While.working + Instrumentalist + Composer + 
             Frequency..Country.grouped + Frequency..Gospel. + 
             Frequency..Metal.grouped + Frequency..Pop. + 
             Frequency..R.B.grouped + Frequency..Rap.grouped + RC1 + RC2, #Primary.streaming.service.grouped (0.6241) +
           data = r_completo2)
summary(m5_b)  #0.2753  <- 0.2832   
drop1(m5_b, test="F")

resettest(m5_b, power = 2, type = "fitted",  data = r_completo2)  #0.7635 <- 0.5134
dev.off()
par(mfrow = c(2, 2))
plot(m5_b)

 