# Analisi statistica sull’ascolto musicale e la salute mentale

Questo progetto, svolto come parte del corso universitario di Data Mining, analizza la relazione tra abitudini musicali e variabili legate al benessere mentale, utilizzando modelli statistici lineari e logistici.

##  Contenuto della repository

- `Bachiorri_Barrow_Mezzanzanica_Saccardo_1MOD copia.r`: script R per la modellazione lineare dell’ascolto musicale (target: ore di ascolto).
- `Bachiorri_Barrow_Mezzanzanica_Saccardo_2MOD copia.R`: script R per la modellazione logistica (target: effetti percepiti della musica).
- `Bachiorri_Barrow_Mezzanzanica_Saccardo_1MOD.pdf` & `2MOD.pdf`: output completi dei modelli con grafici e test diagnostici.
- `Bachiorri_Barrow_Mezzanzanica_Saccardo.Rdata`: dataset contenente le osservazioni preprocessate e pronte per la modellazione.

##  Obiettivo

Studiare l’impatto di variabili come genere musicale preferito, ore di ascolto, e attività durante l’ascolto, su indicatori di salute mentale (ansia, depressione, insonnia, OCD), mediante:

- **Regressione lineare** (con trasformazioni Box-Cox e riduzione della multicollinearità)
- **Regressione logistica** (con gestione di separation e selezione delle variabili tramite AIC e VIF)

##  Pacchetti principali usati

- `mice`, `VIM` – gestione e visualizzazione dei missing values  
- `car`, `mctest` – diagnostica e VIF
- `lmtest`, `gam`, `factorMerger` – test diagnostici e trasformazione di variabili categoriche

## Autori

Nicolò Bachiorri, Emma Barrow, Chiara Mezzanzanica, Emanuele Saccardo
