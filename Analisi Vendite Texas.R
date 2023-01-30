#Import del dataset

#setwd("C:\\Users\\gabri\\Desktop\\PrivateProject\\Analisi-Statistica-Vendite-Texas")

texas_price <- read.csv("realestate_texas.csv")
#Diamo un occhiata al dataset
head(texas_price,10)

attach(texas_price)
#Adesso che sappimao come fatto il dataset iniziamo osservare per bene i dati

#costruiamo la distribuzione di frequenza per la variabile città
ni <- table(city)
fi <- table(city)/length(city)
Ni  
