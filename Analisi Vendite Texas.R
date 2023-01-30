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

#distribuzione di frequenza assoluta e relativa per citta
distr_freq_ass_city = cbind(ni,fi)

#diamo un occhiata allre frequenze relative doppie
#distribuzione di frequenza assoluta per citta e anno
distr_freq_ass_city_mese = table(city,year)

#distribuzione di frequenza relativa per citta e anno
distr_freq_rel_city_anno = table(city,year)/length(city)


#Calcoliamo indici di posizione
summary(sales)

plot(density(sales))

ggplot()+
  geom_density(aes(x=sales,),col="black",fill="lightblue")

  
  
