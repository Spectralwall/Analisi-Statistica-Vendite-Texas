#impostazione zona di lavoro
#setwd("C:\\Users\\gabri\\Desktop\\PrivateProject\\Analisi-Statistica-Vendite-Texas")

#import librerie
library(ggplot2)
library(plotly)
library("gridExtra")
library("moments")

#esportare in bella le tabelle
# pdf("distr_anno.pdf")       # Export PDF
# grid.table(distr_freq_ass_city_anno)
# dev.off()

#Funzioni

#INDICE DI ETEROGENEITA DI GINI
G <- function(x){
  ni=table(x) #frequenze assolute
  fi=ni/length(x)#frequenze relative
  fi2 = fi^2 #frequenze relative al quadrato
  J = length(table(x)) #tipi di classi che abbiamo
  gini = 1-sum(fi2) #G=1-sommatoria di frequenze relative al quadrato
  gini_norm = gini/((J-1)/J)
  return(gini_norm)
}

#DATASET DISTRIBUZIONE ASSOLUTA
distribuzione_assoluta <- function(x){
  n=length(x)
  ni=table(x)
  fi=table(x)/n
  Ni=cumsum(table(x))
  Fi=cumsum(table(x))/n
  return(as.data.frame(cbind(ni,fi,Ni,Fi)))
}

#COEFFIECENTE DI VARIAZIONE
CV <- function(x){
  return((sd(x)/mean(x))*100)
} 

#Funzione che calcola la moda/classe modale
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#import dataset
texas_price <- read.csv("realestate_texas.csv")
#Diamo un occhiata al dataset
head(texas_price,10)

attach(texas_price)

#CITY

#costruiamo la distribuzione di frequenza per la variabile città
ni_city <- table(city)
fi_city <- table(city)/length(city)

#distribuzione di frequenza assoluta e relativa per citta
distr_freq_ass_city = cbind(ni_city,fi_city)

#diamo un occhiata allre frequenze relative doppie
#distribuzione di frequenza assoluta per citta e anno
distr_freq_ass_city_mese = table(city,month)

#distribuzione di frequenza relativa per citta e anno
distr_freq_ass_city_anno = table(city,year)/length(city)



G(month)

#YEAR

#costruiamo la distribuzione di frequenza per la variabile annno
ni_year <- table(year)
fi_year <- table(year)/length(year)

#distribuzione di frequenza assoluta e relativa per anno
distr_freq_ass_year = cbind(ni_year,fi_year)
distr_freq_ass_year

summary(year)

#MONTH

#costruiamo la distribuzione di frequenza per la variabile mese
ni_month <- table(month)
fi_month <- table(month)/length(month)

#distribuzione di frequenza assoluta e relativa per mese
distr_freq_ass_month = cbind(ni_month,fi_month)
distr_freq_ass_month

summary(month)

#VENDITE
#Calcoliamo indici di posizione
max(sales)-min(sales)
summary(sales)
IQR(sales)

#dividiamo in classi le vendite per osservarne la distribuzione di frequenza
sales_cl = cut(sales,seq(50,450,50))#divisa in 7 classi da 1 centimetro l'uno

df_freq_sales = distribuzione_assoluta(sales_cl)
df_freq_sales

sales_cl = cut(sales,seq(50,450,100))#divisa in 7 classi da 1 centimetro l'uno
df_freq_sales2 = distribuzione_assoluta(sales_cl)
df_freq_sales2

var_sales=var(sales)#quanto è varia la nostra distribuzione
sd_sales=sd(sales)#distanza media dei singoli dati
cv_sales=CV(sales)#ci salviamo il coefficente di variazione per futuri confronti

#simmetria
skewness(sales)
#curtosi
kurtosis(sales)-3

ggplot()+
  geom_density(aes(x=sales),col="darkblue",fill="lightblue")+
  geom_vline(aes(xintercept=mean(sales)),
             color="red", linetype="dashed", size=1)+
  geom_vline(aes(xintercept=quantile(sales,seq(0,1,0.25))),
             color="green3", linetype="dashed", size=1)+
  geom_vline(aes(xintercept=median(sales)),
             color="orange", linetype="dashed", size=1)+
  geom_vline(aes(xintercept=Mode(sales)),
             color="yellow", linetype="dashed", size=1)+
  xlab("Sales")+
  ylab("Density")+
  labs(title = "Distribuzione Sales")

#VOLUME
summary(volume)
IQR(volume)
max(volume)-min(volume)
Mode(volume)

#dividiamo in classi il volume per osservarne la distribuzione di frequenza
volume_cl = cut(volume,seq(8,90,10))#divisa in 7 classi da 1 centimetro l'uno

df_freq_volume = distribuzione_assoluta(volume_cl)
df_freq_volume

var_volume=var(volume)#quanto è varia la nostra distribuzione
sd_volume=sd(volume)#distanza media dei singoli dati
cv_volume=CV(volume)#ci salviamo il coefficente di variazione per futuri confronti

ggplot()+
  geom_density(aes(x=volume),col="darkblue",fill="lightblue")+
  geom_vline(aes(xintercept=mean(volume)),
             color="red", linetype="dashed", size=1)+
  geom_vline(aes(xintercept=quantile(volume,seq(0,1,0.25))),
             color="green3", linetype="dashed", size=1)+
  geom_vline(aes(xintercept=median(volume)),
             color="orange", linetype="dashed", size=1)+
  geom_vline(aes(xintercept=Mode(volume)),
             color="yellow", linetype="dashed", size=1)+
  xlab("Sales")+
  ylab("Density")+
  labs(title = "Distribuzione Volume")

#simmetria
skewness(volume)
#curtosi
kurtosis(volume)-3

#MEDIAN PRICE

  
  
