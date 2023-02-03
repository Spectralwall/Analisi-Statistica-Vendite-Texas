#impostazione zona di lavoro
setwd("C:\\Users\\gabri\\Desktop\\PrivateProject\\Analisi-Statistica-Vendite-Texas")

#import librerie
library(ggplot2)
library(plotly)
library("moments")
library("gridExtra")
library(ggthemes)

#esportare in bella le tabelle
# pdf("distr_anno.pdf")       # Export PDF
# grid.table(distr_freq_ass_city_anno)
# dev.off()

#Funzioni

#INDICE DI ETEROGENEITA DI GINI Normalizzato
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

#funzione di normalizzazione
normaliz <- function(x){
  (x-min(x))/(max(x)-min(x))
}

#import dataset
texas_price <- read.csv("realestate_texas.csv")
#Diamo un occhiata al dataset
head(texas_price,20)

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

G(city)

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

#distribuzione di frequenza doppia Anno,Mese (per Probabilità)
distr_freq_ass_anno_mese = table(year,month)
distr_freq_rel_anno_mese = table(year,month)/length(month)

#SALES
#Calcoliamo indici di posizione
sales_range = max(sales)-min(sales)
sales_summary = summary(sales)
sales_IQR=IQR(sales)
mode_sales = Mode(sales)

#-----------------divisione in classi-----------------------------------------
#dividiamo in classi le vendite per osservarne la distribuzione di frequenza
sales_cl = cut(sales,seq(50,450,50))#divisa in 7 classi da 1 centimetro l'uno

df_freq_sales = distribuzione_assoluta(sales_cl)
df_freq_sales

#indice di gini per classe
G(sales_cl)

#grafico a barre
#Distribuzione in classi
ggplot(data=df_freq_sales, aes(x=ni, y=fi,fill=row.names(df_freq_sales))) +
  geom_bar(stat="identity")+
  labs(title="Distribuzione Vendite su 8 classi",
       x="Classi",
       y="Frequenza")+
  theme_fivethirtyeight()+
  theme(axis.title = element_text())+
  guides(fill=guide_legend(title="Class"))


sales_cl = cut(sales,seq(50,450,100))#divisa in 7 classi da 1 centimetro l'uno
df_freq_sales2 = distribuzione_assoluta(sales_cl)
df_freq_sales2

G(sales_cl)

ggplot(data=df_freq_sales2, aes(x=ni, y=fi,fill=row.names(df_freq_sales2))) +
  geom_bar(stat="identity")+
  labs(title="Distribuzione Vendite su 4 classi",
       x="Classi",
       y="Frequenza")+
  theme_fivethirtyeight()+
  theme(axis.title = element_text())+
  guides(fill=guide_legend(title="Class"))
#----------------------------------------------------------

var_sales=var(sales)#quanto è varia la nostra distribuzione
sd_sales=sd(sales)#distanza media dei singoli dati
cv_sales=CV(sales)#ci salviamo il coefficente di variazione per futuri confronti

#simmetria
ass_sales = skewness(sales)
#curtosi
curtosi_sales = kurtosis(sales)-3

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
  labs(title = "Distribuzione Sales")+
  theme_fivethirtyeight()


#VOLUME
volume_range = max(volume)-min(volume)
volume_summary = summary(volume)
volume_IQR=IQR(volume)
mode_volume = Mode(volume)

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
  xlab("Volume")+
  ylab("Density")+
  labs(title = "Distribuzione Volume")

#simmetria
ass_volume=skewness(volume)
#curtosi
curtosi_volume=kurtosis(volume)-3

#MEDIAN PRICE
median_price_range = max(median_price)-min(median_price)
median_price_summary = summary(median_price)
median_price_IQR=IQR(median_price)
mode_median_price = Mode(median_price)

#dividiamo in classi il volume per osservarne la distribuzione di frequenza
median_price_cl = cut(median_price,seq(70000,180000,10000))#divisa in 7 classi da 1 centimetro l'uno

df_freq_median_price = distribuzione_assoluta(median_price_cl)
df_freq_median_price

var_median_price=var(median_price)#quanto è varia la nostra distribuzione
sd_median_price=sd(median_price)#distanza media dei singoli dati
cv_median_price=CV(median_price)#ci salviamo il coefficente di variazione per futuri confronti

ggplot()+
  geom_density(aes(x=median_price),col="darkblue",fill="lightblue")+
  geom_vline(aes(xintercept=mean(median_price)),
             color="red", linetype="dashed", size=1)+
  geom_vline(aes(xintercept=quantile(median_price,seq(0,1,0.25))),
             color="green3", linetype="dashed", size=1)+
  geom_vline(aes(xintercept=median(median_price)),
             color="orange", linetype="dashed", size=1)+
  geom_vline(aes(xintercept=Mode(median_price)),
             color="yellow", linetype="dashed", size=1)+
  xlab("Medina Price")+
  ylab("Density")+
  labs(title = "Distribuzione Median Price")

#simmetria
ass_median_price=skewness(median_price)
#curtosi
curtosi_median_price=kurtosis(median_price)-3

#LISTING
listings_range = max(listings)-min(listings)
listings_summary = summary(listings)
listings_IQR=IQR(listings)
mode_listings = Mode(listings)

#dividiamo in classi il volume per osservarne la distribuzione di frequenza
listings_cl = cut(listings,seq(70000,180000,10000))#divisa in 7 classi da 1 centimetro l'uno

df_freq_listings = distribuzione_assoluta(listings)
df_freq_listings

var_listings=var(listings)#quanto è varia la nostra distribuzione
sd_listings=sd(listings)#distanza media dei singoli dati
cv_listings=CV(listings)#ci salviamo il coefficente di variazione per futuri confronti

ggplot()+
  geom_density(aes(x=listings),col="darkblue",fill="lightblue")+
  geom_vline(aes(xintercept=mean(listings)),
             color="red", linetype="dashed", size=1)+
  geom_vline(aes(xintercept=quantile(listings,seq(0,1,0.25))),
             color="green3", linetype="dashed", size=1)+
  geom_vline(aes(xintercept=median(listings)),
             color="orange", linetype="dashed", size=1)+
  geom_vline(aes(xintercept=Mode(listings)),
             color="yellow", linetype="dashed", size=1)+
  xlab("Listings")+
  ylab("Density")+
  labs(title = "Distribuzione Listings")

#simmetria
ass_listings=skewness(listings)
#curtosi
curtosi_listings=kurtosis(listings)-3

#MONTHS INVENTORY
months_inventory_range = max(months_inventory)-min(months_inventory)
months_inventory_summary = summary(months_inventory)
months_inventory_IQR=IQR(months_inventory)
mode_months_inventory = Mode(months_inventory)

#dividiamo in classi il volume per osservarne la distribuzione di frequenza
months_inventory_cl = cut(months_inventory,seq(70000,180000,10000))#divisa in 7 classi da 1 centimetro l'uno

df_freq_months_inventory = distribuzione_assoluta(months_inventory)
df_freq_months_inventory

var_months_inventory=var(months_inventory)#quanto è varia la nostra distribuzione
sd_months_inventory=sd(months_inventory)#distanza media dei singoli dati
cv_months_inventory=CV(months_inventory)#ci salviamo il coefficente di variazione per futuri confronti

ggplot()+
  geom_density(aes(x=months_inventory),col="darkblue",fill="lightblue")+
  geom_vline(aes(xintercept=mean(months_inventory)),
             color="red", linetype="dashed", size=1)+
  geom_vline(aes(xintercept=quantile(months_inventory,seq(0,1,0.25))),
             color="green3", linetype="dashed", size=1)+
  geom_vline(aes(xintercept=median(months_inventory)),
             color="orange", linetype="dashed", size=1)+
  geom_vline(aes(xintercept=Mode(months_inventory)),
             color="yellow", linetype="dashed", size=1)+
  xlab("Months inventory")+
  ylab("Density")+
  labs(title = "Distribuzione Months inventory")

#simmetria
ass_months_inventory=skewness(months_inventory)
#curtosi
curtosi_months_inventory=kurtosis(months_inventory)-3

#costruzione tabella confronto valori
#cbind(sales_summary,volume_summary,median_price_summary,listings_summary,months_inventory_summary)

df_summary = as.data.frame(cbind(sales_summary,volume_summary,median_price_summary,listings_summary,months_inventory_summary))

df2 = data.frame(sales_summary=c(sales_range,sales_IQR,mode_sales,var_sales,
                                 sd_sales,cv_sales,ass_sales,curtosi_sales),
                 volume_summary=c(volume_range,volume_IQR,mode_volume,var_volume,
                                  sd_volume,cv_volume,ass_volume,curtosi_volume),
                 median_price_summary=c(median_price_range,median_price_IQR,mode_median_price,
                                        var_median_price,sd_median_price,cv_median_price,
                                        ass_median_price,curtosi_median_price),
                 listings_summary=c(listings_range,listings_IQR,mode_listings,var_listings,
                                    sd_listings,cv_listings,ass_listings,curtosi_listings),
                 months_inventory_summary=c(months_inventory_range,months_inventory_IQR,mode_months_inventory,
                                            var_months_inventory,sd_months_inventory,cv_months_inventory,
                                            ass_months_inventory,curtosi_months_inventory))

rownames(df2) <- c("Range","IQR","Mode","Var","SD","CV","Asymmetry","Curtosi")
df_all_value = rbind(df_summary,df2)

png("test.png", height = 30*nrow(df_all_value), width = 170*ncol(df_all_value))
grid.table(df_all_value)
dev.off()

#aggiungere una colonna con il prezzo medio
#per ogni record prendo il volume di vendita e lo divido per il numero dlle vendite
#dopodiche lo moltiplico per mille per averlo sulla stessa unita di misura del prezzo mediano
texas_price$mean_price = (texas_price$volume*1000)/texas_price$sales

#Aggiungo colonna su annunci di vendità
texas_price$perc_satisfied_ads = (texas_price$sales/texas_price$listings)*100


#Raggrupiamo per città e creiamo un mini dattaset con dati utili per i plot
City_sales_volume_meanPrice=texas_price %>% #prendiamo il dataset
  group_by(city)%>% 
  summarise(Totale_Guadagni=sum(volume),
            Totale_vendite=sum(sales),
            Prezzo_Medio=mean(mean_price)*1000,
            Annunci=sum(listings),
            AnnunciMediSoddisfatti=mean(perc_satisfied_ads))

#Plot

#box plot per confronti
#distribuzione prezzo mediano nelle varie città
ggplot(data=texas_price)+
  geom_boxplot(aes(x=city,y=median_price,fill=city))+
  labs(title="Distribuzione prezzo mediano delle case nelle varie città",
       x="Citta",
       y="Prezzo Mediano")+
  theme_fivethirtyeight()+
  theme(axis.title = element_text(),legend.position='none')

#confronto distribuzioni vendite nelle città
ggplot(data=texas_price)+
  geom_boxplot(aes(x=city,y=sales,fill=city))+
  labs(title="Distribuzione vendite nelle varie città",
       x="Citta",
       y="Vendite")+
  theme_fivethirtyeight()+
  theme(axis.title = element_text(),legend.position='none')

#confronto distribuzioni vendite nei vari anni per le citta
texas_price %>% 
  filter(year %in% c(2010,2011,2012,2013,2014)) %>%
  ggplot(aes(x=city, y=sales, fill=factor(year))) +
  geom_boxplot()+
  labs(title="Distribuzione Vendite negli anni per citta",
       x="Citta",
       y="Vendite")+
  theme_fivethirtyeight()+
  theme(axis.title = element_text())+
  guides(fill=guide_legend(title="Anni"))



#grafici a barre per le domande

#In quale città ho venduto di più ?
ggplot(data=texas_price, aes(x=city, y=sales,fill=city)) +
  geom_bar(stat="identity",width=0.5)+
  labs(title="Quale Città ha venduto più Case ?",
       x="Citta",
       y="Numero Vendite")+
  theme_fivethirtyeight()+
  theme(axis.title = element_text(),legend.position='none')

#quale la città che in media a venduto di più ?
city_more_sales_mean=texas_price %>% 
  group_by(city)%>% 
  summarise(media_vendite=mean(sales))

ggplot(data=city_more_sales_mean,
       aes(x=city,y=media_vendite,fill=city)) +
  geom_bar(stat="identity",width=0.5)+
  labs(title="Quale Città ha venduto più Case in media ?",
       x="Citta",
       y="Numero Vendite")+
  theme_fivethirtyeight()+
  theme(axis.title = element_text(),legend.position='none')

#In quale città abbiamo più annunci soddisfatti
ggplot(data=City_sales_volume_meanPrice, aes(x=city, y=AnnunciMediSoddisfatti,fill=city)) +
  geom_bar(stat="identity",width=0.5)+
  labs(title="Quale Città ha soddisfatto più annunci ?",
       x="Citta",
       y="Percentuale Annunci soddisfatti")+
  theme_fivethirtyeight()+
  theme(axis.title = element_text(),legend.position='none')

#Quale stato l'anno con più vendite ?
ggplot(data=texas_price, aes(x=year, y=sales,fill=city)) +
  geom_bar(stat="identity",width=0.5)+
  labs(title="Quale stato l'anno con più vendite ?",
       x="Anni",
       y="Vendite")+
  theme_fivethirtyeight()+
  theme(axis.title = element_text())

#Quale stato l'anno con più vendite ? (ma normalizzato)
ggplot(data=texas_price, aes(x=year, y=sales,fill=city)) +
  geom_bar(stat="identity",position = "fill",width=0.5)+
  labs(title="Quale stato l'anno con più vendite ?",
       x="Anni",
       y="Vendite")+
  theme_fivethirtyeight()+
  theme(axis.title = element_text())

#Anno con più vendite ma con più barre
ggplot()+
  geom_col(data = texas_price,aes(x=year,y=sales,fill=city),position ="dodge")+
  labs(title="Quale stato l'anno con più vendite ?",
       x="Anni",
       y="Vendite")+
  theme_fivethirtyeight()+
  theme(axis.title = element_text())

#In città ha avuto un fatturato più alto ?
ggplot(data=texas_price, aes(x=city, y=volume,fill=city)) +
  geom_bar(stat="identity",width=0.5)+
  labs(title="In che città c'è stato più guadagno ?",
       x="Città",
       y="Millioni")+
  theme_fivethirtyeight()+
  theme(axis.title = element_text(),legend.position='none')

#In quali citta i prezzi sono più alti ?
ggplot(data=City_sales_volume_meanPrice, aes(x=city, y=Prezzo_Medio,fill=city)) +
  geom_bar(stat="identity",width=0.5)+
  labs(title="In quali citta i prezzi sono più alti ?",
       x="Città",
       y="Prezzo")+
  theme_fivethirtyeight()+
  theme(axis.title = element_text(),legend.position='none')


#Quale la città con più annunci ?
ggplot(data=texas_price, aes(x=city, y=listings,fill=city)) +
  geom_bar(stat="identity",width=0.5)+
  labs(title="Quale la città con più annunci ?",
       x="Città",
       y="Annunci")+
  theme_fivethirtyeight()+
  theme(axis.title = element_text(),legend.position='none')

#LINE CHART

#Raggrupiamo per città e creiamo un mini dattaset con dati utili per i plot
time_period_ragrup=texas_price %>% #prendiamo il dataset
  group_by(year,city)%>% 
  summarise(guadagni=sum(volume))

#confronti i guadagni negli anni per citta
ggplot() +
  geom_line(data = time_period_ragrup, 
            aes(x = year, y = guadagni,color=city),size=1)+
  labs(title="Andamento dei guadagni negli Anni per città",
       x="Anni",
       y="Guadagni")+
  theme_fivethirtyeight()+
  theme(axis.title = element_text())

#Raggrupiamo per mesi e anni e succesivamente filtriamo
time_period_ragrup_month=texas_price %>% #prendiamo il dataset
  group_by(year,month,city)%>% 
  summarise(guadagni=sum(volume))

filtra_stampa_anno_vendite <- function(data,anno){
  
  anno_filtrato = filter(data,year==anno)
  
  ggplot() +
    geom_line(data = anno_filtrato, 
              aes(x = month, y = guadagni,color=city),size=1)+
    labs(title="Numero di vendite nei mesi per città",
         x="Mesi",
         y="Guadagni")+
    theme_fivethirtyeight()+
    theme(axis.title = element_text())
}

filtra_stampa_anno_vendite(time_period_ragrup_month,2010)
filtra_stampa_anno_vendite(time_period_ragrup_month,2011)
filtra_stampa_anno_vendite(time_period_ragrup_month,2012)
filtra_stampa_anno_vendite(time_period_ragrup_month,2013)
filtra_stampa_anno_vendite(time_period_ragrup_month,2014)


#Raggrupiamo per mesi e anni e succesivamente filtriamo
time_period_ragrup_month_listing=texas_price %>% #prendiamo il dataset
  group_by(year,month,city)%>% 
  summarise(annunci=sum(listings))

filtra_stampa_anno_annunci <- function(data,anno){
  
  anno_filtrato = filter(data,year==anno)
  
  ggplot() +
    geom_line(data = anno_filtrato, 
              aes(x = month, y = annunci,color=city),size=1)+
    labs(title="Nummero di annunci nei mesi per città",
         x="Mesi",
         y="Guadagni")+
    theme_fivethirtyeight()+
    theme(axis.title = element_text())
}

filtra_stampa_anno_annunci(time_period_ragrup_month_listing,2010)
filtra_stampa_anno_annunci(time_period_ragrup_month_listing,2011)
filtra_stampa_anno_annunci(time_period_ragrup_month_listing,2012)
filtra_stampa_anno_annunci(time_period_ragrup_month_listing,2013)
filtra_stampa_anno_annunci(time_period_ragrup_month_listing,2014)



detach(texas_price)


  
