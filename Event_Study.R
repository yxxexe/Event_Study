#packagei koji mi trebaju install prije pa povuci iz library
library(dplyr)
library(tidyr)
library(openxlsx)
library(ggplot2)
library(scales)
library(lubridate)
rm(list=ls())
#exelica u koju upisujem rezultate
write.excel <- function(x,row.names=FALSE,col.names=TRUE,...) {
  write.table(x,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)
}


# UVOZ PODATAKA ####

# Dionice pojedinaÄno

dionice = read.csv("Dionice_data.csv",header=TRUE,sep=";")
# Konvertiranje datuma u Date format
str(dionice)
dionice$datum = as.Date(dionice$datum)
# Spremanje dataframea
save(dionice,file="dionice.Rda")

# CROBEX #uvezes datume i povrate crobexa

crobex = read.csv("Crobex.csv",header=TRUE,sep=";",na.strings = c(""),stringsAsFactors = FALSE)
# Konvertiranje datuma u date format i zadnje u numeric
crobex$datum = as.Date(crobex$datum)
# Selectanje samo datuma i zadnje cijene #selectanje datuma i povrata crobexa
crobex = crobex[,c(1,6)]
# Spremanje dataframea
save(crobex,file="crobex.Rda")

# Javne opomene #datumi ponuda za preuzimanje 

javop = read.csv("JavneOpomene.csv",header=TRUE,sep=";",na.strings = c(""),stringsAsFactors = FALSE)
str(javop)
javop$Datum = as.Date(javop$Datum)
save(javop,file="JavneOpomene.Rda")


# LOADIRANJE DATAFRAMEOVA ####

load("dionice.Rda")
load("crobex.Rda")
load("JavneOpomene.Rda")

# ANALIZA ####

# Izraèun dnevnih povrata #### #dionice

radnidani = crobex$datum
povrat = rep(NA,nrow(dionice))
dionice = data.frame(dionice,povrat)
for (i in 155745:nrow(dionice)){
  ticker = dionice$simbol[i]
  t1 = dionice$datum[i]
  pom = dionice %>% filter(simbol == ticker & datum<=t1) %>% select(datum,zadnja) %>% arrange(datum)
  if(nrow(pom)>1){
    price1 = pom$zadnja[nrow(pom)]
    price0 = pom$zadnja[nrow(pom)-1]
    t0 = pom$datum[nrow(pom)-1]
    rm(pom)
    brdana = match(t1,radnidani)-match(t0,radnidani)
    povrat =(price1-price0)/price0 # izraèun dnevnog povrata dionice
    povrat = (povrat+1)^(1/brdana)-1
    dionice$povrat[i]=povrat   
  }
}
rm(brdana,i,povrat,price0,price1,t0,t1,ticker)

##### Izraèun abnormalnih povrata - Market return model 
load("JavneOpomene.Rda")
radnidani = crobex$datum
redni_dan = seq(-10,10,1)
event_date = array() # vektor sa datumima kada se dogodio event
class(event_date) <- "Date"
superior = array() # vektor sa datumima kada se dogodio event + 10. dan (za popunjavanje rupa)
class(superior) <- "Date"
dionica_ticker = array() # vektor sa imenima "opomenutih" dionica
abnormalni_povrati = data.frame(redni_dan)
for (i in 1:nrow(javop)){
  datum_objave = javop$datum[i] # datum kada je izašao èlanak, odnosno nulti dan
  ticker = javop$simbol[i] # dionica na koju se referira èlanak
  if (datum_objave %in% radnidani){
    rbr = match(datum_objave,radnidani) # redni broj nultog dana u uzorku radnih dana 
  }
  else{
    rbr = which.max(radnidani>datum_objave)
  }
  # Izraèun normalnog povrata - Å¾elimo izraÄunati koeficijente regresije
  estimation_window = radnidani[(rbr-130):(rbr-11)]
  pom1 = dionice %>% filter(simbol==ticker & datum %in% estimation_window) %>% select(datum,dnc_povr=povrat) # povrati dionice u estimation windowu
  if(nrow(pom1)>20){
    event_date[length(event_date)+1] = radnidani[rbr]
    superior[length(superior)+1] = radnidani[rbr+10]
    dionica_ticker[length(dionica_ticker)+1] = ticker
    pom2 = crobex %>% filter(datum %in% estimation_window) %>% select(datum,cbx_povr=povrat) # povrati crobexa u estimation windowu
    proc_norm = left_join(pom1,pom2,by="datum") # mergane u 1 dataframe
    regr = lm(dnc_povr ~ cbx_povr,data=proc_norm) # regresija u odnosu na povrat trÅ¾iÅ¡ta - Market return model
    kfc = coefficients(regr) # koeficijenti
    rm(pom1,pom2,proc_norm,regr,estimation_window)
    
    # Prikaz stvarnih,normalnih,abnormalnih i trÅ¾iÅ¡nih povrata u event windowu 
    event_window = radnidani[(rbr-10):(rbr+10)]
    event_povrati = data.frame(redni_dan,datum=event_window)
    pom1 = dionice %>% filter(simbol==ticker & datum %in% event_window) %>% select(datum,dnc_povr=povrat) # povrati dionice u estimation windowu
    pom2 = crobex %>% filter(datum %in% event_window) %>% select(datum,cbx_povr=povrat) # povrati crobexa u estimation windowu
    event_povrati = left_join(event_povrati,pom1,by="datum")
    event_povrati = left_join(event_povrati,pom2,by="datum")
    event_povrati = event_povrati %>% mutate(norm_povr=kfc[1]+kfc[2]*cbx_povr) %>% mutate(abnorm_povr=dnc_povr-norm_povr) %>% select(redni_dan,abnorm_povr) 
    abnormalni_povrati = left_join(abnormalni_povrati,event_povrati,by="redni_dan")
    names(abnormalni_povrati)[ncol(abnormalni_povrati)]<-paste("Dogadjaj",i,sep="")
  }
}
rm(pom1,pom2,kfc,event_povrati,rbr,ticker,datum_objave,i,event_window)

# Raèunanje prvog povrata nakon event windova
superior_povrat = array() 
for (i in 1:length(superior)){
  superior_povrat[i] = (dionice %>% filter(simbol==dionica_ticker[i] & datum==superior[i]) %>% select(povrat))[1,1]
}
rm(i,superior)

# Popunjavanje rupa u podacima
for (j in 2:ncol(abnormalni_povrati)){
  br = 0
  for (i in 1:nrow(abnormalni_povrati)){
    if (is.na(abnormalni_povrati[i,j])){
      pom = abnormalni_povrati[i:nrow(abnormalni_povrati),j] # daj ostatak j-tog stupca koji je veæi od i
      pom = pom[!is.na(pom)] # daj one brojeve koji su != NA
      if (length(pom)>0){ # ako postoji broj u event windovu
        abnormalni_povrati[i,j] = pom[1]
      }
      else {
        abnormalni_povrati[i,j] = superior_povrat[j] # ako ne postoji broj, daj povrat iz superior povrata
      }
    }
  }
}
rm(br,pom,superior_povrat,i,j)

# Dropanje stupaca u kojima se pojavljuje NA
stupci = array() # vektor sa indeksima koje treba dropati
for (i in 2:ncol(abnormalni_povrati)){ # petlja za pronalazak stupaca koje treba dropati
  if (sum(is.na(abnormalni_povrati[i]))>0){
    stupci[length(stupci)+1] = i
  }
}
stupci = stupci[-1] # na 1. mjestu je NA pa ga treba maknuti
abnormalni_povrati = abnormalni_povrati[,-stupci] # dropanje stupaca iz abnormalnih povrata
dionica_ticker = dionica_ticker[-stupci]
event_date = event_date[-stupci]
rm(stupci,i)

# zapis abnormalnih povrata u excel
write.excel(abnormalni_povrati)

# za ispis tickera i datuma eventa
dionica_ticker = as.data.frame(dionica_ticker)
write.excel(dionica_ticker)
event_date = as.data.frame(event_date)
write.excel(event_date)

##### Agregiranje abnormalnih povrata i crtanje grafikona
tau1 = -10
tau2 = 10
caar = data.frame(redni_dan=seq(tau1,tau2,1), CAAR = rep(NA,tau2-tau1+1))
for (i in tau1:tau2){
  pom = abnormalni_povrati %>% filter(redni_dan >= tau1 & redni_dan <= i) # abnormalni povrati od tau1 do tau 2 za svaki doga?aj
  car = pom[,-1] %>% summarise_each(funs(sum(.,na.rm=TRUE))) # vektor sa sumiranim AR-ovima za svaki doga?aj posebno unutar specificiranog vr. perioda
  caar$CAAR[caar$redni_dan==i] = mean(as.numeric(car[1,])) # prosje?ni car za taj vremenski prozor
}
ggplot(caar,aes(x=redni_dan,y=CAAR)) + geom_line()
rm(tau1,tau2,i,pom,car)
write.excel(caar)

##### Statistièko testiranje da je oèekivanje kumulativnih abnormalnih povrata != 0
tau1 = -10
tau2 = 10
tstat = data.frame(redni_dan=seq(tau1,tau2,1), pvalue = rep(NA,tau2-tau1+1),stat=rep(NA,tau2-tau1+1))
for (i in tau1:tau2){
  pom = abnormalni_povrati %>% filter(redni_dan >= tau1 & redni_dan <= i) # abnormalni povrati od tau1 do tau 2 za svaki doga?aj
  car = pom[,-1] %>% summarise_each(funs(sum(.,na.rm=TRUE))) # vektor sa sumiranim AR-ovima za svaki doga?aj posebno unutar specificiranog vr. perioda
  t=t.test(car,mu=0)
  tstat$pvalue[tstat$redni_dan==i] = t$p.value
  tstat$stat[tstat$redni_dan==i] = t$statistic
}
rm(tau1,tau2,i,t,pom,car) # na 1. mjestu je NA pa ga treba maknuti
View(tstat)
write.excel(tstat)

##### Statistièko testiranje daL je oèekivanje abnormalnih povrata != 0
tau1 = -10
tau2 = 10
pom = abnormalni_povrati %>% filter(redni_dan >= tau1 & redni_dan <= tau2) # abnormalni povrati od tau1 do tau 2 za svaki dogaðaj
pom = pom[,-1]
tstat = data.frame(redni_dan=seq(tau1,tau2,1), pvalue = rep(NA,tau2-tau1+1),stat=rep(NA,tau2-tau1+1))
for (i in 1:nrow(tstat)){
  t=t.test(pom[i,],mu=0)
  tstat$pvalue[i] = t$p.value
  tstat$stat[i] = t$statistic
}
rm(tau1,tau2,i,t,pom)
View(tstat)
write.excel(tstat)

