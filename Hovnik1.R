library(dplyr)
library(readxl)
library(rvest)
library(tidyr)
library(digest)


library(readxl)
#1. naloga
tabela2010 <- read_excel("tabela2010.xlsx")
tabela2010 <- tabela2010[c(1,21,41,64,86,107,129,151,173,195,216,238),]
tabela2010 <- tabela2010[-17]
tabela2010 <- tabela2010[,-1]
rownames(tabela2010)<- c("04/01/2010", "01/02/2010", "01/03/2010",
                        "01/04/2010","03/05/2010", "01/06/2010",
                        "01/07/2010", "02/08/2010", "01/09/2010",
                        "01/10/2010", "01/11/2010", "01/12/2010") 


tabela2011 <- read_excel("tabela2011.xlsx")
tabela2011 <- tabela2011[c(1,22,42,65,84,106,128,149,172,194,215,237),]
tabela2011 <- tabela2011[,-1]
rownames(tabela2011)<- c("03/01/2011", "01/02/2011", "01/03/2011",
                         "01/04/2011", "02/05/2011", "01/06/2011",
                         "01/07/2011", "01/08/2011", "01/09/2011", 
                         "03/10/2011", "01/11/2011", "01/12/2011") 


tabela2012 <- read_excel("tabela2012.xlsx")
tabela2012 <- tabela2012[c(1,23,44,66,85,107,128,150,173,193,216,238),]
tabela2012 <- tabela2012[,-1]
rownames(tabela2012)<- c("02/01/2012", "01/02/2012", "01/03/2012",
                         "02/04/2012", "02/05/2012", "01/06/2012",
                         "02/07/2012", "01/08/2012", "03/09/2012", 
                         "01/10/2012", "01/11/2012", "03/12/2012") 


#for (i in 1: nrow(tabela2010)){
#  T<-tabela2010[i, "3m"]
#  U<-tabela2010[i, "6m"]
#  L(0,T,U) = 1/(U-T)
#}


obresti <- rbind(tabela2010,tabela2011,tabela2012)
 

 
m3<-ts(obresti[,7],start = c(2010,1), frequency = 12) 
m6<-ts(obresti[,10],start = c(2010,1), frequency = 12) 


graf<-ts.plot(m3,m6, xlab='Time', ylab='%', col = c('red','blue'))
graf <-ts.plot(legend('topright',legend = c('3m','6m'), fill = c('red','blue')))







#2. naloga
nova_tabela <- obresti 
nova_tabela <- nova_tabela[c(7,20,34),]
nova_tabela <- t(nova_tabela)
obrestiD <- data.frame(nova_tabela)
colnames(obrestiD)<- c('01/07/2010','01/08/2011','01/10/2012' )
rownames(obrestiD) <- c(0.25,0.5,0.75,1,2,3,4,5,6,7,8,9,10,11,12)
#dospetja = c(0.25,0.5,0.75,1,2,3,4,5,6,7,8,9,10,11,12)
#obrestiD <- cbind(obrestiD,dospetja)

grafD <- plot(y = obrestiD[,1],
              x = rownames(obrestiD),
              ylim=c(min(0),max(2.3)),
              xlab="Dospetje [mesec]", 
              ylab="%", 
              main="Časovna struktura Euribor")
lines(obrestiD[,1], x = dospetja,col = "dodgerblue1", type ="o", text(11.5,1.1,"1.7.2010", col="dodgerblue1"))
lines(obrestiD[,2], x = dospetja,col = "darkorange", type ="o", text(11.5,1.8,"1.8.2011", col="darkorange"))
lines(obrestiD[,3], x = dospetja,col = "green", type = "o", text(11.5,0.4,"1.10.2012", col="green"))





