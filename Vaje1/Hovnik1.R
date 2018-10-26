library(dplyr)
library(readxl)
library(rvest)
library(tidyr)
library(digest)
library(ggplot2)


library(readxl)
#1. naloga
tabela2010 <- read_excel("Vaje1/tabela2010.xlsx")
tabela2010 <- tabela2010[c(1,21,41,64,86,107,129,151,173,195,216,238),]
tabela2010 <- tabela2010[-17]
tabela2010 <- tabela2010[,-1]
rownames(tabela2010)<- c("04/01/2010", "01/02/2010", "01/03/2010",
                        "01/04/2010","03/05/2010", "01/06/2010",
                        "01/07/2010", "02/08/2010", "01/09/2010",
                        "01/10/2010", "01/11/2010", "01/12/2010") 


tabela2011 <- read_excel("Vaje1/tabela2011.xlsx")
tabela2011 <- tabela2011[c(1,22,42,65,84,106,128,149,172,194,215,237),]
tabela2011 <- tabela2011[,-1]
rownames(tabela2011)<- c("03/01/2011", "01/02/2011", "01/03/2011",
                         "01/04/2011", "02/05/2011", "01/06/2011",
                         "01/07/2011", "01/08/2011", "01/09/2011", 
                         "03/10/2011", "01/11/2011", "01/12/2011") 


tabela2012 <- read_excel("Vaje1/tabela2012.xlsx")
tabela2012 <- tabela2012[c(1,23,44,66,85,107,128,150,173,193,216,238),]
tabela2012 <- tabela2012[,-1]
rownames(tabela2012)<- c("02/01/2012", "01/02/2012", "01/03/2012",
                         "02/04/2012", "02/05/2012", "01/06/2012",
                         "02/07/2012", "01/08/2012", "03/09/2012", 
                         "01/10/2012", "01/11/2012", "03/12/2012") 


obresti <- rbind(tabela2010,tabela2011,tabela2012)

m3<-ts(obresti[,7],start = c(2010,1), frequency = 12) 
m6<-ts(obresti[,10],start = c(2010,1), frequency = 12) 


graf<-ts.plot(m3,m6, xlab='Time', ylab='%', col = c('red','blue'))
legend('topright',legend = c('3m','6m'), fill = c('red','blue'))

#2. naloga
nova_tabela <- obresti 
nova_tabela <- nova_tabela[c(7,20,34),]
nova_tabela <- t(nova_tabela)
obrestiD <- data.frame(nova_tabela)
colnames(obrestiD)<- c('01/07/2010','01/08/2011','01/10/2012' )
rownames(obrestiD) <- c(0.25,0.5,0.75,1,2,3,4,5,6,7,8,9,10,11,12)
dospetja =  rownames(obrestiD)
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

# OPIS OBRESTNIH KRIVULJ: Oblika vseh treh krivulj prikazanih na grafu je normalna. 
# Vidimo, da z večanjem dospetja obrestna mera narašča. 
# Prva in zadnja krivulja sta konkavni, druga pa je na začetku konveksna potem pa se nadaljuje v konkavno 
# obliko. 

#3.naloga

izracun<- data.frame()

for (i in 1: nrow(obresti)){
  L0T <- obresti[i, '3m']
  L0U <- obresti[i, '6m']
  L0TU = 1/(6-3) * ((1+6*L0U*0.01)/(1+3*L0T*0.01)-1) *100
  izracun[i,1]<- L0TU
  
}



terminska <- as.data.frame(cbind(obresti[,c(6,9)],izracun))
terminska[,3] <- c(c(NA, NA, NA), izracun[-c(34:36),])
colnames(terminska) <- c("Euribor3m","Euribor6m","Napoved3m")
rownames(terminska) <- rownames(obresti)

#3c GRAF
leto <- as.vector(cbind(seq(2010, 2010, length.out = 12), seq(2011, 2011, length.out = 12), 
                        seq(2012, 2012, length.out = 12))) %>% as.factor()
terminska <- as.data.frame(cbind(terminska,leto))


g.razsevni <- ggplot(terminska,aes(x = terminska$'Napoved3m', y = terminska$'Euribor3m')) +
  geom_point(aes(colour = terminska$'leto'), size = 2) +
  geom_smooth(method='lm', se = FALSE, color = "darkgray") +
  geom_abline(slope=1, intercept= 0) +
  coord_cartesian(xlim=c(0,2.4),ylim=c(0,2.4)) + 
  labs(title ='3m Euribor 2010 - 2012', y='Opazovano', x = 'Napoved', color = "Leto:") +
  theme_classic()
   
print(g.razsevni)

#graf za leto 2010
terminska2010<- terminska[c(4:12),]
g.razsevni2010 <- ggplot(terminska2010,aes(x = terminska2010$'Napoved3m', y = terminska2010$'Euribor3m')) +
  geom_point(aes(colour = terminska2010$'leto'), size = 2) +
  geom_smooth(method='lm', se = FALSE, color = "darkgray") +
  geom_segment(aes(x = 0, y = 0, xend = 2, yend = 2))+ #simetrala
  coord_cartesian(xlim=c(0.6,1.4),ylim=c(0.6,1.4)) + 
  labs(title ='3m Euribor 2010', y='Opazovano', x = 'Napoved', color = "Leto:") +
  theme_classic()
print(g.razsevni2010)

#graf za leto 2011
terminska2011<- terminska[c(13:24),]
g.razsevni2011 <- ggplot(terminska2011,aes(x = terminska2011$'Napoved3m', y = terminska2011$'Euribor3m')) +
  geom_point(aes(colour = terminska2011$'leto'), size = 2) +
  geom_smooth(method='lm', se = FALSE, color = "darkgray") +
  geom_segment(aes(x = 0, y = 0, xend = 2, yend = 2))+
  coord_cartesian(xlim=c(0.9,2),ylim=c(0.9,2)) + 
  labs(title ='3m Euribor 2011', y='Opazovano', x = 'Napoved', color = "Leto:") +
  theme_classic()
  #scatter.smooth(x=terminska2011$'Napoved3m', y=terminska2011$'Euribor3m', main="Napoved3m ~ Euribor3m") 
print(g.razsevni2011)

#graf za leto 2012
terminska2012<- terminska[c(25:36),]
g.razsevni2012 <- ggplot(terminska2012,aes(x = terminska2012$'Napoved3m', y = terminska2012$'Euribor3m')) +
  geom_point(aes(colour = terminska2012$'leto'), size = 2) +
  geom_smooth(method='lm', se = FALSE, color = "darkgray") +
  geom_abline(slope=1, intercept= 0) +
  coord_cartesian(xlim=c(0.1,2),ylim=c(0.0,2)) + 
  labs(title ='3m Euribor 2012', y='Opazovano', x = 'Napoved', color = "Leto:") +
  theme_classic()
   
  print(g.razsevni2012)

  #3e
  
