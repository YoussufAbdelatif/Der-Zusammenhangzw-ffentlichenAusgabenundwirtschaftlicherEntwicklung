###################################Youssuf Abdelatif: BA_1046495#########################

###Directory setzen:


###Laden der Packages

#options("install.lock"=FALSE)
#install.packages("gridExtra")
#install.packages("readxl")
#install.packages("tseries")
#install.packages("urca")
#install.packages("ggfortify")
#install.packages("tseries")
#install.packages("aTSA")
#install.packages("forecast")
#install.packages("vars")
#install.packages("MLmetrics")
#install.packages("lpir")
#install.packages("magick")
#install.packages("webshot")
#webshot::install_phantomjs()
#install.packages("kableExtra")

library("kableExtra")
library("gridExtra")
library("readxl")
library("tseries")
library("urca")
library("ggfortify")
library("ggplot2")
library("tseries")
library("aTSA")
library("forecast")
library("vars")
library("MLmetrics")
library("mFilter")
library("magick")
library("webshot")
library("lmtest")

###################################################################################################################################################################################################################################################################################

###############################################DATEN Import & Bearbeitung##########################################################


Datenaggnom <- read_excel("Datenaggregiertfinal.xlsx")
#Datenaggnom <- Datenaggnom[1:70,]
Datenaggnom <- Datenaggnom[,-12]
names(Datenaggnom) <- c("Jahre","Ausgabenge","BIPnom","InvAusg","KonAusg","Bruttoinv","Bruttoinvstaat","Bruttoinvpriv","Infl","Arbeitslose","BIPreal","Deflator","CO2","Top10","Lebenserwartung","Finanzsaldo")


Datenagg <- (read_excel("Datenaggregiertfinal.xlsx"))
#Datenagg <- Datenagg[1:70,]
Datenagg <- Datenagg[,-12]
names(Datenagg) <- c("Jahre","Ausgabenge","BIPnom","InvAusg","KonAusg","Bruttoinv","Bruttoinvstaat","Bruttoinvpriv","Infl","Arbeitslose","BIPreal","Deflator","CO2","Top10","Lebenserwartung","Finanzsaldo")
#Bereinigen nominaler Größe 1991 = 100
Datenagg$Ausgabenge <- as.numeric(Datenagg$Ausgabenge/(Datenagg$Deflator/100))
Datenagg$InvAusg <- as.numeric(round(Datenagg$InvAusg/(Datenagg$Deflator/100),2))
Datenagg$KonAusg<- as.numeric(round(Datenagg$KonAusg/(Datenagg$Deflator/100),2))
Datenagg$Bruttoinv <- as.numeric(Datenagg$Bruttoinv/(Datenagg$Deflator/100))
Datenagg$Bruttoinvstaat <- as.numeric(Datenagg$Bruttoinvstaat/(Datenagg$Deflator/100))
Datenagg$Bruttoinvpriv <- as.numeric(Datenagg$Bruttoinvpriv/(Datenagg$Deflator/100))
Datenagg$Top10 <- as.numeric(Datenagg$Top10)
Datenagg$CO2 <- as.numeric(Datenagg$CO2/1000000)

Zinsen <- read_excel("Zinsen.xlsx",col_names = FALSE)
Zinsen$...1<-as.numeric(Zinsen$...2)
Zinsen <- matrix(Zinsen$...2,nrow=12,ncol=71)
Zinsen <- apply(Zinsen,FUN = mean,MARGIN = 2)
Zinsen <- ts(Zinsen)

Datenaufg <- read_excel("DatenAufgabenbereichefinal.xlsx")
names(Datenaufg) <- c("Jahre","Gesamt","Vert","ÖffSi","Schulen","FuE","Kultur","SoSi","Ges","WoWe","Wirtschaft","Verkehr","BIPnom")
Datenaufg[,3:12] <- Datenaufg[,3:12]/(Datenagg$Deflator/100)
Datenaufg <- Datenaufg[1:63,]
Datenaufg <- Datenaufg[,-13]
Datenaufg$Sonstiges <- Datenagg$Ausgabenge[1:63]-rowSums(Datenaufg[,3:12])

#Aufgaben in Prozent:
Datenaufgpro <- (Datenaufg[,3:12]/Datenagg$Ausgabenge[1:63])*100
Datenaufgpro$Sonstiges <- (Datenaufg$Sonstiges[1:63]/Datenagg$Ausgabenge[1:63])*100
Datenaufgpro$Gesamt <- rowSums(Datenaufgpro)


#########In Zeitreihen überführen:##########

Ausgabengsts <- ts(Datenagg$Ausgabenge)
Bruttoinvts <- ts(Datenagg$Bruttoinv)
Bruttoinvstaatts <- ts(Datenagg$Bruttoinvstaat) 
Bruttoinvprivts <- ts(Datenagg$Bruttoinvpriv)
Konausgts <- ts(Datenagg$KonAusg)
Invausgts <- ts(Datenagg$InvAusg)

#Ausgaben nach Aufgabenbereichen:
Verteidigungts <- ts(Datenaufg$Vert)
Bildungswesents <- ts(Datenaufg$Schulen)
ÖffSits <- ts(Datenaufg$ÖffSi)
FuEts <- ts(Datenaufg$FuE)
Kulturts <- Datenaufg$Kultur
SozialeSits <- Datenaufg$SoSi
WoWets <- ts(Datenaufg$WoWe)
Wirtschaftts <- ts(Datenaufg$Wirtschaft)
Verkehrts <- ts(Datenaufg$Verkehr)
Gests <- ts(Datenaufg$Ges)
Sonstigests <- ts(Datenaufg$Sonstiges)
Wivets <- ts(Datenaufg$Wirtschaft+Datenaufg$Verkehr+Datenaufg$WoWe)

#Indikatoren:
BIPrealts <- ts(Datenagg$BIPreal)
BIPnomts <- ts(Datenagg$BIPnom)
Inflationts <- ts(Datenagg$Infl)
Alquotets <- ts(Datenagg$Arbeitslose)
CO2ts <- ts(Datenagg$CO2)
Lebenserwartungts <- ts(Datenagg$Lebenserwartung)
Top10ts <- ts(Datenagg$Top10)
Deflatorts <- ts(Datenagg$Deflator)

########################################################################################################################################################################################################################################################################################################

###########################################Graphische Darstellung#######################################################################

##########ALLGEMEINE ÜBERSICHTEN##########

plot(Datenagg$Jahre,Datenagg$Ausgabenge,main="Entwicklung der Öffentlichen Ausgaben",xlab="Jahre",ylab="Öffentliche Ausgaben",sub="in realen Größen 1991=100",type="l")
plot(Datenagg$Jahre,Datenagg$InvAusg,main="Entwicklung der investiven öffentlichen Ausgaben",xlab="Jahre",ylab="Investive öffentliche Ausgaben",sub="in realen Größen 1991=100",type="l")
plot(Datenagg$Jahre,Datenagg$KonAusg,main="Entwicklung der konsumtiven öffentlichen Ausgaben",xlab="Jahre",ylab="konsumtive öffentliche Ausgaben",sub="in realen Größen 1991=100",type="l")
plot(Datenagg$Jahre,Datenagg$Bruttoinv,main="Entwicklung der gesamten Bruttoinvestitionen",xlab="Jahre",ylab="Bruttoinvestitionen",sub="in realen Größen 1991=100",type="l")
lines(Datenagg$Jahre,Datenagg$Bruttoinvstaat,main="Entwicklung der öffentlichen Bruttoinvestitionen",xlab="Jahre",ylab="Öffentliche Bruttoinvestitionen",sub="in realen Größen 1991=100",type="l")
lines(Datenagg$Jahre,Datenagg$Bruttoinvpriv,main="Entwicklung der privaten Bruttoinvestitionen",xlab="Jahre",ylab="Privaten Bruttoinvestitionen",sub="in realen Größen 1991=100",type="l",col="red")

#Einfache Übersicht der Ausgabearten nach Aufgabebereichen:
par(mfrow=c(2,5))
plot(Datenaufg$Jahre,Datenaufg$Vert, main="Verteidigung",xlab="Jahre",ylab="Ausgaben in Mrd Euro",type="l", sub="in realen Größen 1991=100")
plot(Datenaufg$Jahre,Datenaufg$Schulen, main="Schulen,Hochschule,Sonst.Bildungswesen",xlab="Jahre",ylab="Ausgaben in Mrd Euro",type="l", sub="in realen Größen 1991=100")
plot(Datenaufg$Jahre,Datenaufg$FuE, main="Wissenschaft,Forschung außerhalb der Hochschulen",xlab="Jahre",ylab="Ausgaben in Mrd Euro",type="l", sub="in realen Größen 1991=100")
plot(Datenaufg$Jahre,Datenaufg$Kultur, main="Kulturellen Angelegenheiten und Kirche",xlab="Jahre",ylab="Ausgaben in Mrd Euro",type="l", sub="in realen Größen 1991=100")
plot(Datenaufg$Jahre,Datenaufg$Ges, main="Gesundheit,Sport und Erholung",xlab="Jahre",ylab="Ausgaben in Mrd Euro",type="l", sub="in realen Größen 1991=100")
plot(Datenaufg$Jahre,Datenaufg$WoWe, main="Wohnungswesen und Raumordnung",xlab="Jahre",ylab="Ausgaben in Mrd Euro",type="l", sub="in realen Größen 1991=100")
plot(Datenaufg$Jahre,Datenaufg$Wirtschaft, main="Wirtschaftsförderung",xlab="Jahre",ylab="Ausgaben in Mrd Euro",type="l", sub="in realen Größen 1991=100")
plot(Datenaufg$Jahre,Datenaufg$Verkehr, main="Verkehr und Nachrichtenwesen",xlab="Jahre",ylab="Ausgaben in Mrd Euro",type="l", sub="in realen Größen 1991=100")
plot(Datenaufg$Jahre,Datenaufg$ÖffSi, main="Öffentl. Sicherheit und Ordnung,Rechtsschutz",xlab="Jahre",ylab="Ausgaben in Mrd Euro",type="l", sub="in realen Größen 1991=100")
plot(Datenaufg$Jahre,Datenaufg$SoSi, main="Soziale Sicherung",xlab="Jahre",ylab="Ausgaben in Mrd Euro",type="l", sub="in realen Größen 1991=100")

#Einfache Übersicht der Indikatoren wirtschaftliche Entwicklung:
par(mfrow=c(3,2))
plot(Datenagg$Jahre,Datenagg$BIPreal,main="Entwicklung des realen BIPs",xlab="Jahre",ylab="BIP",sub="in realen Größen 1991=100",type="l")
plot(Datenagg$Jahre,Datenagg$Infl,main="Entwicklung der Inflation (VPI)",xlab="Jahre",ylab="Inflation in % (VPI)",type="l")
plot(Datenagg$Jahre,Datenagg$Arbeitslose,main="Entwicklung der Arbeitslosenquote",xlab="Jahre",ylab="Arbeitslosenquote in %",type="l")
plot(Datenagg$Jahre,Datenagg$CO2,main="Entwicklung des CO2-Ausstoss",xlab="Jahre",ylab="CO2-Ausstoss in Mio. Tonnen",type="l")
plot(Datenagg$Jahre,Datenagg$Top10,main="Armutsgefährdungsquote",xlab="Jahre",ylab="Anteil am Gesamteinkommen",type="l",sub="vor Steuern")
plot(Datenagg$Jahre,Datenagg$Lebenserwartung,main="Entwicklung der Lebenserwartung",xlab="Jahre",ylab="Lebenserwartung in Jahren",type="l")


################ABBILDUNG 1#####################################


y1 <- diff(Datenagg$BIPreal[51:71],differences = 1)/Datenagg$BIPreal[51:70]*100
y2 <- diff(Datenagg$Ausgabenge[51:71],differences = 1)/Datenagg$Ausgabenge[51:70]*100

plota <- ggplot() +
  geom_rect(aes(xmin = 2006.90, xmax = 2009.15, ymin = -Inf, ymax = 10, fill = "blue"), alpha = 0.4) +
  geom_rect(aes(xmin = 2019.80, xmax = Inf, ymin = -Inf, ymax = 10, fill = "orange"), alpha = 0.3) +
  geom_rect(aes(xmin = 2009.90, xmax = 2013.15, ymin = -Inf, ymax = 10, fill = "lightgreen"), alpha = 0.4) +
  geom_line(aes(x = Datenaggnom$Jahre[52:71], y = y1, colour = "black")) +
  geom_line(aes(x = Datenaggnom$Jahre[52:71], y = y2, colour = "red")) +
  geom_point(aes(x = Datenaggnom$Jahre[52:71], y = y1, col = "black")) +
  geom_point(aes(x = Datenaggnom$Jahre[52:71], y = y2, col = "red")) +
  scale_color_identity(name = NULL, labels = c(black = "Bruttoinlandsprodukt", red = "Öffentliche Ausgaben"), guide = "legend") +
  theme_classic() +
  scale_fill_manual(name = "", values = c("lightblue", "lightgreen", "orange"), labels = c("Finanz- Bankenkrise 2007-2009","Europäische Staatsschuldenkrise 2010-2013","Coronapandemie 2020-")) +
  theme(legend.position = "bottom") +
  theme(axis.text.x = element_text(angle = 90)) +
  geom_hline(yintercept = 0, color = "black", size = 1)+
  labs(title="Wachstumsrate des realen Bruttoinlandsprodukts und der realen öffentlichen Ausgaben 2001-2020", subtitle= "1991=100")+ylab("Prozent")+xlab("Jahre")+
  scale_x_continuous("Jahre", labels = as.character(Datenaggnom$Jahre[52:71]), breaks = Datenaggnom$Jahre[52:71])

plota + ggsave(filename = "001A.png",scale = 2,limitsize = FALSE,width = 14,height = 6,units = "cm")

#############################################################################

################ABBILDUNG 2#####################################

plot1 <- ggplot(data.frame(Datenagg$KonAusg[12:71]),aes(x=Datenagg$Jahre[12:71]))+geom_line(aes(Datenagg$Jahre[12:71],Datenagg$KonAusg[12:71]))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  labs(title="Öffentliche Konsumausgaben 1961-2020", subtitle= "1991=100")+ylab("Mrd.Euro")+xlab("Jahre")

plot2 <- ggplot(data.frame(Datenagg$Ausgabenge[12:71]),aes(x=Datenagg$Jahre[12:71]))+geom_line(aes(Datenagg$Jahre[12:71],Datenagg$Ausgabenge[12:71]))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"),legend.position = "bottom")+
  labs(title="Aggregierte öffentliche Ausgaben 1961-2020", subtitle= "1991=100")+ylab("Mrd.Euro")+xlab("Jahre")

plot3 <- ggplot(data.frame(Datenagg$InvAusg[12:71]),aes(x=Datenagg$Jahre[12:71]))+geom_line(aes(Datenagg$Jahre[12:71],Datenagg$InvAusg[12:71]))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  labs(title="Öffentliche Investitionsausgaben 1961-2020", subtitle= "1991=100")+ylab("Mrd.Euro")+xlab("Jahre")

grid.arrange(plot2, plot1,plot3,ncol=3)
abb2 <- grid.arrange(plot2, plot1,plot3,ncol=3)

ggsave(filename = "002A.png",scale = 2,limitsize = FALSE,width = 18,height = 5,units = "cm",plot = abb2)

#############################################################

################ABBILDUNG 3#####################################

Datenaufgkum <- data.frame(Datenaufg$Jahre[12:71])
names(Datenaufgkum) <- "Jahre"
Datenaufgkum$Vert <- Datenaufgpro$Vert[12:71]
Datenaufgkum$Schulen <- Datenaufgkum$Vert+Datenaufgpro$Schulen[12:71]
Datenaufgkum$FuE<- Datenaufgkum$Schulen+Datenaufgpro$FuE[12:71]
Datenaufgkum$Kultur <- Datenaufgkum$FuE+Datenaufgpro$Kultur[12:71]
Datenaufgkum$SoSi <- Datenaufgkum$Kultur+Datenaufgpro$SoSi[12:71]
Datenaufgkum$Ges <- Datenaufgkum$SoSi+Datenaufgpro$Ges[12:71]
Datenaufgkum$WoWe <- Datenaufgkum$Ges+Datenaufgpro$WoWe[12:71]
Datenaufgkum$Wirtschaft <- Datenaufgkum$WoWe+Datenaufgpro$Wirtschaft[12:71]
Datenaufgkum$Verkehr <- Datenaufgkum$Wirtschaft+Datenaufgpro$Verkehr[12:71]
Datenaufgkum$ÖffSi <- Datenaufgkum$Verkehr+Datenaufgpro$ÖffSi[12:71]
Datenaufgkum$Sonstiges <- Datenaufgkum$ÖffSi+Datenaufgpro$Sonstiges[12:71]

plot13 <- ggplot(data.frame(Datenaufgkum$Sonstiges),aes(x=Datenaufgkum$Jahre))+
  geom_ribbon(aes(ymin=Datenaufgkum$ÖffSi,ymax=Datenaufgkum$Sonstiges,fill="blue"))+
  geom_ribbon(aes(ymin=Datenaufgkum$Verkehr,ymax=Datenaufgkum$ÖffSi,fill="yellow"))+geom_ribbon(aes(ymin=Datenaufgkum$Wirtschaft,ymax=Datenaufgkum$Verkehr,fill="gold"))+
  geom_ribbon(aes(ymin=Datenaufgkum$WoWe,ymax=Datenaufgkum$Wirtschaft,fill="grey"))+geom_ribbon(aes(ymin=Datenaufgkum$Ges,ymax=Datenaufgkum$WoWe,fill="orange"))+
  geom_ribbon(aes(ymin=Datenaufgkum$SoSi,ymax=Datenaufgkum$Ges,fill="black"))+geom_ribbon(aes(ymin=Datenaufgkum$Kultur,ymax=Datenaufgkum$SoSi,fill="purple"))+
  geom_ribbon(aes(ymin=Datenaufgkum$FuE,ymax=Datenaufgkum$Kultur,fill="pink"))+geom_ribbon(aes(ymin=Datenaufgkum$Schulen,ymax=Datenaufgkum$FuE,fill="red"))+
  geom_ribbon(aes(ymin=Datenaufgkum$Vert,ymax=Datenaufgkum$Schulen,fill="white"))+ geom_ribbon(aes(ymin=0,ymax=Datenaufgkum$Vert,fill="lightgreen"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"),legend.title = element_text( size=8), legend.text=element_text(size=8))+
  labs(title="Öffentl. Ausgaben Anteile der Aufgabenbereichen 1950-2012", subtitle= "1991=100")+ylab("Prozent")+xlab("Jahre")+
  scale_fill_manual(name="Legende", labels=c("Gesundheit,Sport,Erholung","Sonstiges","Verkehr","Wirtschaftsförderung","Verteidigung","Wohnungswesen","Kultur,Kirche","Soziale Sicherung","Forschung,Entwicklung","Schulen, Bildungswesen","Öffentliche Sicherheit"),
                    values =  c("blue"="blue","yellow"="yellow","gold"="gold","grey"="grey","orange"="orange","black"="black","purple"="purple","pink"="pink","red"="red","white"="white","lightgreen"="lightgreen"),aesthetics = c("colour","fill") )

plot13

ggsave(filename = "003A.png",scale = 2,limitsize = FALSE,width = 18,height = 8,units = "cm",plot = plot13)

################################################################

################ABBILDUNG 4#####################################

plot5 <- ggplot(data.frame(Datenagg$BIPreal[12:71]),aes(x=Datenagg$Jahre[12:71]))+geom_line(aes(Datenagg$Jahre[12:71],Datenagg$BIPreal[12:71]))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  labs(title="Bruttoinlandsprodukt 1961-2020", subtitle= "1991=100")+ylab("Mrd.Euro")+xlab("Jahre")

plot6 <- ggplot(data.frame(Datenagg$Arbeitslose[12:71]),aes(x=Datenagg$Jahre[12:71]))+geom_line(aes(Datenagg$Jahre[12:71],Datenagg$Arbeitslose[12:71]))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  labs(title="Arbeitslosenquote 1961-2020", subtitle= "nach der Bundesagentur für Arbeit")+ylab("Prozent")+xlab("Jahre")

plot7 <- ggplot(data.frame(Datenagg$Infl[12:71]),aes(x=Datenagg$Jahre[12:71]))+geom_line(aes(Datenagg$Jahre[12:71],Datenagg$Infl[12:71]))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  labs(title="Inflationsrate 1961-2020", subtitle= "Lebenshaltung eines vier Personen Haushaltes")+ylab("Prozent")+xlab("Jahre")

abb4 <- grid.arrange(plot5, plot6,plot7,ncol=3)

ggsave(filename = "004A.png",scale = 2,limitsize = FALSE,width = 18,height = 5,units = "cm",plot = abb4)


################################################################

################ABBILDUNG 5#####################################

plot8 <- ggplot(data.frame(Datenagg$CO2[12:71]),aes(x=Datenagg$Jahre[12:71]))+geom_line(aes(Datenagg$Jahre[12:71],Datenagg$CO2[12:71]))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  labs(title="Kohlendioxidausstoß 1961-2020", subtitle= "produktionsbedingte Emissionen")+ylab("Mio. Tonnen")+xlab("Jahre")

plot9 <- ggplot(data.frame(Datenagg$Lebenserwartung[12:71]),aes(x=Datenagg$Jahre[12:71]))+geom_line(aes(Datenagg$Jahre[12:71],Datenagg$Lebenserwartung[12:71]))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  labs(title="Durchschn. Lebenserwartung bei Geburt 1961-2020", subtitle= "geschlechterübergreifend")+ylab("in Jahren")+xlab("Jahre")

plot10 <- ggplot(data.frame(Datenagg$Top10[12:71]),aes(x=Datenagg$Jahre[12:71]))+geom_line(aes(Datenagg$Jahre[12:71],Datenagg$Top10[12:71]))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  labs(title="Armutgefährdungsquote 1961-2020", subtitle= "Anteil der Bevölkerung")+ylab("Prozent")+xlab("Jahre")

abb5 <- grid.arrange(plot9,plot10,plot8,ncol=3)

ggsave(filename = "005A.png",scale = 2,limitsize = FALSE,width = 18,height = 5,units = "cm",plot = abb5)


###############################################################

################ABBILDUNG 6#####################################

plot11 <- ggplot(data.frame(Zinsen[12:71]),aes(x=Datenagg$Jahre[12:71]))+geom_line(aes(Datenagg$Jahre[12:71],Zinsen[12:71]))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  labs(title="Zinsniveau 1961-2020", subtitle= "Spitzenrefinanzierungs-/Lombardsatz")+ylab("Prozent")+xlab("Jahre")

plot12 <- ggplot(data.frame(Datenagg$Deflator[12:71]),aes(x=Datenagg$Jahre[12:71]))+geom_line(aes(Datenagg$Jahre[12:71],Datenagg$Deflator[12:71]))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  labs(title="BIP - Deflator 1961-2020", subtitle= "1991 = 100")+ylab("Punkte")+xlab("Jahre")

abb6 <- grid.arrange(plot11, plot12,ncol=2)

ggsave(filename = "006A.png",scale = 2,limitsize = FALSE,width = 18,height = 5,units = "cm",plot = abb6)


################################################################

#################TABELLE 1: Übersicht Forschung#####################


Studien <- c("Heitger (2001)","Heitger (2001)","Heppke-Falk, Tenhofen und Wolff (2010)","Heppke-Falk, Tenhofen und Wolff (2010)","Perotti (2002)","Afonso und Sousa (2012)","Höppner (2001)","Bode, Gerke, Schellhorn (2006)","Schweinberger (2005)","Baum und Koester (2011)")
Zeitraum <- c("1960-2000","1960-2000","1974-2004","1974-2004","1960-1989","1979-2006","1970-2004","1991-2005","1960-2002","1976-2009")
Ausgaben <- c("konsumtiv","investiv","konsumtiv", "investiv","aggregiert","aggregiert","aggregiert","aggregiert","aggregiert","aggregiert")
kurzfristig <- c("negativ","negativ","insgnifikant", "positiv","positiv","negativ","positiv","positiv","negativ","positiv")
langfristig <- c("negativ","negativ", "insignifikant", "positiv","positiv","negativ","insignifikant","insignifikant","negativ", "insignifikant")

dt <- data.frame(cbind(Studien,Zeitraum,Ausgaben,kurzfristig,langfristig))
dt %>%
  kbl(caption = "Effekt eines Schocks in der Variable der öffentlichen Ausgaben auf das Bruttoinlandsprodukt in realen Größen") %>%
  kable_classic_2(full_width = T, html_font = "Cambria")%>%kable_styling(bootstrap_options = c("striped", "hover"))%>%
  save_kable(file = "table_001T.png",zoom=2)

###############################################################################

#################Tabelle 2: ÜBERSICHT ÜBER DIE MODELLE:#########################################

Modell <- c("Modell  1","Modell  2","Modell  3","Modell  4")
DWV <- c("Ja","Ja","Ja","Ja")
Darm <- c("Nein","Ja","Ja","Ja")
Öffentl.Ausgaben <- c("Öffentl. Ausgaben aggregiert","Öffentl. Ausgaben aggregiert","Investive und konsumtive öffentl. Ausgaben","Bildungswesen und Soziale Sicherung")
Wirtschaftl.Entwicklung <- c("Reales Bruttoinlandsprodukt,Zinsniveau,Inflationsrate","Reales Bruttoinlandsprodukt, Arbeitslosenquote,Lebenserwartung,Kohlenstoffdioxidemissionen,Armutsgefährdungsquote","Reales Bruttoinlandsprodukt, Arbeitslosenquote,Lebenserwartung,Kohlenstoffdioxidemissionen,Armutsgefährdungsquote","Reales Bruttoinlandsprodukt, Arbeitslosenquote,Lebenserwartung,Kohlenstoffdioxidemissionen,Armutsgefährdungsquote")


dt <- data.frame(cbind(Modell,Öffentl.Ausgaben,Wirtschaftl.Entwicklung,DWV,Darm))
dt %>%
  kbl(caption = "Übersicht der in den Modellen berücksichtigten Variablen",col.names = c("Modell","Variablen: Öffentliche Ausgaben","Variablen: Wirtschaftliche Entwicklung","Dummy-Variable: Wiedervereinigung","Dummy-Variable: Armutsgefährdungsquote")) %>%
  kable_classic_2(full_width = F, html_font = "Cambria") %>% kable_styling(row_label_position = "c",bootstrap_options = c("striped", "hover"))%>% 
  row_spec(0:4, align = "c") %>%  save_kable(file = "table_002T.png",zoom=4)



#############################################################################

################Tabelle 3 : ADF - Tests#####################################

Zeitreihe <- c("Aggregierte öffentliche Ausgaben", "Konsumtive Ausgaben", "Investive Ausgaben", "Bildungsausgaben", "Ausgaben für Soziale Sicherung", "Reales Bruttoinlandsprodukt","Zinsniveau","Inflationsrate","Arbeitslosenquote","Lebenserwartung","Kohlenstoffdioxidemissionen","Armutsgefährdungsquote","")
Aufg <- c("Wachstumsrate","Wachstumsrate","Wachstumsrate","Wachstumsrate","Wachstumsrate","Wachstumsrate","Wachstumsrate","Wachstumsrate","Erste Differenz der Wachstumsrate","Wachstumsrate","Wachstumsrate","Erste Differenz der Wachstumsrate","")
Int <- c("I(1)","I(1)","I(1)","I(1)","I(1)","I(1)","I(1)","I(1)","I(2)","I(1)","I(1)","I(2)","")


dt <- data.frame(cbind(Zeitreihe,Aufg,Int))
dt %>%
  kbl(caption = "Implikationen des ADF - Tests: Stationarität der Zeitreihen",col.names = c("Zeitreihe","Transformation","Integrationsgrad")) %>%
  kable_classic_2(full_width =T , html_font = "Cambria",row_label_position = "c",) %>% kable_styling(row_label_position = "c",bootstrap_options = c("striped", "hover"))%>% 
  row_spec(0:13, align = "c") %>%  save_kable(file = "table_003T.png",zoom=4)



######################################################

################Tabelle 4#####################################

Modell <- c("Modell 1","Modell 2","Modell 3","Modell 4")
Var<- c("Var(1)","Var(1)","Var(1)","Var(1)")
Konst <- c("Ja","Ja","Ja","Nein")
End <- c("4","6","7","7")
Dummy <- c("Ja","Ja","Ja","Ja")
aufgenommene_Lags <- c("1","1","1","1")
Beobachtungen <- c("59","58","58","51")


dt <- data.frame(cbind(Modell,aufgenommene_Lags,Var,End,Konst,Dummy,Beobachtungen))
dt %>%
  kbl(caption = "Übersicht über die geschätzten Modelle",col.names = c("Modellname","AIC-Informationskriterium","Modell","Anzahl endogender Variablen","Konstante","Dummy-Variable","Beobachtungen")) %>%
  kable_classic_2(full_width = T, html_font = "Cambria")%>% kable_styling(row_label_position = "c")%>% 
  row_spec(0:4, align = "c") %>%  save_kable(file = "table_004T.png",zoom=4)


###########################################################################

################Tabelle 5#####################################

Modell <- c("Modell 1","Modell 2","Modell 3","Modell 4")
BG <- c("0.1401","0.07949","0.07596","0.02226*")
JB <- c("<0.01","<0.01","<0.01","<0.01")
Hom <- c("0.2873","1","1","1")

dt <- data.frame(cbind(Modell,BG,JB,Hom))
dt %>%
  kbl(caption = "Übersicht über die p-Werte der Tests der Residuenanalyse",col.names = c("Modellname","Breusch-Godfrey-LM-Tests","Jarque-Bera-Test","ARCH - Tests")) %>%
  kable_classic_2(full_width =T , html_font = "Cambria",row_label_position = "c") %>% kable_styling(row_label_position = "c")%>% 
  row_spec(0:4, align = "c") %>% footnote( symbol = c("p-Wert mit Konstante: 0.009106") )         %>% 
save_kable(file = "table_005T.png",zoom=4)

#################################################################

################Tabelle 6#####################################

Modell <- c("Modell 1","Modell 2","Modell 3","Modell 4")
Testst <- c("24.85","69.65","93.60","90.59")
krit <- c("9.49","12.59","14.07","14.07")
d <- c("4","6","7","7")
T <- c("59","58","58","51")
Ent <- c("H0 verwerfen","H0 verwerfen","H0 verwerfen","H0 verwerfen")

dt <- data.frame(cbind(Modell,T,d,Testst,krit,Ent))
dt %>%
  kbl(caption = "Übersicht über die Testergebnisse des Tests auf H0: Die Korrelation der Residuen zwischen den Gleichungen eines VAR - Modells ist zufällig nach Breusch und Pagan (1980) ",col.names = c("Modellname","Beobachtungen","Anzahl endogener Variablen","LM-Teststatistik","kritischer Wert*","Entscheidung")) %>%
  kable_classic_2(full_width = F , html_font = "Cambria",row_label_position = "c") %>% kable_styling(row_label_position = "c")%>% 
  row_spec(0:4, align = "c") %>% footnote( symbol = c("aus der Chi-Quadrat Verteilung mit d Freiheitsgraden zum 5% - Signifikanzniveau") ) %>% 
  save_kable(file = "table_006T.png",zoom=4)

###################################################################

###########################Tabelle 7##############################################

Modell <- c("Modell 1","Modell 2","Modell 3","Modell 3","Modell 4","Modell 4")
Ursache <- c("Aggregierte öffentl. Ausgaben","Aggregierte öffentl. Ausgaben","Konsumtive öffentl. Ausgaben","Investive öffentl. Ausgaben","Öffentl. Bildungsausgaben","Öffentl. Ausgaben für Soziale Sicherung")
Effekt <- c("Reales Bruttoinlandsprodukt, Zinsniveau, Inflationsrate","Reales Bruttoinlandsprodukt, Arbeitslosenquote, Lebenserwartung, Kohlenstoffdioxidemissionen, Armutsgefährdungsquote","Investive öffentl. Ausgaben, Reales Bruttoinlandsprodukt, Arbeitslosenquote, Lebenserwartung, Kohlenstoffdioxidemissionen, Armutsgefährdungsquote","Konsumtive öffentl. Ausgaben, Reales Bruttoinlandsprodukt, Arbeitslosenquote, Lebenserwartung, Kohlenstoffdioxidemissionen, Armutsgefährdungsquote","Öffentl. Ausgaben für Soziale Sicherung, Reales Bruttoinlandsprodukt, Arbeitslosenquote, Lebenserwartung, Kohlenstoffdioxidemissionen, Armutsgefährdungsquote","Öffentl. Bildungsausgaben, Reales Bruttoinlandsprodukt, Arbeitslosenquote, Lebenserwartung, Kohlenstoffdioxidemissionen, Armutsgefährdungsquote")
p <- c("0.635","0.404","0.294","0.277","0.038","0.422")
Ent <- c("H0 nicht verwerfen","H0 nicht verwerfen","H0 nicht verwerfen","H0 nicht verwerfen","H0 verwerfen","H0 nicht verwerfen")


dt <- data.frame(cbind(Modell,Ursache,Effekt,p,Ent))


dt %>%
  kbl(caption = "Ergebnisse des Granger - Kausalitätstests: P - Werte",col.names = c("Modellname","Ursache*","Effekt*","p-Wert","Entscheidung**")) %>%
  kable_classic_2(full_width =TRUE , html_font = "Cambria",row_label_position = "c") %>% kable_styling(row_label_position = "c")%>% 
  row_spec(0:6, align = "c") %>% footnote(symbol =  c("In der jeweiligen Spezifikation des Modells s. Tabellen 3 und 4","Zum 5% - Signifikanzniveau"),symbol_manual = c("*","**") )         %>% 
  save_kable(file = "table_007T.png",zoom=5)

#########################################################################


##########################Tabelle 8################################################

Modell1 <- c("Modell 3","Modell 4")
Ursache1 <- c("Konsumtive öffentl. Ausgaben & Investive öffentl. Ausgaben","Öffentl. Bildungsausgaben & Öffentl. Ausgaben für Soziale Sicherung")
Effekt1 <- c("Reales Bruttoinlandsprodukt, Arbeitslosenquote, Lebenserwartung, Kohlenstoffdioxidemissionen, Armutsgefährdungsquote","Reales Bruttoinlandsprodukt, Arbeitslosenquote, Lebenserwartung, Kohlenstoffdioxidemissionen, Armutsgefährdungsquote")
p1 <- c("0.332","0.422")
Ent1 <- c("H0 nicht verwerfen","H0 nicht verwerfen")


dt <- data.frame(cbind(Modell1,Ursache1,Effekt1,p1,Ent1))


dt %>%
  kbl(caption = "Ergebnisse des Granger - Kausalitätstests: P - Werte",col.names = c("Modellname","Ursache*","Effekt*","p-Wert","Entscheidung**")) %>%
  kable_classic_2(full_width =TRUE , html_font = "Cambria",row_label_position = "c") %>% kable_styling(row_label_position = "c")%>% 
  row_spec(0:2, align = "c") %>% footnote(symbol =  c("In der jeweiligen Spezifikation des Modells s. Tabellen 3 und 4","Zum 5% - Signifikanzniveau"),symbol_manual = c("*","**") )         %>% 
  save_kable(file = "table_008T.png",zoom=4)



##################################################################################################################################################################################################################################################################################


########################################################ZEITREIHEN AUF STATIONARITÄT PRÜFEN##########################################################################


##########ZEITREIHEN DER ÖFFENTLICHEN AUSGABEN###########



summary(ur.df(Ausgabengsts, selectlags = "AIC", lags = 10, type = "trend"))
stationary.test(Ausgabengsts, method = "kpss", lag.short = FALSE)
#beide Tests bestätigen die Nichtstationarität --> Wachstumsrate:
DAusgabengsts<-ts(diff(Ausgabengsts,differences = 1)/Ausgabengsts[1:70])
DAusgabengsts[24] <-0.138 #geschätzter Wert für das Ausgabenwachstum im Jahr 1974, in dem eine Änderung der Haushaltssystematik durchgeführt wurde. Quelle siehe Literatur. 
summary(ur.df(DAusgabengsts[12:70], selectlags = "AIC", lags = 10, type = "trend"))
stationary.test(DAusgabengsts, method = "kpss", lag.short = FALSE)
#beide Tests bestätigen, dass es sich um stationäre Zeitreihen handelt. 

summary(ur.df(Invausgts, selectlags = "AIC", lags = 10, type = "trend"))
stationary.test(Invausgts, method = "kpss", lag.short = FALSE)
#laut ADF und KPSS Test nicht stationär --> Wachstumsrate:
DInvausgts<-ts(diff(Invausgts[11:71],differences = 1)/Invausgts[11:70])
DInvausgts[24]<-0.138 #geschätzter Wert für das Ausgabenwachstum im Jahr 1974, in dem eine Änderung der Haushaltssystematik durchgeführt wurde. Quelle siehe Literatur. 
summary(ur.df(DInvausgts, selectlags = "AIC", lags = 10, type = "drift"))
stationary.test(DInvausgts, method = "kpss", lag.short = FALSE)
#laut ADF-Test stationär --> folge der Indikation. 

summary(ur.df(Konausgts, selectlags = "AIC", lags = 10, type = "trend"))
stationary.test(Konausgts, method = "kpss", lag.short = FALSE)
#laut ADF und KPSS Test nicht stationär --> Wachstumsrate:
DKonausgts<-ts(diff(Konausgts[11:71],differences = 1)/Konausgts[11:70])
DKonausgts[24]<-0.138 #geschätzter Wert für das Ausgabenwachstum im Jahr 1974, in dem eine Änderung der Haushaltssystematik durchgeführt wurde. Quelle siehe Literatur. 
summary(ur.df(DKonausgts, selectlags = "AIC", lags = 10, type = "drift"))
#laut ADF-Test stationär --> folge der Indikation. 

summary(ur.df(SozialeSits, selectlags = "AIC", lags = 10, type = "trend"))
stationary.test(SozialeSits, method = "kpss", lag.short = FALSE)
#laut ADF und KPSS Test nicht stationär --> Wachstumsrate:
DSozialeSits<-ts(diff(SozialeSits,differences = 1)/SozialeSits[1:62])
DSozialeSits[24]<-0.138 #geschätzter Wert für das Ausgabenwachstum im Jahr 1974, in dem eine Änderung der Haushaltssystematik durchgeführt wurde. Quelle siehe Literatur. 
summary(ur.df(DSozialeSits[12:62], selectlags = "AIC", lags = 10, type = "drift"))
#laut ADF-Test stationär --> folge der Indikation. 

Bildungswesents <- Bildungswesents + FuEts
summary(ur.df(Bildungswesents, selectlags = "AIC", lags = 10, type = "trend"))
stationary.test(Bildungswesents, method = "kpss", lag.short = FALSE)
#laut ADF und KPSS Test nicht stationär --> Wachstumsrate:
DBildungswesents<-ts(diff(Bildungswesents,differences = 1)/Bildungswesents[1:62])
DBildungswesents[24]<-0.138 #geschätzter Wert für das Ausgabenwachstum im Jahr 1974, in dem eine Änderung der Haushaltssystematik durchgeführt wurde. Quelle siehe Literatur. 
summary(ur.df(DBildungswesents[12:62], selectlags = "AIC", lags = 10, type = "drift"))
stationary.test(DBildungswesents, method = "kpss", lag.short = FALSE)
#laut ADF-Test stationär --> folge der Indikation. 

summary(ur.df(Wivets, selectlags = "AIC", lags = 10, type = "trend"))
#laut ADF und KPSS Test nicht stationär --> Wachstumsrate:
DWivets<-ts(diff(Wivets,differences = 1)/Wivets[1:62])
DWivets[24]<-0.138 #geschätzter Wert für das Ausgabenwachstum im Jahr 1974, in dem eine Änderung der Haushaltssystematik durchgeführt wurde. Quelle siehe Literatur. 
summary(ur.df(DWivets[12:62], selectlags = "AIC", lags = 10, type = "drift"))
#laut ADF-Test stationär --> folge der Indikation. 

###########ZEITREIHEN WIRTSCHAFTLICHER ENTWICKLUNG#############

summary(ur.df(BIPrealts, selectlags = "AIC", lags = 10, type = "trend"))
stationary.test(BIPrealts, method = "kpss", lag.short = FALSE)
#laut KPSS-Test und ADF-Test nicht stationär --> Wachstumsrate:
DBIPrealts<-ts(diff(BIPrealts,differences = 1)/BIPrealts[1:70])
summary(ur.df(DBIPrealts[12:70], selectlags = "AIC", lags = 10, type = "trend"))
stationary.test(DBIPrealts, method = "kpss", lag.short = FALSE)
#laut ADF-Test I(1)-->erste Differenz verwenden. 

summary(ur.df(Alquotets, selectlags = "AIC", lags = 10, type = "drift"))
stationary.test(Alquotets, method = "kpss", lag.short = FALSE)
#bede Tests bestätigen Nichtstationarität --> Wachstumsrate:
DAlquotets<-ts(diff(Alquotets,differences = 1))/Alquotets[1:70]
summary(ur.df(DAlquotets[12:70], selectlags = "AIC", lags = 10, type = "drift"))
#immer noch nicht stationär --> erste Differenz der Wachstumsrate:
DAlquotets<-ts(diff(Alquotets,differences = 1))
summary(ur.df(DAlquotets[12:70], selectlags = "AIC", lags = 10, type = "drift"))
#laut ADF-Test stationär --> Folge ADF-Tests.

summary(ur.df(CO2ts, selectlags = "AIC", lags = 10, type = "drift"))
stationary.test(CO2ts, method = "kpss", lag.short = FALSE)
#nicht stationär nach ADF-Test --> Wachstumsrate: 
DCO2ts<-ts(diff(CO2ts,differences = 1)/CO2ts[1:70])
summary(ur.df(DCO2ts[12:70], selectlags = "AIC", lags = 10, type = "drift"))
stationary.test(DCO2ts, method = "kpss", lag.short = FALSE)
#laut ADF und KPSS stationär

summary(ur.df(Lebenserwartungts, selectlags = "AIC", lags = 10, type = "trend"))
stationary.test(Lebenserwartungts, method = "kpss", lag.short = FALSE)
#laut beiden Tests nicht stationär --> Wachstumsrate:
DLebenserwartungts<-ts(diff(Lebenserwartungts,differences = 1)/Lebenserwartungts[1:70])
summary(ur.df(DLebenserwartungts[12:70], selectlags = "AIC", lags = 10, type = "drift"))
stationary.test(DLebenserwartungts, method = "kpss", lag.short = FALSE)
#stationär

summary(ur.df(Top10ts, selectlags = "AIC", lags = 10, type = "trend"))
stationary.test(Top10ts, method = "kpss", lag.short = FALSE)
#nicht statinär laut beiden Tests --> Wachstumsrate:
DTop10ts<-ts(diff(Top10ts,differences = 1))/Top10ts[1:70]
summary(ur.df(DTop10ts[12:70], selectlags = "AIC", lags = 10, type = "drift"))
#noch nicht stationär --> erste Differenz der Wachstumsrate:
DTop10ts<- ts(diff(DTop10ts,differences = 1))
summary(ur.df(DTop10ts[12:67], selectlags = "AIC", lags = 10, type = "drift"))
#knapp stationär laut ADF

#nur laut ADF-Test statinär in erster Differenz --> dem ADF-Test folgen. 

summary(ur.df(Deflatorts, selectlags = "AIC", lags = 10, type = "trend"))
stationary.test(Deflatorts, method = "kpss", lag.short = FALSE)
#nicht stationär laut beiden Tests --> Wachstumsrate:
DDeflatorts <- ts((diff(Deflatorts,differences = 1)/Deflatorts[1:70]))
summary(ur.df(DDeflatorts[12:70], selectlags = "AIC", lags = 10, type = "drift"))
stationary.test(DDeflatorts, method = "kpss", lag.short = FALSE)

summary(ur.df(Zinsen, selectlags = "AIC", lags = 10, type = "drift"))
#nicht stationär laut beiden Tests --> Wachstumsrate:
DZinsents <- ts(diff(Zinsen,differences = 1))/Zinsen[1:70]
summary(ur.df(DZinsents[12:70], selectlags = "AIC", lags = 10, type = "drift"))
#stationär laut ADF

summary(ur.df(Inflationts[12:70], selectlags = "AIC", lags = 10, type = "trend"))
stationary.test(Inflationts, method = "kpss", lag.short = FALSE)
#laut ADF und KPSS Test nicht stationär --> Wachstumsrate:
DInflationts<-ts(diff(Inflationts,differences = 1)/Inflationts[1:70])
summary(ur.df(DInflationts[12:70], selectlags = "AIC", lags = 10, type = "drift"))
#laut ADF-Test stationär --> folge der Indikation. 


#############################################################################################################################################



##########AUF KOINTEGRATION TESTEN MIT H0: KEINE KOINTEGRATION##########

#Anmerkung: Nur Modell 1 bezieht Variablen ein, die den gleichen Integrationsgrad aufweisen. 

#Modell 1:
coint.test(Ausgabengsts,matrix(c(BIPrealts,Inflationts,Zinsen),nrow = 71,ncol = 3),nlag = 1)
#Keine Kointegration --> mit ersten Differenzen bzw. Ergebnissen der Tests weiterarbeiten




#####################################DUMMY-VARIABLEN FÜR ALLE MODELLE####################################################################


sdwied <- rep(c(0,1,0),times=c(29,1,29)) #für alle Modelle Dummy-Wiederver.
sdarmut <- rep(c(0,1),times=c(50,9)) #Dummy-Armutsgef.
sdges <- cbind(sdwied,sdarmut) #für Modelle 2-4


########################################################################################################################################################


#########################################Modell 1: AUSGANGSMODELL NACH THEORETISCHEN GRUNDLAGEN##########################################################################


vardata1 <- cbind(DBIPrealts[12:70],DInflationts[12:70])

vardata1 <- cbind(DAusgabengsts[12:70],DBIPrealts[12:70],DZinsents[12:70],DInflationts[12:70])

VARselect(vardata1,lag.max = 5,exogen = sdwied)
#laut AIC 1 Lag wählen.


##########Modell schätzen##########

Var1 <- VAR(vardata1,p = 1,type = "const",exogen = sdwied)


##########Modell AUSWERTEN##########

summary(Var1)
plot(Var1)


#########RESIDUEN AUF UNABHÄNGIGKEIT TESTEN##########

corr1 <- (summary(Var1)$corres) #testen auf Unabhängigkeit der Residuen
lmtest <- (corr1[upper.tri(corr1)])^2
sum(lmtest)*59



##########RESIDUENANALYSE##########

#1. Auf Autokorrelation : H0 = keine Autokorrelation bis Lag...
serial.test(Var1, lags.pt = 15, type = "BG")
#kann nicht verworfen werden --> keine Autokorrelation 




#2.Test auf Normalverteilung: H0 = Normalverteilung liegt vor 
normality.test(Var1)
#in allen Modellen wird H0 verworfen --> keine Normalverteilung


#3. Test auf Homoskedastie: H0 = Homoskedastie
Homosked1 <- arch.test(Var1, lags.multi = 5)
Homosked1
#kann für alle Modelle nicht verworfen werden --> Homoskedastie 



##########Modell interpretieren##########


##Impuls-Antwort-Funktionen:

summary(Var1)$varresult$y1$sigma  #Höhe des Schocks


#Auswirkung auf den jeweiligen Indikator
Var1airf <- irf(Var1, n.ahead = 20, ortho = T,impulse = "y1",response = "y2")
Var1birf <- irf(Var1, n.ahead = 20, ortho = T,response="y3",impulse = "y1")
Var1cirf <- irf(Var1, n.ahead = 20, ortho = T,response="y4",impulse = "y1")
Var1dirf <- irf(Var1, n.ahead = 20, ortho = T,response="y1",impulse = "y1")
Var1eirf <- irf(Var1, n.ahead = 20, ortho = T,response="y1",impulse = "y2")


plot(Var1airf,main = "Auswirkung eines Schocks in der Wachstumsrate der öffentlichen Ausgaben",xlab ="Perioden",ylab="Wachstumsrate des realen Bruttoinlandsprodukt")
plot(Var1birf,main = "Auswirkung eines Schocks in der Wachstumsrate der öffentlichen Ausgaben",xlab ="Perioden",ylab="Wachstumsrate der Inflationsrate")
plot(Var1cirf,main = "Auswirkung eines Schocks in der Wachstumsrate der öffentlichen Ausgaben",xlab ="Perioden",ylab="Wachstumsrate des Zinsniveaus")
plot(Var1dirf,main = "Auswirkung eines Schocks in der Wachstumsrate der öffentlichen Ausgaben",xlab ="Perioden",ylab="Wachstumsrate der öffentlichen Ausgaben")
plot(Var1eirf,main = "Auswirkung eines Schocks der Wachstumsrate des Bruttoinlandsproduktes",xlab ="Perioden",ylab="Wachstumsrate der öffentlichen Ausgaben")



##Granger-Kausalitätstest:

causality(Var1, cause = "y1",boot = TRUE, boot.runs = 1000)

causality(Var1, cause = "y2", boot = TRUE, boot.runs = 1000)


##########################################################################################################################################################################



#########################################Modell 2: AGREEGIERT AUSGABEN & WIRTSCHAFTL. ENTWICKLUNG#########################################################################



vardata2 <- cbind(DAusgabengsts[12:69],DBIPrealts[12:69],DAlquotets[12:69],DLebenserwartungts[12:69],DTop10ts[12:69],DCO2ts[12:69])

VARselect(vardata2,lag.max = 4,exogen = sdges[1:58,])
#laut AIC 1 Lag wählen.


##########Modell schätzen##########

Var2 <- VAR(vardata2,p = 1,type = "const",exogen = sdges[1:58,])


##########Modell AUSWERTEN##########

summary(Var2)
plot(Var2)

#########RESIDUEN AUF UNABHÄNGIGKEIT TESTEN##########

corr2 <- (summary(Var2)$corres) 
lmtest2 <- (corr2[upper.tri(corr2)])^2
sum(lmtest2)*58

##########RESIDUENANALYSE##########

#1. Auf Autokorrelation : H0 = keine Autokorrelation bis Lag...
serial.test(Var2, lags.pt = 15, type = "BG")
#kann nicht verworfen werden --> keine Autokorrelation 




#2.Test auf Normalverteilung: H0 = Normalverteilung liegt vor 
normality.test(Var2)
#in allen Modellen wird H0 verworfen --> keine Normalverteilung


#3. Test auf Homoskedastie: H0 = Homoskedastie
Homosked2 <- arch.test(Var2, lags.multi = 5)
Homosked2
#kann für alle Modelle nicht verworfen werden --> Homoskedastie 



##########Modell interpretieren##########


##Impuls-Antwort-Funktionen:

#Auswirkung auf den jeweiligen Indikator

Var2airf <- irf(Var2, n.ahead = 20, ortho = T,response="y2",impulse = "y1")
Var2birf <- irf(Var2, n.ahead = 20, ortho = T,response="y3",impulse = "y1")
Var2cirf <- irf(Var2, n.ahead = 20, ortho = T,response="y4",impulse = "y1")
Var2dirf <- irf(Var2, n.ahead = 20, ortho = T,response="y5",impulse = "y1")
Var2eirf <- irf(Var2, n.ahead = 20, ortho = T,response="y6",impulse = "y1")
Var2firf <- irf(Var2, n.ahead = 20, ortho = T,response="y1",impulse = "y1")
Var2girf <- irf(Var2, n.ahead = 20, ortho = T,response="y1",impulse = "y2")


plot(Var2airf,xlab ="Perioden",ylab="Wachstumsrate reales Bruttoinlandsprodukt",main="")
plot(Var2birf,main = "Auswirkung eines Schocks in der Wachstumsrate der öffentlichen Ausgaben",xlab ="Perioden",ylab="1. Differenz Wachstumsrate Arbeitslosenquote")
plot(Var2cirf,main = "Auswirkung eines Schocks in der Wachstumsrate der öffentlichen Ausgaben",xlab ="Perioden",ylab="Wachstumsrate Ø Lebenserwartung bei Geburt")
plot(Var2dirf,main = "Auswirkung eines Schocks in der Wachstumsrate der öffentlichen Ausgaben",xlab ="Perioden",ylab="1. Differenz Wachstumsrate Armutsgefährdungsquote")
plot(Var2eirf,main = "Auswirkung eines Schocks in der Wachstumsrate der öffentlichen Ausgaben",xlab ="Perioden",ylab="Wachstumsrate Kohlenstoffdioxidemissionen")
plot(Var2firf,main = "Auswirkung eines Schocks in der Wachstumsrate der öffentlichen Ausgaben",xlab ="Perioden",ylab="Wachstumsrate öffentlichen Ausgaben")
plot(Var2girf,main = "Auswirkung eines Schocks der Wachstumsrate des Bruttoinlandsproduktes",xlab ="Perioden",ylab="Wachstumsrate öffentlichen Ausgaben")



##Granger-Kausalitätstest:

causality(Var2, cause = "y1", boot = TRUE, boot.runs = 1000)

causality(Var2, cause = "y2", boot = TRUE, boot.runs = 1000)


#####################################################################################################################################################################################


#########################################Modell 3: INVESTIVE / KONSUMTIVE AUSGABEN & WIRTSCHAFTLICHE ENTWICKLUNG#########################################################################



vardata3 <- cbind(DKonausgts[2:59],DInvausgts[2:59],DBIPrealts[12:69],DAlquotets[12:69],DLebenserwartungts[12:69],DTop10ts[12:69],DCO2ts[12:69]) #

VARselect(vardata3,lag.max = 4,exogen = sdges[1:58,])
#laut AIC 1 Lag wählen.


##########Modell schätzen##########

Var3 <- VAR(vardata3,p = 1,type = "const",exogen = sdges[1:58,])


##########Modell AUSWERTEN##########

summary(Var3)
plot(Var3)

#########RESIDUEN AUF UNABHÄNGIGKEIT TESTEN##########

corr3 <- (summary(Var3)$corres) 
lmtest3 <- (corr3[upper.tri(corr3)])^2
sum(lmtest3)*58

##########RESIDUENANALYSE##########

#1. Auf Autokorrelation : H0 = keine Autokorrelation bis Lag...
serial.test(Var3, lags.pt = 15, type = "BG")
#kann nicht verworfen werden --> keine Autokorrelation 




#2.Test auf Normalverteilung: H0 = Normalverteilung liegt vor 
normality.test(Var3)
#in allen Modellen wird H0 verworfen --> keine Normalverteilung


#3. Test auf Homoskedastie: H0 = Homoskedastie
Homosked3<- arch.test(Var3, lags.multi = 5)
Homosked3
#kann für alle Modelle nicht verworfen werden --> Homoskedastie 



##########Modell interpretieren##########


##Impuls-Antwort-Funktionen:

#Auswirkung auf den jeweiligen Indikator

##KONSUMAUSGABEN:

Var3aairf <- irf(Var3, n.ahead = 20, ortho = T,response="y3",impulse = "y1")
Var3abirf <- irf(Var3, n.ahead = 20, ortho = T,response="y4",impulse = "y1")
Var3acirf <- irf(Var3, n.ahead = 20, ortho = T,response="y5",impulse = "y1")
Var3adirf <- irf(Var3, n.ahead = 20, ortho = T,response="y6",impulse = "y1")
Var3aeirf <- irf(Var3, n.ahead = 20, ortho = T,response="y7",impulse = "y1")
Var3afirf <- irf(Var3, n.ahead = 20, ortho = T,response="y1",impulse = "y1")
Var3agirf <- irf(Var3, n.ahead = 20, ortho = T,response="y1",impulse = "y3")

plot(Var3aairf,main = "Auswirkung eines Schocks in der Wachstumsrate der konsumtiven öffentlichen Ausgaben",xlab ="Perioden",ylab="Wachstumsrate reales Bruttoinlandsprodukt")
plot(Var3abirf,main = "Schocks in der Wachstumsrate der konsumtiven öffentlichen Ausgaben",xlab ="Perioden",sub="Modell 2",ylab="1. Differenz Wachstumsrate Arbeitslosenquote")
plot(Var3acirf,main = "Auswirkung eines Schocks in der Wachstumsrate der konsumtiven öffentlichen Ausgaben",xlab ="Perioden",ylab="Wachstumsrate  Ø Lebenserwartung bei Geburt")
plot(Var3adirf,main = "Auswirkung eines Schocks in der Wachstumsrate der konsumtiven öffentlichen Ausgaben",xlab ="Perioden",ylab="1. Differenz Wachstumsrate Armutsgefährdungsquote")
plot(Var3aeirf,main = "Auswirkung eines Schocks in der Wachstumsrate der konsumtiven öffentlichen Ausgaben",xlab ="Perioden",ylab="Wachstumsrate Kohlenstoffdioxidemissionen")
plot(Var3afirf,main = "Auswirkung eines Schocks in der Wachstumsrate der konsumtiven öffentlichen Ausgaben",xlab ="Perioden",ylab="Wachstumsrate konsumtive öffentliche Ausgaben")
plot(Var3agirf,main = "Auswirkung eines Schocks in der Wachstumsrate des Bruttoinlandsproduktes",xlab ="Perioden",ylab="Wachstumsrate öffntl. Konsumausgaben")

##INVESTITIONSAUSGABEN:

Var3bairf <- irf(Var3, n.ahead = 20, ortho = T,response="y3",impulse = "y2")
Var3bbirf <- irf(Var3, n.ahead = 20, ortho = T,response="y4",impulse = "y2")
Var3bcirf <- irf(Var3, n.ahead = 20, ortho = T,response="y5",impulse = "y2")
Var3bdirf <- irf(Var3, n.ahead = 20, ortho = T,response="y6",impulse = "y2")
Var3beirf <- irf(Var3, n.ahead = 20, ortho = T,response="y7",impulse = "y2")
Var3bfirf <- irf(Var3, n.ahead = 20, ortho = T,response="y2",impulse = "y2")
Var3bgirf <- irf(Var3, n.ahead = 20, ortho = T,response="y2",impulse = "y3")


plot(Var3bairf,main = "Auswirkung eines Schocks in der Wachstumsrate der investiven öffentlichen Ausgaben",xlab ="Perioden",ylab="Wachstumsrate reales Bruttoinlandsprodukt")
plot(Var3bbirf,main = "Auswirkung eines Schocks in der Wachstumsrate der investiven öffentlichen Ausgaben",xlab ="Perioden",ylab="1. Differenz Wachstumsrate Arbeitslosenquote")
plot(Var3bcirf,main = "",xlab ="Perioden",ylab="Wachstumsrate Ø Lebenserwartung bei Geburt")
plot(Var3bdirf,main = "",xlab ="Perioden",ylab="1. Differenz Wachstumsrate Armutsgefährdungsquote")
plot(Var3beirf,main = "Modell 3: Schock in der Wachstumsrate der öffentlichen Investitionsausgaben",xlab ="Perioden",ylab="Wachstumsrate Kohlenstoffdioxidemissionen")
plot(Var3bfirf,main = "",xlab ="Perioden",ylab="Wachstumsrate investive öffentliche Ausgaben")
plot(Var3bgirf,main = "",xlab ="Perioden",ylab="Wachstumsrate öffentl. Investitionsausgaben")



##Granger-Kausalitätstest:

causality(Var3, cause = "y1", boot = TRUE, boot.runs = 1000)

causality(Var3, cause = "y2", boot = TRUE, boot.runs = 1000)

causality(Var3, cause = c("y1","y2"), boot = TRUE, boot.runs = 1000)

causality(Var3, cause = "y3", boot = TRUE, boot.runs = 1000)


#################################################################################################################################################################################


#########################################Modell 4: BILDUNGS-/SOZIALE SICHERUNG AUSGABEN & ENTWICKLUNG#########################################################################


#Dummy-Variablen an Zeiträume anpassen#########
sdwied1 <- rep(c(0,1,0),times=c(40,1,28))
sdarmut1 <- rep(c(0,1),times=c(54,8))
sdges1 <- cbind(sdwied1[1:62],sdarmut1[1:62])

###############################################


vardata4 <- cbind(DBildungswesents[12:62],DSozialeSits[12:62],DBIPrealts[12:62],DAlquotets[12:62],DLebenserwartungts[12:62],DTop10ts[12:62],DCO2ts[12:62]) #,

VARselect(vardata4,lag.max = 4,exogen = sdges1[12:62])
#laut AIC 1 Lag wählen.


##########Modell schätzen#########

Var4 <- VAR(vardata4,p = 1,type ="none",exogen = sdges1[12:62,])

##########Modell AUSWERTEN##########

summary(Var4)
plot(Var4)

#########RESIDUEN AUF UNABHÄNGIGKEIT TESTEN##########

corr4 <- (summary(Var4)$corres) 
lmtest4 <- (corr4[upper.tri(corr4)])^2
sum(lmtest4)*51

##########RESIDUENANALYSE##########

#1. Auf Autokorrelation : H0 = keine Autokorrelation bis Lag...
serial.test(Var4, lags.pt = 15, type = "BG")
#kann nicht verworfen werden --> keine Autokorrelation 




#2.Test auf Normalverteilung: H0 = Normalverteilung liegt vor 
normality.test(Var4)
#in allen Modellen wird H0 verworfen --> keine Normalverteilung


#3. Test auf Homoskedastie: H0 = Homoskedastie
Homosked4<- arch.test(Var4, lags.multi = 5)
Homosked4
#kann für alle Modelle nicht verworfen werden --> Homoskedastie 



##########Modell interpretieren##########


##Impuls-Antwort-Funktionen:

#BILDUNGSAUSGABEN

Var4aairf <- irf(Var4, n.ahead = 20, ortho = T,response="y3",impulse = "y1")
Var4abirf <- irf(Var4, n.ahead = 20, ortho = T,response="y4",impulse = "y1")
Var4acirf <- irf(Var4, n.ahead = 20, ortho = T,response="y5",impulse = "y1")
Var4adirf <- irf(Var4, n.ahead = 20, ortho = T,response="y6",impulse = "y1")
Var4aeirf <- irf(Var4, n.ahead = 20, ortho = T,response="y7",impulse = "y1")
Var4afirf <- irf(Var4, n.ahead = 20, ortho = T,response="y2",impulse = "y1")
Var4agirf <- irf(Var4, n.ahead = 20, ortho = T,response="y1",impulse = "y3")

plot(Var4aairf,main = "Modell 4:Schock in der Wachstumsrate der öffentlichen Bildungsausgaben ",xlab ="Perioden",ylab="Wachstumsrate reales Bruttoinlandsprodukt")
plot(Var4abirf,main = "Auswirkung eines Schocks in der Wachstumsrate der öffentlichen Bildungsausgaben",xlab ="Perioden",ylab="1. Differenz Wachstumsrate Arbeitslosenquote")
plot(Var4acirf,main = "Modell 4: Schock in der Wachstumsrate der öffentlichen Bildungsausgaben",xlab ="Perioden",ylab="Wachstumsrate  Ø Lebenserwartung bei Geburt")
plot(Var4adirf,main = "",xlab ="Perioden",ylab="1. Differenz Wachstumsrate Armutsgefährdungsquote")
plot(Var4aeirf,main = "",xlab ="Perioden",ylab="Wachstumsrate Kohlenstoffdioxidemissionen")
plot(Var4afirf,main = "",xlab ="Perioden",ylab="Wachstumsrate öffentliche Bildungsausgaben")
plot(Var4agirf,main = "",xlab ="Perioden",ylab="Wachstumsrate öffentliche Bildungsausgaben")

##SOZIALESICHERUNG:

Var4bairf <- irf(Var4, n.ahead = 20, ortho = T,response="y3",impulse = "y2")
Var4bbirf <- irf(Var4, n.ahead = 20, ortho = T,response="y4",impulse = "y2")
Var4bcirf <- irf(Var4, n.ahead = 20, ortho = T,response="y5",impulse = "y2")
Var4bdirf <- irf(Var4, n.ahead = 20, ortho = T,response="y6",impulse = "y2")
Var4beirf <- irf(Var4, n.ahead = 20, ortho = T,response="y7",impulse = "y2")
Var4bfirf <- irf(Var4, n.ahead = 20, ortho = T,response="y2",impulse = "y2")
Var4bgirf <- irf(Var4, n.ahead = 20, ortho = T,response="y2",impulse = "y3")


plot(Var4bairf,main = "Auswirkung eines Schocks in der Wachstumsrate der öffentlichen Ausgaben für Soziale Sicherung",xlab ="Perioden",ylab="Wachstumsrate reales Bruttoinlandsprodukt")
plot(Var4bbirf,main = "Auswirkung eines Schocks in der Wachstumsrate der öffentlichen Ausgaben für Soziale Sicherung",xlab ="Perioden",ylab="1. Differenz Wachstumsrate Arbeitslosenquote")
plot(Var4bcirf,xlab ="Perioden",ylab="Wachstumsrate Ø Lebenserwartung bei Geburt", main = "Modell 4: Schock in der Wachstumsrate der öffentlichen Ausgaben für Soziale Sicherung")
plot(Var4bdirf,main = "",xlab ="Perioden",ylab="1. Differenz Wachstumsrate Armutsgefährdungsquote")
plot(Var4beirf,main = "",xlab ="Perioden",ylab="Wachstumsrate Kohlenstoffdioxidemissionen")
plot(Var4bfirf,main = "",xlab ="Perioden",ylab="Wachstumsrate öffentl. Ausgaben Soziale Sicherung")
plot(Var4bgirf,main = "",xlab ="Perioden",ylab="Wachstumsrate öffentl. Ausgaben Soziale Sicherung")



##Granger-Kausalitätstest:

causality(Var4, cause = "y1", boot = TRUE, boot.runs = 1000)

causality(Var4, cause = "y2", boot = TRUE, boot.runs = 1000)

causality(Var4, cause = c("y1","y2"), boot = TRUE, boot.runs = 1000)


causality(Var4, cause = "y3", boot = TRUE, boot.runs = 1000)


###################################################################################################


################################################ANHANG#############################################################

####################Anhang 1#####################################################

Zeitreihe <- c("Aggregierte öffentliche Ausgaben", "Konsumtive Ausgaben", "Investive Ausgaben", "Bildungsausgaben", "Ausgaben für Soziale Sicherung", "Reales Bruttoinlandsprodukt","Zinsniveau","Inflationsrate","Arbeitslosenquote","Lebenserwartung","Kohlendioxidemissionen","Armutsgefährdungsquote")
KritWertn <- c("3.45","3.45","3.45","3.45","3.45","3.45","2.89","3.45","2.89","3.45","2.89","3.45")
Teststn <-  c("2.20","2.09","2.20","1.86","1.91","2.27","1.04","3.05","1.56","2.28","0.17","1.63")
KritWertw <- c("3.45","2.89","2.89","2.89","2.89","3.45","2.89","2.89","2.89","2.89","2.89","2.89")
Teststw <-  c("4.40","4.18","4.89","3.79","4.45","5.59","5.63","6.43","1.69","4.28","4.28","1.92")
KritWertdiff <- c("-","-","-","-","-","-","-","-","2.89","-","-","2.89")
Teststdiff <-  c("-","-","-","-","-","-","-","-","4.49","-","-","2.8904")

dt <- data.frame(cbind(Zeitreihe,KritWertn,Teststn,KritWertw,Teststw,KritWertdiff,Teststdiff ))
dt %>%
  kbl(caption = "Implikationen des ADF - Tests: Stationarität der Zeitreihen",col.names = c("Zeitreihe","Kritischer Wert in Niveaus","Teststatistik Niveaus","Kritischer Wert Wachstumsraten","Teststatistik Wachstumsraten","Kritischer Wert 1. Diff Wachstumsrate","Teststatistik 1. Diff Wachstumsraten")) %>%
  kable_classic(full_width =T , html_font = "Cambria",row_label_position = "c") %>% kable_styling(row_label_position = "c")%>% 
  row_spec(0:12, align = "c") %>% footnote( general = c("Kritische Werte zum 5% - Signifikanzniveau","Kritische Werte der Tau - Teststatistik","Auf zwei Nachkommastellen gerundet") ) %>% column_spec(c(2,4,6), bold = T, color = "red", background = "lightblue") %>%  save_kable(file = "table_001A.png",zoom=6)


###################################################################################


####################Anhang 2#####################################################


par(mfrow = c(6,2))

plot(Datenagg$Jahre[12:70],DAusgabengsts[12:70],type="l",main="Wachstumsrate aggregierte Ausgaben",ylab="Wachstumsrate",xlab="Jahre")
plot(Datenagg$Jahre[13:70],DKonausgts[2:59],type="l",main="Wachstumsrate Konsumausgaben",ylab="Wachstumsrate",xlab="Jahre")
plot(Datenagg$Jahre[13:70],DInvausgts[2:59],type="l",main="Wachstumsrate Investitionsausgaben",ylab="Wachstumsrate",xlab="Jahre")
plot(Datenagg$Jahre[12:62],DSozialeSits[12:62],type="l",main="Wachstumsrate Ausgaben für Soziale Sicherung",ylab="Wachstumsrate",xlab="Jahre")
plot(Datenagg$Jahre[12:62],DBildungswesents[12:62],type="l",main="Wachstumsrate Bildungsausgaben",ylab="Wachstumsrate",xlab="Jahre")
plot(Datenagg$Jahre[12:70],DBIPrealts[12:70],type="l",main="Wachstumsrate reales Bruttoinlandsprodukt",ylab="Wachstumsrate",xlab="Jahre")
plot(Datenagg$Jahre[12:69],DAlquotets[12:69],type="l",main="1. Diff Wachstumsrate Arbeitslosenquote",ylab="1. Diff Wachstumsrate",xlab="Jahre")
plot(Datenagg$Jahre[12:69],DCO2ts[12:69],type="l",main="Wachstumsrate Kohlendioxidausstoß",ylab="Wachstumsrate",xlab="Jahre")
plot(Datenagg$Jahre[12:69],DZinsents[12:69],type="l",main="Wachstumsrate Zinsniveau",ylab="Wachstumsrate",xlab="Jahre")
plot(Datenagg$Jahre[12:69],DLebenserwartungts[12:69],type="l",main="Wachstumsrate Lebenserwartung",ylab="Wachstumsrate",xlab="Jahre")
plot(Datenagg$Jahre[12:69],DInflationts[12:69],type="l",main="Wachstumsrate Inflation",ylab="Wachstumsrate",xlab="Jahre")
plot(Datenagg$Jahre[12:69],DTop10ts[12:69],type="l",main="1. Diff Wachstumsrate Armutsgefährdungsquote",ylab="1. Diff Wachstumsrate",xlab="Jahre")



dev.off()

##########################Anhang 3########################################################

Modella3 <- c("Modell 1")
pa3 <- c("0.10")
Enta3 <- c("H0 nicht verwerfen")


dt <- data.frame(cbind(Modella3,pa3,Enta3))


dt %>%
  kbl(caption = "Ergebnisse des Kointegrationstests für Modell 1: P - Werte",col.names = c("Modellname","p-Wert","Entscheidung*")) %>%
  kable_classic_2(full_width =T , html_font = "Cambria",row_label_position = "c") %>% kable_styling(row_label_position = "c")%>% 
  row_spec(0:1, align = "c") %>% footnote(symbol =  c("Zum 5% - Signifikanzniveau"),symbol_manual = c("*") )      %>% 
  save_kable(file = "table_003A.png",zoom=4)


###################################################################################


##########################Anhang 4########################################################

sink(file = "Var1.txt")
summary(Var1)
(summary(Var1)$corr)
(summary(Var1)$cov)

sink()

##########################Anhang 5########################################################

sink(file = "Var2.txt")
summary(Var2)
(summary(Var2)$corr)
(summary(Var2)$cov)

sink()
###################################################################################

##########################Anhang 6########################################################

sink(file = "Var3.txt")
summary(Var3)
(summary(Var3)$corr)
(summary(Var3)$cov)

sink()
###################################################################################

##########################Anhang 7########################################################

sink(file = "Var4.txt")
summary(Var4)
(summary(Var4)$corr)
(summary(Var4)$cov)

sink()


###################################################################################


######################ANHANG: ANMERKUNGEN: ALLE WEITEREN IRFS################################

#Modell 1:
plot(irf(Var1, n.ahead = 20, ortho = T,response = "y1"))
plot(irf(Var1, n.ahead = 20, ortho = T,response = "y2"))
plot(irf(Var1, n.ahead = 20, ortho = T,response = "y3"))
plot(irf(Var1, n.ahead = 20, ortho = T,response = "y4"))

#Modell2: 
plot(irf(Var2, n.ahead = 20, ortho = T,response = "y1"))
plot(irf(Var2, n.ahead = 20, ortho = T,response = "y2"))
plot(irf(Var2, n.ahead = 20, ortho = T,response = "y3"))
plot(irf(Var2, n.ahead = 20, ortho = T,response = "y4"))
plot(irf(Var2, n.ahead = 20, ortho = T,response = "y5"))
plot(irf(Var2, n.ahead = 20, ortho = T,response = "y6"))

#Modell3: 
plot(irf(Var3, n.ahead = 20, ortho = T,response = "y1"))
plot(irf(Var3, n.ahead = 20, ortho = T,response = "y2"))
plot(irf(Var3, n.ahead = 20, ortho = T,response = "y3"))
plot(irf(Var3, n.ahead = 20, ortho = T,response = "y4"))
plot(irf(Var3, n.ahead = 20, ortho = T,response = "y5"))
plot(irf(Var3, n.ahead = 20, ortho = T,response = "y6"))
plot(irf(Var3, n.ahead = 20, ortho = T,response = "y7"))

#Modell4: 
plot(irf(Var4, n.ahead = 20, ortho = T,response = "y1"))
plot(irf(Var4, n.ahead = 20, ortho = T,response = "y2"))
plot(irf(Var4, n.ahead = 20, ortho = T,response = "y3"))
plot(irf(Var4, n.ahead = 20, ortho = T,response = "y4"))
plot(irf(Var4, n.ahead = 20, ortho = T,response = "y5"))
plot(irf(Var4, n.ahead = 20, ortho = T,response = "y6"))
plot(irf(Var4, n.ahead = 20, ortho = T,response = "y7"))


################################################################################################################

######################ANHANG: ANMERKUNGEN: ALLE WEITEREN GRANGER_KAUSLITÄTSTESTS################################

#Modell1:
causality(Var1, cause = "y1", boot = TRUE, boot.runs = 1000)
causality(Var1, cause = "y2", boot = TRUE, boot.runs = 1000)
causality(Var1, cause = "y3", boot = TRUE, boot.runs = 1000)
causality(Var1, cause = "y4", boot = TRUE, boot.runs = 1000)

#Modell2:
causality(Var2, cause = "y1", boot = TRUE, boot.runs = 1000)
causality(Var2, cause = "y2", boot = TRUE, boot.runs = 1000)
causality(Var2, cause = "y3", boot = TRUE, boot.runs = 1000)
causality(Var2, cause = "y4", boot = TRUE, boot.runs = 1000)
causality(Var2, cause = "y5", boot = TRUE, boot.runs = 1000)
causality(Var2, cause = "y6", boot = TRUE, boot.runs = 1000)

#Modell3: 
causality(Var3, cause = "y1", boot = TRUE, boot.runs = 1000)
causality(Var3, cause = "y2", boot = TRUE, boot.runs = 1000)
causality(Var3, cause = "y3", boot = TRUE, boot.runs = 1000)
causality(Var3, cause = "y4", boot = TRUE, boot.runs = 1000)
causality(Var3, cause = "y5", boot = TRUE, boot.runs = 1000)
causality(Var3, cause = "y6", boot = TRUE, boot.runs = 1000)
causality(Var3, cause = "y7", boot = TRUE, boot.runs = 1000)

#Modell4:
causality(Var4, cause = "y1", boot = TRUE, boot.runs = 1000)
causality(Var4, cause = "y2", boot = TRUE, boot.runs = 1000)
causality(Var4, cause = "y3", boot = TRUE, boot.runs = 1000)
causality(Var4, cause = "y4", boot = TRUE, boot.runs = 1000)
causality(Var4, cause = "y5", boot = TRUE, boot.runs = 1000)
causality(Var4, cause = "y6", boot = TRUE, boot.runs = 1000)
causality(Var4, cause = "y7", boot = TRUE, boot.runs = 1000)

###################################################################################################################
