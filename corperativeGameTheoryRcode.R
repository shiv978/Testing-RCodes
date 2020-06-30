install.packages("GameTheory")
library(GameTheory)
COALITIONS <- c(4612,1743,581,6918,5381,3075,9000)
LEMAIRE<-DefineGame(3,COALITIONS)
summary(LEMAIRE)


  NAMES <- c("Investor 1","Investor 2","Investor 3")
LEMAIRESHAPLEY <- ShapleyValue(LEMAIRE,NAMES)
summary(LEMAIRESHAPLEY)

LEMAIRENUCLEOLUS<-Nucleolus(LEMAIRE)


###################
COALITIONS <- c(26,27,55,57,53,81,83,82,84,110,108,110,110,110,110)
IPL<-DefineGame(4,COALITIONS)
summary(IPL)

NAMES <- c("RCB","CSK","MI","RR")
IPLSHAPLEY<-ShapleyValue(IPL,NAMES)
summary(IPLSHAPLEY)

##Imputatins using nucleoulus
IPLNUCLEOLUS <- Nucleolus(IPL,type="Cost")
summary(IPLNUCLEOLUS)


########
install.packages("GameTheory")
library(GameTheory)
COALITIONS2 <- c(31,21,20,28,
                 13,73,69,78,
                 54,58,81,48,
                 79,49,79,110,
                 128,98,130,96,
                 126,129,90,133,
                 128,183,142,186,
                 181,187,242)
IPL2<-DefineGame(5,COALITIONS2)
summary(IPL2)

NAMES <- c("SK Raina","KM Jadhav","RA Jadeja","KV Sharma", "MV Sharma")
IPLSHAPLEY<-ShapleyValue(IPL2,NAMES)
summary(IPLSHAPLEY)

##Imputatins using nucleoulus
IPLNUCLEOLUS1 <- Nucleolus(IPL2)
summary(IPLNUCLEOLUS1)

IPLNUCLEOLUS2 <- Nucleolus(IPL2,type="Cost")
summary(IPLNUCLEOLUS2)


