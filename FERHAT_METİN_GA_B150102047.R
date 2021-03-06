install.packages("GA")
library("GA") # GA k�t�phanesi �a��rd�k.

dataset<-data.frame(item=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","R","S","T",
                           "U","V","Y","Z","Q","W"),
                    kar=c(10,20,15,2,30,10,30,32,13,46,11,14,12,45,33,12,
                                     22,16,24,22,41,22,33,21,12),
                    agirlik=c(2,5,10,3,7,5,2,4,2,4,5,2,1,13,6,7,4,2,12,1,14,12,7,6,8))

#kar,a��rl�k ve �r�nlerden olu�an veri seti dataframe'ini olu�turduk.
agirlik_limit<-58  #a��rl�k k�s�t�m�z 58 ton oldu�u i�in a��rl�k limitini belirledik.

#PROBLEM MAKS�M�ZASYON PROBLEM�D�R.
#FITNESS FOKS�YONUNU OLU�TURALIM.
#FONKS�YONDAK� X VEKT�R� 1 VE 0'LARDAN OLU�AN BINARY YAPIDADIR.25 DE�ERDEN OLU�MAKTADIR.
#BA�LANGI� POP�LASYONU PAKET TARAFINDAN RASTGELE OLU�TURULMAKTADIR.PO�LASYON B�Y�KL��� 200'D�R.

fitness<-function(x){
  simdiki_cozum_kar<-x%*%dataset$kar #K�RI ELDE EDEL�M.
  simdiki_cozum_agirlik<-x %*% dataset$agirlik #A�IRLI�I ELDE EDEL�M.
  if (simdiki_cozum_agirlik>agirlik_limit) #A�IRLIK �LE A�IRLIK L�M�T�N� KIYASLAYALIM.
    return(0) else return(simdiki_cozum_kar)
  
}
gaControl("binary"=list(selection="gabin_tourSelection",crossover="gabin_spCrossover"))
#BURADA DEFAULT OLAN PAKET ��ER�S�NDEK� PARAMETRELER� YEN�DEN D�ZENLED�K.SELECTION OLARAK 
#TOURNAMENT SELECTION,CROSSOVER OLARAK SINGLE POINT CROSSOVER KULLANILMI�TIR.
#��MD� GA �LE PARAMETRE DE�ERLER�N� BEL�RLEYEL�M.
#KROMOZOM �EKL� BINARY OLARAK BEL�RLENM��T�R.
GA<-ga(type = "binary",
       
       nBits = 25, #25 GENDEN OLU�MAKTADIR.
       popSize = 200, #POP�LASYON B�Y�KL���
       maxiter = 100, #�TERASYON SAYISI
       pmutation = 0.01, #MUTASYON OLASILI�I
       elitism = 0, #ELITISIM UYGULANMADI BU Y�ZDEN 0
       fitness = fitness
       )
summary(GA) # SUMMARY �LE �ZET B�R ��Z�M TABLOSU ELDE ETT�K
print(GA@solution) #��Z�M VEKT�R�NE BAKTIK
plot(GA) #GRAF�K ��ZD�K.
