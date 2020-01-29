install.packages("GA")
library("GA") # GA kütüphanesi çaðýrdýk.

dataset<-data.frame(item=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","R","S","T",
                           "U","V","Y","Z","Q","W"),
                    kar=c(10,20,15,2,30,10,30,32,13,46,11,14,12,45,33,12,
                                     22,16,24,22,41,22,33,21,12),
                    agirlik=c(2,5,10,3,7,5,2,4,2,4,5,2,1,13,6,7,4,2,12,1,14,12,7,6,8))

#kar,aðýrlýk ve ürünlerden oluþan veri seti dataframe'ini oluþturduk.
agirlik_limit<-58  #aðýrlýk kýsýtýmýz 58 ton olduðu için aðýrlýk limitini belirledik.

#PROBLEM MAKSÝMÝZASYON PROBLEMÝDÝR.
#FITNESS FOKSÝYONUNU OLUÞTURALIM.
#FONKSÝYONDAKÝ X VEKTÖRÜ 1 VE 0'LARDAN OLUÞAN BINARY YAPIDADIR.25 DEÐERDEN OLUÞMAKTADIR.
#BAÞLANGIÇ POPÜLASYONU PAKET TARAFINDAN RASTGELE OLUÞTURULMAKTADIR.POÜLASYON BÜYÜKLÜÐÜ 200'DÜR.

fitness<-function(x){
  simdiki_cozum_kar<-x%*%dataset$kar #KÂRI ELDE EDELÝM.
  simdiki_cozum_agirlik<-x %*% dataset$agirlik #AÐIRLIÐI ELDE EDELÝM.
  if (simdiki_cozum_agirlik>agirlik_limit) #AÐIRLIK ÝLE AÐIRLIK LÝMÝTÝNÝ KIYASLAYALIM.
    return(0) else return(simdiki_cozum_kar)
  
}
gaControl("binary"=list(selection="gabin_tourSelection",crossover="gabin_spCrossover"))
#BURADA DEFAULT OLAN PAKET ÝÇERÝSÝNDEKÝ PARAMETRELERÝ YENÝDEN DÜZENLEDÝK.SELECTION OLARAK 
#TOURNAMENT SELECTION,CROSSOVER OLARAK SINGLE POINT CROSSOVER KULLANILMIÞTIR.
#ÞÝMDÝ GA ÝLE PARAMETRE DEÐERLERÝNÝ BELÝRLEYELÝM.
#KROMOZOM ÞEKLÝ BINARY OLARAK BELÝRLENMÝÞTÝR.
GA<-ga(type = "binary",
       
       nBits = 25, #25 GENDEN OLUÞMAKTADIR.
       popSize = 200, #POPÜLASYON BÜYÜKLÜÐÜ
       maxiter = 100, #ÝTERASYON SAYISI
       pmutation = 0.01, #MUTASYON OLASILIÐI
       elitism = 0, #ELITISIM UYGULANMADI BU YÜZDEN 0
       fitness = fitness
       )
summary(GA) # SUMMARY ÝLE ÖZET BÝR ÇÖZÜM TABLOSU ELDE ETTÝK
print(GA@solution) #ÇÖZÜM VEKTÖRÜNE BAKTIK
plot(GA) #GRAFÝK ÇÝZDÝK.
