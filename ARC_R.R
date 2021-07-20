library(raster)
library(sf)
install.packages("LST") #land surface temperatures (LST) of Aarhus
library(LST)
r <- stack("dados/envi/LC_01/LC08_L1TP_218076_20210628_20210707_01_T1_B2.TIF",
           "dados/envi/LC_01/LC08_L1TP_218076_20210628_20210707_01_T1_B3.TIF",
           "dados/envi/LC_01/LC08_L1TP_218076_20210628_20210707_01_T1_B4.TIF",
           "dados/envi/LC_01/LC08_L1TP_218076_20210628_20210707_01_T1_B5.TIF",
           "dados/envi/LC_01/LC08_L1TP_218076_20210628_20210707_01_T1_B6.TIF",
           "dados/envi/LC_01/LC08_L1TP_218076_20210628_20210707_01_T1_B7.TIF")

area_estudo_arc <- rgdal::readOGR("Area_estudo_arc/ARC_AE.shp")

crs(r)

names(r) <- c("b2", "b3", "b4", "b5", "b6", "b7")

plot(r$b6)

a1 <- mask(r, area_estudo_arc)
a1

a2 <- crop(a1, area_estudo_arc)
a2

plot(a2$b7)
writeRaster(a2, filename = "dados_recortados_R", format="GTiff", overwrite=TRUE)

r4 <- raster("dados_processadors/tent1clip/tent2_prjt/")
r5 <- raster("dados_processadors/tent1clip/tent2_prjt/ten2_prj.tif")
r5 <- mask(r5, area_estudo_arc)
r5 <- crop(r5, area_estudo_arc)

#area de estudo .shp
AE <- read_sf("dados_processadors/banda_10_11_crop.tif")

# importando nossa mascara binaria
# fazendo um stack para testar
dados <- stack(r5, a2)
names(dados) <- c("resposta","b2", "b3", "b4", "b5", "b6", "b7")

plot(dados$resposta)
plot(dados$b2)

# Calculando indices
## NDVI 
dados$NDVI <- (dados$b5 - dados$b4)/(dados$b5 + dados$b4)
## NDWI
dados$NDWI <- (dados$b3 - dados$b5)/(dados$b3 + dados$b5)
#plotando o NDVi e NDWI
par(mfrow = c(1,2))
plot(dados$NDVI, col = gray(0:100/100), main = "NDVI")
plot(dados$NDWI, col = gray(0:100/100), main = "NDWI")

#E_Skokovic(red = red, NDVI = NDVI, band = band)
E_skovi <- E_Skokovic(red = dados$b4, NDVI = dados$NDVI, band = "band 10")
plot(E_skovi)

# Trabalhando os dados para realizar uma regressão
clean <- data.frame(na.omit(values(dados)))
head(clean)
m <- glm(resposta~NDVI+NDWI, data=clean, family = binomial)
m2 <- glm(resposta~NDVI, data = clean,family =  binomial(link="logit"))
summary(m2)

#stack

dossel <- stack("dados_processadors/DSM_DEM/canopy/CANOPY.tif")
dossel_2 <- mask(dossel,area_estudo_arc)
dossel_3 <- crop(dossel_2,area_estudo_arc)
dossel_4 <- resample(dossel_3,dados, method="ngb")
dados$canopy <- dossel_4
#stack a classificação 

kmeans <- raster("dados_processadors/kmeans/kmeans-processa.tif")
kmeans1 <- mask(kmeans, area_estudo_arc)
kmeans2 <- crop(kmeans1,area_estudo_arc)
dados$class <- kmeans2

#stack a média de b10 e b11 para avaliar o perfil de distribuição
b10_11 <- raster("dados_processadors/B10_celsius/b10_processado321.tif")
b10_11.1 <- mask(b10_11, area_estudo_arc)
b10_11.2 <- crop(b10_11.1, extent(dados))
b10_11 <- resample(b10_11.2,dados)
#atribuindo no stack b10 e b11 média
dados$temp_média <- b10_11
pairs(dados)
# O MODELO DIGNO 
clean <- data.frame(na.omit(values(dados)))
head(clean)
plot(dados$class)
modelo <- glm(resposta~NDVI+NDWI+class+canopy, data=clean, family = binomial)
modelo_1 <- glm(resposta~NDVI+NDWI+class+canopy, data = clean,family =  binomial(link="logit"))

#nosso ROI
rgdal::writeOGR(area_estudo_arc, dsn = "area_estudo2.shp",driver = "ESRI Shapefile", layer = "area_estudo")


#stack a area de predição
predic_area <- rgdal::readOGR("Area_estudo_arc/area_predict.shp")

# definindo o local do arquivo
b3 <- raster("predict/predict_B3.tif")
# extraindo a mascara
b3.1 <- mask(b3, 
             predic_area) 
# recortando 
b3.2 <- crop(b3.1, 
             predic_area)
# reamostrando
b3 <- resample(b3.2, 
               canopy_predict.2,
               method="ngb")

b4 <- raster("predict/predic_4.tif")
b4.1 <- mask(b4,predic_area)
b4.2 <- crop(b4.1, extent(b3.2))
b5 <- raster("predict/predic_B5.tif")
b5.1 <- mask(b5, predic_area)
b5.2 <- crop(b5.1, extent(b3.2))
canopy_predict <- raster("predict/predict-canopy.tif")
canopy_predict.1 <- mask(canopy_predict,predic_area)
canopy_predict.2 <- crop(canopy_predict.1,extent(b3.2))
kmens_predict <- raster("predict/predict_kmeans.tif")
kmens_predict.1 <- mask(kmens_predict, predic_area)
kmens_predict.2 <- crop(kmens_predict.1,extent(b3.2))
resposta_p <- raster("predict/predict_resposta.tif")
resposta_p.1 <- mask(resposta_p, predic_area)
resposta_p.2 <- crop(resposta_p.1, extent(b3.2))
temp_media <- raster("predict/predic_tempmed.tif")
temp_media1 <- mask(temp_media, predic_area)
temp_media2 <- crop(temp_media1,extent(b3.2))


#reamostrando para "stackar"
b3 <- resample(b3.2, canopy_predict.2, method="ngb")
b4 <- resample(b4.2, canopy_predict.2,method="ngb")
b5 <- resample(b5.2, canopy_predict.2,method="ngb")
canopy_predict <- resample(canopy_predict.2, canopy_predict.2,method="ngb")
kmens_predict <- resample(kmens_predict.2, canopy_predict.2,method="ngb")
resposta_p <- resample(resposta_p.2, canopy_predict.2, method= "ngb")     
par(mfrow = c(1,2))
plot(b3.2)
plot(kmens_predict.2)

#stack 
predict_raster <- stack(b3, b4,b5,canopy_predict,kmens_predict)
names(predict_raster) <- c("B3","B4","B5","canopy_p","Kmeans_p")

predict_raster$NDVI <- (predict_raster$B5 - predict_raster$B4)/(predict_raster$B5 + predict_raster$B4)
predict_raster$NDWI <- (predict_raster$B3 - predict_raster$B5)/(predict_raster$B3 + predict_raster$B5)
predict_raster$resposta <- resposta_p 


 pairs(dados[[8:9]])
 pairs(dados[[8:12]])
 pairs(dados[[c(8,12)]])
 pairs(dados[[c(9,12)]])
 pairs(dados[[c(10,12)]])
 pairs(dados[[c(11,12)]])
 pairs(dados[[c(10,12)]])
 pairs(dados[[c(11,12)]])
 pairs(dados[[3:5]])

## PREVENDO VALORES PARA A TEMPERATURA
predict_raster2 <- stack(predict_raster)

names(predict_raster2)<- c("B3","B4","B5","canopy_p","Kmeans_p","NDVI","NDWI","resposta_p")
data <- data.frame(na.omit(values(predict_raster2)))
head(data)

data1 <- data.frame(na.omit(values(predict_raster3)))

#modelos lineares generalizados
func2 <- glm(resposta_p_inversa~canopy_p+Kmeans_p+NDVI,
            data = data, 
            family =  binomial(link="logit"))
#modelos lineares generalizados
func <- glm(resposta_p~canopy_p+Kmeans_p+NDVI,
            data = data, 
            family =  binomial(link="logit"))
summary(predict_raster2$resposta_p_inversa)
exp(coef(logit))
exp(cbind(OR=coef(func), confint(func)))

allmean = data.frame(canopy_p=mean(data$canopy_p),
                     Kmeans_p=mean(data$Kmeans_p),
                     NDVI=mean(data$NDVI),
                     NDWI=mean(data$NDWI))

anova(func, test = "Chisq")
allmean$pred.prob = predict(func, newdata=allmean, type="response")


step(func, direction = 'both')

anova(func, test = "Chisq")

anova(func, func2, test="Chisq")

#redução da multicolinearidade
func2 <- update(func,~. -NDWI)
# aqui verificando o VIF observamos os valores dentre o previsto em literatura
faraway::vif(func2)
exp(cbind(OR = coef(func2), confint(func2)))

#aplicando a predição do modelo
temperatura_final <- predict(predict_raster2,
                             model = func2,
                             type = "response")
#plotando o mapa do resultado
prob_inv <- 1- temperatura_final
plot(prob_inv,
     col = gray(0:100/100),
     main = "Predição do Modelo")

summary(temperatura_final)
temperatura_final <- prob_inv
mapa <- data.frame(na.omit(values(temperatura_final)))
names(mapa) <- c("probit_")
head(mapa)

#tabela de contingencia
xtabs(~resposta_p + Kmeans_p, data = data)

diferenca <- 2 - predict_raster2$resposta_p

#reclassificando a partir de uma matriz
mat1 <- c(0, 0.6, 0, 0.6, 1, 1)
rclmat1 <- matrix(mat, ncol=3, byrow=TRUE)
mapa1 <- reclassify(temperatura_final, rclmat)

mat2 <- c(0, 0.75, 0, 0.75, 1, 1)
rclmat2 <- matrix(mat2, ncol=3, byrow=TRUE)
mapa2 <- reclassify(temperatura_final, rclmat2)

mat3 <- c(0, 0.85, 0, 0.85, 1, 1)
rclmat3 <- matrix(mat3, ncol=3, byrow=TRUE)
mapa3 <- reclassify(temperatura_final, rclmat3)

mat4 <- c(0, 0.95, 0, 0.95, 1, 1)
rclmat4 <- matrix(mat4, ncol=3, byrow=TRUE)
mapa4 <- reclassify(temperatura_final, rclmat4)

par(mfrow = c(2,2))
plot(mapa1, main="A - Limiar 0.60", legend=F)
plot(mapa2,main="B - Limiar 0.75", legend=F)
plot(mapa3,main="C - Limiar 0.85", legend=F)
plot(mapa4,main="D - Limiar 0.95", legend=F)

par(mfrow = c(1,1))

plot(mapa4,main="D - Limiar ", legend=F)


diff4 <- overlay(mapa4,
                 predict_raster2$resposta_p,
                 fun=function(a,b) return(a==b))
plot(diff,
     col=c('#FFE4E1','#228B22'),
     main= "Mascara de predições corretas e incorretas para o limiar de 0.6",
     legend=FALSE,
     axes=FALSE)
legend("left", legend=c("corretp", "incorreto"),
       col=c("#228B22", "#FFE4E1"), pch = 15, cex=0.45)

plot(predict_raster2$resposta_p)

mapa3 <- mask(mapa3,predict_raster2$resposta_p)
mapa3 <- crop(mapa3,extent(predict_raster2$resposta_p))
mapa3 <- resample(mapa3,predict_raster2$resposta_p, method="ngb")
diff <- brick(predict_raster2$resposta_p, mapa3)

par(mfrow = c(2,2))
hist(predict_raster$B3, main = "GREEN")
hist(predict_raster$B4, main = "RED")
hist(predict_raster$B5, main = "NIR")
hist(predict_raster2$NDVI, main = "NDVI")

pairs(predict_raster2[[c(1:4)]])

hist(predict_raster2$canopy_p, main ="Altura de Dossel")
hist(predict_raster2$NDVI, main = "NDVI")
hist(predict_raster2$Kmeans_p, main = "Classificação K-Médias")
hist(predict_raster2$resposta_p, main = "Mascara Binária")

pairs(predict_raster2[[c(3:8)]])
