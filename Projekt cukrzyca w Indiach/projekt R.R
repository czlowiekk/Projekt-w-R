# Wprowadzanie danych
library(readr)
dane <-read_csv("Diabetes.csv") #Wynik=Klasa: 0 - nie posiada cukrzycy, 1 - posiada cukrzyce
# Wprowadzanie potrzebnych pakietów
library(HDclassif)
library(cluster)
library(randomForest)
library(moments)
library(outliers)
library(nortest)
library(silvermantest)
library(heatmaply)


# Podstawowe statystyki opisowe
summary(dane)

# Liczenie odchylenia standardowego
apply(dane[, -ncol(dane)], 2, sd)

# Skośność
apply(dane[, -ncol(dane)], 2, skewness)

# Test Shapiro-Wilka na normalność
shapiro.test(dane$Ciaze)
shapiro.test(dane$Glukoza)
shapiro.test(dane$"Cisnienie krwi")
shapiro.test(dane$"Grubosc skory")
shapiro.test(dane$Insulina)
shapiro.test(dane$BMI)
shapiro.test(dane$"Funkcja rodowodu cukrzycy")
shapiro.test(dane$Wiek)

# Test Silvermana na wielomodalność

silverman.test(dane$Ciaze,k=1)
silverman.test(dane$Glukoza,k=1)
silverman.test(dane$"Cisnienie krwi",k=1)
silverman.test(dane$"Grubosc skory",k=1)
silverman.test(dane$Insulina,k=1)
silverman.test(dane$BMI,k=1)
silverman.test(dane$"Funkcja rodowodu cukrzycy",k=1)
silverman.test(dane$Wiek,k=1)

# Liczba modów
nr.modes(dane$Ciaze)
nr.modes(hist(dane$Ciaze)$counts)
nr.modes(dane$Glukoza)
nr.modes(hist(dane$Glukoza)$counts)
nr.modes(dane$"Cisnienie krwi")
nr.modes(hist(dane$"Cisnienie krwi")$counts)
nr.modes(dane$"Grubosc skory")
nr.modes(hist(dane$"Grubosc skory")$counts)
nr.modes(dane$Insulina)
nr.modes(hist(dane$Insulina)$counts)
nr.modes(dane$BMI)
nr.modes(hist(dane$BMI)$counts)
nr.modes(dane$"Funkcja rodowodu cukrzycy")
nr.modes(hist(dane$"Funkcja rodowodu cukrzycy")$counts)
nr.modes(dane$Wiek)
nr.modes(hist(dane$Wiek)$counts)

# Test Grubbsa na obserwacje odstające

for (i in 1:8) {
  print(grubbs.test(data.frame(dane)[,i]))
}

library(heatmaply)

# Korelacja i wizualizacja heatmapy
heatmaply(cor(dane[, -ncol(dane)], method = 'pearson'),main = "heatmapa dla Pearsona")
heatmaply_cor(cor(dane[, -ncol(dane)], method = 'pearson'), main = "heatmapa dla Pearsona")


#macierz korelacji Kendall i wizualizacja heatmapy
heatmaply(cor(dane[, -ncol(dane)], method = 'kendall'),main = "heatmapa dla Kendall")
heatmaply_cor(cor(dane[, -ncol(dane)], method = 'kendall'),main = "heatmapa dla Kendall")


#Analiza składowych głównych (PCA)
pca.dane <- prcomp(dane[, 1:8])
summary(pca.dane)

# Pierwsza składowa główna
pca.dane$rotation[, 1]


# Wizualizacja PCA
df_pca <- data.frame(x = pca.dane$x[, 1], y = pca.dane$x[, 2], z = pca.dane$x[, 3], type = dane$Wynik)
plot_ly(df_pca, x = ~x, y = ~y, z = ~z, color = ~type, mode = 'scatter3d')

df.pca <- data.frame(x = pca.dane$x[, 1], y = pca.dane$x[, 2], z = pca.dane$x[, 3], type =as.factor(dane$Wynik))
plot_ly(df.pca, x = ~x, y = ~y, z = ~z, color = ~type, type = 'scatter3d')

# Klasteryzacja k-means
km.dane <- kmeans(dane[, 1:8], centers = 3)
table(km.dane$cluster, dane$Wynik)


# Wizualizacja k-means w PCA
df_km <- data.frame(x = pca.dane$x[, 1], y = pca.dane$x[, 2], z = pca.dane$x[, 3], type = as.factor(km.dane$cluster))
plot_ly(df_km, x = ~x, y = ~y, z = ~z, color = ~type, type = 'scatter3d')

# Wartość wewnątrzgrupowej sumy kwadratów (wss) dla różnej liczby klastrów
wss <- NA
for (i in 2:10) {
  wss <- c(wss, kmeans(dane[, 1:8], centers = i)$tot.withinss)
}
plot(1:length(wss), wss, type = "b", xlab = "Liczba klastrów", ylab = "WSS", main = "Wykres Elbowa dla analizy klastrów w cukrzycy")

# Analiza aglomeracyjna
plot(agnes(dist(dane[, 1:8], method = 'minkowski', p = 1)))
plot(agnes(dist(dane[, 1:8], method = 'minkowski', p = 1), diss=TRUE))
plot(diana(dane[, 1:8]))

# Random Forest - walidacja krzyżowa
rf_cv <- rfcv(dane[, 1:8], as.factor(dane$Wynik))
rf_cv$error.cv

# Wagi zmiennych
rf_imp <- randomForest(dane[, 1:8], as.factor(dane$Wynik), importance = TRUE)
varImpPlot(rf_imp)

# Model Random Forest
rf_model <- randomForest(dane[, 1:8], as.factor(dane$Wynik))
rf_model$confusion