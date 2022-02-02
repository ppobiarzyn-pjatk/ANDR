#1. Napisz funkcję sprawdzająca czy 1 liczba jest podzielna przez druga użyj - %%
divisibleBy<-function (x, y){
  if(x %% y != 0)
    cat(x, "is not divisible by", y)

  else
    cat(x, "is divisible by", y)
}

divisibleBy(5,5)
divisibleBy(10,3)

#2. Pociąg z Lublina do Warszawy przejechał połowę drogi ze średnią prędkością 120 km/h.
# Drugą połowę przejechał ze średnią prędkością 90 km/h.
# Jaka była średnia prędkość pociągu.

v1 = 120
v2 = 90

v_mean = (2 * v1 * v2) / (v1 + v2)

cat("Średnia prędkość pociagu to", v_mean)

#3. Utwórz funkcję obliczającą współczynnik korelacji r Pearsona dla 2 wektorów o tej samej długości.
# Wczytaj dane plik dane.csv i oblicz współczynnik dla wagi i wzrostu. W komentarzu napisz co oznacza wynik.

correlation<-function (x, y){
  print(cor(x, y))
}

df<-read.csv('dane.csv', TRUE, ";" )

correlation(df[,1], df[,2])

#####
# Współczynnik r bardzo wysoki, więc wysoka korelacja między wagą i wzrostem.
# Wynika z tego, że im większa waga tym większy wzrost i na odwrót.
#####



#4. Napisz funkcję zwracającą ramke danych z danych podanych przez użytkownika
# stworzDataFrame <- function(ile=1)
# W pierwszym wierszu użytkownik podaje nazwy kolumn.
# w kolejnych wierszach zawartość wierszy ramki danych ( tyle wierszy ile podaliśmy w argumencie ile.
# ile=1 oznacza, że gdy użytkownik nie poda żadnej wartości jako parametr, domyślna wartością będzie 1)

stworzDataFrame<-function(ile=1){
  colName<-scan("", what="list", nlines=1)
  df<-data.frame()
  for(i in 1:ile){
    df <- rbind(df, scan("", n=length(colName)))
  }
  colnames(df)<-colName
  View(df)
}
stworzDataFrame(4)



#5. Napisz funkcję , która pobiera sciezkeKatalogu, nazweKolumny, jakaFunkcje, DlaIluPlikow i liczy:
#mean, median,min,max w zależności od podanej nazwy funkcji w argumencie,
# z katologu który podaliśmy i z tylu plików ilu podaliśmy dla wybranej nazwy kolumny.


liczZplikow <- function(sciezka, nazwaKolumny, jakaFunkcja="mean", DlaIluPlikow=1){

  pliki <- list.files(sciezka, pattern="*.csv")
  setwd(sciezka)
  my_data <- list()
  columnName <- paste("X", nazwaKolumny, sep="")
  for (i in 1:DlaIluPlikow){
    my_data <-rbind(my_data, na.omit(read.csv(pliki[i])[columnName]))
  }

  if (jakaFunkcja == "mean") mean(my_data[ , 1])
  else if (jakaFunkcja == "max" ) max(my_data[ , 1])
  else if (jakaFunkcja == "min") min(my_data[ , 1])
  else if (jakaFunkcja == "median") median(my_data[ , 1])

}

liczZplikow('smogKrakow/', '215_pm10', jakaFunkcja = 'mean', DlaIluPlikow = 6)
