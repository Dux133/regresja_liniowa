library(ggplot2)
# 1. Wczytanie danych i podstawowa eksploracja: ---------------------------

# Zaczynamy od załadowania wbudowanego zbioru danych mtcars.
# Wczytanie danych --------------------------------------------------------
data("mtcars")
# Podstawowe informacje o danych ------------------------------------------
str(mtcars)
summary(mtcars)
# Zbiór danych mtcars zawiera informacje o różnych modelach samochodów,
# takie jak zużycie paliwa (mpg), moc silnika (hp), waga (wt) i inne.

# 2. Podział danych na zbiór treningowy i testowy: ------------------------

# Aby sprawdzić działanie funkcji predict(), 
# podzielimy dane na dwa zbiory: treningowy i testowy.

set.seed(123)  # Ustawienie ziarna dla powtarzalności wyników
sample_index <- sample(1:nrow(mtcars), size = 0.7 * nrow(mtcars))
# sample_index to 70% indeksów z całego zbioru
train_data <- mtcars[sample_index,] # Zbiór treningowy czyli 70% indeksów
test_data <- mtcars[-sample_index,] # Zbiór testowy ("-" oznacza pozostałe
# indeksy czyli 30% pozostałych indeksów z całości )

# 3. Budowa modelu regresji liniowej:  ------------------------------------

# Użyjemy modelu regresji
# liniowej do przewidywania zużycia paliwa (mpg) na podstawie
# wagi pojazdu (wt) i mocy silnika (hp)

# Budowa modelu -----------------------------------------------------------
model <- lm( mpg ~ wt + hp, data = train_data)
# Podsumowanie modelu -----------------------------------------------------
summary(model)

# 4. Predykcja za pomocą funkcji predict(): -------------------------------

# Użyjemy modelu aby przewidzieć zużycie paliwa (mpg) dla danych testowych
predictions <- round(predict(model, newdata = test_data),1)
# Wyświetlenie pierwszych kilku wyników
head(predictions)

# 5. Teraz porónamy przewidywania przez model z rzeczywistymi wart --------
porownanie <- data.frame(
  real = test_data$mpg,
  prognoza = predictions)
print(porownanie)


# 6. Kolejny model --------------------------------------------------------

# Tworzę kolejny model dla zużycia paliwa (mpg) w zależności od ilości
# cylindrów oraz masy bazując na tych samych danych train oraz test

model_dwa <- lm( mpg ~ wt + cyl, data =  train_data )
summary(model_dwa)
predictions_dwa <- round(predict(model_dwa, newdata = test_data),1)
head(predictions_dwa)
# Model opcjonalny
model_trzy <- lm( mpg ~ wt + drat , data =  train_data )
summary(model_trzy)
predictions_trzy <- round(predict(model_trzy, newdata = test_data),1)
head(predictions_trzy)
# Zestawienie modeli
zestawienie <- data.frame(test_data$mpg,
                          predictions, 
                          predictions_dwa,
                          predictions_trzy)
zestawienie


# 7. Wizualizacja wyników -------------------------------------------------

ggplot(zestawienie, aes(x = test_data.mpg, y = predictions )) +
  geom_point(color = "blue")+
  geom_abline(intercept = 0, slope = 1, linetype = "dashed")+
  geom_point(aes(y = test_data.mpg), color = "red", size = 2, alpha = 0.7)+
  geom_point(aes(y = predictions_dwa), color = "green")



