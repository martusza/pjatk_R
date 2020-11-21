# Zadanie 1
is_divisible <- function(num1, num2) {
  rest <- num1%%num2
  if (rest ==0) {
    sprintf('Liczba %s jest podzielna przez %s', num1, num2)
  } else {
    sprintf('Liczba %s nie jest podzielna przez %s', num1, num2)
  }
}

is_divisible(4,2)
is_divisible(4,3)

# Zadanie 2
average_velocity <- function(v1, v2) {
  v_avg <- 2*v1*v2/(v1+v2)
  return (v_avg)
}
v_avg <- average_velocity(120, 90)
sprintf('Srednia predkosc pociagu wynosila %s', v_avg)
# Srednia predkosc pociagu wynosila 102.86 km/h

# Zadanie 3
pearson_corr <- function(x, y) {
  x <- as.numeric(x)
  y <- as.numeric(y)
  xy <- x*y
  x2 <- x**2
  y2 <- y**2
  sum_xy <- sum(xy)
  sum_x2 <- sum(x2)
  sum_y2 <- sum(y2)
  n <- length(x)
  r <- (n*sum_xy-sum(x)*sum(y))/sqrt((n*sum_x2-sum(x)**2)*(n*sum_y2-sum(y)**2))
  return(r)
}

df <- read.csv("dane.csv", sep=';')
print(pearson_corr(df$wzrost, df$waga))
# Wynik 0.98 oznacza pozytywną, liniową zależność pomiędzy wagą i wzrostem 

# Zadanie 4
createDataFrame <- function(ile=1) {
  print('Wprowadz nazwy kolumn oddzielone przecinkiem')
  user_columns <- (readLines(con=stdin(),1))
  columns <- strsplit(user_columns, ",")
  ncol = length(unlist(columns))
  df = data.frame(matrix(NA, nrow = ile, ncol = ncol))
  
  for (i in 1:ile) {
    print('Wprowadz wiersz, wartosci oddziel przecinkami')
    user_row <- (readLines(con=stdin(),1))
    rowek <- unlist(strsplit(user_row, ','))
    df[i,] <- rowek
  }
  names(df) <- unlist(columns)
  return (df)
}

df <- createDataFrame(3)
print(df)
