exponencial <- 4
base <- 10
n <- base ** exponencial
vueltas <- 100
#Buble Sort
bubble_sort <- function(x, ascending = TRUE) {
  n <- length(x)
  if (ascending) {
    for (i in 1:(n - 1)) {
      for (j in 1:(n - i)) {
        if (x[j + 1] < x[j]) {
          tmp <- x[j]
          x[j] <- x[j + 1]
          x[j + 1] <- tmp
        }
      }
    }
  }
  else {
    for (i in 1:(n - 1)) {
      for (j in 1:(n - i)) {
        if (x[j + 1] > x[j]) {
          tmp <- x[j]
          x[j] <- x[j + 1]
          x[j + 1] <- tmp
        }
      }
    }
  }
  x
}
#Selection Sort
selection_sort <- function(x, ascending = TRUE) {
  max <- length(x)
  if (ascending) {
    for (j in 1:(max - 1)) {
      m <- x[j]
      p <- j
      for (k in (j + 1):max) {
        if (x[k] < m) {
          m <- x[k]
          p <- k
        }
        ## end if
      }
      ## end for k
      x[p] <- x[j]
      x[j] <- m
    }
    ## end for j
  }
  ## end ascending if
  else {
    for (j in 1:(max - 1)) {
      m <- x[j]
      p <- j
      for (k in (j + 1):max) {
        if (x[k] > m) {
          m <- x[k]
          p <- k
        }
        ## end if
      }
      ## end for k
      x[p] <- x[j]
      x[j] <- m
    }
    ## end for j
  }
  ## end ascending else
  x
}
#Quick Sort
# Quick sort algorithm:
quickSort <- function(arr) {
  # Pick a number at random.
  mid <- sample(arr, 1)

  # Place-holders for left and right values.
  left <- c()
  right <- c()

  # Move all the smaller values to the left, bigger values to the right.
  lapply(arr[arr != mid], function(d) {
    if (d < mid) {
      left <<- c(left, d)
    }
    else {
      right <<- c(right, d)
    }
  })

  if (length(left) > 1) {
    left <- quickSort(left)
  }

  if (length(right) > 1) {
    right <- quickSort(right)
  }

  # Finally, return the sorted values.
  c(left, mid, right)
}
#Merge Sort
mmerge <- function(a, b) {
  r <- numeric(length(a) + length(b))
  ai <- 1;
  bi <- 1;
  j <- 1;
  for (j in 1:length(r)) {
    if ((ai <= length(a) && a[ai] < b[bi]) || bi > length(b)) {
      r[j] <- a[ai]
      ai <- ai + 1
    } else {
      r[j] <- b[bi]
      bi <- bi + 1
    }
  }
  r
}

mmergesort <- function(A) {
  if (length(A) > 1) {
    q <- ceiling(length(A) / 2)
    a <- mmergesort(A[1:q])
    b <- mmergesort(A[(q + 1):length(A)])
    mmerge(a, b)
  } else {
    A
  }
}
#Insertion Sort
insertion_sort <- function(A) {
  for (j in 2:length(A)) {
    key = A[j]
    i = j - 1
    while (i > 0 && A[i] > key) {
      A[(i + 1)] = A[i]
      i = i - 1
    }
    A[(i + 1)] = key
  }
  A
}

tB <- numeric() #Creando vec para guardar los tiempos
tS <- numeric()
tQ <- numeric()
tM <- numeric()
tI <- numeric()

for (i in 1:vueltas) {
  print(paste("Iteracion: ", i))
  x <- runif(n) #genera un vector de tamaño “n” (r= random, unif= uniforme)

  #métodos de ordenamiento
  #Midiendo los tiempos de ordenamiento

  t0 <- Sys.time() #Valor inicial (tiempo de referencia)

  y1 <- bubble_sort(x)
  t1 <- Sys.time() #obtiene el tiempo del sistema
  y2 <- insertion_sort(x)
  t2 <- Sys.time()
  y3 <- mmergesort(x)
  t3 <- Sys.time()
  y4 <- quickSort(x)
  t4 <- Sys.time()
  y5 <- selection_sort(x)
  t5 <- Sys.time()

  #Calculando los tiempos de ejecución y concatencando los resultados
  tB <- c(tB, as.numeric(t1 - t0))
  tS <- c(tS, as.numeric(t2 - t1))
  tQ <- c(tQ, as.numeric(t3 - t2))
  tM <- c(tM, as.numeric(t4 - t3))
  tI <- c(tI, as.numeric(t5 - t4))
}


#Lista de los tiempos
t <- list("Bubble" = tB,
  "Selection" = tS,
  "Quick" = tQ,
  "Merge" = tM,
  "Insertion" = tI)
# Recrear la t con la lista 
# ordenada por la media

boxplot(t, main = paste("Figura ", exponencial, "- 10^", exponencial),
          xlab = "Algoritmo",
          ylab = "Tiempo (s)",
          col = c("red", "blue", "green", "yellow", "purple")
          )

# Crear una tabla
# Algoritmo Media
# ejeAlg    0.2 \u00B1 0.1
table <- data.frame(
  Algoritmo = names(t),
  Media = sapply(t, function(x) paste(round(mean(x), 6), "±", round(sd(x), 6)))
)
table
