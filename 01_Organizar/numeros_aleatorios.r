n <- 10 ** 4
vueltas <- 100
# Bubble Sort
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

#Merge sort
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

#Insertion sort
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

#Quicksort
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

#Selection sort
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

bubbleTime <- numeric()
selectionTime <- numeric()
quickSortTime <- numeric()
mergeTime <- numeric()
insertionTime <- numeric()

for (i in 1:vueltas) {
  print(paste("Iteracion: ", i))
  x <- runif(n)

  #Bubble Sort
  bubbleStart <- Sys.time()
  bubble_res <- bubble_sort(x)
  bubbleEnd <- Sys.time()

  bubbleTime <- c(bubbleTime, as.numeric(bubbleEnd - bubbleStart))

  #Selection Sort
  selectionStart <- Sys.time()
  selection_res <- selection_sort(x)
  selectionEnd <- Sys.time()

  selectionTime <- c(selectionTime, as.numeric(selectionEnd - selectionStart))

  #Quick Sort
  quickSortStart <- Sys.time()
  quickSort_res <- quickSort(x)
  quickSortEnd <- Sys.time()

  quickSortTime <- c(quickSortTime, as.numeric(quickSortEnd - quickSortStart))

  #Merge sort
  mergeStart <- Sys.time()
  mergesort_res <- mmergesort(x)
  mergeEnd <- Sys.time()

  mergeTime <- c(mergeTime, as.numeric(mergeEnd - mergeStart))

  #Insertion Sort
  insertionStart <- Sys.time()
  insertion_res <- insertion_sort(x)
  insertionEnd <- Sys.time()

  insertionTime <- c(insertionTime, as.numeric(insertionEnd - insertionStart))
}

t <- list("Bubble Sort" = bubbleTime,
    "Selection Sort" = selectionTime,
    "Quick Sort" = quickSortTime,
    "Merge sort" = mergeTime,
    "Insertion sort" = insertionTime)
# Recrear la t con la lista 
# ordenada por la media
sapply(t, mean)

boxplot(t, main = "Tiempos de ejecucion de algoritmos",
          xlab = "Algoritmo",
          ylab = "Tiempo (s)",
          names = c("Bubble", "Selection", "Quick", "Merge", "Insertion"),
          col = c("red", "blue", "green", "yellow", "purple")
          )

# Crear una tabla
# Algoritmo Media
# ejeAlg    0.2 \u00B1 0.1
table <- data.frame(
  Algoritmo = names(t),
  Media = sapply(t, function(x) paste(mean(x), "±", sd(x)))
)
table
