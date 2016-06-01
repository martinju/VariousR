### Going through the following tutorial on memory usage:
### http://adv-r.had.co.nz/memory.html


install.packages("ggplot2")
install.packages("pryr")
install.packages("devtools")
devtools::install_github("hadley/lineprof")


### Object size


library(pryr)
object_size(1:10)
#> 88 B
object_size(mean)
#> 832 B
object_size(mtcars)
#> 6.74 kB

sizes <- sapply(0:50, function(n) object_size(seq_len(n)))
plot(0:50, sizes, xlab = "Length", ylab = "Size (bytes)", 
     type = "s")

object_size(numeric())
#> 40 B
object_size(logical())
#> 40 B
object_size(raw())
#> 40 B
object_size(list())
#> 40 B

plot(0:50, sizes - 40, xlab = "Length", 
     ylab = "Bytes excluding overhead", type = "n")
abline(h = 0, col = "grey80")
abline(h = c(8, 16, 32, 48, 64, 128), col = "grey80")
abline(a = 0, b = 4, col = "grey90", lwd = 4)
lines(sizes - 40, type = "s")

x <- 1:1e6
object_size(x)
#> 4 MB

y <- list(x, x, x)
object_size(y)
#> 4 MB

object_size(x, y)
#> 4 MB

x1 <- 1:1e6
y1 <- list(1:1e6, 1:1e6, 1:1e6)

object_size(x1)
#> 4 MB
object_size(y1)
#> 12 MB
object_size(x1, y1)
#> 16 MB
object_size(x1) + object_size(y1) == object_size(x1, y1)
#> [1] TRUE

object_size("banana")
#> 96 B
object_size(rep("banana", 10))
#> 216 B


### Exercises:

# 3
vec <- lapply(0:50, function(i) c("ba", rep("na", i)))
str <- lapply(vec, paste0, collapse = "")


## Memory usage and garbage collection

library(pryr)
mem_used()

# Need about 4 mb to store 1 million integers
mem_change(x <- 1:1e6)
#> 4.01 MB
# We get that memory back when we delete it
mem_change(rm(x))
#> -4 MB


# Create a big object
mem_change(x <- 1:1e6)
#> 4 MB
# Also point to 1:1e6 from y
mem_change(y <- x)
#> 1.29 kB
# Remove x, no memory freed because y is still pointing to it
mem_change(rm(x))
#> 1.18 kB
# Now nothing points to it and the memory can be freed
mem_change(rm(y))
#> -4 MB



f1 <- function() {
  x <- 1:1e6
  x+10
}
mem_change(x <- f1())
#> 1.19 kB
object_size(x)
#> 48 B

f2 <- function() {
  x <- 1:1e6
  a ~ b
}
mem_change(y <- f2())
#> 4 MB
object_size(y)
#> 4 MB

f3 <- function() {
  x <- 1:1e6
  function() 10
}
mem_change(z <- f3())
#> 4 MB
object_size(z)
#> 4.01 MB

#### Bottom line: Creating variables within functions is fine --
  ## Creating functions and other "environments" within functions
  ## takes up a lot of space


### Memory profiling with lineprof ###


read_delim <- function(file, header = TRUE, sep = ",") {
  # Determine number of fields by reading first line
  first <- scan(file, what = character(1), nlines = 1,
                sep = sep, quiet = TRUE)
  p <- length(first)
  
  # Load all fields as character vectors
  all <- scan(file, what = as.list(rep("character", p)),
              sep = sep, skip = if (header) 1 else 0, quiet = TRUE)
  
  # Convert from strings to appropriate types (never to factors)
  all[] <- lapply(all, type.convert, as.is = TRUE)
  
  # Set column names
  if (header) {
    names(all) <- first
  } else {
    names(all) <- paste0("V", seq_along(all))
  }
  
  # Convert list into data frame
  as.data.frame(all)
}

library(ggplot2)
write.csv(diamonds, "diamonds.csv", row.names = FALSE)

library(lineprof)

source("read_delim_code.R")
prof <- lineprof(read_delim("diamonds.csv"))
shine(prof)



### Loops:

x <- data.frame(matrix(runif(100 * 1e4), ncol = 100))
medians <- vapply(x, median, numeric(1))

for(i in seq_along(medians)) {
  x[, i] <- x[, i] - medians[i]
}


