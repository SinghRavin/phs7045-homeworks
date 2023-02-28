
# ------------------- Question 1 -------------------- #

fun1 <- function(n = 100, k = 4, lambda = 4) {
  x <- NULL
  
  for (i in 1:n)
    x <- rbind(x, rpois(k, lambda))
  
  return(x)
}

fun1alt <- function(n = 100, k = 4, lambda = 4) {
  return(matrix(rpois(n*k,4),nrow=n,ncol=4))
}

# Benchmarking
bench::mark(
  fun1(),
  fun1alt(), relative = TRUE, check = FALSE
)

# ------------------- Question 2 -------------------- #

# Total row sums
fun1 <- function(mat) {
  n <- nrow(mat)
  ans <- double(n) 
  for (i in 1:n) {
    ans[i] <- sum(mat[i, ])
  }
  ans
}

fun1alt <- function(mat) {
  return(rowSums(mat))
}

# Cumulative sum by row
fun2 <- function(mat) {
  n <- nrow(mat)
  k <- ncol(mat)
  ans <- mat
  for (i in 1:n) {
    for (j in 2:k) {
      ans[i,j] <- mat[i, j] + ans[i, j - 1]
    }
  }
  ans
}

fun2alt <- function(mat) {
  t(apply(mat, 1, cumsum))
}

# Use the data with this code
set.seed(2315)
dat <- matrix(rnorm(200 * 100), nrow = 200)

# Test for the first
bench::mark(
  fun1(dat),
  fun1alt(dat), relative = TRUE
)

# Test for the second
bench::mark(
  fun2(dat),
  fun2alt(dat), relative = TRUE
)

# ------------------- Question 3 -------------------- #

# Data Generating Process (10 x 10,000 matrix)
set.seed(1234)
x <- matrix(rnorm(1e4), nrow=10)

# Find each column's max value
fun2 <- function(x) {
  apply(x, 2, max)
}

fun2alt <- function(x) {
  vec <- max.col(t(x))
  return_vec <- NULL
  for (i in 1:dim(x)[2]){
    return_vec[i]<-x[vec[i],i]
  }
  return(return_vec)
}

# Benchmarking
bench::mark(
  fun2(x),
  fun2alt(x), relative = TRUE
)


