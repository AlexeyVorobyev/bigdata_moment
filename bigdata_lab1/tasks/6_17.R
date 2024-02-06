vec <- runif(n = 80, min = -40, max = 100)
vec

vec_more_than_mean <- Filter(function(item) item > mean(vec), vec)
vec_more_than_mean

matr <- matrix(vec, nrow = 8, ncol = 10)
matr

even <- matr[, seq(from = 2, to = 10, by = 2)]
even
odd <- matr[, seq(from = 1, to = 10, by = 2)]
odd