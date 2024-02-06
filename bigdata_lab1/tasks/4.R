vec3 <- seq(from = 3, to = 27, by = 3)
vec3[2]
vec3[5]
vec3[7]
vec3[length(vec3) - 1]
vec3[c(1:(length(vec3) - 2), length(vec3))]
vec3[c(1:5, 7:length(vec3))]
vec3[100]
vec3[2:length(vec3) - 1]
Filter(function (item) item > 4 && item < 10,vec3)
Filter(function (item) item < 4 || item > 10,vec3)
print(vec3)