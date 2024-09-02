library(GA)

f <- function(x) {
  return(x^3)
}

ga_min <- ga(type = "real-valued",
             fitness = function(x) -f(x),
             lower = -10, upper = 10,
             popSize = 50, maxiter = 100, run = 50)

ga_max <- ga(type = "real-valued",
             fitness = f,
             lower = -10, upper = 10,
             popSize = 50, maxiter = 100, run = 50)

summary(ga_min)
summary(ga_max)

plot(ga_min)
plot(ga_max)