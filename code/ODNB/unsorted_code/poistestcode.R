x = rnorm(100)
y = rep(0, times = 100)
for(i in 1:100) {
y[i] = sample(1:100, size = 1, prob = dnorm(1:100, mean = 10*x[i]+50, sd = 20))
}

rnd.test = glm(y~x, family = "poisson")




pred = rnd.test$coef %*% t(as.matrix(cbind(1, x)))
pred = exp(pred)

true.vals = y
wn = which(y > 0)

true.times.log = true.vals
true.times.log[wn] = true.vals[wn] * (log(true.vals[wn]) - log(pred[wn]))
2 * sum(true.times.log + pred - true.vals)
