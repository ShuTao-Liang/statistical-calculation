Gibbs_simulation = function(n = 20, alpha = 5, beta = 2, N = 2000){
  M = matrix(0, (N + 1), 2)
  M[1,2] = rbeta(1, alpha, beta)
  for(i in 1:N){
    M[i+1,1] = rbinom(1, n, M[i,2])
    M[i+1,2] = rbeta(1, (M[i+1,1] + alpha), (n - M[i+1,1] + beta))
  }
  M[2:(N+1),]
}


moving_average = function(x){
  n = length(x)
  avg = numeric(n)
  s = 0
  for(i in 1:n){
    s = s + x[i]
    avg[i] = s/i
  }
  avg
}

N = 10000
M = Gibbs_simulation(N = N)

avg = moving_average(M[,2])
plot(1:N, avg, pch = 19, col = 1, cex = .1)



Gibbs_simulation = function(n = 20, alpha = 5, beta = 2, N = 2000){
  x = numeric(N + 1)
  y = numeric(N + 1)
  M = matrix(0, (N + 1), 2)
  y[1] = rbeta(1, alpha, beta)
  for(i in 1:N){
    x[i+1] = rbinom(1, n, y[i])
    y[i+1] = rbeta(1, (x[i+1] + alpha), (n - x[i+1] + beta))
  }
  M[,1] = x[2:(N+1)]
  M[,2] = y[2:(N+1)]
  M
}
