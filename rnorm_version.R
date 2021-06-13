#############################################



#############################################

rnorm_version1 = function(n, mu = 0, sigma = 1){
  x = numeric(n)        #生成n维空向量存储随机数
  #若需要到y，可以建立y = numeric(n)
  part1 = sqrt(-2*log(runif(n)))
  part2 = 2*pi*runif(n)
  x = mu + sigma*part1*cos(part2)     #通过变换将标准正态随机数变成服从参数为(mu, sigma)的正态分布
  #y = mu + sigma*part1*sin(part2)
}


############################################

rnorm_version2 = function(n, mu = 0, sigma = 1){
  x = numeric(n)        #生成n维空向量存储随机数
  for(i in 1:n){        #通过for循环生成n个随机数
    repeat{             #使用repeat循环在生成的均匀分布小于e^(-(x - 1)^2/2))时跳出
      U1 = runif(1)
      X = -log(U1)
      Y = runif(1)
      if(Y <= exp(-(X - 1)^2/2)) break
    }
    x[i] = X
  }
  for(j in 1:n){        #在通过一次for循环判断正负
    U2 = runif(1)
      if(U2 < 0.5){     #以0.5为阈值
        x[j] = x[j]
      }
      else{
        x[j] = -x[j]
      }
  }
  x = mu + sigma*x      #通过变换将标准正态随机数变成服从参数为(mu, sigma)的正态分布
}


##########################################

rnorm_version3 = function(n, mu = 0, sigma = 1){
  x = numeric(n)        #生成n维空向量存储随机数
  #若需要到y，可以建立y = numeric(n),或者x = numeric(2n)
  for(i in 1:n){        
    repeat{             #使用repeat循环在生成的均匀分布小于S < 1时跳出
      U1 = runif(1)
      U2 = runif(1)
      V1 = 2*U1 - 1
      V2 = 2*U2 - 1
      S = V1^2+V2^2
      if(S < 1) break
    }
    part1 = sqrt(-2*log(S)/S)
    x[i] = part1*V1
    #y[i] = part1*V2,或者x[i+n] = part1*V2
  }
  x = mu + sigma*x      #通过变换将标准正态随机数变成服从参数为(mu, sigma)的正态分布
}
