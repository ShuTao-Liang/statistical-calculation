##############################################

rpois_version1 = function(n, lambda = 1){
  x = numeric(n)    #生成n维空向量存储随机数
  for(i in 1:n){    #通过for循环生成n个随机数
    U = runif(1)    #生成1个0,1间的均匀分布随机数
    p = exp(-lambda)    #计算e^(-lambda)
    F = p
    k = 0
    while(U > F){     #迭代累加直到U>F
      p = p*lambda/(k+1)
      F = F + p    
      k = k+1
    }
    x[i] = k
  }
  
  x
}

##############################################


rpois_version2 = function(n, lambda = 1){
  x = numeric(n)    #生成n维空向量存储随机数
  for(i in 1:n){    #通过for循环生成n个随机数
    U = runif(1)    #生成1个0,1间的均匀分布随机数
    K = floor(lambda)    #对lambda向下取整
    p = exp(-lambda)    #计算e^(-lambda)
    F = p
    Fvs = c(0,F)        #生成向量Fvs存储k从0到K的分布函数F的值,目的,①是后续判断需要用到0,②是
                        #用存储空间换取计算时间，因为在后续判断中可能会向前计算(行48—行50)
    for(k in 0:K){      #迭代累加到K
      p = p*lambda/(k + 1)
      F = F + p         
      Fvs = c(Fvs, F)   #将新的F存入Fvs
    }
    
    while(TRUE){        #一直循环判断,直到满足落入Fk和Fk+1之间
      
      if(U > F){        #向后传播
        p = p*lambda/(K + 2)#K+2=K+1+1
        F = F + p
        Fvs = c(Fvs, F) #将新的F存入Fvs
        K = K + 1
      }
      
      if(U <= Fvs[K + 2]){#向前传播
        K = K - 1
      }
      
      if((U <= F)&(U > Fvs[K + 2])){#推出循环的条件，在报告中会详实说明
        K = K + 1
        break
        
      }
    }
    x[i] = K
  }
  x
}

##############################################

exp_version1 = function(n, lambda = 1){       #生成指数分布随机数
  -log(runif(n))/lambda
}

##############################################

rpois_version3 = function(n, lambda = 1){
  x = numeric(n)        #生成n维空向量存储随机数
  y = exp(-lambda)      #计算e^(-lambda)
  for(i in 1:n){        #通过for循环生成n个随机数
    p = 1
    k = 0
    while(p >= y){
      p = p*runif(1)
      k = k+1
    }
    x[i] = k - 1
  }
  x
}
  
  
  
  