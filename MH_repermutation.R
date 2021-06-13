# x: 初始排列
# row: 需要输出的抽样次数
# n: 排列的元素个数
# bound: 即sigma(i*x_i)的界限
repermutation = function(x, row, n, bound){
  flag = FALSE         #设置标签flag用于后续的判断出错位置和原因
  sum1 = sum(1:n*x)    # 要判断的条件的当前值
  
  #计算当前近邻个数
  neighb = function(x){
    nn = 1
    sum0 = sum(1:n*x)
    for(i in 1:(n-1)){
      for(j in (i+1):n){
        sum0_1 = sum0 - i*x[i] - j*x[j] + i*x[j] + j*x[i]
        if(sum0_1 >= bound){
          nn = nn + 1
        }
      }
    }
    nn
  }
  
  #将结果保存至row行,n列的new_matrix矩阵中
  new_matrix = matrix(0, row, n)
  n_success = 0     #已找到的个数
  n_fail = 0        #失败次数
  n_try = 0         #总试验次数
  nn1 = neighb(x)   #计算当前近邻个数
  while(n_success < row){
    n_try = n_try + 1
    if(flag){
      print(c("总抽样次数:", n_try))  #程序复杂,用这些print来判断出错原因
    }
    n_fail = n_fail + 1
    if(n_fail > 1000){
      print(c("抽样失败1000次,n_success:", n_success,"总抽样次数:", n_try))   
      #失败次数过多,程序模拟效果不好,则需要改进抽样方式
      print(x)
      break
    }
    
    
    item1 = ceiling(runif(1)*n)   #随机选取第一个下标
    item2 = ceiling(runif(1)*(n-1))   #随机选取第二个下标
    if(item2 >= item1){
      item2 = item2 + 1   #对第二个下标的改进,避开item1和item2为同一个,提高抽样的效率
    }
    
    #想法是先进行交换,不满足bound的条件在换回来
    #按照交换后的序列,计算sigma(i*x_i)
    sum2 = sum1 - item1*x[item1] - item2*x[item2] + item1*x[item2] + item2*x[item1]
    
    if(sum2 >= bound){    #判断交换后的序列的sigma(i*x_i)是否满足bound条件
      leap1 = x[item1]    #若满足做以下几步的交换
      x[item1] = x[item2]
      x[item2] = leap1
      Try = FALSE         #设置标签Try用于判断交换的序列x是否记录    
      nn2 = neighb(x)
      if(nn2 < nn1){      #min(1,N(x)/N(y)),先判断N(x)/N(y)大于1的情况
        Try = TRUE        #更改Try,记录x
        
      }
      else{               #min(1,N(x)/N(y)),再判断N(x)/N(y)小于1的情况
        U = runif(1)      #0~1间的随机数
        
        if(U < (nn1/nn2)){
          Try = TRUE      #在U < N(X)/N(y)下,更改标签Try,记录x
        }
      }
      
      if(Try){            #在Try = TRUE下记录x
        n_success = n_success + 1
        n_fail = 0
        new_matrix[n_success,] = x
        nn1 = nn2
        sum1 = sum2
      }
      else{               #上述多项判断都不满足,再把序列交换回来
        leap1 = x[item1]
        x[item1] = x[item2]
        x[item2] = leap1
        n_fail = n_fail + 1
        new_matrix[(n_success + 1),] = x
      }
    }
    else{   
      n_fail = n_fail + 1 #判断交换后的序列的sigma(i*x_i)不满足bound条件,记录失败次数
    }
  }
  new_matrix#输出抽样结果
}

#一次生成101000个抽样时间和内存有点hold不足
#所以采用分批次抽样
a = 300000                #sigma(i*x_i)的界限
batch = 100               #每批次生成100个抽样
n_all = 1010              #总共101000个抽样
men = matrix(0, batch, 100)#新建100*100的矩阵存储每一批次的抽样
full = NULL               #缺省值
x = 1:100                 #排列
for (i in 1:n_all){
  men = repermutation(x, batch, 100, a)#生成100个抽样
  full = rbind(full,men)  #不断将每一批次的数据矩阵拼接在一起
  x = men[batch,]         #抽取出每一批次的最后一行作为下一批次抽样的起始
}
write.csv(full,file = "C:\\Users\\24253\\Desktop\\repermutation.csv")#写入抽样数据
full1 = full[10000:101000,] #前10000个抽样作为老化期，剔除前1000个抽样
#计算均值
mean_vec = NULL
for (j in 1:100){
  mean_vec = c(mean_vec, mean(full1[,j]))
}
mean_vec
#协方差矩阵
cov_matrix = cov(full1)
View(cov_matrix)

#老化期的选择
moving_average = function(x){ #moving average查看老化期长短
  n = length(x)
  avg = numeric(n)
  s = 0
  for(i in 1:n){
    s = s + x[i]
    avg[i] = s/i
  }
  avg
}

avg_permutation = moving_average(full[,100])
plot(1:101000, avg_permutation, pch = 19, col = 1, cex = .1)

plot(1:101000, avg_permutation, pch = 19, col = 1, cex = .1, xlim = c(0,20000))
