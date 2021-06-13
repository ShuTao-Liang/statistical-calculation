#h0(x) = 1
fun1 = function(x){
  1
}
#h1(x) = x
fun2 = function(x){
  x
}
#h2 = x^2
fun3 = function(x){
  x^2
}

#fun:需要计算积分的函数
#b:积分上限
#a:积分下限
#n:区间[a,b]划分的个数
############################################################
#中点公式
Mid_point_integrate = function(fun, b = 1, a = 0, n = 10000){
  h = (b - a)/n
  fun_sum = 0
  for(i in 1:n){
    x = a + ((i - 1) + 1/2)*h   
    fun_sum = fun_sum + fun(x)  #计算sigma{f(a + (i + 1/2)*h)} for i from 0 to n-1
  }
  h*fun_sum   #返回h*sigma{f(a + (i + 1/2)*h)} for i from 0 to n-1
}

#############################################################
#复合梯形公式
composite_trapezoidal_integrate = function(fun, b = 1, a = 0, n = 10000){
  h = (b - a)/n
  funa = fun(a)
  funb = fun(b)
  fun_sum = 0
  for(i in 1:(n-1)){
    x = a + i*h
    fun_sum = fun_sum + fun(x)  #计算sigma{f(a + i*h)} for i from 1 to n-1
  }
  (h/2)*(funa + 2*fun_sum + funb)   #返回h*sigma{f(a + i*h)} for i from 1 to n-1
}


#############################################################
#Simpson公式
Simpson_integrate = function(fun, b = 1, a = 0, n = 10000){
  h = (b - a)/n
  funa = fun(a)   #计算f(a)
  funb = fun(b)   #计算f(b)
  fun_sum1 = 0
  fun_sum2 = 0
  for(i in 1:n){
    x1 = a + ((i - 1) + 1/2)*h
    fun_sum1 = fun_sum1 + fun(x1)   #计算sigma{f(a + (i + 1/2)*h)} for i from 0 to n-1
  }
  for(i in 1:(n-1)){
    x2 = a + i*h
    fun_sum2 = fun_sum2 + fun(x2)   #计算sigma{f(a + i*h)} for i from 1 to n-1
  }
  (h/6)*(funa + 4*fun_sum1 + 2*fun_sum2 + funb)
  #返回积分值
}
