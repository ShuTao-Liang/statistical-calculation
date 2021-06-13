#############################################################################
how_far = function(Obs, Exp){
  max(abs(Obs-Exp))
}

test_pois = function(x, lambda = 1, M = 2000){
  cross = table(x)
  n = length(x)
  N = max(x)
  expected_prob = dpois(0:N, lambda)
  expected_counts = n*expected_prob
  d_obs = how_far(as.vector(cross), expected_counts)
  how_far_values = NULL
  for(i in 1:M){
    x = rpois(n, lambda)               #使用内置的rpois()生成随机数来检验
    Numbers = table(x)                 #统计每个元素的个数
    if(length(Numbers) != N+1){        #跳过与待检验数据的元素不等长，因为数值较大后的概率极小，可忽略
      next
    }
    how_far_values = c(how_far_values, how_far(Numbers, expected_counts))
  }
  result = c(how_far_values, d_obs)
}


a = rpois_version1(1000, 3)            #目前还没学会怎么将rpois_version"i"作为定义的test_pois的变量,
                                       #在后续的检验中手动更改

result = test_pois(a, 3)


hist(result[c(1:length(result) - 1)], xlim=c(0, 60))
abline(v=result[c(length(result))], col='red', lwd=2)
quantile(result[c(1:length(result) - 1)],c(0.025,0.975))
result[c(length(result))]



#####以下为思考版本##########################################################
#############################################################################
a = rpois_version1(1000, 3)
cross = table(a)
n = length(a)
N = max(a)
expected_prob = dpois(0:N, 3)
expected_counts = n*expected_prob
expected_counts = round(expected_counts)
how_far_values = NULL
for(i in 1:5000){
  x = rpois(n, lambda = 3)
  Numbers = table(x)
  if(length(Numbers) != N+1){
    next
  }
  how_far_values = c(how_far_values, how_far(Numbers, expected_counts))
}
how_far_values 
hist(how_far_values)
#############################################################################


