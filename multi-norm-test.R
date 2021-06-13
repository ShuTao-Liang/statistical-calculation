data = X3_3
data = data.frame(data)
data[,1]
for(i in 1:12){
  
  print(shapiro.test(data[,i]))
}

library(MVN)
mvn(data[,1:4], mvnTest = c("mardia"), multivariatePlot = c("qq"))
mvn(data[,5:8], mvnTest = c("mardia"), multivariatePlot = c("qq"))
mvn(data[,9:12], mvnTest = c("mardia"), multivariatePlot = c("qq"))
