library(boot)
data.Reg <- c(3.63,3.63,3.64,3.65,3.65,3.69,3.69,3.69,3.71,3.71,3.71,3.73,3.73,3.73,3.73,4.35,4.29,4.29,4.29,4.29,4.28,4.19,4.19,4.19,4.19,4.19,4.19,4.19,4.19,4.17)
data.Pre <- c(3.81,3.81,3.89,3.89,3.93,3.93,3.94,3.95,3.95,3.95,3.95,3.95,3.95,3.97,3.98,4.74,4.59,4.59,4.59,4.59,4.56,4.55,4.55,4.49,4.49,4.49,4.49,4.49,4.49,4.49)
data.Mid <- c(3.77,3.79,3.82,3.83,3.85,3.85,3.85,3.85,3.85,3.85,3.85,3.87,3.88,3.88,3.88,4.65,4.49,4.45,4.39,4.39,4.39,4.39,4.39,4.39,4.39,4.39,4.35,4.35,4.35,4.32)
data.Die <- c(3.76,3.77,3.79,3.79,3.79,3.79,3.79,3.85,3.85,3.85,3.87,3.89,3.89,3.89,3.89,4.49,4.49,4.49,4.49,4.49,4.49,4.39,4.39,4.39,4.39,4.39,4.39,4.39,4.39,4.39)

getRate = function(dat,indices) {
  newdata = data.Reg[indices]
  return(mean(newdata))
}
(patch.boot <- boot(data=data.Reg, statistic=getRate, R=2000))
boot.ci(patch.boot, conf=.95, type=c('norm','basic', 'perc', 'bca'))

getRate = function(dat,indices) {
  newdata = data.Pre[indices]
  return(mean(newdata))
}
(patch.boot <- boot(data=data.Pre, statistic=getRate, R=2000))
boot.ci(patch.boot, conf=.95, type=c('norm', 'basic', 'perc', 'bca'))

getRate = function(dat,indices) {
  newdata = data.Mid[indices]
  return(mean(newdata))
}
(patch.boot <- boot(data=data.Mid, statistic=getRate, R=2000))
boot.ci(patch.boot, conf=.95, type=c('norm','basic', 'perc', 'bca'))

getRate = function(dat,indices) {
  newdata = data.Die[indices]
  return(mean(newdata))
}
(patch.boot <- boot(data=data.Die, statistic=getRate, R=2000))
boot.ci(patch.boot, conf=.95, type=c('norm','basic', 'perc', 'bca'))

result = function(i,j,k){
  answer = 0.35*i - 0.25*j + 0.7*k
  return (answer)
}
result(1500,0,1000)
result(4500,2,200)
result(4500,4,480)
result(1500,2,1000)
result(4500,4,480)
result(8000,6,582)
result(4500,8,411)
result(17500,9,1000)
result(8000,7,582)
result(4500,4,582)
result(8000,11,1000)
result(17500,9,1000)
result(17500,7,1000)
result(8000,1,1000)
result(17500,7,900)
result(17500,9,1000)
result(17500,7,772)
result(8000,6,772)
result(8000,12,582)
result(8000,7,772)
result(10000,4,772)
result(6000,5,582)
result(6000,11,411)
result(8000,6,200)
result(4500,4,582)