install.packages("tigerstats")
library(tigerstats)

# 이항분포
# 0-5까지의 값을 볼 예정
binomy <- dbinom(0:5,size=5,prob=0.2)
plot(0:5,binomy,type='h',lwd=5,col="grey",ylab="Probability",xlab="확률변수
X",main=c("X~B(5,0.2)"))
dbinom(0, size=5, prob=0.2) # P(X=0)인 확률: dbinom(x, size, prob)
# P(X<=1)인 확률: pbinom(x,size,prob,lower.tail=TRUE)
pbinom(1,size=5,prob=0.2,lower.tail = TRUE)
또는
sum(dbinom(0:1,size=5,prob=0.2)) # dbinom을 sum 해도 동일
# P(X>1)인 확률: pbinom(x,size,prob,lower.tail=FALSE)
pbinom(1,size=5,prob=0.2,lower.tail = FALSE)
# 5번 시행중 1번 이하일 확률
pbinomGC(bound = 1,region = "below",size = 5,prob = 0.2,graph = TRUE)
# 20번 시행중 10번 이상일 확률
pbinomGC(bound=9, region="above", size=20, prob=0.3, graph=TRUE) 


#포아송분포 그리기
plot(dpois(x=c(0:30),lambda = 6), type = 'h', main = "Poisson distribution, lambda=6")
#람다 6일 때, 특정 값 x 일 확률
dpois(x=3,lambda = 6)
#누적확률(q까지)
ppois(q=5,lambda = 6,lower.tail = TRUE)
#누적확률(범위)
sum(dpois(x=c(0:5),lambda = 6))

#신뢰구간 추정
#Z값을 통한 신뢰구간 추정
n <- 250
Xbar <- 1821
sd <- 192
SE <- sd/sqrt(n)
a05 <- 0.05 #유의수준
z.quantile <- qnorm(1-alpha/2)
#qnorm은 해당하는 표준화값 제시
error.Z <- z.quantile*SE.Z
CI.L.Z <- xbar-error.Z
CI.U.Z <- xbar+error.Z 

#t값을 통한 신뢰구간 추정
n <- 250
Xbar <- 1821
sd <- 192
SE <- sd/sqrt(n)
a05 <- 0.05 #유의수준
error.t <- qt(1-alpha/2, df=n-1)*SE.t
CI.L.t <- xbar-error.t
CI.U.t <- xbar+error.t

install.packages(“readxl”)
library(readxl)
ex3 <- read_xlsx("d:/data.xlsx", sheet = "Sheet1")
n <- 250
Xbar <- mean(ex3$En)
sd <- sd(ex3$En)
SE <- sd/sqrt(n)
a05 <- 0.05 #유의수준
error.t <- qt(1-a05/2, df=n-1)*SE
CI.L.t <- xbar-error.t
CI.U.t <- xbar+error.t CI.L.t
CI.U.t

#일표본 T검정
t.test(ex3$En, mu=2000, conf.level = 0.95)

#통계적 검정

#집단별 평균분석
install.packages(“readxl”)
library(readxl)
ex4 <- read_excel("##파일의 경로## 생명보험통계자료.xlsx")
tapply(ex4$월수입, ex4$직업, mean) #평균
tapply(ex4$월수입, ex4$직업, sd) #표준편차
tapply(ex4$직업, ex4$직업, length) #개수
#일표본 T검정
t.test(ex4$보험가입금액,mu=10,conf.level = 0.95)
#독립표본 T검정
var.test(ex4$보험가입금액~ex4$성별, conf.level = 0.95)
t.test(ex4$보험가입금액~ex4$성별, var.equal=TRUE, conf.level=0.95)

#비모수검정
#카이-스퀘어 검정_uniform
chi1 <- c(42,45,51,47,60)
chisq.test(chi1, simulate.p.value = FALSE) #카이-스퀘어 검정_non-uniform
chi2 <- c(58,9,18,8,7)
chi2.expected <- c(60,10,10,10,10)
chisq.test(chi2, p = chi2.expected, rescale.p = TRUE) #확률값 총합을 1로 조정
chi2.expected2 <- c(0.6,0.1,0.1,0.1,0.1)
chisq.test(chi2, p=chi2.expected2, simulate.p.value = TRUE)


