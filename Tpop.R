6/2 #ctrl+enter#
a <- 6    #화살표 alt + - #
b <- 2

rank <- c(1:8) 
rate <- c(.603,.547,.546,.532,.462,.457,.450,.402) 

ex1 <- data.frame(rank, rate)
View(ex1)

Tpop <- read.csv("C:/Temp/Tpop.csv")
head(Tpop)
tail(Tpop)

# 표준점수 
Tpop$Z_p10 <- scale(Tpop$T_Pop10) #2010년의인구, scale함수는 지정한 변수를 z값으로바꿔주는 것
Tpop$Z_p05 <- scale(Tpop$T_Pop05)

# 기초 통계량
summary(Tpop$T_Pop10)

# 범위표준화
Tpop$range_p10 <- (Tpop$T_Pop10-min(Tpop$T_Pop10)) / (max(Tpop$T_Pop10)-min(Tpop$T_Pop10)) # 0-1사이값임
Tpop$range_p10

# 산점도 그리기
install.packages("ggplot2")
library(ggplot2)

ggplot(data=Tpop, aes(x= Tpop$Male10, y= Tpop$Female10))+geom_point() ggplot(data=Tpop, aes(x= Tpop$Male10, y= Tpop$Female10))+geom_point(colour="blue") 

ggplot(Tpop,aes(Tpop$Male10, Tpop$Female10))+geom_point()+   scale_x_continuous(name="Fluorescent intensity/arbitrary units", labels = scales::comma)+   scale_y_continuous(name="Fluorescent intensity/arbitrary units", labels = scales::comma) 

# 히스토그램 그리기
ggplot(data=Tpop,aes(x= Tpop$Male10))+geom_histogram()
ggplot(data=Tpop,aes(x= Tpop$Male10))+ geom_histogram(binwidth = 100000,colour="red",fill="yellow") 
ggplot(data=Tpop,aes(x= Tpop$Male10)) + geom_histogram(binwidth = 100000, aes(y=..density.., fill=..count..),colour="black") + scale_fill_gradient(low="white", high = "#496ff5") 
ggplot(data=Tpop,aes(x= Tpop$Male10)) + geom_histogram(binwidth = 100000, aes(y=..density.., fill=..count..),colour="black") + scale_fill_gradient(low="white", high = "#496ff5")+geom_density(colour="red") + scale_x_continuous(name="Fluorescent intensity/arbitrary units", labels = scales::comma) + scale_y_continuous(name="Fluorescent intensity/arbitrary units", labels = scales::comma) 


# 상자그림 그리기 

boxplot(Tpop$Female10, main="박스플롯", ylab=" Tpop$Female10") 
ggplot(data= Tpop, aes(x=1, y= Tpop$Female10)) + geom_boxplot(fill='slategrey',color='darkslategrey',width=0.3) ##그래프 색/크기 바꾸기 
ggplot(data= Tpop, aes(x=1, y= Tpop$Female10)) + geom_boxplot(outlier.color = 'red',outlier.shape = 2)  ##이상치 색/모양 바꾸기 
ggplot(Tpop, aes(x=1, y= Tpop$Female10)) + geom_boxplot(outlier.shape = NA) #이상치제거 
ggplot(data= Tpop, aes(x=1, y= Tpop$Female10)) + geom_boxplot() ggplot(Tpop, aes(factor(Tpop), TOTAL_VIEWS)) + geom_boxplot() 


team <- c("삼성","SK","롯데","KIA","LG","두산","한화","넥센")     # 문자는 따옴표로 입력 
game <- c(118,119, 124,126,120,118,122, 119) 
win <- c(70,64,65,67,55,53,54,47) 
tie <- c(2,2,5,0,1,2,2,2) 
lose <- c(46,53,54,59,64,63,66,70) 
rate <- c(.603,.547,.546,.532,.462,.457,.450,.402) 
rate  ##rate 출력 
#> [1] 0.603 0.547 0.546 0.532 0.462 0.457 0.450 0.402 
options(digits=2)               # 소수점이하 자리수 표시 
rate #> [1] 0.60 0.55 0.55 0.53 0.46 0.46 0.45 0.40 
