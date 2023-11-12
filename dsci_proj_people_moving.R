#패키지 준비
install.packages("dplyr")
install.packages("tidyverse")
library(dplyr)
library(tidyverse)


#데이터 준비
home <- read_csv("having_house.csv",locale=locale("ko",encoding="EUC-KR"),
                  col_names=T,na=".")
View(home)

move <- read_csv("p_move_f.csv",locale=locale("ko",encoding="EUC-KR"),
                  col_names=T,na=".")
View(move)

sell <- read_csv("sell_p.csv",locale=locale("ko",encoding="EUC-KR"),
                 col_names=T,na=".")
View(sell)


##데이터 정제(주택보급률)
summary(home$경기도)

#boxplot(home$경기도)
#
#home_iqr_price<-IQR(home$경기도)
#
#home_iqr_price
#
#sell$경기도<-ifelse(home$경기도>summary(home$경기도)[5]+
#                   sell_iqr_price*1.5,
#                 NA,home$경기도)

home1<-na.omit(home)

boxplot(home$경기도)
table(is.na(home$경기도))
View(home1)
home1


##데이터 정제(주택매매지수)
summary(sell$경기도)

#boxplot(sell$경기도)
#
#sell_iqr_price<-IQR(sell$경기도)
#
#sell_iqr_price
#
#sell$경기도<-ifelse(sell$경기도>summary(sell$경기도)[5]+
#                   sell_iqr_price*1.5,
#                 NA,sell$경기도)

sell1<-na.omit(sell)

boxplot(sell1$경기도)
table(is.na(sell1$경기도))
View(sell1)
sell1


##데이터 정제(인구이동률)
summary(move$경기도)
#boxplot(move$경기도)
#
#move_iqr_price<-IQR(move$경기도)
#
#move_iqr_price
#
#move$경기도<-ifelse(move$경기도>summary(move$경기도)[5]+
#                   move_iqr_price*1.5,
#                 NA,move$경기도)

move1<-na.omit(move)

boxplot(move1$경기도)
table(is.na(move1$경기도))
View(move1)
move1
summary(move1)


##데이터가공
#인구이동률
summarize_move <- move1 %>%
  group_by(연도) %>%
  summarize(인구이동률 = mean(인구이동률))

summarize_move

#주택매매지수
summarize_sell <- sell1 %>%
  group_by(연도) %>%
  summarize(주택매매지수 = mean(주택매매지수))

summarize_sell

#주택보급률
summarize_home <- home1 %>%
  group_by(연도) %>%
  summarize(주택보급률 = mean(주택보급률))

summarize_home


#데이터병합
total2 <- left_join(home1,summarize_sell,by=c("연도"))

total1 <- left_join(total2,summarize_move,by=c("연도"))

total1

#그래프 생성(주택매매지수)
summarize_sell %>% 
  ggplot(aes(연도,주택매매지수)) +
  geom_point() +
  geom_line() +
  geom_smooth() +
  labs(title = "주택매매지수") +
  ylim(60,110)

#그래프 생성(주택보급률)
home1 %>% 
  ggplot(aes(연도,주택보급률)) +
  geom_point() +
  geom_line() +
  geom_smooth() +
  labs(title = "주택보급률") +
  ylim(95,100)

#그래프 생성(인구이동률)
summarize_move %>% 
  ggplot(aes(연도,인구이동률)) +
  geom_point() +
  geom_line() +
  geom_smooth() +
  labs(title = "인구이동률") +
  ylim(0,1.8)


#lm모델생성 후 회귀식 모델 산출 및 시각화
total_model1<- lm (인구이동률~주택보급률,data=total1)
total1
total_model1
summary(total_model1)
plot(total_model1$주택보급률, total_model1$인구이동률, xlab = "주택보급률"
     , ylab = "인구이동률", main = "주택보급률과 인구이동률")
abline(total_model1, col = "red")

#인구이동률 = -15.7334 + 0.1701 * 주택보급률
#R-squared = 0.095
#p-value = 0.2004

#lm모델생성 후 회귀식 모델 산출 및 시각화
total_model2<- lm(인구이동률~주택매매지수,data=total1)
total_model2
summary(total_model2)
plot(total_model2$주택매매지수, total_model2$인구이동률, xlab = "주택매매지수"
     , ylab = "인구이동률", main = "주택매매지수와 인구이동률")
abline(total_model2, col = "blue")

#인구이동률 = -0.95434 + 0.02339 * 주택매매지수
#R-squared = 0.5203
#p-value = 0.01854

##practice
# 선형 회귀모델 생성
lm_model <- lm(인구이동률 ~ 주택보급률, data = dfa)

# 회귀분석 결과 요약
summary(lm_model)

# 그래프 그리기
plot(주택보급률 ~ 연도, data = dfa, type = "n", xlab = "연도", ylab = "주택보급률")
points(주택보급률 ~ 연도, data = dfa, pch = 16)
abline(lm_model, col = "red")

# 다항회귀 모델 생성(다항회귀 사용 필요 x)
poly_model <- lm(인구이동률 ~ poly(주택보급률, 3, raw = TRUE), data = dfa)

# 예측값 계산(예측값 사용시 사용)
new_data <- data.frame(주택보급률 = seq(min(dfa$주택보급률), max(dfa$주택보급률), length.out = 100))
predicted <- predict(poly_model, newdata = new_data)

# 그래프 그리기
plot(주택보급률 ~ 인구이동률, data = dfa, xlab = "주택보급률", ylab = "인구이동률")
lines(new_data$주택보급률, predicted, col = "red")


#연습
df <- data.frame(
  연도 = c(2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021),
  주택보급률 = c(97.3, 97.3, 97.7, 97.9, 98.2, 98.3, 99.0, 99.2, 98.0, 96.8),
  주택매매지수 = c(74.1, 70.9, 72.1, 75.3, 78.0, 79.8, 82.7, 83.0, 88.0, 100.7),
  인구이동률 = c(0.683, 0.608, 0.483, 0.75, 1.066, 0.917, 1.341, 1.025, 1.275, 1.125)
)

# 주택보급률과 인구이동률 간의 선형 회귀모델
lm_model1 <- lm(인구이동률 ~ 주택보급률, data = df)

# 주택매매지수와 인구이동률 간의 선형 회귀모델
lm_model2 <- lm(인구이동률 ~ 주택매매지수, data = df)

# 회귀선형 그래프 그리기
plot(df$주택보급률, df$인구이동률, xlab = "주택보급률", ylab = "인구이동률", main = "주택보급률과 인구이동률")
abline(lm_model1, col = "red")
plot(df$주택매매지수, df$인구이동률, xlab = "주택매매지수", ylab = "인구이동률", main = "주택매매지수와 인구이동률")
abline(lm_model2, col = "blue")

#상관계수 분석
correlation1 <- cor(df$주택보급률, df$인구이동률)
correlation2 <- cor(df$주택매매지수, df$인구이동률)
correlation1
correlation2
r_squared1 <- summary(lm_model1)$r.squared
p_value1 <- summary(lm_model1)$coefficients[2, 4]
r_squared2 <- summary(lm_model2)$r.squared
p_value2 <- summary(lm_model2)$coefficients[2, 4]
r_squared1
p_value1
r_squared2
p_value2
