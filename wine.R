w_df <- read.csv('w_df_x_y_encoded.csv')
w_df$country <- factor(w_df$country)
w_df$variety <- factor(w_df$variety)
library('tidyverse')
library('gridExtra')
var_pri <- w_df %>% ggplot(aes(variety, price)) + geom_boxplot()
cou_pri <- w_df %>% ggplot(aes(country, price)) + geom_boxplot()
var_poi <- w_df %>% ggplot(aes(variety, points)) + geom_boxplot()
cou_poi <- w_df %>% ggplot(aes(country, points)) + geom_boxplot()

grid.arrange(var_pri, cou_pri,var_poi, cou_poi, nrow=2, ncol=2)
# 와인품종/가격, 제조국가/가격, 와인품종/점수, 제조국가/점수 box plot

pri_var_lm <- lm(price ~ variety, data=w_df)
summary(pri_var_lm)
# Multiple R-squared:  0.06229,	Adjusted R-squared:  0.06204
# variety 변수가 price 변수를 약 6% 설명
pri_cou_lm <- lm(price ~ country, data=w_df)
summary(pri_cou_lm)
# Multiple R-squared:  0.02353,	Adjusted R-squared:  0.02324
# country 변수가 price 변수를 약 2% 설명

poi_var_lm <- lm(points ~ variety, data=w_df)
summary(poi_var_lm)
# Multiple R-squared:  0.07302,	Adjusted R-squared:  0.07278 
# variety 변수가 price 변수를 약 7% 설명
poi_cou_lm <- lm(points ~ country, data=w_df)
summary(poi_cou_lm)
# Multiple R-squared:  0.0535,	Adjusted R-squared:  0.05322 
# country 변수가 price 변수를 약 5% 설명


# 위의 수치들로 미루어보아 선형모델을 이용하여 country, variety로 price/points 를 설명하기 어렵다.

# two - way anova model 을 사용하여 분산분석을 실행.



w_df_2 <- w_df %>% group_by(variety, country)%>% summarise(mean=mean(points),sd=sd(points))
w_df_2


ggplot(data=w_df_2, aes(x=variety,y=mean))+
    geom_point(aes(col=country,shape=country))+ 
    geom_line(aes(group=country,col=country,lty=country))


fit<-aov(points~country*variety,data=w_df) 
summary(fit)


fit_2 <- aov(points~country+variety,data=w_df) 
summary(fit_2)



interaction.plot(x.factor=w_df$country,trace.factor=w_df$variety,response=w_df$points,
                 fun=mean, type="b",pch=c(2,4),col=c(2,4),xlab="country",ylab="Mean of points")

# 범주형 x 값이 너무 많으므로 country 를 preprocessing 에서 나누었던 in_worldmap으로 대체 하였음.

w_df_anova <- read.csv('w_df_anova_encoded.csv')
w_df_anova
w_df_anova$in_worldmap <- factor(w_df_anova$in_worldmap)
w_df_anova$variety <- factor(w_df_anova$variety)

# 변경한 데이터셋으로 다시 two-way anova
# 유의미한 분석값이 나오기는 어려움.

w_df_anova_2 <- w_df_anova %>% group_by(variety, in_worldmap)%>% summarise(mean=mean(points),sd=sd(points))
w_df_anova_2

ggplot(data=w_df_anova_2, aes(x=variety,y=mean))+
    geom_point(aes(col=in_worldmap,shape=in_worldmap))+ 
    geom_line(aes(group=in_worldmap,col=in_worldmap,lty=in_worldmap))

fit_anova_1 <- aov(points~in_worldmap*variety,data=w_df_anova)
summary(fit_anova_1)

fit_anova_2 <- aov(points~in_worldmap+variety,data=w_df_anova) 
summary(fit_anova_2)

