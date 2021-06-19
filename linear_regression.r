{
  library("readxl")
  library(ggplot2)
  library(car)
  library(corrplot)
  library("writexl")
  library(lmtest)
  
}

setwd('C:\\Users\\balli\\OneDrive\\Desktop')

# Импорт данных
data <- read_excel("omega.xlsx", sheet = 1)
data <- data[data$Category == "ПОТРЕБИТЕЛЬСКОЕ КРЕДИТОВАНИЕ",]
data$Device <- as.factor(data$Device)
data$Place <- as.factor(data$Place)
data$BannerType <- as.factor(data$BannerType)
data$`QueryType (search only)` <- as.factor(data$`QueryType (search only)`)
data$TargetingType <- as.factor(data$TargetingType)
data$Client <- as.factor(data$Client)
data$Cost_rub_wo_NDS <- scale(data$Cost_rub_wo_NDS)

# изучим некоторые графики
ggplot(data=data) + geom_point(aes(x=Cost_rub_wo_NDS, y=Clicks, color=Device))

ggplot(data=data) + geom_point(aes(x=Cost_rub_wo_NDS, y=Shows, color=Device))

ggplot(data=data) + geom_point(aes(x=Cost_rub_wo_NDS, y=Сonversions, color=Device))
str(data)

# построим модель для конверсии

mod_0 <- lm(Сonversions~Device+Cost_rub_wo_NDS+Place+BannerType+TargetingType+`QueryType (search only)`, data = data)
summary(mod_0)
AIC(mod_0)
stepAIC(mod_0)
bptest(mod_0)
plot(mod_0)
# есть гетероседастичность
coeftest(mod_0)

mod_1 <- update(mod_0, .~.-BannerType)
summary(mod_1)
AIC(mod_1)
coeftest(mod_1)

# Построим модель для общей конверсии
mod_0 <- lm(`CR12 (%)`~Device+Cost_rub_wo_NDS+Place+BannerType+TargetingType+`QueryType (search only)`, data = data)
summary(mod_0)
AIC(mod_0)
stepAIC(mod_0)
bptest(mod_0)
plot(mod_0)
# есть гетероседастичность
coeftest(mod_0)

mod_1 <- update(mod_0, .~.-BannerType)
summary(mod_1)
AIC(mod_1)
coeftest(mod_1)
plot(mod_1)

ggplot(data=data) + geom_point(aes(y=`CR12 (%)`, x=Cost_rub_wo_NDS, color=Place))


# Построим модель для отношения целевых визитов к затраченным деньгам

mod_0 <- lm(`goal_cost*10000`~Device+Place+BannerType+TargetingType+`QueryType (search only)`, data = data)
summary(mod_0)
AIC(mod_0)
stepAIC(mod_0)
bptest(mod_0)
plot(mod_0)
# есть гетероскедастичность
coeftest(mod_0)

mod_1 <- update(mod_0, .~.-BannerType)
summary(mod_1)
AIC(mod_1)
coeftest(mod_1)
plot(mod_1)

ggplot(data=data, aes(x=Cost_rub_wo_NDS, y=`goal_cost*10000`, color=`QueryType (search only)`)) + geom_point() + geom_smooth(method='lm')+theme_light() +xlab('Рекламный бюджет')+ylab('Отношение конверсии к бюджету')+ggtitle('Зависимость целевой метрики от рекламного бюджета')
