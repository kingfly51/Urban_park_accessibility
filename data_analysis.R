library(readxl)
merged_data_cities <- read_excel("merged_data_cities.xlsx")
use_data <- merged_data_cities[is.na(merged_data_cities$county),]

use_data <- use_data[use_data$age<100 &use_data$age>17,]
use_data <- use_data[use_data$gender!=3,]
use_data <- use_data[complete.cases(use_data$walking_distance_m),]
use_data <-use_data[use_data$walking_distance_m<6000,]
use_data <- use_data[use_data$living_time>2,]


hist(use_data$walking_distance_m)
hist(use_data$Future_suicide)

# 复制原始数据
use_data_std <- use_data

# 对连续变量进行标准化
continuous_vars <- c("age", "education", "income", "living_time", "Future_suicide",
                     "Previous_suicidal_thoughts","walking_distance_m")

# 标准化连续变量
use_data_std[continuous_vars] <- scale(use_data_std[continuous_vars])

# 人格特质变量也需要标准化（因为是调节变量）
personality_vars <- c("Openness", "Conscientiousness", "Extraversion", 
                      "Agreeableness", "Neuroticism")
use_data_std[personality_vars] <- scale(use_data_std[personality_vars])

#####3.1 Sociodemographic characteristics of the sample#####

table(use_data$gender)#973 1061 
table(use_data$education)#29  69 139 122 286 314 940 111  24 
table(use_data$income)#73 142 220 256 277 284 288 207 119 168 
table(use_data$nation)#1892  142

psych::describe(use_data$age)#33.45±13.95
psych::describe(use_data$living_time)#4.51±1.2
psych::describe(use_data$Previous_suicidal_thoughts)#2.5±1
psych::describe(use_data$Future_suicide)#2.54±1.53
psych::describe(use_data$park_count_500m)#0.13±0.39
psych::describe(use_data$total_park_count)#6.18±3.36
psych::describe(use_data$dep)#18.78±6.01
psych::describe(use_data$anx)#18.29±6.98
psych::describe(use_data$stress)#10.46±4.02
psych::describe(use_data$walking_distance_m)#1477.79±919.79
psych::describe(use_data$Openness)#6.95±1.9
psych::describe(use_data$Conscientiousness)#7.04±1.75
psych::describe(use_data$Extraversion)#6.58±2.01
psych::describe(use_data$Agreeableness)#7.12±1.71
psych::describe(use_data$Neuroticism)#5.83±1.96

psych::describe(merged_data_filter$mean_temperature)#27.21±4.05
psych::describe(merged_data_filter$avg_AQI.x)#36.32±16.48
psych::describe(merged_data_filter$NDVI)#0.53±0.11
psych::describe(merged_data_filter$GDP)#11279.12±14175.34
psych::describe(merged_data_filter$dem_mean)#459.55±737.01
psych::describe(merged_data_filter$面积)#14208.1±13601.14
psych::describe(merged_data_filter$mean_precipitation)#7.39±15.29

#####3.2 The Association Between Urban Park Accessibility and Suicide Risk#####
m1 <- lm(Future_suicide ~ 
                walking_distance_m,
              data = use_data_std)
summary(m1)

m2 <- lm(Future_suicide ~ 
           age + 
           gender +
           education + 
           income + 
           nation +
           living_time +
           walking_distance_m,
         data = use_data_std)
summary(m2)

m3 <- lm(Future_suicide ~ 
           age + 
           gender +
           education + 
           income + 
           nation +
           living_time +
           Previous_suicidal_thoughts + 
           walking_distance_m,
         data = use_data_std)
summary(m3)

m4 <- lm(Future_suicide ~ 
           age + 
           gender +
           education + 
           income + 
           nation +
           living_time +
           Previous_suicidal_thoughts + 
           total_park_count +
           park_count_500m +
           walking_distance_m,
         data = use_data_std)
summary(m4)

m5 <- lm(Future_suicide ~ 
           age + 
           gender +
           education + 
           income + 
           nation +
           living_time +
           Previous_suicidal_thoughts + 
           total_park_count +
           park_count_500m +
           dep+
           anx+
           stress+
           walking_distance_m,
         data = use_data_std)
summary(m5)


library(readxl)
long_city <- read_excel("D:/Rdaima/urban_park_accessibility/long_city.xlsx")
merged_data_filter <- merge(use_data_std,long_city,by.x=c("cities_simple","date"),by.y = c("cites","date"))
unmatched_cities <- unique( use_data_std$cities_simple[! use_data_std$cities_simple %in% merged_data_filter$cities_simple])

m6 <- lm(Future_suicide ~ 
                age + 
                gender +
                education + 
                income + 
                nation +
                living_time +
                Previous_suicidal_thoughts + 
                total_park_count +
                park_count_500m +
                dep+
                anx+
                stress+
                mean_temperature+
                avg_AQI.x+
                NDVI+
                GDP+
                dem_mean+
                面积+
                mean_precipitation+
                walking_distance_m,
              data = merged_data_filter)

summary(m6)

library(lme4)
library(lmerTest)
m7 <- lmer(Future_suicide ~ 
                  age + 
                  gender +
                  education + 
                  income + 
                  nation +
                  living_time +
                  Previous_suicidal_thoughts + 
                  total_park_count +
                  park_count_500m +
                  dep+
                  anx+
                  stress+
                  mean_temperature+
                  avg_AQI.x+
                  NDVI+
                  GDP+
                  dem_mean+
                  面积+
                  mean_precipitation+
                  walking_distance_m +
                  (1|cities_simple),
                data = merged_data_filter)

summary(m7)

m8 <- lmer(Future_suicide ~ 
                  age + 
                  gender +
                  education + 
                  income + 
                  nation +
                  living_time +
                  Previous_suicidal_thoughts + 
                  total_park_count +
                  park_count_500m +
                  dep+
                  anx+
                  stress+
                  mean_temperature+
                  avg_AQI.x+
                  NDVI+
                  GDP+
                  dem_mean+
                  面积+
                  mean_precipitation+
                  walking_distance_m +
                  (1+walking_distance_m|cities_simple),
                data = merged_data_filter)

summary(m8)

AIC(m1)#5772.747
BIC(m1)#5789.6

AIC(m2)#5776.553
BIC(m2)#5827.113

AIC(m3)#5495.392
BIC(m3)#5551.57

AIC(m4)#5499.268
BIC(m4)#5566.681

AIC(m5)#5379.131
BIC(m5)#5566.681

AIC(m6)#5340.145
BIC(m6)#5463.497

AIC(m7)#5529.751
BIC(m7)#5658.709

AIC(m8)#5528.673
BIC(m8)#5668.846


#####3.3 The Association Between Urban Park Accessibility and Suicide Risk: The Moderating Role of Agreeableness#####


lm0_std <- lm(Future_suicide ~ 
                walking_distance_m * Openness +
                walking_distance_m * Conscientiousness +
                walking_distance_m * Extraversion +
                walking_distance_m * Agreeableness +
                walking_distance_m * Neuroticism,
              data = use_data_std)

summary(lm0_std)


lm1_std <- lm(Future_suicide ~ 
                age + 
                gender +
                education + 
                income + 
                nation +
                living_time +
                walking_distance_m * Openness +
                walking_distance_m * Conscientiousness +
                walking_distance_m * Extraversion +
                walking_distance_m * Agreeableness +
                walking_distance_m * Neuroticism,
              data = use_data_std)

summary(lm1_std)


lm2_std <- lm(Future_suicide ~ 
                age + 
                gender +
                education + 
                income + 
                nation +
                living_time +
                Previous_suicidal_thoughts + 
                walking_distance_m * Openness +
                walking_distance_m * Conscientiousness +
                walking_distance_m * Extraversion +
                walking_distance_m * Agreeableness +
                walking_distance_m * Neuroticism,
              data = use_data_std)

summary(lm2_std)


lm3_std <- lm(Future_suicide ~ 
                age + 
                gender +
                education + 
                income + 
                nation +
                living_time +
                Previous_suicidal_thoughts + 
                total_park_count +
                park_count_500m +
                walking_distance_m * Openness +
                walking_distance_m * Conscientiousness +
                walking_distance_m * Extraversion +
                walking_distance_m * Agreeableness +
                walking_distance_m * Neuroticism,
              data = use_data_std)

summary(lm3_std)


lm4_std <- lm(Future_suicide ~ 
                age + 
                gender +
                education + 
                income + 
                nation +
                living_time +
                Previous_suicidal_thoughts + 
                total_park_count +
                park_count_500m +
                dep+
                anx+
                stress+
                walking_distance_m * Openness +
                walking_distance_m * Conscientiousness +
                walking_distance_m * Extraversion +
                walking_distance_m * Agreeableness +
                walking_distance_m * Neuroticism,
              data = use_data_std)

summary(lm4_std)


######控制其他环境因素
######导入city层面的环境数据
library(readxl)
long_city <- read_excel("D:/Rdaima/urban_park_accessibility/long_city.xlsx")
merged_data_filter <- merge(use_data_std,long_city,by.x=c("cities_simple","date"),by.y = c("cites","date"))
unmatched_cities <- unique( use_data_std$cities_simple[! use_data_std$cities_simple %in% merged_data_filter$cities_simple])
cat("未匹配上的城市名称：\n")
for (city in unmatched_cities) {
  cat(city, "\n")
}

lm5_std <- lm(Future_suicide ~ 
                age + 
                gender +
                education + 
                income + 
                nation +
                living_time +
                Previous_suicidal_thoughts + 
                total_park_count +
                park_count_500m +
                dep+
                anx+
                stress+
                mean_temperature+
                avg_AQI.x+
                NDVI+
                GDP+
                dem_mean+
                面积+
                mean_precipitation+
                walking_distance_m * Openness +
                walking_distance_m * Conscientiousness +
                walking_distance_m * Extraversion +
                walking_distance_m * Agreeableness +
                walking_distance_m * Neuroticism,
              data = merged_data_filter)

summary(lm5_std)


library(lme4)
library(lmerTest)
lm6_std <- lmer(Future_suicide ~ 
                age + 
                gender +
                education + 
                income + 
                nation +
                living_time +
                Previous_suicidal_thoughts + 
                total_park_count +
                park_count_500m +
                dep+
                anx+
                stress+
                mean_temperature+
                avg_AQI.x+
                NDVI+
                GDP+
                dem_mean+
                面积+
                mean_precipitation+
                walking_distance_m * Openness +
                walking_distance_m * Conscientiousness +
                walking_distance_m * Extraversion +
                walking_distance_m * Agreeableness +
                walking_distance_m * Neuroticism+
                (1|cities_simple),
              data = merged_data_filter)

summary(lm6_std)

lm7_std <- lmer(Future_suicide ~ 
                  age + 
                  gender +
                  education + 
                  income + 
                  nation +
                  living_time +
                  Previous_suicidal_thoughts + 
                  total_park_count +
                  park_count_500m +
                  dep+
                  anx+
                  stress+
                  mean_temperature+
                  avg_AQI.x+
                  NDVI+
                  GDP+
                  dem_mean+
                  面积+
                  mean_precipitation+
                  walking_distance_m * Openness +
                  walking_distance_m * Conscientiousness +
                  walking_distance_m * Extraversion +
                  walking_distance_m * Agreeableness +
                  walking_distance_m * Neuroticism+
                  (1+walking_distance_m|cities_simple),
                data = merged_data_filter)

summary(lm7_std)


AIC(lm7_std)#5537.206
BIC(lm7_std)#5743.447
performance::model_performance(lm7_std)

AIC(lm6_std)#5547.426
BIC(lm6_std)#5732.453
performance::model_performance(lm6_std)

AIC(lm5_std)#5301.683
BIC(lm5_std)#5481.103
performance::model_performance(lm5_std)

AIC(lm4_std)#5343.142
BIC(lm4_std)#5483.586

AIC(lm3_std)#5437.64
BIC(lm3_std)#5561.23

AIC(lm2_std)#5433.881
BIC(lm2_std)#5546.236

AIC(lm1_std)#5714.116
BIC(lm1_std)#5820.854

AIC(lm0_std)#5710.06
BIC(lm0_std)#5783.091

library(performance)
model_performance1 <- model_performance(m6)
print(model_performance1)
model_performance2 <- model_performance(lm5_std)
print(model_performance2)

#model 6a abic,5504.53
n <- nrow(merged_data_filter)
k1 <- length(coef(m6))
log_lik1 <- logLik(m6)
log_lik_value1 <- as.numeric(log_lik1)
ABIC_empirical1 <- -2 * log_lik_value1 + k1 * (log(n) + 1)

#model 6b abic,5504.53
n <- nrow(merged_data_filter)
k <- length(coef(lm5_std))
log_lik <- logLik(lm5_std)
log_lik_value <- as.numeric(log_lik)
ABIC_empirical <- -2 * log_lik_value + k * (log(n) + 1)


library(emmeans)
simple_slopes_agree <- emtrends(lm5_std, 
                                specs = "Agreeableness", 
                                var = "walking_distance_m",
                                at = list(Agreeableness = c(-1, 0, 1))) 


summary(simple_slopes_agree, infer = c(TRUE, TRUE))



library(ggplot2)
library(ggeffects)
pred_data <- ggpredict(lm5_std, terms = c("walking_distance_m", "Agreeableness [-1,0,1]"))
ggplot(pred_data, aes(x = x, y = predicted, color = group)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), 
              alpha = 0.3, linetype = "dashed") +
  labs(title = "",
       x = "Urban Park Accessibility",
       y = "Future Suicide",
       color = "Agreeableness",
       fill = "Agreeableness") +
  scale_color_manual(
    values = c("#F8766D", "#00BA38", "#619CFF"),
    labels = c("Low", "Medium", "High")
  ) +
  scale_fill_manual(
    values = c("#F8766D", "#00BA38", "#619CFF"),
    labels = c("Low", "Medium", "High")
  ) +
  theme_minimal()





#####3.4 Disparate Results for New Residents and Long-Term Residents#####
library(readxl)
merged_data_cities <- read_excel("merged_data_cities.xlsx")
use_data1 <- merged_data_cities[is.na(merged_data_cities$county),]

use_data1 <- use_data1[use_data1$age<100 &use_data1$age>17,]
use_data1 <- use_data1[use_data1$gender!=3,]
use_data1 <- use_data1[complete.cases(use_data1$walking_distance_m),]
use_data1 <-use_data1[use_data1$walking_distance_m<6000,]
use_data1 <- use_data1[use_data1$living_time<=2,]



use_data_std1 <- use_data1

continuous_vars <- c("age", "education", "income", "living_time", "Future_suicide",
                     "Previous_suicidal_thoughts","walking_distance_m")

use_data_std1[continuous_vars] <- scale(use_data_std1[continuous_vars])

personality_vars <- c("Openness", "Conscientiousness", "Extraversion", 
                      "Agreeableness", "Neuroticism")
use_data_std1[personality_vars] <- scale(use_data_std1[personality_vars])


library(readxl)
long_city <- read_excel("D:/Rdaima/urban_park_accessibility/long_city.xlsx")
merged_data_filter1 <- merge(use_data_std1,long_city,by.x=c("cities_simple","date"),by.y = c("cites","date"))
unmatched_cities1 <- unique( use_data_std1$cities_simple[! use_data_std1$cities_simple %in% merged_data_filter1$cities_simple])
cat("未匹配上的城市名称：\n")
for (city in unmatched_cities1) {
  cat(city, "\n")
}

lm5_std1 <- lm(Future_suicide ~ 
                age + 
                gender +
                education + 
                income + 
                nation +
                living_time +
                Previous_suicidal_thoughts + 
                total_park_count +
                park_count_500m +
                dep+
                anx+
                stress+
                mean_temperature+
                avg_AQI.x+
                NDVI+
                GDP+
                dem_mean+
                面积+
                mean_precipitation+
                walking_distance_m * Openness +
                walking_distance_m * Conscientiousness +
                walking_distance_m * Extraversion +
                walking_distance_m * Agreeableness +
                walking_distance_m * Neuroticism,
              data = merged_data_filter1)

summary(lm5_std1)


###检验新居民中主效应的显著性

library(readxl)
merged_data_cities <- read_excel("merged_data_cities.xlsx")
use_data1 <- merged_data_cities[is.na(merged_data_cities$county),]

use_data1 <- use_data1[use_data1$age<100 &use_data1$age>17,]
use_data1 <- use_data1[use_data1$gender!=3,]
use_data1 <- use_data1[complete.cases(use_data1$walking_distance_m),]
use_data1 <-use_data1[use_data1$walking_distance_m<6000,]
use_data1 <- use_data1[use_data1$living_time<=2,]



use_data_std1 <- use_data1

continuous_vars <- c("age", "education", "income", "living_time", "Future_suicide",
                     "Previous_suicidal_thoughts","walking_distance_m")

use_data_std1[continuous_vars] <- scale(use_data_std1[continuous_vars])

personality_vars <- c("Openness", "Conscientiousness", "Extraversion", 
                      "Agreeableness", "Neuroticism")
use_data_std1[personality_vars] <- scale(use_data_std1[personality_vars])


library(readxl)
long_city <- read_excel("D:/Rdaima/urban_park_accessibility/long_city.xlsx")
merged_data_filter1 <- merge(use_data_std1,long_city,by.x=c("cities_simple","date"),by.y = c("cites","date"))
unmatched_cities1 <- unique( use_data_std1$cities_simple[! use_data_std1$cities_simple %in% merged_data_filter1$cities_simple])
cat("未匹配上的城市名称：\n")
for (city in unmatched_cities1) {
  cat(city, "\n")
}

lm5_std2 <- lm(Future_suicide ~ 
                 age + 
                 gender +
                 education + 
                 income + 
                 nation +
                 living_time +
                 Previous_suicidal_thoughts + 
                 total_park_count +
                 park_count_500m +
                 dep+
                 anx+
                 stress+
                 mean_temperature+
                 avg_AQI.x+
                 NDVI+
                 GDP+
                 dem_mean+
                 面积+
                 mean_precipitation+
                 walking_distance_m,
               data = merged_data_filter1)

summary(lm5_std2)
