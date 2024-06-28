library(readxl)
library(ggplot2)
library(dplyr)
library("corrplot")
library(mgcv)
library(gridExtra)

#################### EDA
##### processing the population data
pop_data <- read_excel("population.xlsx", sheet = "Table_2")[-c(1:5),]
ncol(pop_data)
colnames(pop_data)[c(1:5,96)] <- c("hbt", "AN", "sex", "y", "all_ages","90+")
colnames(pop_data)[6:95] <- as.character(0:89)
pop_data_1 <- pop_data[pop_data$sex=="Persons" & pop_data$hbt!="S92000003",]
head(pop_data_1)

# population trend
pop_data_1$y <- as.numeric(pop_data_1$y)
pop_data_2 <- pop_data_1[pop_data_1$y>=2018,]
ggplot(pop_data_2, aes(x = y, y = all_ages, color = hbt, group = hbt)) +
  geom_line() +
  labs(x = "Year", y = "All Ages", title = "All Ages Change from 2018 to 2021 by hbt") +
  theme_minimal() +
  scale_color_discrete(name = "hbt") +
  facet_wrap(~ hbt, scales = "free_y", ncol = 4)

##### processing the monthly data
month_data <- read.csv("monthly.csv")[,-2]
mdnames <- colnames(month_data);mdnames

colnames(month_data) <- c("month","hbt","loc","type","day","we","hour","inout","total")
month_data$type[month_data$type=="Emergency Department"] <- "AE"
month_data$type[month_data$type!="AE"] <- "MIU"

uh <- unique(month_data$hour);month_data$h <- month_data$total*0 - 1 ## hour of day
for (i in 1:length(uh)) month_data$h[month_data$hour==uh[i]] <- i-1
month_data$m <- month_data$month %% 100 ## month of year
month_data$y <- month_data$month %/% 100 ## year
ud <- unique(month_data$day);id <- c(5,1,6,7,4,2,3)
month_data$dow <- month_data$y
for (i in 1:7) month_data$dow[month_data$day==ud[i]] <- id[i] ## day of week
month_data$cm <- (month_data$y-2018)*12 + month_data$m ## cumulative month
month_data$ch <- (month_data$dow-1)*24+month_data$h ## cumulative hour of week

# add the population into the month data
month_data <- month_data %>%
  left_join(pop_data_2 %>% select(hbt, y, pop = all_ages), by = c("hbt", "y"))

# add covid and seasonal factors into the month data
month_data$covid <- ifelse(
  (month_data$y == 2020 & month_data$m >= 3) | 
    (month_data$y == 2021) | 
    (month_data$y == 2022 & month_data$m <= 3), 
  1, 0
)

month_data$season <- case_when(
  month_data$m %in% 3:5 ~ "Spring",
  month_data$m %in% 6:8 ~ "Summer",
  month_data$m %in% 9:11 ~ "Autumn",
  month_data$m %in% c(12, 1, 2) ~ "Winter",
  TRUE ~ NA_character_
)
head(month_data)

## pick a location and plot weekly cycle each month for first 6 years...
month_data_e <- month_data[month_data$type!="MIU",]
month_data_e$day <- factor(month_data_e$day)
location_month <- unique(month_data_e$loc)

## trend in a day
par(mfrow=c(2,3))
for (loci in location_month) {
  dat <- month_data_e[month_data_e$loc==loci,]
  for (yi in 2018:2023) {
    yl <- range(dat$total)
    with(dat[dat$y==yi&dat$m==1&dat$dow==1,],
         plot(h,total,type="l",ylim=yl,
              xlab="hour of a day",ylab=unique(dat$loc),main=yi))
    for (dow in 2:7) with(dat[dat$y==yi&dat$m==1&dat$dow==dow,],
                         lines(h,total,col=dow))
  }
  readline()
}

## trend in a month
par(mfrow=c(2,3))
for (loci in location_month) {
  dat <- month_data_e[month_data_e$loc==loci,]
  for (yi in 2018:2023) {
    yl <- range(dat$total)
    with(dat[dat$y==yi&dat$m==1,],
         plot(ch[order(ch)],total[order(ch)],type="l",ylim=yl,
              xlab="hour of week",ylab=unique(dat$loc),main=yi))
    for (m in 2:12) with(dat[dat$y==yi&dat$m==m,],
                         lines(ch[order(ch)],total[order(ch)],col=m))
  }
  readline()
}

## trend in 2018-2023
all_locations_data <- data.frame()

for (loci in location_month) {
  dat <- month_data_e[month_data_e$loc==loci,]
  cm_total_sum <- aggregate(total ~ cm, data = dat, sum)
  cm_total_sum$loc <- loci
  all_locations_data <- rbind(all_locations_data, cm_total_sum)
}

head(all_locations_data)

ggplot(all_locations_data, aes(x = cm, y = total, color = loc, group = loc)) +
  geom_line() +
  labs(title = "CM vs Total attendances for All Locations",
       x = "CM",
       y = "Total",
       color = "Location") +
  theme_minimal()

##### processing the weekly data
week_data <- read.csv("weekly.csv")[,-c(2,5)]
wdnames <- colnames(week_data); wdnames
colnames(week_data) <- c("wend","hbt","loc","total","lt4","gt4","pc.lt4",
                   "gt8","pc.gt8","gt12","pc.gt12")

week_data$dom <- week_data$wend %% 100 ## day of month
dm <- week_data$wend %/%100 ## year month
week_data$m <- dm %% 100 ## month
week_data$y <- dm %/% 100 ## year
week_data$date <- as.Date(as.character(week_data$wend),"%Y%m%d")
week_data$julian <- julian(week_data$date,origin=as.Date("2015-02-22"))

# add covid and seasonal factors into the week data
week_data$covid <- ifelse(
  (week_data$y == 2020 & week_data$m >= 3) | 
    (week_data$y == 2021) | 
    (week_data$y == 2022 & week_data$m <= 3), 
  1, 0
)

week_data$season <- case_when(
  week_data$m %in% 3:5 ~ "Spring",
  week_data$m %in% 6:8 ~ "Summer",
  week_data$m %in% 9:11 ~ "Autumn",
  week_data$m %in% c(12, 1, 2) ~ "Winter",
  TRUE ~ NA_character_
)
head(week_data)

## weekly data plots. black total, green <4h, blue 4-8h, red >8h
location_week <- unique(week_data$loc)[-c(16,17)]
par(mfrow=c(5,6),mar=c(4,4,1,1))
for (loci in location_week) {
  with(week_data,plot(y<-total[loc==loci],type="l",ylab=loci,ylim=c(0,max(y))))
  with(week_data,lines(lt4[loc==loci],col="lightgreen"))
  with(week_data,lines(gt4[loc==loci]-gt8[loc==loci],col="blue"))
  with(week_data,lines(gt8[loc==loci],col="red"))
}

## seasonal trend
year_week <- unique(week_data$y)[-10]
par(mfrow=c(3,3),mar=c(4,4,1,1))
for (i in year_week) {
  with(week_data[week_data$loc==location_week[1],],plot(y<-total[y==i],type="l",ylab=i,ylim=c(0,max(y))))
  with(week_data[week_data$loc==location_week[1],],lines(lt4[y==i],col=3))
  with(week_data[week_data$loc==location_week[1],],lines(gt4[y==i]-gt8[y==i],col=4))
  with(week_data[week_data$loc==location_week[1],],lines(gt8[y==i],col=2))
}

## correlation
cor_matrix <- cor(week_data[, c("total", "lt4", "gt4", "pc.lt4", 
                                "gt8", "pc.gt8", "gt12", "pc.gt12","julian","y",
                                "m","covid")])
#print(cor_matrix)
par(mfrow=c(1,1),mar=c(4,4,1,1))

corrplot(cor_matrix, method = "color", col=colorRampPalette(c("blue", "white", "red"))(200), 
         type = "upper", order = "hclust", 
         addCoef.col = "black", 
         tl.col = "black", tl.srt = 45,
         diag = FALSE) 

##### processing discharge data
dis_data <- read.csv("discharge.csv")[,-2]
dsnames <- colnames(dis_data);dsnames

colnames(dis_data) <- c("month","hbt","loc","type","age","ageqf","dis","disqf","total")
dis_data$type[dis_data$type=="Emergency Department"] <- "AE"
dis_data$type[dis_data$type!="AE"] <- "MIU"
dis_data$dis_id[dis_data$dis=="Admission to same Hospital"] <- 1
dis_data$dis_id[dis_data$dis!="Admission to same Hospital"] <- 0
dis_data$m <- dis_data$month %% 100 ## month of year
dis_data$y <- dis_data$month %/% 100 ## year
dis_data_e <- dis_data[dis_data$type!="MIU",]
head(dis_data_e)

# divide ages into four categories
dis_data <- dis_data %>%
  mutate(age_group = case_when(
    age %in% c("Under 18", "18-24" ) ~ "Young people",
    age %in% c("25-39", "40-64") ~ "Middle-aged people",
    age %in% c("65-74", "75 plus") ~ "Elderly people",
    TRUE ~ "Others"
  ))

# groups and aggregates numbers by age group and discharge status
dis_data_sum <- dis_data %>%
  filter(age_group %in% c("Young people", "Middle-aged people", 
                          "Elderly people", "Others")) %>%
  group_by(age_group, dis_id) %>%
  summarise(total = sum(total), .groups = 'drop') %>%
  mutate(age_group = factor(age_group, 
                            levels = c("Young people", "Middle-aged people", 
                                       "Elderly people", "Others")))

ggplot(dis_data_sum, aes(x = age_group, y = total, fill = factor(dis_id))) +
  geom_bar(stat = "identity") +
  labs(x = "age groups", y = "number of people", fill = "diacharge status", 
       title = "Comparison of hospitalizations and discharges by age group") +
  scale_fill_manual(values = c("0" = "blue", "1" = "red"), 
                    labels = c("discharged", "not discharged")) +
  theme_minimal()


########## model 1 attendance
month_data_e$season <- factor(month_data_e$season)
month_data_e$inout <- factor(month_data_e$inout)
head(month_data_e)

## gam model choosing
dat1 <- month_data_e[month_data_e$loc==location_month[1],]
gam_m1 <- gam(total~s(h,bs="cc",k=20)+ 
              s(ch,k=80,bs="ad")+ 
              s(m,bs="cc")+
              inout+
              covid+ 
              season+
              te(m,y,bs=c("cc","cr"))+
              te(m,cm,k=c(10,15),bs=c("cc","cr")),
            family=tw,data=dat1,knots=list(m=c(0,12),ch=c(0,168),h=c(0,24)))

anova(gam_m1); AIC(gam_m1)
par(mfrow=c(2,2))
gam.check(gam_m1)

gam_m2 <- gam(total~s(h,bs="cc",k=20)+ 
                s(ch,k=80,bs="ad")+ 
                s(m,bs="cc")+
                covid+ 
                season+
                te(m,y,bs=c("cc","cr"))+
                te(m,cm,k=c(10,15),bs=c("cc","cr")),
              family=tw,data=dat1,knots=list(m=c(0,12),ch=c(0,168),h=c(0,24)))

anova(gam_m2); AIC(gam_m2)
par(mfrow=c(2,2))
gam.check(gam_m2)

## split the train and test sets
start_year <- 2023
end_year <- 2024
start_months <- c(5, 6, 7, 8, 9, 10, 11)
end_months <- c(10, 11, 12, 1, 2, 3, 4)

# set up the training and test sets
train_sets <- list()
test_sets <- list()

for(i in 1:length(start_months)){
  
  test_start_year <- start_year
  test_start_month <- start_months[i]
  test_end_year <- start_year + ifelse(end_months[i] >= 10,0,1)
  test_end_month <- (end_months[i] - 1) %% 12 + 1
  
  # set up the training sets and test sets
  train_set <- month_data_e[1:(which(month_data_e$y ==  test_start_year 
                                     & month_data_e$m ==  test_start_month)[1]-1),]
  
  test_set <-  month_data_e[which(month_data_e$y ==  test_start_year 
                                  & month_data_e$m ==  test_start_month)[1]
                            :tail(which(month_data_e$y ==  test_end_year& 
                                          month_data_e$m ==  test_end_month),1),]
  
  # add them to the lists
  train_sets[[i]] <- train_set
  test_sets[[i]] <- test_set
}

##### gam model
# generate 5 numbers in 1-30
set.seed(66)
rm <- sample(1:30, 5)

for(loci in location_month[rm]){
  
  rmse_gam <- numeric(length(train_sets))
  
  for(i in 1:length(train_sets)){
    
    train_set <- train_sets[[i]][train_sets[[i]]$loc==loci,]
    test_set <- test_sets[[i]][test_sets[[i]]$loc==loci,]
    
    # train the model
    m1_1 <- gam(total~s(h,bs="cc",k=20)+ 
                  s(ch,k=80,bs="ad")+ 
                  s(m,bs="cc")+
                  covid+ 
                  season+
                  te(m,y,bs=c("cc","cr"))+
                  te(m,cm,k=c(10,15),bs=c("cc","cr")),
                family=tw,data=train_set,knots=list(m=c(0,12),ch=c(0,168),h=c(0,24)))
    
    # test the model
    predictions <- predict(m1_1, newdata=test_set, type="response")
    
    # calculate the rmse
    rmse <- sqrt(mean((test_set$total - predictions)^2))
    rmse_gam[i] <- rmse
  }
  cat("rmse of gam of month data for", loci, "are:", rmse_gam , "\n")
}

##### glm model
for(loci in location_month[rm]){
  
  rmse_gl <- numeric(length(train_sets))
  
  for (i in 1:length(train_sets)) {
    train_set <- train_sets[[i]][train_sets[[i]]$loc==loci,]
    test_set <- test_sets[[i]][test_sets[[i]]$loc==loci,]
    
    # build the glm model
    glm_1 <- glm(total ~ h + ch + m + covid + season + m*y + m*cm, 
                 family=gaussian, data=train_set)
    
    # predict in the test sets
    predictions <- predict(glm_1, newdata=test_set, type="response")
    
    # calculate the rmse value
    rmse_gl_1 <- sqrt(mean((test_set$total - predictions)^2))
    rmse_gl[i] <- rmse_gl_1
  }
  cat("rmse of glm of month data for", loci, "are:", rmse_gl , "\n")
}

########## model 2 waiting time
week_data$loc <- factor(week_data$loc)
week_data$season <- factor(week_data$season)
#head(week_data)

# model selection
gam_w1 <- gam(cbind(lt4,gt4)~s(dom,bs="cr") + 
                s(m,bs="cr")+ 
                s(y,k=9,bs="cr")+
                s(loc, bs="re") + 
                s(julian, k=40, bs="cr")+
                season+
                covid,
              family=binomial,data=week_data)
anova(gam_w1); AIC(gam_w1)
par(mfrow=c(2,2))
gam.check(gam_w1)

gam_w2 <- gam(cbind(lt4,gt4)~ s(dom,bs="cr")+
                s(y,k=9,bs="cr")+
                s(loc, bs="re") + 
                s(julian, k=40, bs="cr")+
                season+
                covid,
              family=binomial,data=week_data)
anova(gam_w2); AIC(gam_w2)
par(mfrow=c(2,2))
gam.check(gam_w2)

gam_w3 <- gam(cbind(lt4,gt4)~ s(y,k=9,bs="cr")+
                s(loc, bs="re") + 
                s(julian, k=40, bs="cr")+
                season+
                covid,
              family=binomial,data=week_data)
anova(gam_w3); AIC(gam_w3)
par(mfrow=c(2,2))
gam.check(gam_w3)

# set up the variables
start_year_w <- 2023
end_year_w <- 2024
start_months_w <- c(6, 7, 8, 9, 10, 11, 12)
end_months_w <- c(11, 12, 1, 2, 3, 4, 5)

# set up the training and test sets
train_sets_w <- list()
test_sets_w <- list()

for(i in 1:length(start_months_w)){
  
  test_start_year_w <- start_year_w
  test_start_month_w <- start_months_w[i]
  test_end_year_w <- start_year_w + ifelse(end_months_w[i] >= 11,0,1)
  test_end_month_w <- (end_months_w[i] - 1) %% 12 + 1
  
  # set up the training sets and test sets
  train_set_w <- week_data[1:(which(week_data$y ==  test_start_year_w 
                             & week_data$m ==  test_start_month_w)[1]-1),]
  
  test_set_w <-  week_data[which(week_data$y ==  test_start_year_w 
                          & week_data$m ==  test_start_month_w)[1]
                    :tail(which(week_data$y ==  test_end_year_w& 
                                  week_data$m ==  test_end_month_w),1),]
  
  # add them to the lists
  train_sets_w[[i]] <- train_set_w
  test_sets_w[[i]] <- test_set_w
}

##### gam model
## gam1
rmse_w <- numeric(length(train_sets_w))

for(i in 1:length(train_sets_w)){
  
  train_set_w <- train_sets_w[[i]]
  test_set_w <- test_sets_w[[i]]
  
  # train the model
  m2_1 <- gam(cbind(lt4,gt4)~s(dom,bs="cr") + 
               s(m,bs="cr")+ 
               s(y,k=9,bs="cr")+
               s(loc, bs="re") + 
               s(julian, k=20, bs="cr")+
               season+
               covid,
             family=binomial,data=train_set_w)
  
  # test the model
  predictions <- predict(m2_1, newdata=test_set_w, type="response")
  
  # calculate the rmse
  rmse <- sqrt(mean(((test_set_w$pc.lt4)/100 - predictions)^2))
  
  rmse_w[i] <- rmse
}

cat("rmse of gam1 of week data:",rmse_w, "\n")

## gam 2
rmse_w_1 <- numeric(length(train_sets_w))

for(i in 1:length(train_sets_w)){
  
  train_set_w <- train_sets_w[[i]]
  test_set_w <- test_sets_w[[i]]
  
  # train the model
  m2_2 <- gam(cbind(lt4,gt4)~s(dom,bs="cr") + 
                s(y,k=9,bs="cr")+
                s(loc, bs="re") + 
                s(julian, k=20, bs="cr")+
                season+
                covid,
              family=binomial,data=train_set_w)
  
  # test the model
  predictions <- predict(m2_2, newdata=test_set_w, type="response")
  
  # calculate the rmse
  rmse <- sqrt(mean(((test_set_w$pc.lt4)/100 - predictions)^2))
  
  rmse_w_1[i] <- rmse
}

cat("rmse of gam2 of week data:",rmse_w_1, "\n")

## gam 3
rmse_w_2 <- numeric(length(train_sets_w))

for(i in 1:length(train_sets_w)){
  
  train_set_w <- train_sets_w[[i]]
  test_set_w <- test_sets_w[[i]]
  
  # train the model
  m2_3 <- gam(cbind(lt4,gt4)~
                s(y,k=9,bs="cr")+
                s(loc, bs="re") + 
                s(julian, k=20, bs="cr")+
                season+
                covid,
              family=binomial,data=train_set_w)
  
  # test the model
  predictions <- predict(m2_3, newdata=test_set_w, type="response")
  
  # calculate the rmse
  rmse <- sqrt(mean(((test_set_w$pc.lt4)/100 - predictions)^2))
  
  rmse_w_2[i] <- rmse
}

cat("rmse of gam3 of week data:",rmse_w_2, "\n")


##### glm model
rmse_gl_w <- numeric(length(train_sets_w))

for(i in 1:length(train_sets_w)){
  train_set_w <- train_sets_w[[i]]
  test_set_w <- test_sets_w[[i]]
  
  # build the glm model
  glm_2 <- glm(cbind(lt4,gt4) ~dom + m + y + loc + julian + season + covid, 
               family=binomial, data=train_set_w)
  
  # predict in the test sets
  predictions_gl <- predict(glm_2, newdata=test_set_w, type="response")
  
  # calculate the rmse value
  rmse_gl_2 <- sqrt(mean(((test_set_w$pc.lt4)/100 - predictions_gl)^2))
  
  rmse_gl_w[i] <- rmse_gl_2
}

cat("rmse of glm1 of week data:",rmse_gl_w, "\n")

## glm2
rmse_gl_w2 <- numeric(length(train_sets_w))

for(i in 1:length(train_sets_w)){
  train_set_w <- train_sets_w[[i]]
  test_set_w <- test_sets_w[[i]]
  
  # build the glm model
  glm_3 <- glm(cbind(lt4,gt4) ~ dom + y + loc + julian + season + covid, 
               family=quasibinomial, data=train_set_w)
  
  # predict in the test sets
  predictions_gl <- predict(glm_3, newdata=test_set_w, type="response")
  
  # calculate the rmse value
  rmse_gl_2 <- sqrt(mean(((test_set_w$pc.lt4)/100 - predictions_gl)^2))
  
  rmse_gl_w2[i] <- rmse_gl_2
}

cat("rmse of glm2 of week data:",rmse_gl_w2, "\n")

## glm3
rmse_gl_w3 <- numeric(length(train_sets_w))

for(i in 1:length(train_sets_w)){
  train_set_w <- train_sets_w[[i]]
  test_set_w <- test_sets_w[[i]]
  
  # build the glm model
  glm_4 <- glm(cbind(lt4,gt4) ~ y + loc + julian + season + covid, 
               family=binomial, data=train_set_w)
  
  # predict in the test sets
  predictions_gl <- predict(glm_4, newdata=test_set_w, type="response")
  
  # calculate the rmse value
  rmse_gl_2 <- sqrt(mean(((test_set_w$pc.lt4)/100 - predictions_gl)^2))
  
  rmse_gl_w3[i] <- rmse_gl_2
}

cat("rmse of glm3 of week data:",rmse_gl_w3, "\n")

# plot
combine1 <- data.frame(
  Model = rep(c("GAM1", "GLM1"), each = 7), 
  Prediction = rep(1:7, 2),  
  RMSE = c(rmse_w, rmse_gl_w)
)

combine2 <- data.frame(
  Model = rep(c("GAM2", "GLM2"), each = 7), 
  Prediction = rep(1:7, 2),  
  RMSE = c(rmse_w_1, rmse_gl_w2)
)

combine3 <- data.frame(
  Model = rep(c("GAM3", "GLM3"), each = 7), 
  Prediction = rep(1:7, 2),  
  RMSE = c(rmse_w_2, rmse_gl_w3)
)

p1 <- ggplot(combine1, aes(x = as.factor(Prediction), y = RMSE, fill = Model)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  labs(x = "Prediction", y = "RMSE", title = "RMSE of GAM1 and GLM1") +
  scale_fill_manual(values = c("#4E79A7", "#F28E2B")) +  
  theme_minimal()  

p2 <- ggplot(combine2, aes(x = as.factor(Prediction), y = RMSE, fill = Model)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  labs(x = "Prediction", y = "RMSE", title = "RMSE of GAM2 and GLM2") +
  scale_fill_manual(values = c("#4E79A7", "#F28E2B")) +  
  theme_minimal()  

p3 <- ggplot(combine3, aes(x = as.factor(Prediction), y = RMSE, fill = Model)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  labs(x = "Prediction", y = "RMSE", title = "RMSE of GAM3 and GLM3") +
  scale_fill_manual(values = c("#4E79A7", "#F28E2B")) +  
  theme_minimal() 

grid.arrange(p1, p2, p3, ncol = 2)








