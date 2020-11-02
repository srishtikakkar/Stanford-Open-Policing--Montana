#loading required libraries
library(dplyr)
library(tidyverse)
library(lubridate)
library(forcats)
library(gridExtra)
library(MASS)
library(pscl)
library(AER)
library(devtools)
library(DHARMa)

# import data
montana<- read_csv("mt_statewide_2020_04_01.csv")

#Having a look at dimensions of data and names. Have a peek at the datatypes of columns
dim(montana)
names(montana)
str(df)

#Learning about missing values in the dataframe:
na_count <-sapply(montana, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)
print(na_count)

#adding new variable district to the dataset, used as a predictor in Modeling Methods
montana<-montana%>%
  mutate(district = ifelse(county_name %in% c(" Mineral County", "Missoula County", "Ravalli County", "Sanders County"),
                           "I", ifelse(county_name %in% c("Cascade County", "Fergus County", "Golden Valley County", 
                                                          "Judith Basin County",
                                                          "Musselshell County", "Petroleum County", "Teton County", "Wheatland County"), 
                                       "II",  ifelse(county_name %in% c("Beaverhead County", "Deer Lodge County", "Granite County", 
                                                                        "Jefferson County", 
                                                                        "Lewis & Clark County", "Madison County", "Powell County", "Silver Bow counties County"),
                                                     "III", ifelse(county_name %in% c(" Big Horn County", 
                                                                                      "Carbon County", "Stillwater County", "Sweet Grass County", "Yellowstone County"),
                                                                   "IV", ifelse(county_name %in% c("Carter County ", "Custer County", 
                                                                                                   "Daniels County", "Dawson County", "Fallon County", "Garfield County", "McCone County", "Powder River County", 
                                                                                                   "Prairie County", "Richland County", "Roosevelt County",
                                                                                                   "Rosebud County", "Sheridan County", "Treasure County", "Valley County", "Wibaux County"),"V",
                                                                             ifelse(county_name %in% c("Flathead County","Lake County","Lincoln County"),
                                                                                       "VI",ifelse(county_name %in% c("Broadwater County", "Gallatin County", "Madison County", "Meagher County", 
                                                                                                                      "Park County"),"VII","VIII"))))))))
#Collapsing various categories of Reason for Stop Attribute into similar groups for better analysis
montana$reason_for_stop<-montana$reason_for_stop %>% fct_collapse(speeding = c("--- - SPEED OVER LEGAL","--- - TRUCK SPEED", "--- - RECKLESS DRIVING","--- - CARELESS DRIVING","CARELESS DRIVING","--- - SPEED BASIC RULE","SPEED","--- - HIT AND RUN","RECKLESS DRIVING"))
                                                                                                                      
montana$reason_for_stop<-montana$reason_for_stop %>% fct_collapse(toll_related = c("EXPIRED TAG ( - MONTHS OR LESS )","OTHER TAG/REGISTRATION VIOLATIONS", "TOLL EVASION","EXPIRED TAG ( MORE THAN - MONTHS )" ))
                                                                                                                      
montana$reason_for_stop<-montana$reason_for_stop %>% fct_collapse(issues_with_vehicle = c("--- - OTHER DEFECTIVE EQUIPMENT","FAULTY EQUIPMENT", "NO REGISTRATION","LOADS ON VEHICLE","--- - DEFECTIVE BRAKES","--- - DEFECTIVE EXHAUST","--- - DISPLAYING ONLY ONE LICENSE PLATE" ))
                                                                                                                      
montana$reason_for_stop<-montana$reason_for_stop %>% fct_collapse(license_issues = c("--- - VEHICLE LICENSE","OTHER DRIVERS LICENSE VIOLATIONS", "--- - FOREIGN LICENSE","--- - DRIVERS LICENSE", "DRIVING WHILE LICENSE SUSPENDED/REVOKED","--- - OVER LICENSE CAPACITY","--- - SUSPENDED OR REVOKED D/L" ))
                                                                                                                      
montana$reason_for_stop<-montana$reason_for_stop %>% fct_collapse(criminal_offence = c("OTHER CRIMINAL TRAFFIC VIOLATIONS","PROPERTY CRIMES", "OTHER CRIMINAL VIOLATIONS","CRIMES AGAINST PERSONS", "--- - CHILD RESTRAINT","--- - FURNISHING LIQUOR TO MINORS","--- - DRUGS","--- - ILLEGAL POSSESSION OF LIQUOR"))
                                                                                                                      
montana$reason_for_stop<-montana$reason_for_stop %>% fct_collapse(road_rule_violations = c("--- - FAIL TO / IMPROPER SIGNAL","--- - IMPROPER PASSING", "--- - CROSSING DIVIDER/BARRIER","--- - IMPROPER LANE TRAVEL", "--- - PEDESTRIAN","FOLLOWING TOO CLOSELY","DRIVING WITHOUT HEADLIGHTS","DRIVING ON WRONG SIDE OF ROAD","OBSTRUCTING THE ROADWAY","IMPROPER TURN ","IMPROPER PASSING","--- - IMPROPER TURN","--- - DRIVING OVER CENTERLINE","--- - FAIL TO STOP - SIGN OR LIGHT","--- - RIGHT OF WAY","IMPROPER START"))
                                                                                                                      
#Performing Exploratory Analysis

#Montana Statewide Percentage of Stops by Race/Ethnicity
df<-montana %>%
  dplyr::select(subject_race)%>%
  drop_na()%>%
  group_by(subject_race) %>%
  summarise(number_of_stops=n())%>%
  ungroup()%>%
  mutate(percentage_stops=(number_of_stops/sum(number_of_stops))*100)

#Reason for Stop by Gender
df1<-montana %>%
  dplyr::select(reason_for_stop,subject_sex)%>%
  drop_na()%>%
  group_by(reason_for_stop,subject_sex)%>%
  summarise(stops=n())%>%
  arrange(desc(stops))%>%
  head(10)%>%
  ungroup()

ggplot(df1,aes(x=reorder(reason_for_stop,stops),y=stops,fill=subject_sex))+
  geom_bar(stat="identity",position = "dodge")+
  scale_y_continuous(labels=scales::comma)+
  theme_bw()+
  xlab("Top 5 reason for stop") +
  ylab("Number ofStops")

#Number of Speeding Stops by Hour and Outcome
df2<-montana %>%
  dplyr::select(reason_for_stop,time,outcome)%>%
  drop_na()%>%
  group_by(reason_for_stop,hour=hour(time),outcome)%>%
  summarise(speeding_stops=n())%>%
  arrange(desc(speeding_stops))%>%
  filter(reason_for_stop=="speeding")%>%
  ungroup()

ggplot(df2,aes(x=hour,y=speeding_stops,col=outcome))+
  geom_point()+
  geom_line()+
  scale_y_continuous(labels=scales::comma)+
  theme_bw()+
  xlab("Time at which speeding stop occurred") +
  ylab("Number of Speeding Stops")

#Number of speeding stops by Driver Gender and Outcome
df3<-montana %>%
  dplyr::select(reason_for_stop,subject_sex,outcome,time)%>%
  drop_na()%>%
  group_by(reason_for_stop,subject_sex,outcome,hour=hour(time))%>%
  summarise(speeding_stops=n())%>%
  arrange(desc(speeding_stops))%>%
  filter(reason_for_stop=="speeding")%>%
  ungroup()

ggplot(df3, aes(hour, speeding_stops,col=outcome)) +
 geom_point()+
  geom_line()+
  facet_grid(subject_sex~.)+
  theme_bw()

#generating poisson distribution with different values of  λ,the parameter describing the rate, 
#that is the mean of the distribution
x <- seq(0,20,5)
plot(x, dpois(x, lambda=1),type = "o")
plot(x, dpois(x, lambda=3),type="o")
plot(x, dpois(x, lambda=5),type="o")
plot(x, dpois(x, lambda=10),type="o")

#Histogram for the number of speeding stops in each group of outcome (knowing the distribution of our repsonse
#under each outcome group)
g0 <- ggplot(data=speeding_stops_a,aes(x=speedingstops))
g1<-g0 +geom_histogram(fill="grey",colour="black") + theme_bw() + xlab("# of speeding stops for arrest")
g2 <- ggplot(data=speeding_stops_c,aes(x=speedingstops))
g3<-g2 +geom_histogram(fill="grey",colour="black") + theme_bw() + xlab("# of speeding stops for citation")
g4 <- ggplot(data=speeding_stops_w,aes(x=speedingstops))
g5<-g4 +geom_histogram(fill="grey",colour="black") + theme_bw()+ xlab("# of speeding stops for warning")
grid.arrange(g5, g3, g1, nrow = 1)

#preparing data for modeling for each outcome group (Warning, Citation and Arrest) with variables of interest
speeding_stops_w<-montana%>%
  dplyr::select(reason_for_stop,subject_sex,time,outcome,district)%>%
  group_by(reason_for_stop,subject_sex,outcome,hour=hour(time),district)%>%
  summarise(speedingstops=n())%>%
  filter(reason_for_stop=="speeding")%>%
  filter(outcome=="warning")%>%
  arrange(desc(speedingstops))%>%
  drop_na()%>%
  ungroup()

speeding_stops_c<-montana%>%
  dplyr::select(reason_for_stop,subject_sex,time,outcome,district)%>%
  group_by(reason_for_stop,subject_sex,outcome,hour=hour(time),district)%>%
  summarise(speedingstops=n())%>%
  filter(reason_for_stop=="speeding")%>%
  filter(outcome=="citation")%>%
  arrange(desc(speedingstops))%>%
  drop_na()%>%
  ungroup()

speeding_stops_a<-montana%>%
  dplyr::select(reason_for_stop,subject_sex,time,outcome,district)%>%
  group_by(reason_for_stop,subject_sex,outcome,hour=hour(time),district)%>%
  summarise(speedingstops=n())%>%
  filter(reason_for_stop=="speeding")%>%
  filter(outcome=="arrest")%>%
  arrange(desc(speedingstops))%>%
  drop_na()%>%
  ungroup()

#fitting regression models
# normal theory regression using maximum likelihood 
normal <- glm(speedingstops ~ subject_sex+hour+district,
              data = speeding_stops_w, family = gaussian(link="identity"))
normal_c <- glm(speedingstops ~ subject_sex+hour+district,
                data = speeding_stops_c, family = gaussian(link="identity"))
normal_a <- glm(speedingstops ~ subject_sex+hour+district,
                data = speeding_stops_a, family = gaussian(link="identity"))
# summary of results
summary(normal) 
summary(normal_c)
summary(normal_a)

#poisson regression model and summary of results
summary(mp <- glm(speedingstops ~ subject_sex+hour+district, family=poisson,data=speeding_stops_w))
summary(mp_c <- glm(speedingstops ~ subject_sex+hour+district, family=poisson,data=speeding_stops_c))
summary(mp_a <- glm(speedingstops ~ subject_sex+hour+district, family=poisson,data=speeding_stops_a))

#negative binomial regression and summary of results
summary(mnb <- glm.nb(speedingstops ~ subject_sex+hour+district, data=speeding_stops_w))
summary(mnb_c <- glm.nb(speedingstops ~ subject_sex+hour+district, data=speeding_stops_c))
summary(mnb_a <- glm.nb(speedingstops ~ subject_sex+hour+district, data=speeding_stops_a))

#Model Diagnostics
# normal residuals density plot
par(mfrow=c(1,3))
plot(density(residuals(normal)),main = "Residual for linear regression (warning group)")
plot(density(residuals(normal_c)),main = "Residual for linear regression (citation group)")
plot(density(residuals(normal_a)), main = "Residual for linear regression (arrest group)")

#Predicted values vs. residual plot. 
plot(predict(normal, type="response"), residuals(normal), main="Typical
Regression", ylab="Residuals", xlab="Predicted")
abline(h=0,lty=1,col="gray")
lines(lowess(predict(normal,type="response"),residuals(normal)), lwd=2, lty=2)

plot(predict(normal_c, type="response"), residuals(normal_c), main="Typical
Regression", ylab="Residuals", xlab="Predicted")
abline(h=0,lty=1,col="gray")
lines(lowess(predict(normal_c,type="response"),residuals(normal_c)), lwd=2, lty=2)

plot(predict(normal_a, type="response"), residuals(normal_a), main="Typical
Regression", ylab="Residuals", xlab="Predicted")
abline(h=0,lty=1,col="gray")
lines(lowess(predict(normal_a,type="response"),residuals(normal_a)), lwd=2, lty=2)

#over-dispersion test on poisson regression model
dispersiontest(mp,trafo = 1)
sim_mp <- simulateResiduals(fittedModel = mp,n = 250,refit = T)
testDispersion(sim_mp)
plot(sim_mp)

# diagnostic plots for negative binomial model 
par(mfrow=c(1,3))
plot(as.factor(speeding_stops_w$subject_sex),resid(mqp),xlab="Sex (0 = Female, 1 = Male)",
     ylab="Residuals")
plot(speeding_stops_w$hour,resid(mqp),xlab="Hour", ylab="Residuals")
plot(as.factor(speeding_stops_w$district),resid(mqp),xlab="District (1 = I,
2 = II, 3=III, 4=IV, 5=V, 6=VI, 7=VII, 8=VIII)", ylab="Residuals")

#Estimated rates at which people of different districts were stopped for different categories of outcome, 
#as estimated from Poisson regression using districtI and female driver as a baseline. 
#Rates are plotted on an exponent scale.  
a1<-data.frame(exp(mnb$coefficients))
b1<-data.frame(exp(mnb_c$coefficients))
c1<-data.frame(exp(mnb_a$coefficients))
a1$rate_warning<-a1$exp.mnb.coefficients.
a1$parameters<-row.names(a1)
a1<-a1[-1]
b1$rate_citation<-b1$exp.mnb_c.coefficients.
b1$parameters<-row.names(b1)
b1<-b1[-1]
c1$rate_arrest<-c1$exp.mnb_a.coefficients.
c1$parameters<-row.names(c1)
c1<-c1[-1]
d1<-cbind(a1,b1,c1)
d1<-d1[-1,]
d1<-d1[,c(-2,-4)]
data_long1 <- gather(d1, outcome, rate, rate_warning:rate_arrest, factor_key=TRUE)
data_long1$parameters<-as.factor(data_long1$parameters)
ggplot(data = data_long1, aes(x=parameters, y=rate,col=outcome,group=1)) +
  geom_line()+
  geom_point()+
  ggtitle("Estimated Rates for each Outcome Group") +
  xlab("Variables")+
  facet_grid(outcome ~ .)+
  ylab("Rate of Speeding Stops")+
  theme_bw()+
  theme(legend.position = "none")

#Model Comparison
#overlaying model fit on distribution of data to check best fitted model
# predicted values for linear regression (done for warning group)
normal.y.hat <- predict(normal, type = "response")
normal.y <- normal$y
normal.yUnique <- 0:max(normal.y)
normal.nUnique <- length(normal.yUnique)
phat.normal <- matrix(NA, length(normal.y.hat), normal.nUnique)
dimnames(phat.normal) <- list(NULL, normal.yUnique)
for (i in 1:normal.nUnique) {
  phat.normal[, i] <- dnorm(mean = normal.y.hat, sd = sd(residuals(normal)),x =
                              normal.yUnique[i])
}
# mean of the normal predicted probabilities for each value of the outcome
phat.normal.mn <- apply(phat.normal, 2, mean)

#predcitions for poisson and negative binomial model
phat.pois <- predprob(mp)
phat.pois.mn <- apply(phat.pois, 2, mean) 
phat.nb <- predprob(mnb)
phat.nb.mn <- apply(phat.nb, 2, mean)

#overlaying predictions on data distribution to check best fit model
par(mfrow=c(1,1))
hist(speeding_stops_w$speedingstops, probability = TRUE, 
     main = "Distribution for Warning group", xlab = "Speeding Stops", 
     breaks = 30)
lines(x = seq(0,3000,length.out = 3044), y = phat.normal.mn, pch = 18,col = "blue", lty = 1)
lines(x = seq(0, 3000, length.out = 3044), y = phat.pois.mn,pch = 19, col = "red", lty=2) 
lines(x = seq(0, 3000, length.out = 3044), y = phat.nb.mn, pch = 22, col = "chartreuse4", lty=3) 
# Add a legend to the plot
legend("topright", legend=c("Normal Distribution", "Poisson", "Negative Binomial"),
       col=c("blue", "red", "green"), lty = 1:3, cex=0.5)

#Model Comparsion using AIC (warning group)
compare.models <- list( )
compare.models[[1]] <- normal 
compare.models[[2]] <- mp
compare.models[[3]] <- mnb
compare.names <- c("Normal","Poisson","NB")
compare.results <- data.frame(models = compare.names)
compare.results$aic.val <- unlist(lapply(compare.models, AIC))
compare.results$aic.delta <- compare.results$aic.val-min(compare.results$aic.val)
compare.results$aic.likelihood <- exp(-0.5* compare.results$aic.delta)
compare.results$aic.weight <-compare.results$aic.likelihood/sum(compare.results$aic.likelihood)

#Selecting best features for negative binomial regression model(done)
cand.models <- list( )
cand.models[[1]] <- glm.nb(speedingstops ~ subject_sex+hour+district, data = speeding_stops_w)
cand.models[[2]] <- glm.nb(speedingstops ~ subject_sex+hour, data = speeding_stops_w)
cand.models[[3]] <- glm.nb(speedingstops ~ hour+district, data
                           = speeding_stops_w)
cand.models[[4]] <- glm.nb(speedingstops ~ subject_sex+district,
                           data = speeding_stops_w)
model.names <- c("Full", "sex/hour","hour/district","sex/district")
names(cand.models) <- model.names
results <- data.frame(models = model.names)
results$bic.val <- unlist(lapply(cand.models, BIC))
results$bic.rank <- rank(results$bic.val)
results$aic.val <- unlist(lapply(cand.models, AIC))
results$aic.delta <- results$aic.val-min(results$aic.val) 
results$aic.likelihood <- exp(-0.5* results$aic.delta)
results$aic.weight <- results$aic.likelihood/sum(results$aic.likelihood)
results <- results[rev(order(results[, "aic.weight"])),]
results$cum.aic.weight <- cumsum(results[, "aic.weight"])
