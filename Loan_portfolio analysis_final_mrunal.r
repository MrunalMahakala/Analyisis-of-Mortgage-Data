install.packages("dplyr")
install.packages("ggplot2")
install.packages("forecast")
install.packages("readxl")
install.packages("zoo")
install.packages("VIM")
install.packages("cowplot") ##for cowplot grid plot multiple plots
install.packages("fastDummies")
install.packages("rpart")#for decision tree
install.packages("rpart.plot")
install.packages("randomForest")
install.packages("caret")
install.packages("cluster")
install.packages("stats")#Kmeans
install.packages("psych")
install.packages("tidyr")
install.packages("gridExtra")
install.packages("neuralnet")
install.packages("e1071")
install.packages("C50")
install.packages("tseries")#adf test


#####loading libraries#############
library(tseries)
library(e1071)
library(neuralnet)
library(gridExtra)
library(tidyr)
library(C50)
library(stats)
library(caret)
library(randomForest)
library(rpart.plot)
library(rpart)
library(ggplot2)
library(forecast)
library(corrplot)
library(zoo)
library(cowplot)
library(VIM)
library(fastDummies)# for dummies creation
library(psych)
library(dplyr)
library(readr)

Mortgage <- read.csv("~/A R-Directory/New folder/Mortgage.csv")
View(Mortgage)



#################Aggregating the data
Summarized_data<- Mortgage %>%
  group_by(id) %>%
  summarise(
    time = max(time),
    orig_time = min(orig_time),
    first_time = min(first_time),
    mat_time = max(mat_time),
    balance_time = last(balance_time),
    LTV_time = last(LTV_time),
    interest_rate_time = mean(interest_rate_time),
    hpi_time = last(hpi_time),
    gdp_time = mean(gdp_time),
    uer_time = mean(uer_time),
    REtype_CO_orig_time = max(REtype_CO_orig_time),
    REtype_PU_orig_time = max(REtype_PU_orig_time),
    REtype_SF_orig_time = max(REtype_SF_orig_time),
    investor_orig_time = max(investor_orig_time),
    balance_orig_time = last(balance_orig_time),
    FICO_orig_time = mean(FICO_orig_time),
    LTV_orig_time = mean(LTV_orig_time),
    Interest_Rate_orig_time = mean(Interest_Rate_orig_time),
    hpi_orig_time = mean(hpi_orig_time),
    default_time = max(default_time),
    payoff_time = max(payoff_time),
    status_time = max(status_time)
  )
str(Summarized_data)
View(Summarized_data)




#deriving the terms of loan completed as length of loan and terms completed from length of loan
Summarized_data <- Summarized_data %>%
  mutate(Length_of_Loan = mat_time - orig_time) %>%
  mutate(terms_completed = abs(time)- orig_time)
print(Summarized_data[,c(24,25)])
#terms_completed-> start of loan period + latest time period (since we summarized time to latest time)
#Removing the Active customer records and add to separete data as we want to predict the probability of active customer being defaulted.
data_base_active_customers <- Summarized_data %>%
  filter(status_time == 0)
Summarized_data <- anti_join(Summarized_data, data_base_active_customers, by = "id")

#transformation of Aggregated dataset
# removing id and status
Summarized_data <- Summarized_data[, !names(Summarized_data) %in% c("id","status_time")]

############################Summary statistics########


######Aggregated data #############


summary(Summarized_data)
############Visualizing NA values
missing_prop <- colMeans(is.na(Summarized_data))

sorted_vars <- names(missing_prop[order(missing_prop, decreasing = TRUE)])

aggr(Summarized_data[,sorted_vars],prop=FALSE,numbers=TRUE)
par(mfrow = c(1, 1))

####removing NA values for Aggregated data
Summarized_data<-na.omit(Summarized_data)
#re-checking NA
sum(is.na(Summarized_data))
#########################################################################################

###check for zeros########

zero_values <- sum(Summarized_data == 0, na.rm = TRUE)
zero_values
#checking for each column
zero_values <- colSums(Summarized_data == 0, na.rm = TRUE)
zero_values


#########################################################################################
#######EDA################################################################################
#####box plots for numerical #
numeric_vars <- c("FICO_orig_time", "hpi_orig_time","hpi_time","interest_rate_time", "Interest_Rate_orig_time", "balance_time","balance_orig_time", "uer_time", "gdp_time", "mat_time", "LTV_orig_time","Length_of_Loan","terms_completed")
###box plots
par(mfrow = c(4, 4)) 

for (feature in numeric_vars) {
  
  boxplot(Summarized_data[[feature]], main = feature, xlab = "Category", 
          ylab = "Frequency", 
          col = c("red", "lightblue", "green", "orange", "purple"))
}
par(mfrow=c(1,1))

# bar plot for character columns in one whole plot

#charcters columns

char_vars <- setdiff(names(Summarized_data), numeric_vars)
char_vars<-setdiff(char_vars, c("id", "time", "orig_time", "first_time","LTV_time"))
par(mfrow = c(2,3))

for (i in 1:length(char_vars)) {
      barplot(table(Summarized_data[[char_vars[i]]]), 
              main = char_vars[i], 
              xlab = "Category", 
              ylab = "Frequency", 
              col = c("red", "lightblue", "green", "orange", "purple"))
    }
    
par(mfrow = c(1, 1))
    
    

    ##################################BI Variate Analysis(Target vs attribute)###########
    
    #target default rate
    
    
    # Defined FICO score ranges
    fico_breaks <- c(400,450,500,550,600,650,700,750,800,850,900)  
    # Grouping the data by FICO score ranges and calculating the probability of default rate for each range
    default_prob <- Summarized_data %>%
      mutate(FICO_range = cut(FICO_orig_time, breaks = fico_breaks, labels = c("400-450","450-500","500-550","550-600","600-650","650-700", "700-750","750-800", "800-850","850-900"), include.lowest = TRUE)) %>%
      group_by(FICO_range) %>%
      summarise(default_rate = mean(default_time, na.rm = TRUE))
    
    ggplot(default_prob, aes(x = FICO_range, y = default_rate)) +
      geom_bar(stat = "identity", fill = "red", color = "black") +
      geom_text(aes(label = round(default_rate, 2)), vjust = -0.5) +  # Add text labels for each bar
      labs(x = "FICO Score Range", y = "Probability of Default Rate", 
           title = "Probability of Default Rate vs. FICO Score") +
      theme_minimal()
    
    
    #LTV-ratio
    ltv_breaks <- c(0,10,20,30,40,50,60,70,80,90,100,110,120,130,140,150,160)
    
    ltv_default_prob <- Summarized_data %>%
      mutate(LTV_range = cut(LTV_time, breaks = ltv_breaks, labels = c("0-10%","10-20%","20-30%","30-40%","40-50%","50-60%","60-70%","70-80%","80-90%","90-100%","100-110%","110-120%","120-130%","130-140%","140-150%","more than 150%"))) %>%
      group_by(LTV_range) %>%
      summarise(default_rate = mean(default_time, na.rm = TRUE))
    
    ggplot(ltv_default_prob, aes(x = as.factor(LTV_range), y = default_rate)) +
      geom_bar(stat = "identity", fill = "red", color = "black") +
      labs(x = "LTV Ratio Range", y = "Probability of Default Rate", 
           title = "Probability of Default Rate vs. LTV Ratio") +
      theme_minimal()
    

    
    
    # un-employment rate 
    unemployment_breaks <- seq(1, 10, by = 1)
    
    unemployment_default_prob <- Summarized_data %>%
      mutate(unemployment_range = cut(uer_time, breaks = unemployment_breaks, labels = FALSE)) %>%
      group_by(unemployment_range) %>%
      summarise(default_rate = mean(default_time, na.rm = TRUE))
    
    ggplot(unemployment_default_prob, aes(x = as.factor(unemployment_range), y = default_rate)) +
     
      geom_bar(stat = "identity",fill = "red", color = "black") +
      labs(x = "Unemployment Rate Range (%)", y = "Probability of Default Rate", 
           title = "Probability of Default Rate vs. Unemployment Rate") +
           theme_minimal()
    
    
    
    ############# hpi index
    hpi_breaks <- c(100,125 ,150, 175,200, 225, 250)
    
    hpi_default_prob <- Summarized_data %>%
      mutate(HPI_range = cut(hpi_time, breaks = hpi_breaks, labels = c("100-125" ,"125-150", "150-175","175-200", "200-225", "above 250"))) %>%
      group_by(HPI_range) %>%
      summarise(default_rate = mean(default_time, na.rm = TRUE))
    
    ggplot(hpi_default_prob, aes(x = as.factor(HPI_range), y = default_rate)) +
      geom_bar(stat = "identity", fill = "red", color = "black")
      labs(x = "HPI Index Range", y = "Probability of Default Rate", 
           title = "Probability of Default Rate vs. HPI Index") +
      theme_minimal()
    
    
    
    ####################################target as interest rate####################
    cleaned_data <- Summarized_data %>%
      filter(Interest_Rate_orig_time != 0)
    # Sorting the data by LTV and average interest if same LTV
    interest_vs_ltv<-cleaned_data %>%
      mutate(LTV_bin= cut(LTV_orig_time, breaks = 10)) %>%
      group_by(LTV_bin) %>%
      summarise(Interest_Rate_avg = mean(Interest_Rate_orig_time, na.rm = TRUE))
    
    interest_vs_ltv$Interest_Rate_zscore <- scale(interest_vs_ltv$Interest_Rate_avg)
    
    
    # Sorting the data by FICO and average interest if same FICO 
    
    interest_vs_fico <- cleaned_data %>%
      mutate(FICO_bin = cut(FICO_orig_time, breaks = 20)) %>%
      group_by(FICO_bin) %>%
      summarise(Interest_Rate_avg = mean(Interest_Rate_orig_time, na.rm = TRUE))
    
    interest_vs_fico$Interest_Rate_zscore <- scale(interest_vs_fico$Interest_Rate_avg)
    
    # Sorting the data by HPI and average interest if same HPI
    interest_vs_hpi <- cleaned_data %>%
      mutate(HPI_bin = cut(hpi_orig_time, breaks = 10)) %>%
    
      group_by(HPI_bin) %>%
      summarise(Interest_Rate_avg = mean(Interest_Rate_orig_time, na.rm = TRUE))
    
    interest_vs_hpi$Interest_Rate_zscore <- scale(interest_vs_hpi$Interest_Rate_avg)
####################plot for LTV and FICO and HPI
    
    
    plot_interest_fico <- ggplot(interest_vs_fico, aes(x = FICO_bin, y = Interest_Rate_zscore)) +
      geom_bar(stat = "identity", fill = "skyblue") +
      labs(x = "FICO Score", y = "Interest Rate", title = "Interest Rate vs. FICO Score")
    
    # Plot for Interest Rate vs. LTV Ratio
    plot_interest_ltv <- ggplot(interest_vs_ltv, aes(x = LTV_bin, y = Interest_Rate_zscore)) +
      geom_bar(stat = "identity", fill = "lightgreen") +
      labs(x = "LTV Ratio", y = "Interest Rate", title = "Interest Rate vs. LTV Ratio")
    
    # Plot for Interest Rate vs. HPI
    plot_interest_hpi <- ggplot(interest_vs_hpi, aes(x = HPI_bin, y = Interest_Rate_zscore)) +
      geom_bar(stat = "identity", fill = "lightcoral") +
      labs(x = "HPI", y = "Interest Rate", title = "Interest Rate vs. HPI")
    
    grid.arrange(plot_interest_fico, plot_interest_ltv, plot_interest_hpi, nrow = 3)
    
    
    
##############################################################################################################
#correlation plot
    cols_to_exclude <- c("orig_time", "mat_time", "time")
    numeric_vars <- numeric_vars[!numeric_vars %in% cols_to_exclude]
    
    # Calculate correlation matrix
    mortgage_correlation_matrix <- cor(Summarized_data[, numeric_vars], use = "complete.obs")
    
    
    
    corrplot(mortgage_correlation_matrix, method = "color", type = "upper", addCoef.col = "black",tl.col = "black", tl.srt = 45)
    
    
    pairs.panels(mortgage_correlation_matrix[,numeric_vars])
    
    
#############################################################################
    ####Vif
    car::vif(lm(default_time ~ ., data=Summarized_data[,-which(names(Summarized_data) %in% c("orig_time", "mat_time", "time"))]))
    
    #vif2
    car::vif(lm(default_time ~ ., data=Summarized_data[,-which(names(Summarized_data) %in% c("orig_time", "mat_time", "time","hpi_orig_time","balance_orig_time","LTV_orig_time"))]))
    
    #vif final
    car::vif(lm(default_time ~ ., data=Summarized_data[,-which(names(Summarized_data) %in% c("orig_time", "mat_time", "time","hpi_orig_time","balance_orig_time","LTV_orig_time","gdp_time"))]))
    
    
    #classification models to classify if a customer is going to payoff or going to default.
    
    #Target  default =1
    
    model_excluded_features<-c('status_time','payoff_time',"orig_time", "mat_time", "time","hpi_orig_time","balance_orig_time","LTV_orig_time","gdp_time","Interest_Rate_orig_time")
    
    re_data<-Summarized_data[,-which(names(Summarized_data) %in% model_excluded_features)]
    
#partitioning 
    

    set.seed(149)
    training_index <- sample(rownames(re_data), size = nrow(re_data) * 0.6)
    
    validation_index <- sample(setdiff(rownames(re_data), training_index), size = nrow(re_data) * 0.2)
 
    morti_training_data<-re_data[training_index,]
    morti_validation_data<-re_data[validation_index,]
    holdout_data_morti<-re_data[setdiff(row.names(re_data),union(training_index,validation_index)),]
    
   print(morti_training_data$Length_of_Loan)
    
    sum(is.na(morti_training_data))
    
    sum(is.na(morti_validation_data))
    dim(morti_training_data)
    dim(morti_validation_data)
    dim(holdout_data_morti)
    
    prop.table(table(morti_training_data$default_time))[2]*100
    prop.table(table(morti_validation_data$default_time))[2]*100
    prop.table(table(holdout_data_morti$default_time))[2]*100
    
  

###classification of default or will pay out the loan at present situation of customer record

#######logistic algorithm#######
    
log_main_model <- glm(default_time ~ .-default_time, data = morti_training_data, family = binomial)

summary(log_main_model)

predicted <- predict(log_main_model, newdata = morti_validation_data, type = "response")
predicted_class <- ifelse(predicted > 0.5, 1, 0)  # Convert probabilities to binary predictions



# confusion matrix
cnf_log <- confusionMatrix(as.factor(predicted_class), as.factor(morti_validation_data$default_time), positive = "1")
print("Confusion Matrix:")
print(cnf_log)



############step wise forward ##########

null_features<- glm(default_time~1, data = morti_training_data, family = "binomial")
full_features <- glm(default_time ~ ., data = morti_training_data, family = "binomial")
forward_step_wise <- step(null_features, scope=list(lower=null_features, upper=full_features), direction = "forward")
summary(forward_step_wise)

predict_forward <- predict(forward_step_wise, newdata = morti_validation_data, type = "response")
predict_class <- ifelse(predict_forward > 0.5, 1, 0)

cnf_forward_log <- confusionMatrix(as.factor(predict_class), as.factor(morti_validation_data$default_time), positive = "1")
print("Confusion Matrix:")
print(cnf_forward_log)

#################Backward Step-wise logistic regression###############

backward_step_model <- glm(default_time ~ ., data = morti_training_data, family = "binomial")
step.backward <- step(backward_step_model, direction = "backward")

predict_backward <- predict(step.backward , newdata = morti_validation_data, type = "response")
predict_class_backward <- ifelse(predict_backward > 0.5, 1, 0)

cnf_backward_log <- confusionMatrix(as.factor(predict_class_backward), as.factor(morti_validation_data$default_time), positive = "1")
print("Confusion Matrix:")
print(cnf_backward_log)


##################c.50 for default#################

c50model_default<-C5.0(morti_training_data[,-which(names(morti_training_data) %in% c('default_time'))],as.factor(morti_training_data$default_time))
c50model_default
# rpart.plot(c50model_default)
#plot(c50model_default,main="c50 plot")
#validation
c50_pred<-predict(c50model_default, newdata = morti_validation_data[,-which(names(morti_validation_data) %in% c('default_time'))])
conf_matr_c50 <- confusionMatrix(factor(c50_pred,levels=c('1','0')),as.factor(morti_validation_data$default_time), positive = "1" )

print(conf_matr_c50)

####trail 6############
c50model_default_6<-C5.0(morti_training_data[,-which(names(morti_training_data) %in% c('default_time'))],as.factor(morti_training_data$default_time),trials = 6)
c50model_default_6
#plot(c50model_default_10,main="c50 plot")
c50_pred_6<-predict(c50model_default_6, newdata = morti_validation_data[,-which(names(morti_validation_data) %in% c('default_time'))])
conf_matr_c50_6 <- confusionMatrix(factor(c50_pred_6,levels=c('1','0')),as.factor(morti_validation_data$default_time), positive = "1" )

print(conf_matr_c50_6)

################trial 10#######
c50model_default_10<-C5.0(morti_training_data[,-which(names(morti_training_data) %in% c('default_time','status_time'))],as.factor(morti_training_data$default_time),trials = 10)
c50model_default_10
#plot(c50model_default_10,main="c50 plot")
c50_pred_10<-predict(c50model_default_10, newdata = morti_validation_data[,-which(names(morti_validation_data) %in% c('default_time'))])
conf_matr_c50_10 <- confusionMatrix(factor(c50_pred_10,levels=c('1','0')),as.factor(morti_validation_data$default_time), positive = "1" )

print(conf_matr_c50_10)

########### validation using the holdout data
predicted_hold <- predict(c50model_default, newdata = holdout_data_morti)

conf_matr_c50_hold <- confusionMatrix(factor(predicted_hold, levels = c('1', '0')), 
                                      factor(holdout_data_morti$default_time, levels = c('1', '0')), 
                                      positive = "1")
print(conf_matr_c50_hold)

############################################# performing model with active customers
data_base_active_customers$default_class<-predict(c50model_default,newdata=data_base_active_customers)

View(data_base_active_customers)



######################################Time series################################################33
par(mfrow = c(1, 1))
#Time series pre processing for average interest rates on Original time basis
time_interest_rate <- Mortgage %>%
  filter(Interest_Rate_orig_time != 0) %>% 
  group_by(orig_time) %>%
  summarise(adjusted_Interest_Rate_orig_time = mean(Interest_Rate_orig_time))
print(time_interest_rate)

complete_time_series <- complete(time_interest_rate, orig_time = seq(min(time_interest_rate$orig_time), max(time_interest_rate$orig_time), by = 1))
complete_time_series
#interpolation with neighbours
time_series_zoo <- zoo::zoo(complete_time_series)
print(time_series_zoo)
# Interpolating missing values using neighboring values
interpolated_series <- zoo::na.approx(time_series_zoo)
print(interpolated_series)

#the dataset after converting into time series 
ts_data <- ts(interpolated_series[,2], start = c(2014, 1), frequency = 12)
ts_data
plot(ts_data[],xlab = "Time", ylab = "Interest Rate", main = "Interest Rate Over Time")


ts_data
#decompose data
plot(stl(ts_data, "periodic"))
par(mfrow = c(1, 1))

# Split the time series into training and validation sets
training_ts <- window(ts_data, end = c(2020, 12))
validation_ts <- window(ts_data, start = c(2021, 1))

#plot for interest rate
plot(ts_data, xlab = "Time", ylab = "Interest Rate", main = "Interest Rate Over Time")
lines(training_ts, col = "green")
lines(validation_ts, col = "lightblue")
legend("topright", legend = c("Training", "Validation"), col = c("green", "lightblue"), lty = 1)



# Seasonal naive model for interest rate
snaive_model_interest <- snaive(training_ts)


# Forecast for Interest Rate
snaive_forecast_interest <- forecast(snaive_model_interest, h = 24)  
plot(snaive_forecast_interest)

accuracy(snaive_forecast_interest,validation_ts)

########################holts winter with ets


interest_ets_model <- ets(training_ts,model = 'ZAA')


ets_forecast_values <- forecast(interest_ets_model, h = 24) 

plot(ets_forecast_values)
accuracy(ets_forecast_values,validation_ts)


residuals <- residuals(interest_ets_model)
plot(residuals)
############################### auto ARIMA 


###############check for stationary or not 

# p-value < 0.05 indicates the TS is stationary
adf.test(ts_data)

##########we could see that interest rate is not stationary data contains trend or seasonality


#################ACF and PACF

autocorrelation <- Acf(training_ts, lag.max = 12) 
pacf<-Pacf(training_ts, lag.max = 12)


######################auto arima 

arima_interest<-auto.arima(training_ts,max.p = 12)

summary(arima_interest)


arima_forecast<- forecast(arima_interest, h = 24)

plot(arima_forecast)

accuracy(arima_forecast,validation_ts)



################# Plot observed data and forecasts
plot(training_ts,xlim=c(2018,2024), xlab = "Time", ylab = "Interest Rate", main = "Interest Rate Over Time with Forecasts")
lines(arima_forecast$mean, col = "blue")
lines(ets_forecast_values$mean, col = "green")

lines(validation_ts,col="orange")
lines(snaive_model_interest$mean,col="violet")
legend("topleft", legend = c("Train","Validation", "ARIMA Forecast", "Holt-Winters Forecast","SNaive"), 
       col = c("black", "orange","blue", "green","violet"), lty = 1)


#############Final Time series model is done with whole data and forecast 2 year period forecast
final<-auto.arima(ts_data,max.p = 12)
forecast_final <- forecast(final, h = 24)

plot(ts_data, xlim = c(2014, 2025), type = "l", xlab = "Time", ylab = "Interest Rate", main = "Interest Rate Over Time with Forecasts")

lines(forecast_final$mean, col = "green")

legend("topleft", legend = c("Observed", "Arima Forecast"), 
       col = c("black", "green"), lty = 1)


##########values of final prediction
print(forecast_final$mean)







