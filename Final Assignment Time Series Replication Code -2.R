#Final Assignment Time Series Replication Code 
#3 December 
###EVOLUTION OF OIL DEMAND AND OIL SUPPLY SHOCKS ANNUAL AVERAGES 
#Figure 2 
library(vars)
library(tseries)
library(urca)
library(foreign)
library(ks)
library(pracma)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(readxl)
library(readxl)
data_oil <- read_excel("//Users/mariaferreres/Downloads/Table Data Replication Oil TT.xlsx")
head(data_oil)
#growth rate world oil production 
groilproduction <- data_oil$`Growth Rate World Oil Production`
grwoilproduction <- ts(groilproduction, start = c(1973,1),frequency = 12)

#global real activity 
gra <- data_oil$`Global Real Activity`
gractivity <- ts(gra, start = c(1973,1),frequency = 12)
#real price of oil
rpo <- data_oil$`Real Price of Oil`
rpoil <- ts(rpo, start = c(1973,1),frequency = 12)

cpi <- read.table("/Users/mariaferreres/Downloads/OIL TT/cpi.txt")
beagdp <- read.table("/Users/mariaferreres/Downloads/OIL TT/beagdp.txt")

d2 <- ts.intersect(
  growth_oil_production = grwoilproduction,
  global_real_activity = gractivity,
  real_price_of_oil = rpoil
)
head(d2)
d2
# Increase the size of the plotting window (for macOS)
quartz(width = 10, height = 6)

# Assuming rpoil is your time series object
plot(rpoil, main = "Real Price of Oil Over Time", xlab = "Year", ylab = "Real Price of Oil", col = "blue", type = "l")

time <- seq(from = as.Date("1976-01-01") + 2/12, to = as.Date("2007-12-01"), by = "months")
length(time)
q <- 3
t <- 419
h <- 15
p <- 24
#Reduced form VAR
library(vars)
res1 <- VAR(d2, p = 24, type = "const")

myA <- matrix(data = NA, nrow = 3, ncol = 3)
myA[1,2] <- 0 
myA[1,3] <- 0
myA[2,3] <- 0


res2_dm <- vars::SVAR(res1, Amat = myA, estmethod = "direct",
                      hessian = TRUE, method="BFGS")

AMat <- res2_dm$A
(solve(AMat))

myres <- yhatfunc(svarobj = res2_dm)
myres


residuals <- residuals(svarobj)
residuals # Uhat(1:q,:) this is the Matlab equivalent and I do get the same numbers 
str(residuals)
residuals
dim(residuals)
inv_solve_AMat
Ehat1 <- residuals %*% inv_solve_AMat

q1 <- Ehat1[, 1]
q1
q1 <- c(mean(q1[1:2]+q1[1:1])/2, q1)

q2 <- Ehat1[, 2] #second column
q2
q2 <- c(mean(q2[1:2]+q2[1:1])/2, q2)

q3 <- Ehat1[, 3] #second column
q3
q3 <- c(mean(q3[1:2]+q3[1:1])/2, q3)


time <- 1973:2007
length(time)

q1a <- numeric(length(time))
q2a <- numeric(length(time))
q3a <- numeric(length(time))


for (i in 1:length(time)) {
  q1a[i] <- mean(q1[12*(i-1) + 1:12])
  q2a[i] <- mean(q2[12*(i-1) + 1:12])
  q3a[i] <- mean(q3[12*(i-1) + 1:12])
  
}

length(q1a)
#FIGURE 2 
# Plot the first subplot
plot(time, q1a, type = "l", ylim = c(-1, 1),
     xlab = "Year", ylab = "Oil Supply Shock",
     main = "Oil Supply Shock")
abline(h = 0, col = "red")

#Second subplot 
plot(time, q2a, type = "l", ylim = c(-1, 1),
     xlab = "Year", ylab = "Aggregate Demand Shock",
     main = "Aggregate Demand Shock")
abline(h = 0, col = "red")


#Third subplot 
plot(time, q3a, type = "l", ylim = c(-1, 1),
     xlab = "Year", ylab = "Oil Specific Demand Shock",
     main = "Oil-Specific Demand Shock")
abline(h = 0, col = "red")
##################
library(vars)
library(tseries)
library(urca)
library(foreign)
library(ks)
library(pracma)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(readxl)
library(readxl)
data_oil <- read_excel("//Users/mariaferreres/Downloads/Table Data Replication Oil TT.xlsx")
head(data_oil)
#growth rate world oil production 
groilproduction <- data_oil$`Growth Rate World Oil Production`
grwoilproduction <- ts(groilproduction, start = c(1973,1),frequency = 12)

#global real activity 
gra <- data_oil$`Global Real Activity`
gractivity <- ts(gra, start = c(1973,1),frequency = 12)
#real price of oil
rpo <- data_oil$`Real Price of Oil`
rpoil <- ts(rpo, start = c(1973,1),frequency = 12)

cpi <- read.table("/Users/mariaferreres/Downloads/OIL TT/cpi.txt")
beagdp <- read.table("/Users/mariaferreres/Downloads/OIL TT/beagdp.txt")

d2 <- ts.intersect(
  growth_oil_production = grwoilproduction,
  global_real_activity = gractivity,
  real_price_of_oil = rpoil
)
head(d2)
d2
#ESTIMATE REDUCED FORM VAR 
res1 <- VAR(d2, p=24, type = "const")
Acoef(res1)[[1]]
Acoef(res1)[[2]]
Acoef(res1)[[3]]
Acoef(res1)[[4]]
#covariance matrix of residuals
summary(res1)$covres

##Recusive VAR Using Cholesky Decomposition 
#Cholesky Decomposition 
mychol <- chol(summary(res1)$covres)
# Transpose of the decomposition
Ainv <- t(mychol)
# See result
Ainv

variable_names <- colnames(res1$y)
print(variable_names)


#IMPULSE RESPONSE FUNCTIONS 
#recover structural shocks as wt = Aut and calculate impulse response function of 
#an unexpected shock to the real price of oil 
#ORTHOGONAL IMPUSEL RESPONSE FUNCTION, THAT MAYBE WRONG?
irf_result_growth_oil <- irf(res1, impulse = "growth_oil_production", response = "growth_oil_production", cumulative = TRUE, n.ahead = 15)
plot(irf_result_growth_oil, xlim = c(0, 15), ylim = c(-25, 15), xlab = "Months", ylab = "Impulse Response")
str(irf_result_growth_oil)

irf_result_activity <- irf(res1, impulse = "growth_oil_production", response = "global_real_activity", cumulative = TRUE, n.ahead = 15)
plot(irf_result_activity, xlim = c(0, 15), ylim = c(-5, 10), xlab = "Months", ylab = "Impulse Response")


irf_result_real_price_of_oil <- irf(res1, impulse = "growth_oil_production", response = "real_price_of_oil", cumulative = TRUE, n.ahead = 15)
plot(irf_result_real_price_of_oil, xlim = c(0, 15), ylim = c(-7, 10), xlab = "Months", ylab = "Impulse Response")

#esimtae svar
myA <- matrix(data = NA, nrow = 3, ncol = 3)
myA[1,2] <- 0 
myA[1,3] <- 0
myA[2,3] <- 0
myA






install.packages("readxl")
library(readxl)

inv_solve_AMat #equivalent to inv(chol(SIGMA)')

data_oil <- read_excel("//Users/mariaferreres/Downloads/Table Data Replication Oil TT.xlsx")
#Recursive Identification 

#growth rate world oil production 
groilproduction <- data_oil$`Growth Rate World Oil Production`
grwoilproduction <- ts(groilproduction, start = c(1973,2),frequency = 12)

#global real activity 
gra <- data_oil$`Global Real Activity`
gractivity <- ts(gra, start = c(1973,2),frequency = 12)
#real price of oil
rpo <- data_oil$`Real Price of Oil`
rpoil <- ts(rpo, start = c(1973,2),frequency = 12)

cpi <- read.table("/Users/mariaferreres/Downloads/OIL TT/cpi.txt")
beagdp <- read.table("/Users/mariaferreres/Downloads/OIL TT/beagdp.txt")

combined_ts <- ts.intersect(grwoilproduction, gractivity, rpoil)
d3 <- window(combined_ts, start = c(1973, 2), end = c(2007, 12))

str(d3)
colnames(d3)
colnames(d3) <- c("Growth Oil", "Growth Activity", "Real Price of Oil")

#see volatility of real price of oil 
plot.ts(d3, plot.type="multiple", 
        main="Oil", 
        xlab = "", 
        col = "blue")

###SEE VOLATILY REAL PRICE OF OIL IN A DIFFERENT PLOT 
#Let us create a new data set with the quarter as the first column
d1_1 <- as.data.frame(cbind(time(d3),d3))
colnames(d1_1) <- c("quarter","Growth Oil","Growth Activity","Real Price of Oil")

library(dplyr)
library(tidyr)
library(ggplot2)
# Now we will pivot to a "long" format
d1_2 <- d1_1 %>%
  pivot_longer(!quarter, names_to="myvar", values_to="val") %>%
  as.data.frame()

ggplot(data=d1_2, aes(quarter,val)) +
  geom_line(color = "steelblue") +
  geom_hline(yintercept=0, linetype="dashed", 
             color = "red") +
  labs(title = "Oil",
       y = "percentage", x = "") +
  facet_wrap(~myvar, ncol=1, scales = "free")


#Estimate the Reduced Form VAR
library(vars)
res1 <- VAR(d3, p=24, type = "const")

#Cholesky decomposition 

Acoef(res1)[[1]]
Acoef(res1)[[2]]
Acoef(res1)[[3]]
Acoef(res1)[[4]]


summary(res1)$covres

###CHOLESKY DECOMPOSITION AND IMPULSE RESPONSE FUNCTION (IRF) PLOTS 

#Recursive VAR Using Cholesky Decomposition 
# Cholesky decomposition
mychol <- chol(summary(res1)$covres)
mychol
# Transpose of the decomposition
Ainv <- t(mychol)
Ainv

###SVAR (STRUCTURAL VAR) ESTIMATION 
#Estimate SVAR

myA <- matrix(data = NA, nrow = 3, ncol = 3)
myA[1,2] <- 0 
myA[1,3] <- 0
myA[2,3] <- 0

myA

#Setting structural restriction matrix myA 

res2 <- vars::SVAR(res1, Amat = myA, estmethod = "direct",
                   hessian = TRUE, method="BFGS")

AMat <- summary(res2)$A
AMat

#compare with inverse Cholesky  
solve(Ainv)

#von der Form her ähnlich
horizon <- 17
#Shock to Growth of Oil
irf_results <- irf(res1, impulse = "Growth.Oil", response = c("Growth.Oil", "Growth.Activity", "Real.Price.of.Oil"), n.ahead = horizon)
plot(irf_results, main = "Impulse Response Functions", col = c("blue", "red", "green"))
#Shock to Real Activity 
irf_results <- irf(res1, impulse = "Growth.Activity", response = c("Growth.Oil", "Growth.Activity", "Real.Price.of.Oil"), n.ahead = horizon)
plot(irf_results, main = "Impulse Response Functions", col = c("blue", "red", "green"))
#Shock to Real Price of Oil
irf_results <- irf(res1, impulse = "Real.Price.of.Oil", response = c("Growth.Oil", "Growth.Activity", "Real.Price.of.Oil"), n.ahead = horizon)
plot(irf_results, main = "Impulse Response Functions", col = c("blue", "red", "green"))



#Growth Oil
irf_results1 <- irf(res1, impulse = "Growth.Oil", response = "Growth.Oil", n.ahead = horizon)
plot(irf_results1, main = "Impulse Response Function: Growth.Oil on Growth.Oil", col = "blue", ylim = c(-25, 15))

irf_results2 <- irf(res1, impulse = "Growth.Oil", response = "Growth.Activity", n.ahead = horizon)
plot(irf_results2, main = "Impulse Response Function: Growth.Activity on Growth.Oil", col = "blue", ylim = c(-5, 10))

irf_results3 <- irf(res1, impulse = "Growth.Oil", response = "Real.Price.of.Oil", n.ahead = horizon)
plot(irf_results3, main = "Impulse Response Function: Real.Price.of.Oil on Growth.Oil", col = "blue", ylim = c(-5, 10))

#Growth Activity 
irf_results4 <- irf(res1, impulse = "Growth.Activity", response = "Growth.Oil", n.ahead = horizon)
plot(irf_results4, main = "Impulse Response Function: Growth.Oil on Growth.Activityl", col = "blue", ylim = c(-25, 15))

irf_results5 <- irf(res1, impulse = "Growth.Activity", response = "Growth.Activity", n.ahead = horizon)
plot(irf_results5, main = "Impulse Response Function: Growth.Oil on Growth.Activityl", col = "blue", ylim = c(-25, 15))

irf_results6 <- irf(res1, impulse = "Growth.Activity", response = "Real.Price.of.Oil", n.ahead = horizon)
plot(irf_results6, main = "Impulse Response Function: Real.Price.of.Oil on Growth.Activityl", col = "blue", ylim = c(-25, 15))


#Real Price of Oil
irf_results7 <- irf(res1, impulse = "Real.Price.of.Oil", response = "Growth.Oil", n.ahead = horizon)
plot(irf_results7, main = "Impulse Response Function: Growth.Oil on Growth.Activityl", col = "blue", ylim = c(-25, 15))

irf_results8 <- irf(res1, impulse = "Real.Price.of.Oil", response = "Growth.Activity", n.ahead = horizon)
plot(irf_results8, main = "Impulse Response Function: Growth.Oil on Growth.Activityl", col = "blue", ylim = c(-25, 15))

irf_results9 <- irf(res1, impulse = "Real.Price.of.Oil", response = "Real.Price.of.Oil", n.ahead = horizon)
plot(irf_results9, main = "Impulse Response Function: Real.Price.of.Oil on Growth.Activityl", col = "blue", ylim = c(-25, 15))

# Inference is based on a recursive-design wild bootstrap with 2000 replications. 
#Point estimates with one-and two-standard error bands
#The confidence intervals were constructed using a recursive-design wild bootstrap 
#the horizon is 15 




########################################################################
########################################################################
########################################################################
########################################################################


#Figure 
library(vars)
library(dplyr)
library(tidyverse)
library(ggplot2)
yhatfunc <- function(svarobj){
  
  # ---- Input -------- #
  # svarobj created with vars::SVAR
  # --------------------#
  
  
  # Number of variables in the VAR
  myk <- svarobj$var$K
  
  # Number of lags in the VAR
  myp <- svarobj$var$p
  
  # Estimated A matrix (K x K)
  AMat <- svarobj$A
  
  # Estimated B matrix (K x K)
  BMat <- svarobj$B
  
  # B^{-1}A matrix => (K x K)
  # Note: used to create structural shocks
  ABMat <- solve(BMat)%*%AMat
  
  # Matrix of reduced form residuals
  # rows = T-p, cols = K
  rf_resid <- residuals(svarobj$var)
  
  # Generate matrix of structural shocks
  # rows = K, cols = T-p
  str_shock <- ABMat%*%t(rf_resid)
  
  # Number of usable periods for historical decomposition
  # = Number of elements in the residual vectors = T-p
  T <- ncol(str_shock)
  
  # Phi: coefficient matrices for reduced form MA representation
  # array of dimension (K,K,T)
  myphimat <- Phi(svarobj$var, nstep = T)
  
  # Theta: coefficient matrices for structural MA representation
  # array of dimension (K,K,T)
  mythetamat <- array(data=NA, dim=c(myk,myk,T))
  for(i in 1:T){
    mythetamat[,,i] <- myphimat[,,i]%*%solve(ABMat)
  }
  
  
  # ----------------------------------------- #
  # -------- Generate yhat vectors ---------- #
  # Empty array of dimension (K+1, T, K)
  # First dimension: K shocks + total contribution of all shocks
  # Second dimension: time periods
  # Third dimension: variables in VAR
  yhat_mat <- array(data=NA, dim=c((myk+1),T,myk))
  
  # --- Main loop over variables in VAR
  for(k in 1:myk){
    
    # Matrix of zeros to hold results on yhat for k-th variable
    yhat_mat[,,k] <- matrix(data=0, nrow=(myk+1), ncol=T)
    
    # Compute yhat and populate the matrix
    # column i = yhat in period i; 
    # Loop over the usable periods 
    for (i in 1:T){
      
      # Temporary zero vector (Kx1)
      temp_vec1 <- matrix(data=0, nrow = myk, ncol = 1)
      
      # Loop to sum theta_s*w_{t-s} from s=0 to s=t-i
      for(j in 1:i){
        # Structural impulse response matrix (the theta matrix)
        mytheta <- mythetamat[,,j]
        
        # Empty vector
        temp_vec2 <- numeric(length = myk)
        
        # Multiply out the k-th row of mytheta with the w_{t-s} vector
        for(l in 1:myk){
          temp_vec2[l] <- mytheta[k,l]*str_shock[l,i-j+1]
        }
        # Add over iterations
        temp_vec1 <- temp_vec1 + matrix(
          data=temp_vec2, nrow=myk, ncol=1
        )
      }
      
      # --- Store result for period t=i
      # First k elements: contributions of each shock
      # Last element: yhat = sum of contributions
      yhat_mat[,i,k] <- matrix(
        data = c(temp_vec1, sum(temp_vec1)),
        nrow=(myk+1), ncol = 1
      )
      
    }
    
  } # End: Main loop over variables in VAR
  
  # ----- Return results
  # Returned object: array of dims: k+1, T, k
  return(yhat_mat)
  
}
tsplotyhat <- function(yhatobj,data,j){
  
  # ------- INPUT ---------
  # yhatobj: object created by yhat function
  # data: data set used for var
  # j: which variable to plot
  # ---------------------
  
  # Data set
  d1 <- data
  
  # Sample mean of variable
  mean_yj <- mean(d1[,j],na.rm = TRUE)
  
  # Usable observations
  T <- ncol(yhatobj[,,1])
  
  # Row holding yhat vector (this is the last row)
  k_new <- nrow(yhatobj[,,1])
  
  # --- Data frame for plot
  # Create variables
  dv1 <- data.frame(
    # Demeaned variable (usable observations)
    ydmean = (tail(d1[,j],n=T)-mean_yj),
    # Corresponding yhat
    yhat = yhatobj[k_new,,j]
  )
  # Pivot to long
  dv2 <- as.data.frame(dv1) %>%
    mutate(pd=row_number()) %>%
    pivot_longer(!pd, names_to = "ytype",
                 values_to = "val") %>%
    as.data.frame()
  
  
  # Create plot
  p <- ggplot(data = dv2, aes(x=pd,y=val,group=ytype)) +
    geom_line(aes(color=ytype)) +
    labs(
      title="Time series plot of demeaned-y and y-hat",
      x="periods",y="value"
    )
  
  # Return plot
  return(p)
}
bchrtyhat <- function(yhatobj,j,t1,t2){
  
  # ----- Input -----------#
  # yhatobj: object created by "yhatfunc"
  # j (integer): variable of interest
  # t1: initial period
  # t2: final period
  # -----------------------
  
  # Read matrix
  y_hat <- yhatobj[,,j]
  
  # Number of rows
  k1 <- nrow(y_hat)
  
  # Names of the vector of shocks
  vshocks <- paste0("shock",1:(k1-1))
  
  # Vector of names
  ch_n <- c(vshocks, "total")
  
  # Vector of change in values
  ch_val <- numeric(k1)
  for(i in 1:k1){
    ch_val[i] <- y_hat[i,t2] - y_hat[i,t1]
  }
  
  # Create data frame for plot
  d1 <- data.frame(nms=ch_n, vls=ch_val)
  
  # Create plot
  p <- ggplot(data = d1, aes(x=nms,y=vls)) +
    geom_bar(stat = "identity", fill="steelblue") +
    labs(
      title="Contribution of Each Shock to Total Change",
      x="",y="change"
    )
  
  # Return result
  return(p)
  
}

library(readxl)
data_oil <- read_excel("//Users/mariaferreres/Downloads/Table Data Replication Oil TT.xlsx")
head(data_oil)
#growth rate world oil production 
groilproduction <- data_oil$`Growth Rate World Oil Production`
grwoilproduction <- ts(groilproduction, start = c(1976,1),frequency = 12)
#global real activity 
gra <- data_oil$`Global Real Activity`
gractivity <- ts(gra, start = c(1976,1),frequency = 12)
gractivity
#real price of oil
rpo <- data_oil$`Real Price of Oil`
rpoil <- ts(rpo, start = c(1976,1),frequency = 12)

cpi <- read.table("/Users/mariaferreres/Downloads/OIL TT/cpi.txt")
beagdp <- read.table("/Users/mariaferreres/Downloads/OIL TT/beagdp.txt")

d2 <- ts.intersect(
  growth_oil_production = grwoilproduction,
  global_real_activity = gractivity,
  real_price_of_oil = rpoil
)
head(d2)
# Increase the size of the plotting window (for macOS)
quartz(width = 10, height = 6)
#REAL PRICE OF OIL OVER TIME 
plot(rpoil, main = "Real Price of Oil Over Time", xlab = "Year", ylab = "Real Price of Oil", col = "blue", type = "l")

time <- seq(from = as.Date("1976-01-01") + 2/12, to = as.Date("2007-12-01"), by = "months")

q <- 3
t <- 419
h <- 15
p <- 24
#Reduced form VAR
library(vars)
res1 <- VAR(d2, p = 24, type = "const")

myA <- matrix(data = NA, nrow = 3, ncol = 3)
myA[1,2] <- 0 
myA[1,3] <- 0
myA[2,3] <- 0


res2_dm <- vars::SVAR(res1, Amat = myA, estmethod = "direct",
                      hessian = TRUE, method="BFGS")


AMat <- res2_dm$A
(solve(AMat))

myres <- yhatfunc(svarobj = res2_dm)
myres
# Load necessary libraries
library(ggplot2)
library(tidyr)
#compares observed values 'ydmean' to predicted values 'yhat' 
#first variables growth oil production 
tsplotyhat(yhatobj = myres, data = d2, j=1)
#global real activity 
tsplotyhat(yhatobj = myres, data = d2, j=2)
#real price of oil
tsplotyhat(yhatobj = myres, data = d2, j=3)

variable_names <- colnames(d2)
print(variable_names[1])
print(variable_names[2])
print(variable_names[3])

# Assuming yhat_mat is a three-dimensional array
yhat_mat <- myres
# Extract variables
growth_oil_production <- yhat_mat[1,,3]
global_real_activity <- yhat_mat[2,,3]
real_price_of_oil <- yhat_mat[3,,3]
growth_oil_production
all_months <- seq(as.Date("1976-01-01"), as.Date("2007-12-31"), by = "month")
length(all_months)
length(growth_oil_production)
year <- as.numeric(format(all_months, "%Y"))
month <- as.numeric(format(all_months, "%m"))
time_data <- data.frame(year = year, month = month)
length(time_data)
head(time_data)
dim(time_data)

# Create data frames for each variable
ctrb_growth <- data.frame(pd = 1:length(growth_oil_production), contrib = growth_oil_production)
ctrb_global <- data.frame(pd = 1:length(global_real_activity), contrib = global_real_activity)
ctrb_price <- data.frame(pd = 1:length(real_price_of_oil), contrib = real_price_of_oil)

time_data$Date <- as.Date(paste(time_data$year, time_data$month, "01", sep = "-"))


# Increase the size of the plotting window (for macOS)
quartz(width = 10, height = 6)

# Assuming rpoil is your time series object
plot(rpoil, main = "Real Price of Oil Over Time", xlab = "Year", ylab = "Real Price of Oil", col = "blue", type = "l")

dim(ctrb_growth)
length(all_months)
start_date <- as.Date("1976-01-01")
ctrb_growth$Date <- seq(start_date, by = "1 month", length.out = nrow(ctrb_growth))
library(ggplot2)


#FIGURE 4 
# Create separate line plots for each variable
plot_growth <- ggplot(data = ctrb_growth, aes(x = Date, y = contrib)) +
  geom_line(color = "blue") +
  labs(title = "Contribution of growth_oil_production",
       x = "period", y = "contribution") +
  scale_y_continuous(limits = c(-100, 100)) 
print(plot_growth)

ctrb_global$Date <- seq(start_date, by = "1 month", length.out = nrow(ctrb_growth))
plot_global <- ggplot(data = ctrb_global, aes(x = Date, y = contrib)) +
  geom_line(color = "green") +
  labs(title = "Contribution of global_real_activity",
       x = "period", y = "contribution") +
  scale_y_continuous(limits = c(-100, 100))
print(plot_global)

ctrb_price$Date <- seq(start_date, by = "1 month", length.out = nrow(ctrb_growth))
plot_price <- ggplot(data = ctrb_price, aes(x = Date, y = contrib)) +
  geom_line(color = "red") +
  labs(title = "Contribution of real_price_of_oil",
       x = "date", y = "contribution") +
  scale_y_continuous(limits = c(-100, 100))
print(plot_price)

#The first figure shows the change in the natural log of oil prices with respect 
#to the log price observed in January 2003.

#Figure 5
cpi <- read.table("/Users/mariaferreres/Downloads/OIL TT/cpi.txt")
cpi
head(cpi)
tail(cpi)
cpi_data <- ts(cpi, start = c(1974,1),frequency = 4)
cpi_data
#chained 2000 dollar GDP 74.IV-07.IV
beagdp <- read.table("/Users/mariaferreres/Downloads/OIL TT/beagdp.txt")
head(beagdp)
beagdp_data<- ts(beagdp, start = c(1974,4),frequency = 4)
tail(beagdp_data)
head(beagdp)
y <- diff(log(beagdp[,3])) * 400
length(y)
start_date <- as.Date("1974-04-01")
y_dates <- seq(start_date, length.out = length(y), by = "3 months")
inflation_rate <- data.frame(Date = y_dates, GDP_Change = y)
head(inflation_rate)
tail(inflation_rate)

myres <- yhatfunc(svarobj = res2_dm)
myres

q1 <- Ehat[, 1]
q1
q1 <- c(mean(q1[1:2]+q1[1:1])/2, q1)

q2 <- Ehat[, 2] #second column
q2
q2 <- c(mean(q2[1:2]+q2[1:1])/2, q2)

q3 <- Ehat[, 3] #second column
q3
q3 <- c(mean(q3[1:2]+q3[1:1])/2, q3)

start_date <- as.Date("1975-01-01")
end_date <- as.Date("2007-01-01")
time <- seq(start_date, end_date, by = "3 months")
head(time)
q1a <- numeric(length(time))
q2a <- numeric(length(time))
q3a <- numeric(length(time))

impulse_response_df <- data.frame(Date = time, q1a = numeric(length(time)), q2a = numeric(length(time)), q3a = numeric(length(time)))
for (i in 1:length(time)) {
  impulse_response_df$q1a[i] <- mean(c(q1[3*(i-1) + 1], q1[3*(i-1) + 2], q1[3*(i-1) + 3]))
  impulse_response_df$q2a[i] <- mean(c(q2[3*(i-1) + 1], q2[3*(i-1) + 2], q2[3*(i-1) + 3]))
  impulse_response_df$q3a[i] <- mean(c(q3[3*(i-1) + 1], q3[3*(i-1) + 2], q3[3*(i-1) + 3]))
}
print(impulse_response_df)
impulse_response_df
# Matching dates and filling in BEAGDP values
beagdp1 <- read.table("/Users/mariaferreres/Downloads/OIL TT/beagdp.txt")
head(beagdp1)
tail(beagdp1)
beagdp1<- ts(beagdp1, start = c(1974,4),frequency = 4)
head(beagdp1)
beagdp1 <- beagdp1[-1, ]
head(beagdp1)
tail(beagdp1)
start_date1 <- as.Date("1975-01-01")
dates <- seq(start_date1, length.out = length(y), by = "3 months")
tail(dates)
beagdp_data <- data.frame(Date = dates, GDP_Change = beagdp1)
head(beagdp_data)
tail(beagdp_data)




head(impulse_response_df)
tail(impulse_response_df)
library(dplyr)

# Assuming your data frame is named impulse_response_df
impulse_response_df_filtered <- impulse_response_df %>%
  filter(Date <= "2007-10-01")

# Display the filtered data
print(impulse_response_df_filtered)
merged_data <- merge(impulse_response_df_filtered, beagdp_data, by = "Date")
merged_data
library(vars)


selected_columns <- c("q1a", "q2a", "q3a", "GDP_Change.V1", "GDP_Change.V2", "GDP_Change.V3")
filtered_data <- merged_data[selected_columns]
var_model <- VAR(filtered_data, p = 12, type = "const")
irf_result <- irf(var_model, impulse = "GDP_Change.V3", response = c("q1a", "q2a", "q3a"), n.ahead = 12, cumulative = FALSE)



var_model <- VAR(merged_ts)
irf_result <- irf(var_model, impulse = "GDP_Change.V3", response = c("q1a", "q2a", "q3a"), n.ahead = 12, cumulative = FALSE)
plot(irf_result, main = "Impulse Response Functions", col = c("blue", "red", "green"))

res1
head(res1)

irf_results <- irf(res1, impulse = "Growth.Oil", response = c("Growth.Oil", "Growth.Activity", "Real.Price.of.Oil"), n.ahead = horizon)
plot(irf_results, main = "Impulse Response Functions", col = c("blue", "red", "green"))



library(readxl)

inv_solve_AMat #equivalent to inv(chol(SIGMA)')

data_oil <- read_excel("//Users/mariaferreres/Downloads/Table Data Replication Oil TT.xlsx")
#Recursive Identification 

#growth rate world oil production 
groilproduction <- data_oil$`Growth Rate World Oil Production`
head(groilproduction)
grwoilproduction <- ts(groilproduction, start = c(1973,2),frequency = 4)


#global real activity 
gra <- data_oil$`Global Real Activity`
#CONVERT TO QUARTERLY FREQUENCY!!
gractivity <- ts(gra, start = c(1973,2),frequency = 4)
#real price of oil
rpo <- data_oil$`Real Price of Oil`
rpoil <- ts(rpo, start = c(1973,2),frequency = 4)

cpi <- read.table("/Users/mariaferreres/Downloads/OIL TT/cpi.txt")
beagdp <- read.table("/Users/mariaferreres/Downloads/OIL TT/beagdp.txt")

combined_ts <- ts.intersect(grwoilproduction, gractivity, rpoil)
d3 <- window(combined_ts, start = c(1973, 2), end = c(2007, 4))
date_vector <- seq(as.Date('1973-04-01'), length.out = nrow(d3), by = '3 months')
d3 <- cbind(Date = date_vector, d3)
colnames(d3) <- c("Date", "Growth Oil", "Growth Activity", "Real Price of Oil")
head(d3)
d3 <- d3[, -1]
head(d3)
start_date2 <- as.Date("1973-04-01")
dates2 <- seq(start_date2, length.out = length(d3), by = "3 months")
tail(dates)
data3 <- data.frame(Date = dates2, GDP_Change = d3)
data3



#see volatility of real price of oil 
plot.ts(d3, plot.type="multiple", 
        main="Oil", 
        xlab = "", 
        col = "blue")

###SEE VOLATILY REAL PRICE OF OIL IN A DIFFERENT PLOT 
#Let us create a new data set with the quarter as the first column
d1_1 <- as.data.frame(cbind(time(d3),d3))
colnames(d1_1) <- c("quarter","Growth Oil","Growth Activity","Real Price of Oil")

library(dplyr)
library(tidyr)
library(ggplot2)
# Now we will pivot to a "long" format
d1_2 <- d1_1 %>%
  pivot_longer(!quarter, names_to="myvar", values_to="val") %>%
  as.data.frame()

ggplot(data=d1_2, aes(quarter,val)) +
  geom_line(color = "steelblue") +
  geom_hline(yintercept=0, linetype="dashed", 
             color = "red") +
  labs(title = "Oil",
       y = "percentage", x = "") +
  facet_wrap(~myvar, ncol=1, scales = "free")


#Estimate the Reduced Form VAR
library(vars)
res1 <- VAR(d3, p=24, type = "const")





##########
#EXTENSION TIME SERIES REPLICATION 
#Extension
#Final Assignment Time Series Replication Code 
#3 December 
###EVOLUTION OF OIL DEMAND AND OIL SUPPLY SHOCKS ANNUAL AVERAGES 
#Figure 2 
#sample period:1973.2-2009.8
library(vars)
library(tseries)
library(urca)
library(foreign)
library(ks)
library(pracma)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(readxl)
data_extension <- read_table("/Users/mariaferreres/Desktop/Extension/data copy.txt")
head(data_extension)
tail(data_extension)
colnames(data_extension)
#growth rate world oil production 
Growth_Rate <- data_extension$Growth_Rate_World_Oil_Production
Growth_Rate_Oil_Production2 <- ts(Growth_Rate, start = c(1976,1),frequency = 12)

#global real activity 
Real_Activity <- data_extension$Global_Real_Activity
Global_Real_Activity2 <- ts(Real_Activity, start = c(1976,1),frequency = 12)

#real price of oil
Price_of_oil <- data_extension$Real_Price_of_Oil
Real_Price_of_Oil2 <- ts(Price_of_oil, start = c(1976,1),frequency = 12)

d_extension<- ts.intersect(
  growth_oil_production = Growth_Rate_Oil_Production2,
  global_real_activity = Global_Real_Activity2,
  real_price_of_oil = Real_Price_of_Oil2
)
head(d_extension)

q <- 3
time2 <- seq(from = as.Date("1976-01-01") + 1/12, to = as.Date("2015-01-01"), by = "months")
h <- 15
p <- 24
library(vars)
var <- VAR(d_extension, p=p, type = "const")
var
#Recursive identification 
myA <- matrix(data = NA, nrow = 3, ncol = 3)
myA[1,2] <- 0 
myA[1,3] <- 0
myA[2,3] <- 0
myA

res <- vars::SVAR(var, Amat = myA, estmethod = "direct",
                  hessian = TRUE, method="BFGS")

AMat2 <- res$A
solve(AMat2)
inv_solve_AMat <- solve(solve(AMat2))
inv_solve_AMat #equivalent to inv(chol(SIGMA)')
residuals2 <- residuals(var)
dim(residuals2)
dim(inv_solve_AMat)
Ehat_ex <- residuals2 %*% inv_solve_AMat


r1 <- Ehat_ex[, 1]
r1 <- c(mean(r1[1:2]+r1[1:1])/2, r1)

r2 <- Ehat_ex[, 2] #second column
r2
r2 <- c(mean(r2[1:2]+r2[1:1])/2, r2)

r3 <- Ehat_ex[, 3] #second column
r3
r3 <- c(mean(r3[1:2]+r3[1:1])/2, r3)

time2 <- 1973:2015
length(time2)

r1a <- numeric(length(time2))
r2a <- numeric(length(time2))
r3a <- numeric(length(time2))


for (i in 1:length(time2)) {
  r1a[i] <- mean(r1[12*(i-1) + 1:12])
  r2a[i] <- mean(r2[12*(i-1) + 1:12])
  r3a[i] <- mean(r3[12*(i-1) + 1:12])
  
}
length(time2)
length(r2a)

#FIGURE 2 
# Plot the first subplot
plot(time2, r1a, type = "l", ylim = c(-1, 1),
     xlab = "Year", ylab = "Oil Supply Shock",
     main = "Oil Supply Shock")
abline(h = 0, col = "red")

#Second subplot 
plot(time2, r2a, type = "l", ylim = c(-1, 1),
     xlab = "Year", ylab = "Aggregate Demand Shock",
     main = "Aggregate Demand Shock")
abline(h = 0, col = "red")


#Third subplot 
plot(time2, r3a, type = "l", ylim = c(-1, 1),
     xlab = "Year", ylab = "Oil Specific Demand Shock",
     main = "Oil-Specific Demand Shock")
abline(h = 0, col = "red")



horizon <- 15 
colnames(d_extension)

irf_results1 <- irf(var, impulse = "growth_oil_production", response = c("growth_oil_production", "global_real_activity", "real_price_of_oil"), n.ahead = horizon)
plot(irf_results1, main = "Impulse Response Functions", col = c("blue", "red", "green"))

irf_results2 <- irf(var, impulse = "global_real_activity", response = c("growth_oil_production", "global_real_activity", "real_price_of_oil"), n.ahead = horizon)
plot(irf_results2, main = "Impulse Response Functions", col = c("blue", "red", "green"))

irf_results3 <- irf(var, impulse = "real_price_of_oil", response = c("growth_oil_production", "global_real_activity", "real_price_of_oil"), n.ahead = horizon)
plot(irf_results3, main = "Impulse Response Functions", col = c("blue", "red", "green"))





#FIGURE 4 EXTENSION 

#Figure 4 
#24 November 
#Figure 1 
library(vars)
library(dplyr)
library(tidyverse)
library(ggplot2)
yhatfunc <- function(svarobj){
  
  # ---- Input -------- #
  # svarobj created with vars::SVAR
  # --------------------#
  
  
  # Number of variables in the VAR
  myk <- svarobj$var$K
  
  # Number of lags in the VAR
  myp <- svarobj$var$p
  
  # Estimated A matrix (K x K)
  AMat <- svarobj$A
  
  # Estimated B matrix (K x K)
  BMat <- svarobj$B
  
  # B^{-1}A matrix => (K x K)
  # Note: used to create structural shocks
  ABMat <- solve(BMat)%*%AMat
  
  # Matrix of reduced form residuals
  # rows = T-p, cols = K
  rf_resid <- residuals(svarobj$var)
  
  # Generate matrix of structural shocks
  # rows = K, cols = T-p
  str_shock <- ABMat%*%t(rf_resid)
  
  # Number of usable periods for historical decomposition
  # = Number of elements in the residual vectors = T-p
  T <- ncol(str_shock)
  
  # Phi: coefficient matrices for reduced form MA representation
  # array of dimension (K,K,T)
  myphimat <- Phi(svarobj$var, nstep = T)
  
  # Theta: coefficient matrices for structural MA representation
  # array of dimension (K,K,T)
  mythetamat <- array(data=NA, dim=c(myk,myk,T))
  for(i in 1:T){
    mythetamat[,,i] <- myphimat[,,i]%*%solve(ABMat)
  }
  
  
  # ----------------------------------------- #
  # -------- Generate yhat vectors ---------- #
  # Empty array of dimension (K+1, T, K)
  # First dimension: K shocks + total contribution of all shocks
  # Second dimension: time periods
  # Third dimension: variables in VAR
  yhat_mat <- array(data=NA, dim=c((myk+1),T,myk))
  
  # --- Main loop over variables in VAR
  for(k in 1:myk){
    
    # Matrix of zeros to hold results on yhat for k-th variable
    yhat_mat[,,k] <- matrix(data=0, nrow=(myk+1), ncol=T)
    
    # Compute yhat and populate the matrix
    # column i = yhat in period i; 
    # Loop over the usable periods 
    for (i in 1:T){
      
      # Temporary zero vector (Kx1)
      temp_vec1 <- matrix(data=0, nrow = myk, ncol = 1)
      
      # Loop to sum theta_s*w_{t-s} from s=0 to s=t-i
      for(j in 1:i){
        # Structural impulse response matrix (the theta matrix)
        mytheta <- mythetamat[,,j]
        
        # Empty vector
        temp_vec2 <- numeric(length = myk)
        
        # Multiply out the k-th row of mytheta with the w_{t-s} vector
        for(l in 1:myk){
          temp_vec2[l] <- mytheta[k,l]*str_shock[l,i-j+1]
        }
        # Add over iterations
        temp_vec1 <- temp_vec1 + matrix(
          data=temp_vec2, nrow=myk, ncol=1
        )
      }
      
      # --- Store result for period t=i
      # First k elements: contributions of each shock
      # Last element: yhat = sum of contributions
      yhat_mat[,i,k] <- matrix(
        data = c(temp_vec1, sum(temp_vec1)),
        nrow=(myk+1), ncol = 1
      )
      
    }
    
  } # End: Main loop over variables in VAR
  
  # ----- Return results
  # Returned object: array of dims: k+1, T, k
  return(yhat_mat)
  
}
tsplotyhat <- function(yhatobj,data,j){
  
  # ------- INPUT ---------
  # yhatobj: object created by yhat function
  # data: data set used for var
  # j: which variable to plot
  # ---------------------
  
  # Data set
  d1 <- data
  
  # Sample mean of variable
  mean_yj <- mean(d1[,j],na.rm = TRUE)
  
  # Usable observations
  T <- ncol(yhatobj[,,1])
  
  # Row holding yhat vector (this is the last row)
  k_new <- nrow(yhatobj[,,1])
  
  # --- Data frame for plot
  # Create variables
  dv1 <- data.frame(
    # Demeaned variable (usable observations)
    ydmean = (tail(d1[,j],n=T)-mean_yj),
    # Corresponding yhat
    yhat = yhatobj[k_new,,j]
  )
  # Pivot to long
  dv2 <- as.data.frame(dv1) %>%
    mutate(pd=row_number()) %>%
    pivot_longer(!pd, names_to = "ytype",
                 values_to = "val") %>%
    as.data.frame()
  
  
  # Create plot
  p <- ggplot(data = dv2, aes(x=pd,y=val,group=ytype)) +
    geom_line(aes(color=ytype)) +
    labs(
      title="Time series plot of demeaned-y and y-hat",
      x="periods",y="value"
    )
  
  # Return plot
  return(p)
}
bchrtyhat <- function(yhatobj,j,t1,t2){
  
  # ----- Input -----------#
  # yhatobj: object created by "yhatfunc"
  # j (integer): variable of interest
  # t1: initial period
  # t2: final period
  # -----------------------
  
  # Read matrix
  y_hat <- yhatobj[,,j]
  
  # Number of rows
  k1 <- nrow(y_hat)
  
  # Names of the vector of shocks
  vshocks <- paste0("shock",1:(k1-1))
  
  # Vector of names
  ch_n <- c(vshocks, "total")
  
  # Vector of change in values
  ch_val <- numeric(k1)
  for(i in 1:k1){
    ch_val[i] <- y_hat[i,t2] - y_hat[i,t1]
  }
  
  # Create data frame for plot
  d1 <- data.frame(nms=ch_n, vls=ch_val)
  
  # Create plot
  p <- ggplot(data = d1, aes(x=nms,y=vls)) +
    geom_bar(stat = "identity", fill="steelblue") +
    labs(
      title="Contribution of Each Shock to Total Change",
      x="",y="change"
    )
  
  # Return result
  return(p)
  
}
data_extension <- read_table("/Users/mariaferreres/Desktop/Extension/data copy.txt")
head(data_extension)
tail(data_extension)
colnames(data_extension)
#growth rate world oil production 
Growth_Rate <- data_extension$Growth_Rate_World_Oil_Production
Growth_Rate_Oil_Production2 <- ts(Growth_Rate, start = c(1976,1),frequency = 12)

#global real activity 
Real_Activity <- data_extension$Global_Real_Activity
Global_Real_Activity2 <- ts(Real_Activity, start = c(1976,1),frequency = 12)

#real price of oil
Price_of_oil <- data_extension$Real_Price_of_Oil
Real_Price_of_Oil2 <- ts(Price_of_oil, start = c(1976,1),frequency = 12)

d_extension<- ts.intersect(
  growth_oil_production = Growth_Rate_Oil_Production2,
  global_real_activity = Global_Real_Activity2,
  real_price_of_oil = Real_Price_of_Oil2
)
head(d_extension)



time <- seq(from = as.Date("1976-01-01") + 2/12, to = as.Date("2007-12-01"), by = "months")

q <- 3
t <- 419
h <- 15
p <- 24


#Reduced form VAR
library(vars)
var <- VAR(d_extension, p=24, type = "const")

myA <- matrix(data = NA, nrow = 3, ncol = 3)
myA[1,2] <- 0 
myA[1,3] <- 0
myA[2,3] <- 0


res <- vars::SVAR(var, Amat = myA, estmethod = "direct",
                  hessian = TRUE, method="BFGS")


AMat3 <- res$A
(solve(AMat3))

myres2 <- yhatfunc(svarobj = res)
myres2
# Load necessary libraries
library(ggplot2)
library(tidyr)
#compares observed values 'ydmean' to predicted values 'yhat' 
#first variables growth oil production 
tsplotyhat(yhatobj = myres, data = d2, j=1)
#global real activity 
tsplotyhat(yhatobj = myres, data = d2, j=2)
#real price of oil
tsplotyhat(yhatobj = myres, data = d2, j=3)

variable_names <- colnames(d2)
print(variable_names[1])
print(variable_names[2])
print(variable_names[3])

# Assuming yhat_mat is a three-dimensional array
yhat_mat <- myres2
# Extract variables
growth_oil_production <- yhat_mat[1,,3]
global_real_activity <- yhat_mat[2,,3]
real_price_of_oil <- yhat_mat[3,,3]

all_months <- seq(as.Date("1976-01-01"), as.Date("2015-01-01"), by = "month")
year <- as.numeric(format(all_months, "%Y"))
month <- as.numeric(format(all_months, "%m"))
time_data <- data.frame(year = year, month = month)
length(time_data)
head(time_data)
dim(time_data)

# Create data frames for each variable
ctrb_growth <- data.frame(pd = 1:length(growth_oil_production), contrib = growth_oil_production)
ctrb_global <- data.frame(pd = 1:length(global_real_activity), contrib = global_real_activity)
ctrb_price <- data.frame(pd = 1:length(real_price_of_oil), contrib = real_price_of_oil)

dim(ctrb_growth)
length(all_months)
start_date <- as.Date("1976-01-01")
ctrb_growth$Date <- seq(start_date, by = "1 month", length.out = nrow(ctrb_growth))
library(ggplot2)


#FIGURE 4 
quartz(width = 10, height = 6)
# Create separate line plots for each variable
plot_growth <- ggplot(data = ctrb_growth, aes(x = Date, y = contrib)) +
  geom_line(color = "blue") +
  labs(title = "Contribution of growth_oil_production",
       x = "period", y = "contribution") +
  scale_y_continuous(limits = c(-100, 100)) +
  theme_minimal()
print(plot_growth)


ctrb_global$Date <- seq(start_date, by = "1 month", length.out = nrow(ctrb_global))
plot_global <- ggplot(data = ctrb_global, aes(x = Date, y = contrib)) +
  geom_line(color = "green") +
  labs(title = "Contribution of global_real_activity",
       x = "period", y = "contribution") +
  scale_y_continuous(limits = c(-100, 100))+
  theme_minimal()

print(plot_global)


ctrb_price$Date <- seq(start_date, by = "1 month", length.out = nrow(ctrb_price))
plot_price <- ggplot(data = ctrb_price, aes(x = Date, y = contrib)) +
  geom_line(color = "red") +
  labs(title = "Contribution of real_price_of_oil",
       x = "date", y = "contribution") +
  scale_y_continuous(limits = c(-100, 100))+
  theme_minimal()
print(plot_price)






