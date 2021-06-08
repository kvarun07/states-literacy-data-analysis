## Varun Khurana
## 2019124

## ----comment=""------------------------------------
df = read.csv("C:\\Users\\varun\\Desktop\\eco_assignment\\eco_dataset.csv")
head(df, 5)


## ----comment=""------------------------------------
a = mean(x = df$GER)
print(a)


## ----comment=""------------------------------------
b = mean(x = df$EL)
print(b)


## ----comment=""------------------------------------
c = mean(x = df$DW)
print(c)


## ----comment=""------------------------------------
d = mean(x = df$BT)
print(d)


## ----comment=""------------------------------------
e = mean(x = df$GT)
print(e)


## ----comment=""------------------------------------
f = mean(x = df$CA)
print(f)


## ----comment=""------------------------------------
a = var(df$GER)
print(a)


## ----comment=""------------------------------------
a = var(df$EL)
print(a)


## ----comment=""------------------------------------
a = var(df$DW)
print(a)


## ----comment=""------------------------------------
a = var(df$BT)
print(a)


## ----comment=""------------------------------------
a = var(df$GT)
print(a)


## ----comment=""------------------------------------
a = var(df$CA)
print(a)


## ----echo=FALSE------------------------------------
hist(df$GER, 
     main = "Histogram of GER", 
     xlab = "Gross Enrollment Ratio (GER)", 
     col = "#66ff00")


## ----echo=FALSE, out.width = "400px", fig.align='center'----
hist(df$DW, 
     main = "Histogram of DW Facilities", 
     xlab = "Percentage of schools having Drinking Water Facility",
     col = "ORANGE")


## ----echo=FALSE, out.width="400px", fig.align='center'----
toilet_diff = df$BT - df$GT
hist(toilet_diff, 
     main = "Histogram of Difference in Toilet Facilities", 
     xlab = "Difference in % of schools having boys toilets and girls toilets", 
     col = "#FF0099")



## --------------------------------------------------
df2 = df
df3 = read.csv("C:\\Users\\varun\\Desktop\\eco_assignment\\literacy_rates_dataset.csv")
df2 = merge(df2, df3, by.x = "STATE", by.y = "State")


## ---- comment="", message=FALSE--------------------
df2 = subset(df2, select = -c(Male, Female, S.No., X..Change))
library(dplyr)
df2 = rename(df2, LIT = Literacy)
head(df2, 5)


## --------------------------------------------------
a0 <- quantile(df2$LIT, 0.00)   # 0 %ile (min literacy rate)
a <- quantile(df2$LIT, 0.33)    # 33 %ile 
b <- quantile(df2$LIT, 0.67)    # 67 %ile
c <- quantile(df2$LIT, 1.00)    # 100 %ile (max literacy rate)


## ----comment="", echo=FALSE------------------------
cat("Percentiles:\n")
print(a0)
cat("\n")
print(a)
cat("\n")
print(b)
cat("\n")
print(c)


## ----echo=FALSE------------------------------------
# Now labelling each state as per their Literacy category
df2$LIT_CATEGORY = c("")
for (i in 1:length(df2$LIT)){
  if (df2$LIT[i] <= a){
    df2$LIT_CATEGORY[i] = "LOW"
  }
  else if (df2$LIT[i] > a && df2$LIT[i] <= b){
    df2$LIT_CATEGORY[i] = "MEDIUM"
  }
  else if (df2$LIT[i] > b) {
    df2$LIT_CATEGORY[i] = "HIGH"
  }
  else{
    print("Missed...")
  }
}


## ----include=FALSE---------------------------------
count_high=0 
count_low=0
count_medium = 0
count_total = 0
length(df2$LIT_CATEGORY)
for (i in 1:length(df2$LIT_CATEGORY)){
  count_total = count_total +1
  
  if (df2$LIT_CATEGORY[i] == "HIGH"){
    count_high = count_high +1
  }
  else if (df2$LIT_CATEGORY[i] == "MEDIUM"){
    count_medium = count_medium +1
  }
  else if (df2$LIT_CATEGORY[i] == "LOW"){
    count_low = count_low +1
  }
  else{
      print("Missed... \n")
  }
}
print(count_high)
print(count_medium)
print(count_low)
print(count_high + count_medium + count_low)
print(count_total)

summary(df2$LIT_CATEGORY)


## ----comment="", echo=FALSE------------------------
df4 <- setNames(aggregate(x=df2$GER, by=list(df2$LIT_CATEGORY), 
                          FUN=mean), c("LIT_CATEGORY", "GER"))
df4 <- df4[order(-df4$GER),]
rownames(df4) <- NULL
knitr::kable(df4)


## ----comment="", echo=FALSE------------------------
df4 <- setNames(aggregate(x=df2$BT, by=list(df2$LIT_CATEGORY), 
                          FUN=mean), c("LIT_CATEGORY", "BT"))
df4 <- df4[order(-df4$BT),]
rownames(df4) <- NULL
knitr::kable(df4)


## ----comment="", echo=FALSE------------------------
df4 <- setNames(aggregate(x=df2$GT, by=list(df2$LIT_CATEGORY), 
                          FUN=mean), c("LIT_CATEGORY", "GT"))
df4 <- df4[order(-df4$GT),]
rownames(df4) <- NULL
knitr::kable(df4)


## ----comment="", echo=FALSE------------------------
df4 <- setNames(aggregate(x=df2$DW, by=list(df2$LIT_CATEGORY), 
                          FUN=mean), c("LIT_CATEGORY", "DW"))
df4 <- df4[order(-df4$DW),]
rownames(df4) <- NULL
knitr::kable(df4)


## ----comment="", echo=FALSE------------------------
df4 <- setNames(aggregate(x=df2$EL, by=list(df2$LIT_CATEGORY), 
                          FUN=mean), c("LIT_CATEGORY", "EL"))
df4 <- df4[order(-df4$EL),]
rownames(df4) <- NULL
knitr::kable(df4)


## ----comment="", echo=FALSE------------------------
df4 <- setNames(aggregate(x=df2$CA, by=list(df2$LIT_CATEGORY), 
                          FUN=mean), c("LIT_CATEGORY", "CA"))
df4 <- df4[order(-df4$CA),]
rownames(df4) <- NULL
knitr::kable(df4)


## --------------------------------------------------
df5 = df
df5$STATE_CATEGORY = c("")


## ----comment="", echo=FALSE------------------------
for (i in 1:length(df5$STATE)) {
  
  if ((df5$STATE[i] == "Himachal Pradesh") || 
      (df5$STATE[i] == "Uttarakhand") || 
      (df5$STATE[i] == "Manipur") || 
      (df5$STATE[i] == "Sikkim") || 
      (df5$STATE[i] == "Mizoram") || 
      (df5$STATE[i] == "Assam") || 
      (df5$STATE[i] == "Arunachal Pradesh") || 
      (df5$STATE[i] == "Nagaland") || 
      (df5$STATE[i] == "Tripura") || 
      (df5$STATE[i] == "Meghalaya")){
    df5$STATE_CATEGORY[i] = "NEHS"
  }
  
  else if ((df5$STATE[i] == "Delhi") || 
          (df5$STATE[i] == "Chandigarh") || 
          (df5$STATE[i] == "Daman & Diu") || 
          (df5$STATE[i] == "Puducherry") || 
          (df5$STATE[i] == "Goa") || 
          (df5$STATE[i] == "Dadra & Nagar Haveli") || 
          (df5$STATE[i] == "Andaman & Nicobar Islands") || 
          (df5$STATE[i] == "Jammu & Kashmir") || 
          (df5$STATE[i] == "Lakshadweep") || 
          (df5$STATE[i] == "") || 
          (df5$STATE[i] == "")){
    df5$STATE_CATEGORY[i] = "UTC"
  }
  
  else if ((df5$STATE[i] == "Karnataka") || 
          (df5$STATE[i] == "Maharashtra") || 
          (df5$STATE[i] == "Tamil Nadu") || 
          (df5$STATE[i] == "Telangana") || 
          (df5$STATE[i] == "Kerala") || 
          (df5$STATE[i] == "Andhra Pradesh") || 
          (df5$STATE[i] == "Odisha")){
    df5$STATE_CATEGORY[i] = "SS"
  }
  
  else if ((df5$STATE[i] == "Haryana") || 
          (df5$STATE[i] == "Gujarat") || 
          (df5$STATE[i] == "Punjab") || 
          (df5$STATE[i] == "Uttar Pradesh") || 
          (df5$STATE[i] == "Rajasthan") || 
          (df5$STATE[i] == "Madhya Pradesh") || 
          (df5$STATE[i] == "Jharkhand") || 
          (df5$STATE[i] == "Bihar") || 
          (df5$STATE[i] == "Chhattisgarh") || 
          (df5$STATE[i] == "West Bengal")){
    df5$STATE_CATEGORY[i] = "OTH"
  }
  
  else{
    df5$STATE_CATEGORY[i] = "Missed !"
    cat("\nMissed...", i)
  }
}


## ----comment=""------------------------------------
head(df5, 5)


## ----echo=FALSE------------------------------------
df6 <- setNames(aggregate(x=df5$GER, by=list(df5$STATE_CATEGORY), 
                          FUN=mean), c("STATE_CATEGORY", "Mean_GER"))
df7 <- setNames(aggregate(x=df5$GER, by=list(df5$STATE_CATEGORY), 
                          FUN=var), c("STATE_CATEGORY", "Variance_GER"))
df6 <- merge(df6, df7, by.x = "STATE_CATEGORY", by.y = "STATE_CATEGORY")
df6 <- df6[order(-df6$Mean_GER),]
rownames(df6) <- NULL
knitr::kable(df6)


## ----echo=FALSE------------------------------------
df6 <- setNames(aggregate(x=df5$DW, by=list(df5$STATE_CATEGORY), 
                          FUN=mean), c("STATE_CATEGORY", "Mean_DW"))
df7 <- setNames(aggregate(x=df5$DW, by=list(df5$STATE_CATEGORY), 
                          FUN=var), c("STATE_CATEGORY", "Variance_DW"))
df6 <- merge(df6, df7, by.x = "STATE_CATEGORY", by.y = "STATE_CATEGORY")
df6 <- df6[order(-df6$Mean_DW),]
rownames(df6) <- NULL
knitr::kable(df6)


## ----echo=FALSE------------------------------------
df6 <- setNames(aggregate(x=df5$EL, by=list(df5$STATE_CATEGORY), 
                          FUN=mean), c("STATE_CATEGORY", "Mean_EL"))
df7 <- setNames(aggregate(x=df5$EL, by=list(df5$STATE_CATEGORY), 
                          FUN=var), c("STATE_CATEGORY", "Variance_EL"))
df6 <- merge(df6, df7, by.x = "STATE_CATEGORY", by.y = "STATE_CATEGORY")
df6 <- df6[order(-df6$Mean_EL),]
rownames(df6) <- NULL
knitr::kable(df6)


## ----echo=FALSE------------------------------------
df6 <- setNames(aggregate(x=df5$BT, by=list(df5$STATE_CATEGORY), 
                          FUN=mean), c("STATE_CATEGORY", "Mean_BT"))
df7 <- setNames(aggregate(x=df5$BT, by=list(df5$STATE_CATEGORY), 
                          FUN=var), c("STATE_CATEGORY", "Variance_BT"))
df6 <- merge(df6, df7, by.x = "STATE_CATEGORY", by.y = "STATE_CATEGORY")
df6 <- df6[order(-df6$Mean_BT),]
rownames(df6) <- NULL
knitr::kable(df6)


## ----echo=FALSE------------------------------------
df6 <- setNames(aggregate(x=df5$GT, by=list(df5$STATE_CATEGORY), 
                          FUN=mean), c("STATE_CATEGORY", "Mean_GT"))
df7 <- setNames(aggregate(x=df5$GT, by=list(df5$STATE_CATEGORY), 
                          FUN=var), c("STATE_CATEGORY", "Variance_GT"))
df6 <- merge(df6, df7, by.x = "STATE_CATEGORY", by.y = "STATE_CATEGORY")
df6 <- df6[order(-df6$Mean_GT),]
rownames(df6) <- NULL
knitr::kable(df6)


## ----echo=FALSE------------------------------------
df6 <- setNames(aggregate(x=df5$CA, by=list(df5$STATE_CATEGORY), 
                          FUN=mean), c("STATE_CATEGORY", "Mean_CA"))
df7 <- setNames(aggregate(x=df5$CA, by=list(df5$STATE_CATEGORY), 
                          FUN=var), c("STATE_CATEGORY", "Variance_CA"))
df6 <- merge(df6, df7, by.x = "STATE_CATEGORY", by.y = "STATE_CATEGORY")
df6 <- df6[order(-df6$Mean_CA),]
rownames(df6) <- NULL
knitr::kable(df6)


## ----comment="", echo=FALSE------------------------
df8 <- df
reg_a <- lm(GER ~ DW + BT + GT + EL + CA, data=df8)
summary(reg_a)


## ----comment="", echo=FALSE------------------------
X <- model.matrix(reg_a)
sigma_sq <- (summary(reg_a)$sigma)**2      # sigma^2
inv_matrix <- solve(t(X) %*% X)            # (X'X) inverse
var_beta_a <- sigma_sq*inv_matrix
options(digits = 4)
#var_beta_a
knitr::kable(var_beta_a)


## ----comment="", echo=FALSE------------------------
options(scipen=1, digits = 4)
knitr::kable(diag(var_beta_a), col.names = c("Variance"))
options(scipen=0, digits=7)


## ----include=FALSE---------------------------------
var_a <- vcov(reg_a)
var_a


## ----include=FALSE---------------------------------
(summary(reg_a)$sigma)**2


## ----include=FALSE---------------------------------
df9 = df2
df9$H = c(0)
df9$M = c(0)
df9$L = c(0)


## ----include=FALSE---------------------------------
for (i in 1:length(df9$LIT_CATEGORY)){
  if (df9$LIT_CATEGORY[i] == "HIGH"){
    df9$H[i] = 1
  }
  else if (df9$LIT_CATEGORY[i] == "MEDIUM"){
    df9$M[i] = 1
  }
  else if (df9$LIT_CATEGORY[i] == "LOW"){
    df9$L[i] = 1
  }
  else {
    cat("Missed:", i, "\n")
  }
}


## ----comment="", echo=FALSE------------------------
reg_b <- lm(GER ~ DW + BT + GT + EL + CA + H + M, data=df9)
summary(reg_b)


## ----comment="", echo=FALSE------------------------
X <- model.matrix(reg_b)
sigma_sq <- (summary(reg_b)$sigma)**2      # sigma^2
inv_matrix <- solve(t(X) %*% X)            # (X'X) inverse
var_beta_b <- sigma_sq*inv_matrix
options(digits = 4)
#var_beta_b
knitr::kable(var_beta_b)


## ----echo=FALSE------------------------------------
options(scipen=1, digits = 4)
knitr::kable(diag(var_beta_b), col.names = c("Variance"))
options(scipen=0, digits=7)


## ----include=FALSE---------------------------------
(summary(reg_b)$sigma)**2


## ----include=FALSE---------------------------------
df10 = df5
df10$N = c(0)
df10$U = c(0)
df10$S = c(0)
df10$O = c(0)


## ----include=FALSE---------------------------------
for (i in 1:length(df10$STATE_CATEGORY)){
  if (df10$STATE_CATEGORY[i] == "NEHS"){
    df10$N[i] = 1
  }
  else if (df10$STATE_CATEGORY[i] == "UTC"){
    df10$U[i] = 1
  }
  else if (df10$STATE_CATEGORY[i] == "SS"){
    df10$S[i] = 1
  }
  else if (df10$STATE_CATEGORY[i] == "OTH"){
    df10$O[i] = 1
  }
  else {
    cat("Missed:", i, "\n")
  }
}


## ----comment="", echo=FALSE------------------------
reg_c <- lm(GER ~ DW + BT + GT + EL + CA + N + U + S, data=df10)
summary(reg_c)


## ----comment="", echo=FALSE------------------------
X <- model.matrix(reg_c)
sigma_sq <- (summary(reg_c)$sigma)**2      # sigma^2
inv_matrix <- solve(t(X) %*% X)            # (X'X) inverse
var_beta_c <- sigma_sq*inv_matrix
options(digits = 3)
#var_beta_c
knitr::kable(var_beta_c)


## ----include=FALSE---------------------------------
vcov(reg_c)


## ----echo=FALSE------------------------------------
options(scipen=1, digits = 4)
knitr::kable(diag(var_beta_c), col.names = c("Variance"))
options(scipen=0, digits=7)


## ----include=FALSE---------------------------------
(summary(reg_c)$sigma)**2


## --------------------------------------------------


