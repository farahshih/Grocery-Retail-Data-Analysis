#### This script is used to build linear regression models to predict the sales of soft drinks
#### There are many user-defined functions list at the end of the script.
#### Questions we wanted to answer here:  
#### 1) What factors have significant influence on the sales of soft drinks?
#### 2) How competitor's price and promotion activities influence the selected product?
#### 3) How the model predicts?

setwd("./Retail_Analytics/")
library(data.table)  ## using fread() is much faster than read.csv()
library(plyr)
library(dplyr)
library(ggplot2)
library(dtplyr)
library(bit64)
library(psych)
options(scipen = 999)


##################### Load Data #####################
#####################################################
retailer<-fread('./Panel_Data/Master_Files/Latest/retailers.tsv',sep ='\t')
head(retailer)

products<-fread('./Scanner_Data/Exploratory_Models/Master_Files/Latest/products.tsv', sep ='\t',
                select = c('upc','upc_ver_uc','upc_descr','product_group_descr','brand_descr','size1_units','size1_amount','multi'))

### Select skus
brands<-unique(products$brand_descr)
pepsi<-brands[grepl("PEPSI", brands)]
cola<-brands[grepl("COCA-COLA", brands)]
soda_brands <-products %>% filter (brand_descr %in% pepsi | brand_descr %in% cola) %>% 
  filter(size1_amount == 12.00 & size1_units=="OZ")
unique(soda_brands$product_group_descr)
soda_brands<-transform(soda_brands, upc_descr=paste(upc_descr, paste(size1_amount, size1_units, sep=' '), multi, sep=' / '))

### Read data for each year and combine 
rms_path<-Sys.glob(file.path("./Scanner_Data/Exploratory_Models/", "Annual_Files_*", "rms_versions_*.tsv"))
store_path<-Sys.glob(file.path("./Scanner_Data/Exploratory_Models/", "Annual_Files_*", "stores_*.tsv"))
soda_path<-Sys.glob(file.path("./Scanner_Data/Exploratory_Models/1484_*.tsv"))
soda_path

datalist <-list()
for (i in 1:6) {
  dat <- read_and_subset(rms_path[i], store_path[i], soda_path[i], soda_brands)
  print(dim(dat))
  datalist[[i]] <- dat # add it to the datalist
}
## combine all data into one data frame
soda <- data.table::rbindlist(datalist)
rm(datalist, dat)

## prepare rms file
datalist <-list()
for (i in 1:6) {
  dat <-fread(rms_path[i], sep ='\t')
  dat <- merge(dat, soda_brands[,c(1:2), with=FALSE], by=c("upc","upc_ver_uc"))
  datalist[[i]] <- dat # add it to the datalist
}
rms <- data.table::rbindlist(datalist)
rm(datalist, dat)
rms<-merge(rms, soda_brands, by=c("upc","upc_ver_uc"))

rm(products)
save.image("soda")

### read product extra feature file
products_ext_path<-Sys.glob(file.path("./Scanner_Data/Exploratory_Models/", "Annual_Files_*", "products_extra_*.tsv"))
cols<-c("upc","upc_ver_uc","panel_year","flavor_descr","formula_descr","container_descr","organic_claim_descr")

datalist <-list()
for (i in 1:6) {
  dat <-fread(products_ext_path[i], sep ='\t', select = cols)
  dat <- merge(dat, rms[,c(1:3), with=FALSE], by=c("panel_year","upc","upc_ver_uc"))
  datalist[[i]] <- dat # add it to the datalist
}
products_ext <- data.table::rbindlist(datalist)
rm(datalist, dat)


##################### Transform Variables ########################
##################################################################
## compute new variables (unit_price, ttl_sales)
soda<-compute_new_vars(soda)

## create week number
soda<-make_week_num(soda)

# create upcid = upc + upc_ver_uc
soda<-transform(soda, upcid = paste0(upc, upc_ver_uc))



############## Select products and stores ################
##########################################################

### compute product popularity
soda_agg <- soda %>% 
  group_by(panel_year, upc) %>% 
  summarise(ttl_sales = sum(ttl_sales)) %>%
  arrange(desc(ttl_sales)) 
soda_20ls<-soda_agg %>% do(head(., n = 30))
soda_20ls<-soda_20ls[,2:4,with=FALSE]
soda_20ls<-merge(soda_20ls, rms, by=c("panel_year","upc"))
soda_20ls<-merge(soda_20ls, products_ext, by=c("panel_year","upc","upc_ver_uc"))
names(soda_20ls)

### select products according to pack size
soda_samepack<-soda_20ls %>% filter(multi==12)
names(soda_samepack)
soda_samepack<-soda_samepack[,c(1:5,7,11:14,8:10,6), with=FALSE]

soda_samepack<-transform(soda_samepack, upcid = paste0(upc, upc_ver_uc))
table(soda_samepack$upc_descr)  
length(unique(soda_samepack$upcid))

### select top20 products
soda_20<-soda %>% filter(upcid %in% unique(soda_samepack$upcid))
length(unique(soda_20$upcid))

### aggregate ttl_sales by store
TopStores<-soda_20 %>% group_by(store_code_uc) %>% 
  summarise(ttl_sales = sum(ttl_sales)) %>% 
  arrange(desc(ttl_sales)) 
dim(TopStores)
stores_ls<-TopStores %>% do(head(., n = 100))   
sum(stores_ls$ttl_sales)/sum(TopStores$ttl_sales)

sample_data<-soda_20 %>% filter(store_code_uc %in% stores_ls$store_code_uc) 

### remove stores that don't have either Feature or Display data 
sample_data<-sample_data[complete.cases(sample_data),]

sample_data %>% group_by(store_code_uc) %>% 
  summarise(feature_count = sum(feature), 
            display_count = sum(display))

length(unique(sample_data$store_code_uc)) 
unique(sample_data[,c("store_code_uc","retailer_code"), with=FALSE])

############## Prepare Predictors ################
##################################################
### encode promotion indicator
attach(sample_data)
sample_data$prom<-ifelse(feature==1 & display==1,"both",
                         ifelse(feature==1 & display==0, "feature",
                                ifelse(feature==0 & display==1, "display","No_Prom")))
sample_data$prom_int<-ifelse(feature==0 & display==0, 0, 1)
detach(sample_data)
head(sample_data)

sample_data$prom<-with(sample_data, as.factor(prom))

### select the pool of competitor 
sample_data<-merge(sample_data, rms[,c(1:3,4,6),with=FALSE], by=c("upc","upc_ver_uc","panel_year"))
head(sample_data)

unique(sample_data[,c('upcid','upc_descr','brand_descr'), with=FALSE])
# #(for coke)
ls<-c("PSI R CL CN FM 12P / 12 OZ / 12","PSI R CL CF CN FM 12P / 12 OZ / 12", "PSI-CT R CL CN 12P / 12 OZ / 12",
"PSI WLD CH R CH/CL CN FM 12P / 12 OZ / 12", "PSI VANILLA R VN/CL CN FM 12P / 12 OZ / 12", "PSI TWIST R LN/CL CN FM 12P / 12 OZ / 12")
compete_ls<-unique(sample_data[,c("upcid","upc_descr","brand_descr"), with=FALSE]) %>% filter(upc_descr %in% ls)

#(for pepsi)
# compete_ls<-unique(sample_data[,c("upcid","upc_descr","brand_descr"), with=FALSE])[17:23][,"upcid", with=FALSE]

selected<-sample_data %>% dplyr::select(store_code_uc, week, upcid, unit_price, prom_int) %>%
  filter(upcid %in% compete_ls$upcid)
head(selected)
# there are duplicated rows. Discard these data
selected[duplicated(selected[,c(1:3), with=FALSE]),]
ls<-selected[duplicated(selected[,c(1:3), with=FALSE]),]$upcid
selected<-selected %>% filter(!(store_code_uc==7892266 & week==7 & upcid %in% ls))

### 0. compute the fraction of competitors being promoted
df0 <- dcast(selected, store_code_uc + week ~ upcid, value.var = "prom_int")
df0 <- data.frame(df0)
df0$com_prom_frac <- rowMeans(df0[,c(3:ncol(df0))], na.rm = TRUE)
df0 <- df0[,c(1:2,ncol(df0))]
head(df0)

### reshape the data frame
df1 <- dcast(selected, store_code_uc + week ~ upcid, value.var = "unit_price")
df1

### sort competitors' price
df2<-apply(df1[,c(3:9), with=FALSE], 1, sort)
df2[1:10]

max_length <- max(sapply(df2,length))
df3<-t(sapply(df2, function(x){c(x, rep(NA, max_length - length(x)))}))
df3<-data.frame(df3) ## the column names are meaningless
#http://stackoverflow.com/questions/31752475/r-sorting-each-row-of-a-data-frame
#http://stackoverflow.com/questions/5531471/combining-unequal-columns-in-r
head(df3)
apply(df3, 2, function(x){sum(is.na(x))})  # pick top3 competitors
# the last few columns have lots of NAs, so we only keep top3 competitors' price
df4<-df3[,c(1:3)]

### clean the dataset
head(df1)
df4<-cbind(df1[,c(1:2), with=FALSE], df4)
df4<-df4[complete.cases(df4),]
names(df4)
names(df4)[3:5]<-c("cp1","cp2","cp3")
head(df4)
length(unique(df4$week))

### combine with respones data 
head(sample_data)

## pick the target that has sales almost every year (more data points)
# target<-sample_data %>% dplyr::select(store_code_uc, week, week_end, upc_descr, units, unit_price, prom, retailer_code) %>%
#  filter(upc_descr == "PSI R CL CN FM 12P / 12 OZ / 12")
target<-sample_data %>% dplyr::select(store_code_uc, week, week_end, upc_descr, units, unit_price, prom, retailer_code) %>%
  filter(upc_descr == "COCA-COLA R CL CN FP 12P / 12 OZ / 12")
unique(target$upc_descr)

final_df<-merge(target, df4, by=c("store_code_uc","week"))
final_df<-merge(final_df, df0, by=c("store_code_uc","week"))
sum(is.na(final_df$com_prom_frac))
head(final_df)

### clean data (delete stores that have only few data points. < 190 weeks)
length(unique(final_df$store_code_uc))  ## total 29 stores
length(unique(final_df$week)) ## total 313 weeks
table(final_df$store_code_uc) 
final_df<-final_df %>% filter(!(store_code_uc %in% c(5342350,7757995)))
table(final_df$store_code_uc) 

### add holiday event
holidays<-read.csv("./Nielsen_Project/Codes/holiday2.csv")
head(holidays)
final_df<-merge(final_df, holidays, by="week_end", all.x = TRUE)
final_df<-data.frame(final_df)

names(final_df)
sum(is.na(final_df[,c(13:21)]))
final_df[is.na(final_df)]<-0

### add seasonality
winter<-c(1:8, 49:60, 101:113, 153:165, 205:217, 257:269, 310:313)
spring<-c(9:22, 61:74, 114:126, 166:178, 218:230, 270:282)
summer<-c(23:35, 75:87, 127:139, 179:191, 231:243, 283:296)
fall<-c(36:48, 88:100, 140:152, 192:204, 244:256, 297:309)
attach(final_df)
final_df$season<-ifelse(week %in% winter, "winter",
                      ifelse(week %in% spring, "spring",
                             ifelse(week %in% summer, "summer", "fall")))
detach(final_df)


### re-arrange column sequence
names(final_df)
names(final_df)[c(6,17)]<-c("own_price","July.4th")
final_df<-final_df[,c(4,5,3,22,13:21,2,8,6,7,9:12)]
head(final_df)

### compute price differences
final_df<-transform(final_df, own_cp1_diff = own_price - cp1, own_cp2_diff = own_price - cp2,
                    own_cp3_diff = own_price - cp3)

### standardize price
final_df<-transform(final_df, scale_cp1=scale(cp1), scale_cp2=scale(cp2),scale_cp3=scale(cp3),
                    scale_own_cp1_diff=scale(own_cp1_diff), scale_own_cp2_diff=scale(own_cp2_diff),
                    scale_own_cp3_diff=scale(own_cp3_diff), scale_ownP=scale(own_price))
str(final_df)

### take log of unit_price
final_df<-transform(final_df, lg_own_price=log(own_price), lg_cp1=log(cp1),
                    lg_cp2=log(cp2),lg_cp3=log(cp3))

### take log of units sold
final_df<-transform(final_df, log_units=log(units))
head(final_df)

## format predictor types
str(final_df)
names(final_df)
final_df[,c(4:15)]<-lapply(final_df[,c(4:15)], function(x){as.factor(as.character(x))})

### Checking dependencies
table(final_df$retailer_code)
sub1<-final_df %>% filter (retailer_code == 128)
pairs.panels(sub1[,c(4,16:21)])
# cp1, cp2, cp3 are highly dependent ==> maybe just use cp1


################# Building Model #####################
#####################################################
## split data into training, validation, and test datasets
final_df<-split(final_df, portion = c(0.6, 0.2, 0.2))
names(final_df)

### feature selections (excluding those are highly dependent)
train_df<-subset(final_df, tag=="train")
fmla<-as.formula(paste("units~", paste(names(train_df)[c(3:13,15:18,21)], collapse= "+")))
fmla<-as.formula(paste("units~", paste(names(train_df)[c(3:13,15,17,21,32:33)], collapse= "+")))
fmla<-as.formula(paste("log_units~", paste(names(train_df)[c(3:13,15,17,21,32:33)], collapse= "+")))
lm_model<-lm(fmla, train_df)
s_model <- step(lm_model)
summary(s_model)
s_model$anova

### based on feature selection results, fit the model and plot residuals
## Pepsi
my_model<-fit_lm(final_df, "units~", c(3:12,15:18), train = TRUE) # price
my_model<-fit_lm(final_df, "units~", c(3:9,12,15,17,32,33), train = TRUE) # log price
my_model<-fit_lm(final_df, "log_units~", c(3:4, 6:9, 11:12,15,17, 32,33), train = TRUE)

## Coke
my_model<-fit_lm(final_df, "units~", c(3, 7:13,15:17,21), train = TRUE)
my_model<-fit_lm(final_df, "units~", c(3, 7:13,15,17,21,32), train = TRUE)
my_model<-fit_lm(final_df, "log_units~", c(3:8, 11:13,15,17,21,32:33), train = TRUE)

summary(my_model)
validation(final_df, my_model, data_tag = "validation")

### plot the residuals from different models (log-price=>units, log-price=>log-units, , price=>units)
vd_df <- subset(final_df, tag=="validation")
predictors<-names(my_model$model)[-1]
response<-names(my_model$model)[1]
pred <- predict(my_model, subset(vd_df, select = predictors))
mydf<-data.frame(vd_df[,c("week","retailer_code","prom","own_price",response)], pred=pred)
head(mydf)

# if it's log_units, transfer it to actual units sold
mydf<-transform(mydf, units = exp(log_units), pred_units = exp(pred))
mydf$residual<-mydf$pred_units - mydf$units
#mydf$residual<-mydf$pred - mydf$log_units

# if units, no need to transfer
mydf$residual<-mydf$pred - mydf$units

head(mydf)
mean(abs(mydf$residual))
table(mydf$retailer_code)
ggplot(subset(mydf, retailer_code==128),aes(x=week, y=residual, color=prom, group=prom)) + 
  geom_point(size=1.5) + ggtitle("Scatter plot of residuals - retailer(128)") + ylab("residual(units)")

## Examine those weeks with big residual value
week_num<-unique(sample_data[,c("week_end","week"), with=FALSE])
holi<-read.csv("./Nielsen_Project/Codes/holiday.csv")
week_num<-merge(week_num, holi, by=c("week_end"), all.x = TRUE)
week_num$holiday_int[is.na(week_num$holiday_int)]<-0
#write.csv(week_num, "week_holiday_matching.csv", row.names = FALSE)
mydf<-merge(mydf, week_num, by="week", all.x = TRUE)
mydf$holiday_int<-factor(mydf$holiday_int,levels = c(1,2,0),
                         labels = c("the week and before", "after", "not a holiday"))
str(mydf)
mydf<-mydf[order(-mydf$residual),]
head(mydf)

ggplot(subset(mydf, retailer_code==128),aes(x=week, y=residual, color=holiday_int, group=holiday_int)) + 
  geom_point(size=1.5) + ggtitle("Scatter plot of residuals - retailer(128)") + ylab("residual(units)")


## ...., retailer_code, own_cp difference
m3<-fit_lm(final_df, "log_units~", c(3:5,7:9,13:15), train = TRUE)
summary(m3)
validation(final_df, m3, data_tag = "validation")

## ...., retailer_code, log(price)
m4<-fit_lm(final_df, "units~", c(3:5,7,9,23:26), train = TRUE)
summary(m4)
validation(final_df, m4, data_tag = "validation")


######################## Visualization #####################
############################################################
## Scatter plot of own_price, cp1, cp2, cp3, cp4 vs. units sold
ggplot(final_df,aes(x=own_price, y=units, group=prom, color=prom)) + geom_point(size=1.5) + 
  theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))

ggplot(final_df,aes(x=own_cp1_diff, y=units, group=prom, color=prom)) + geom_point(size=1.5) + 
  theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))

table(final_df$retailer_code)
unique(final_df[,c("store_code_uc","retailer_code")])
sub<-final_df %>% filter(retailer_code == 128)
ggplot(sub,aes(x=lg_own_price, y=log(units), group=prom, color=prom)) + 
  geom_point(size=1.5) + ggtitle("Scatter plot of unit_price vs. unit sales - retailer(128)(5 stores)")
ggplot(sub,aes(x=own_price, y=log(units), group=prom, color=prom)) + 
  geom_point(size=1.5) + ggtitle("Scatter plot of unit_price vs. unit sales - retailer(128)(5 stores)")


####################### Functions ############################
##############################################################

#### Load data ans subset relevant brands
read_and_subset<-function(rms_file, store_file, scanner_data, selected_brands){
  rms<-fread(rms_file, sep ='\t')
  # only keep relevant rms
  rms <- merge(rms, selected_brands[,c(1:2), with=FALSE], by=c("upc","upc_ver_uc"))
  
  store<-fread(store_file, sep='\t', select = c("store_code_uc","retailer_code","channel_code","fips_state_descr","fips_county_descr"))
  store<-merge(store, retailer, by="retailer_code", all.x=TRUE, all.y=FALSE)
  store<-store %>% filter(channel_type == "Grocery")
  
  ## Read scanner data and select relevant upc
  scanner_df<-fread(scanner_data, sep ='\t') 
  scanner_df<-merge(scanner_df, rms, by="upc")
  scanner_df<-merge(scanner_df, store, by="store_code_uc")
  return(scanner_df)
}


#### compute new variables from the scanner data 
# unit_price = price/prmult
# total_sales = unit_price * units
compute_new_vars<-function(raw_scanner){
  raw_scanner$unit_price<-with(raw_scanner, price/prmult)
  raw_scanner$ttl_sales<-with(raw_scanner, unit_price*units)
  return((raw_scanner))
}

#### create week number 
make_week_num<-function(scanner){
  week_end<-unique(scanner$week_end)
  week_end<-data.frame(week_end = week_end[order(week_end)], week=c(1:length(week_end)))
  scanner<-merge(scanner, week_end, by="week_end")
  return(scanner)
}

#### plot time series plot of sales
store_sales_ts<-function(data, brand_name){
  title<-paste("Time series of sales of top SKUs ", brand_name, " in top stores",sep="")
  p<-ggplot(data,aes(x=week,y=ttl_sales,group=prom, color=prom)) +
    geom_point(size=1) + facet_grid(store_code_uc~upc_descr) + 
    theme(strip.text=element_text(size=7.5, face="bold"), legend.position="top") +
    ggtitle(title)
  print(p)
}

#### plot time series plot of units sold
store_units_ts<-function(data, brand_name){
  title<-paste("Time series of total units of top SKUs ", brand_name, " in top stores",sep="")
  p<-ggplot(data,aes(x=week,y=units,group=prom, color=prom)) +
    geom_point(size=1) + facet_grid(store_code_uc~upc_descr) + 
    theme(strip.text=element_text(size=7.5, face="bold"), legend.position="top") +
    ggtitle(title)
  print(p)
}

#### plot time series plot of unit_price
store_unitP_ts<-function(data, brand_name){
  title<-paste("Time series of unit_price of top SKUs ", brand_name, " in top stores",sep="")
  p<-ggplot(data,aes(x=week,y=unit_price, group=prom, color=prom)) +
    geom_point(size=1) + facet_grid(store_code_uc~upc_descr) + 
    theme(strip.text=element_text(size=7.5, face="bold"), legend.position="top") +
    ggtitle(title)
  print(p)
}

## perform cross-validation and return r-squared value
cv<-function(fit){
  mydata<-fit$model
  cvlm<-cv.lm(data=mydata, fit, m=10, printit = FALSE) 
  # computes the r-squared value
  cor(cvlm$cvpred, mydata[,1])**2
}

## split into training data, validation data, and test data
split<-function(cast_data, portion=c(0.6, 0.2, 0.2)){
  week_num<-unique(cast_data$week)
  first<-floor(length(week_num)*portion[1])
  second<-floor(length(week_num)*portion[2])
  train_wk<-week_num[1:first]
  vd_wk<-week_num[(first+1):(first+1+second)]
  cast_data$tag<-ifelse(cast_data$week %in% train_wk, "train", 
                        ifelse(cast_data$week %in% vd_wk, "validation", "test"))
  return(cast_data)
}

## fit a lm model
fit_lm<-function(cast_data, response_col, predictor_cols, train=TRUE){
  fmla<-as.formula(paste(response_col, paste(names(cast_data)[predictor_cols], collapse= "+")))
  if(train==TRUE){
    cast_data<-subset(cast_data, tag=="train")
    model<-lm(fmla, cast_data)
  }else{
    model<-lm(fmla, cast_data)
  }
  return(model)
}

## (validation set approach) predict on the validation data or test data and compute r-squared value
validation<-function(cast_data, fit, data_tag="validation"){
  sub <- subset(cast_data, tag==data_tag)
  predictors<-names(fit$model)[-1]
  response<-names(fit$model)[1]
  pred <- predict(fit, subset(sub, select = predictors))
  rvalue <- cor(pred, sub[,response])**2
  return(rvalue)
}

