# Retail Scanner Data Prediction - A list of user-defined functions
Fu-Chi Shih  
Summary:    
This is a support document for the main analysis described in Modeling_AggSales.md. Here describes all functions that we created to clean data, aggregate sales data, and build models. 

### Load Required Libraries

```r
library(ggplot2) ; library(dplyr) ; library(forecast)
library(psych) ; library(DAAG) ;library(reshape2)
```

### Define Data Cleaning and Data Aggregation Functions

#### 1. Load UPC information 
Read upc dataset, delete upc that has missing value, and combine product description (``DESCRIP``) and size (``SIZE``) as a new column ``des``.         
**- Parameter :**        
``path``: the file path for upc dataset     
**- Return :**     
A cleaned data frame of all upc information 

```r
readupc<-function(path){
    upc<-read.csv(path,quote = "")
    upc$DES<-paste(upc$DESCRIP,upc$SIZE,sep=" ")
    names(upc)<-tolower(names(upc))
    upc<-upc[,c("upc","des")]
    upc<-upc[upc$des!=" ",]
    return(upc)
}
```

#### 2. Clean sales data
Delete suspect data* and entries that have no sales (``price``, ``move``, or ``qty`` equal 0)    
*(suspect data means ``ok``=0. The feature ``ok`` is a flag set by Dominick's to indicate that the data for that week are suspects.)     
**- Parameter :**      
``rawdata``: a data frame containing weekly sales data of a specific category     
**- Return :**      
a cleaned data frame of weekly sales data of a specific category


```r
clean<-function(rawdata){
    names(rawdata)<-tolower(names(rawdata))
    filter (rawdata, ok==1, price!=0, move!=0, qty!=0)
}
```

#### 3. Merge sales table and upc table into one data frame
**- Parameter :**        
``data``: a cleaned data frame of weekly sales of a specific category    
``upc``: a cleaned data frame of all upc information    
**- Return :**       
a full data frame of weekly sales data and upc information 

```r
merge_tables<-function(data, upc){
    m<-merge(data, upc, by="upc")
    return (m)
}
```

#### 4. Calculate new variables
Compute sales, price per unit, and encode a promotion indictator         
**- Parameter :**        
``data``: a full data frame of weekly sales data and upc information    
**- Return :**       
a full data frame of weekly sales data, upc information, and other new variables (``unit_price``, ``sales``, and ``prom``) 

```r
mutate_data<-function(data){
    # compute unit_price and sales
    m<-data %>% mutate(sales=price*move/qty) %>% 
        mutate(upc=abs(upc)) %>%
        mutate(unit_price=price/qty)
    
    # encode all promotion types as 1, no promotion as 0
    m$prom<-1
    m$prom[m$sale==""]<-0
    return(m)
}
```

#### 5. Merge + Mutate
Combine function merge_tables and mutate_data into one data processing function   
**- Parameter :**        
``data``: a cleaned data frame of weekly sales data of a specific category    
``upc``: a cleaned data frame of all upc information    
**- Return :**       
a full data frame of weekly sales, upc information, and other new variables (``unit_price``, ``sales``, and ``prom``) 

```r
process_data<-function(data, upc){
    cleaned_df<-merge_tables(data, upc)
    cleaned_df<-mutate_data(cleaned_df)
    return(cleaned_df)
}
```

#### 6. Compute SKUs popularity
Aggregate sales data by upc and sort each product by sales   
**- Parameter :**        
``cleaned_df``: a full data frame of weekly sales data, upc information, and other new variables (``unit_price``, ``sales``, and ``prom``)     
``upc``: a cleaned data frame of all upc information  
**- Return :**       
a data frame that ranks each product's poplarity by its sales percentage  

```r
agg<-function(cleaned_df, upc){
    agg_data<-aggregate(cleaned_df$sales, by=list(cleaned_df$upc), FUN=sum)
    names(agg_data)<-c("upc","sales")
    agg_data<-merge(agg_data, upc,by="upc")
    agg_data<-agg_data[order(agg_data$sales, decreasing = TRUE),]
    agg_data$pct<-prop.table(agg_data$sales)
    agg_data$rank<-c(1:nrow(agg_data))
    return (agg_data)
}
```

#### 7. Compute promotion frequency for each SKU
Compute how often the product was promoted. We use following formulation to calculate promotion frequency: Total times of promotions in all stores and all weeks / [(total number of stores) + (total weeks)]    
**- Parameter :**        
``cleaned_df``: a full data frame of weekly sales data, upc information, and other new variables (``unit_price``, ``sales``, and ``prom``)       
``agg``: a data frame that ranks each product's poplarity by its sales percentage  
**- Return :**       
a data frame that contains each product's poplarity and its promotion frequency 

```r
prom_count<-function(cleaned_df, agg){
    mydata<-cleaned_df %>% group_by(upc) %>% 
      summarise(prom_count=sum(prom), n_store=n_distinct(store), n_week = n_distinct(week))
    mydata$prom_freq<-with(mydata, prom_count/(n_store*n_week))
    mydata<-mydata[,c("upc","prom_freq")]
    final<-merge(agg, mydata, by="upc", sort=FALSE)
    return(final)
}
```

#### 8. Compute store-weighted price for each SKU in each week
Remember our sales data is on store-level and collected weekly. In order to predict total sales from all stores, we need to have a weighted price for all stores at each week. We use this formulation to compute the weighted pric of each sku at each week: Summation of the price in each store*sales in each store / total sales in all stores.    
**- Parameter :**        
``cleaned_df``: a full data frame of weekly sales data, upc information, and other new variables (``unit_price``, ``sales``, and ``prom``)       
**- Return :**       
a full data frame of weekly sales data, upc information, and other new variables (``unit_price``, ``sales``, ``prom``, and ``w_price``)   


```r
sku_ttl<-function(cleaned_df){
    sku<-cleaned_df %>% select(store, week, sales, upc, prom, move) %>%
        group_by(upc,week) %>%
        summarise(ttlsales=sum(sales), prom_n = sum(prom), store_n = n_distinct(store), ttlmv=sum(move))
    # prom_n: means number of stores running promotion that week
    # store_n: number of stores having sales data that week
    
    ## compute store-weighted price of each sku per week
    agg2<-cleaned_df %>% group_by(upc, week) %>% mutate(w_price=sum(unit_price*move)/sum(move))
    agg2<-agg2[,c("upc","week","w_price")]
    agg2<-agg2[!duplicated(agg2), ]
    final<-inner_join(sku,agg2,by=c("upc","week"))
    return (final)
}
```


#### 9. Subet the sales data of the ith-popular product
This function select the sales data of the i-th popular product.   
**- Parameter :**        
``cleaned_df``: a full data frame of weekly sales data, upc information, and other new variables (``unit_price``, ``sales``, ``prom``, and ``w_price``)           
``agg``:a data frame that ranks each product's poplarity by its sales percentage  
``i``: a scalar indicating the rank of the sku that user want to look at    
**- Return :**       
all weekly sales data of the i-th popular product


```r
sub_sku<-function(cleaned_df, agg, i){
    i<-as.numeric(i)
    temp<-cleaned_df %>% filter(upc %in% agg$upc[i])
    return (temp)
}
```

#### 10. Perform 10-folds cross-validation and compute the r-squared value for all folds
**Parameter :**        
``fit``: a fitted ``lm`` object   
**- Return :**       
R-squared value from 10-folds cross-validation 

```r
cv<-function(fit){
  mydata<-fit$model
  cvlm<-cv.lm(data=mydata, fit, m=10, plotit = FALSE) 
  # computes the r-squared value for all folds
  cor(cvlm$cvpred, mydata$ttlsales)**2
}
```

#### 11. Averge promotion frequency 
For each week, this function computes average promotion frequency in the past 4 weeks.    
**- Parameter :**        
``temp``:  all weekly sales data of the i-th popular product         
``period``: a scalar indicating the length of period that the user wants to use  
**- Return :**       
all weekly sales data of the i-th popular product, with one new variable added: ``promfreq``  


```r
prom_freq<-function(period=4, temp){
  ttl_prom_n<-rollapply(temp$prom_n, period, sum) # sum total times of promotions ran for every passing period
  max_store_n<-rollapply(temp$store_n, period, max) # return the maximum number of stores runing promotion for each passing period
  promfreq<-ttl_prom_n/(max_store_n*period) # compute average promotion frequency
  promfreq<-c(rep(NA, period-1),promfreq)
  final<-merge(temp, promfreq, by=0, sort=FALSE)[,-1] #delete the 1st column: row.names
  names(final)[ncol(final)]<-"promfreq"  # change the name of the last column
  return(final)
}
```


#### 12. Compute the regular price
For each week, this function computes the most common price in the past 12 weeks.   
**- Parameter :**  
``cat``: a full data frame of weekly sales data, upc information, and other new variables   
``agg``:a data frame that ranks each product's poplarity by its sales percentage  
``i``: a scalar indicating the rank of the sku that user want to look at     
``period``: a scalar indicating the length of period that the user wants to use    
**- Return :**         
a data frame of the regular price for each week


```r
find_reg_price<-function(cat, agg, i, period=12){
  sub<-sub_sku(cat, agg, i) #subset raw data of the i-th sku
  period_seq <- sort(unique(sub$week))
  tot_len <- length(period_seq)
  final <- data.frame(week=sort(unique(sub$week)), reg_price=rep(NA,tot_len))
  for (t in 1:(tot_len-period+1)){
    pool<-period_seq[t:(t+period-1)]  # select the week number each iteration to look at
    end <- pool[period] #define the last week in this pool
    sub_pool <- sub %>% filter(week %in% pool) %>% select(store, week, unit_price)
    reg <- names(which.max(table(sub_pool$unit_price)))
    final[final$week==end,"reg_price"] <- reg
  }
  final$reg_price <- as.numeric(final$reg_price)
  return(final)
}
```


#### 13. Compute reference price per week
We develope the following formula to compute reference price.   
reference price of current week = reference price of the previous week * alpha + actual price of the previous week * (1 - alpha)      
Note: Assuming that the current price in the first week is also the reference price for the first week.     
**- Parameter :**  
``alpha``: a scalar (used as weighting of actual price)   
``tempdata``: all weekly sales data of the product        
**- Return :**         
all weekly sales data of the product, with new variables added: ``price_ref_diff``, ``ref+wp.ratio``,``wp_reg.ratio``  


```r
reference_price<-function(tempdata, alpha){
    tot_period = dim(tempdata)[1]
    X = data.frame(tempdata[,"w_price"])   # format as df
    r0 = data.frame(X[1,]) #format as df
    r = r0
    for (i in 1:(tot_period-1)){
        tmp = (alpha*r[i,])+((1-alpha)*X[i,])
        r = data.frame(rbind(r, tmp))
    }
    temp<-cbind(tempdata, r)
    names(temp)[ncol(temp)]<-"reference_p"  # change the name of the last column
    temp$price_ref_diff<-with(temp,w_price-reference_p)
    temp$ref_wp.ratio<-with(temp,reference_p/w_price)
    temp$wp_reg.ratio<-with(temp,w_price/reg_price)
    return(temp)
}
```


#### 14. Compute the average weeks since last promotion
Note: Assuming that all stores were running promotion during the week before the first week.  
**- Parameter :**  
``data``: a full data frame of weekly sales data, upc information, and other new variables   
``agg``:a data frame that ranks each product's poplarity by its sales percentage  
``i``: a scalar that indicates the i-th product that the user wants to look at    
``tempdata``: all weekly sales data of the i-th popular product    
**- Return :**         
all weekly sales data of the i-th popular product, with a new variable added: ``last_prom``


```r
last_prom<-function(data,agg,tempdata,i){
  sub<-sub_sku(data, agg, i) #subset raw data of the i-th sku
  matrix<-tapply(sub$prom, list(sub$week, sub$store), mean)
  lp = matrix
  matrix[is.na(matrix)] <- -1
  for (j in 1:ncol(matrix)){
    p = as.numeric(rownames(matrix)[1])-1
    for (m in 1:nrow(matrix)){
      if (matrix[m,j] == 0){
        lp[m,j] <- as.numeric(rownames(matrix)[m]) - p;
        p <- p;
      } else if (matrix[m,j] == 1){
        lp[m,j] <- as.numeric(rownames(matrix)[m]) - p;
        p <- as.numeric(rownames(matrix)[m]);
      } else {
        p <- p
      }
    }
  }
  ave_lp<-matrix(apply(lp, 1, mean, na.rm=TRUE))
  update<-merge(tempdata, ave_lp, by=0, sort=FALSE)[,-1] #delete the 1st column: row.names
  names(update)[ncol(update)]<-"last_prom" #change the name of the last column
  return(update)
} 
```

#### 15. Compute the promotion factor
If the current week has promotion (either price discount and other promotions), this variable (``prom.last_prom``) represents the average periods of time since last promotion. If the current week has no promotion, ``prom.last_prom`` is encoded as 0.   
**- Parameter :**     
``tempdata``: all weekly sales data of the i-th popular product (must have variable ``last_prom`` in the ``tempdata`` data frame)   
**- Return :**         
all weekly sales data of the i-th popular product, with a new variable added: ``prom.last_prom``   


```r
promotion_factor<-function(tempdata){
  tempdata[tempdata$prom_n > 0, "prom"] <- 1
  tempdata[tempdata$prom_n == 0, "prom"] <- 0
  tempdata$discount<-as.numeric(with(tempdata, reg_price > w_price))
  tempdata$prom_max<-apply(tempdata[,c("prom","discount")], 1, max)
  tempdata$prom.last_prom<-with(tempdata, last_prom*prom_max)
  return(tempdata)
}
```

#### 16. Multiple time series plot
**- Parameter :**     
``tempdata``: all weekly sales data of the top-i product and all new variables     
**- Return :**         
a multiple time series plot of ``ttlsales`` (total sales), ``w_price`` (weighted price), ``n_prom`` (number of stores running promotion)     

```r
multi_tsplot<-function(tempdata){
    sub<-tempdata[,c("week","ttlsales","prom_n","w_price")]
    sub_melt<-melt(sub, id.vars = "week")
    p<-ggplot(sub_melt,aes(x=week, y=value)) + geom_line() + 
        facet_grid(variable~., scales = "free_y") + 
        ggtitle("Multiple Time Series Plots") +
        theme(strip.text=element_text(size = 15, color = "orangered"), 
              plot.title=element_text(vjust = +2, size = 17, color = "blue", face="bold"))
    print(p)
}
```

#### 17. Plot price vs. reference price
**- Parameter :**     
``tempdata``: all weekly sales data of the i-th popular product (including the new variable ``reference_p``)     
**- Return :**         
a time series plot of ``w_price`` (weighted price) and ``reference_p`` (reference price)


```r
Rprice_plot<-function(tempdata){
    time_period = 1:nrow(tempdata)
    plot(time_period, tempdata$w_price, type = "o", col = "red")
    lines(time_period, tempdata$reference_p, type = "l", col = "black", lwd=2)
    title(main = "Price(red) vs Referene Price(black)")
}
```
