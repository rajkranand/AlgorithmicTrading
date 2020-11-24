library(quantmod)
setwd("C:/Users/gaura/Dropbox (MIT)/MIT Acad/458 Financial Data Science and Computing")

### Loading SPX data from file
spx_data <- read.csv("spx_data.csv", header = TRUE)
## Skipping the first row containing NA
spx_data <- spx_data[2:nrow(spx_data),]
## Removing extra columns
spx_data <- spx_data[,c("Date","PX_OPEN","PX_HIGH","PX_LOW","PX_LAST")]
## Converting date column
spx_data$Date <- as.Date(spx_data$Date, format = "%m/%d/%Y")
## Converting other columns to float
spx_data[names(spx_data) != "Date"] <- lapply(spx_data[names(spx_data) != "Date"], function(x) as.numeric(as.character(x)))

## Finding date range
spx_data$Date[nrow(spx_data)] - spx_data$Date[1]

## Checking completeness of data
summary(spx_data)

## Data Integrity check
#Checking if PX_HIGH is greater than PX_LOW
sum(which(spx_data$PX_HIGH < spx_data$PX_LOW)) # Pass

#Checking for negative prices
apply(spx_data[,names(spx_data) != "Date"], 2, function(x) sum(which(x < 0))) ## Pass


#Checking for zero prices
apply(spx_data[,names(spx_data) != "Date"], 2, function(x) sum(which(x == 0))) ## Pass

#Checking for duplications
print(paste0("Duplicated rows in spx data: ", dim(spx_data[duplicated(spx_data),])[1])) ## Pass

#Checking if high is the highest value and low is the lowest value
sum(spx_data$PX_HIGH != apply(spx_data[,c("PX_OPEN", "PX_LAST", "PX_LOW", "PX_HIGH")],1, max),na.rm = TRUE)  ## Pass
sum(spx_data$PX_LOW != apply(spx_data[,c("PX_OPEN", "PX_LAST", "PX_LOW", "PX_HIGH")],1, min),na.rm = TRUE)  ## Pass

## Probability that market daily high occurs at open
sum(spx_data$PX_HIGH == spx_data$PX_OPEN, na.rm = TRUE)/sum(!is.na(spx_data$PX_HIGH))

## Probability that market daily high occurs at close
sum(spx_data$PX_HIGH == spx_data$PX_LAST, na.rm = TRUE)/sum(!is.na(spx_data$PX_HIGH))

## Probability that market daily low occurs at open
sum(spx_data$PX_LOW == spx_data$PX_OPEN, na.rm = TRUE)/sum(!is.na(spx_data$PX_LOW))

## Probability that market daily low occurs at close
sum(spx_data$PX_LOW == spx_data$PX_LAST, na.rm = TRUE)/sum(!is.na(spx_data$PX_LOW))

## Finding probabilities for data from when HIGH, LOW, OPEN and LAST are different
## Finding date from when the data are different
new_date <- spx_data$Date[which.min(spx_data$PX_HIGH == apply(spx_data[,c("PX_OPEN", "PX_LAST", "PX_LOW", "PX_HIGH")],1, mean))]

new_data <- spx_data[which(index(spx_data$Date) == new_date):nrow(spx_data),]
sum(new_data$PX_HIGH == new_data$PX_OPEN, na.rm = TRUE)/sum(!is.na(new_data$PX_HIGH))
sum(new_data$PX_HIGH == new_data$PX_LAST, na.rm = TRUE)/sum(!is.na(new_data$PX_HIGH))
sum(new_data$PX_LOW == new_data$PX_OPEN, na.rm = TRUE)/sum(!is.na(new_data$PX_LOW))
sum(new_data$PX_LOW == new_data$PX_OPEN, na.rm = TRUE)/sum(!is.na(new_data$PX_LOW))

## Variance Ratio test for PX_HIGH
Variance <- var(diff(log(spx_data$PX_HIGH[!is.na(spx_data$PX_HIGH)])))
for (n in 2:100) {
  Variance[n] <- var(diff(log(spx_data$PX_HIGH[!is.na(spx_data$PX_HIGH)][seq(from=n, to=length(spx_data$PX_HIGH[!is.na(spx_data$PX_HIGH)]), by=n)])))
}
plot(Variance,xlab="n",main="Variance of Returns From n-day
Observations");grid()

getSymbols("^GSPC",
            from = "1982/09/15",
            to = "1982/10/12",
            periodicity = "daily")

### Intraday range
closest_index_to_start_date <- which(abs(spx_data$Date - as.Date("1980-01-01")) == min(abs(spx_data$Date - as.Date("1980-01-01"))))
closest_index_to_end_date <- which(abs(spx_data$Date - as.Date("2011-08-30")) == min(abs(spx_data$Date - as.Date("2011-08-30"))))
intra_data <- spx_data[closest_index_to_start_date:closest_index_to_end_date,]

intraday_range <- data.frame("Date" = intra_data$Date, "Intraday_range" = (intra_data$PX_HIGH - intra_data$PX_LOW)/intra_data$PX_LOW)
top20_intraday <- intraday_range[order(intraday_range$Intraday_range, decreasing = TRUE)[1:20],]

## Finding number of dates in final 3 years
length(which(top20_intraday$Date >= as.Date("2008-09-01") & top20_intraday$Date <= as.Date("2011-08-30")))

###    Overnight return
overnight <- data.frame("Date" = intra_data$Date, "overnight_return" = (intra_data$PX_OPEN - lag.xts(intra_data$PX_LAST))/lag.xts(intra_data$PX_LAST))
top20_overnight <- overnight[order(overnight$overnight_return, decreasing = TRUE)[1:20],]

bottom20_overnight <- overnight[order(overnight$overnight_return, decreasing = FALSE)[1:20],]

### Calculating one-day jump
### In order to calculate rolling volatility, first of all we need to take care of the missing rows of data. 
### Forward-filling the blank rows will create generate duplicates price data, creating 0-one day jump. So we will omit the missing rows

spx_data_clean <- na.omit(spx_data)
spx_data_clean$logReturn <- log(spx_data_clean$PX_LAST/lag.xts(spx_data_clean$PX_LAST))
spx_data_clean$RollingVol <- rollapply(spx_data_clean$logReturn, width = list(c(-63:-1)), function(x) sqrt(var(x)), fill= NA, align = "right")
spx_data_clean$impact <- spx_data_clean$logReturn/spx_data_clean$RollingVol
spx_data_clean <- na.omit(spx_data_clean)

top20_impact <- spx_data_clean[order(spx_data_clean$impact, decreasing = TRUE)[1:20],]

length(which(top20_impact$Date >= as.Date("2008-09-01") & top20_impact$Date <= as.Date("2011-08-30")))

rollapply(zoo(1:10), width =  list(c(-3:-1)), sum, fill = NA, align = "right")
