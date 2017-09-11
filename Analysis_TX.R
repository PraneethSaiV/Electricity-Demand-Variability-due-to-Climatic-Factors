Rsq = function(prediction, actual) {
	SST = sum((actual - mean(actual)) ^ 2)
	SSE = sum((prediction - actual) ^ 2)
	RSQ = 1 - (SSE / SST)
	return(RSQ)
}
RMS = function(prediction, actual) {
	rmse = sqrt(mean((prediction - actual) ^ 2))
	return(rmse)
}
setwd("D:/Roshanak Nateghi/Project/Data")
data = read.csv("sales_revenue.csv")
data.cleaned = na.omit(data)
data.texas = subset(data.cleaned, State == "TX")
data.sales = data.texas[, 6]
data.sales_commercial = data.texas[, 9]

#__________________________________________
data.wea.TX = read.csv("TXDEW.csv")
data.emply.TX = read.csv("TXunemployment.csv")
data.climate.TX = read.csv("TX.csv")

names(data.cleaned)[names(data.cleaned) == "Sales.1"] = "Sales_commercial"
names(data.cleaned)[names(data.cleaned) == "Price.1"] = "Price_commercial"
names(data.cleaned)[names(data.cleaned) == "Revenue.1"] = "Revenue_commercial"


dataformonthly = data.frame(tapply(data.cleaned$Sales, data.cleaned$Year, mean))
dataforyearly = data.frame(tapply(data.cleaned$Sales, data.cleaned$Month, mean))
dataforstatewise = data.frame(tapply(data.cleaned$Sales, data.cleaned$State, mean))
priceformonthly = data.frame(tapply(data.cleaned$Price, data.cleaned$Year, mean))

dataformonthly_commercial = data.frame(tapply(data.cleaned$Sales_commercial, data.cleaned$Year, mean))
dataforyearly_commercial = data.frame(tapply(data.cleaned$Sales_commercial, data.cleaned$Month, mean))
dataforstatewise_commercial = data.frame(tapply(data.cleaned$Sales_commercial, data.cleaned$State, mean))
priceformonthly_commercial = data.frame(tapply(data.cleaned$Price_commercial, data.cleaned$Year, mean))

weathertemp_yearly = data.frame(tapply(data.wea.TX$TEMP, substr(as.character(data.wea.TX$YEARMODA), 1, 4), mean))
weathertemp_month = data.frame(tapply(data.wea.TX$TEMP, substr(as.character(data.wea.TX$YEARMODA), 5, 6), mean))

weatherdew_yearly = data.frame(tapply(data.wea.TX$DEWP, substr(as.character(data.wea.TX$YEARMODA), 1, 4), mean))
weatherdew_month = data.frame(tapply(data.wea.TX$DEWP, substr(as.character(data.wea.TX$YEARMODA), 5, 6), mean))

compare_sales = data.frame(data.cleaned$Sales)
compare_sales[, 2] = "Residential"
colnames(compare_sales) = c("Sales", "Type")
dum = data.frame(data.cleaned$Sales_commercial)
dum[, 2] = "Commercial"
colnames(dum) = c("Sales", "Type")

compare_sales = rbind(compare_sales, dum)




library("RColorBrewer")
library("ggplot2")
library("PerformanceAnalytics")
library("vioplot")
library("ggmap")

#Trends:
par(mfrow = c(1, 2))

#Sales v/s Price (R)
ggplot(data = data.cleaned, aes(x = Price, y = Sales)) + geom_point(alpha = 0.25, color = "royalblue4") + labs(x = "Price (cents/KWhour)", y = "Sales (MegaWattHour)", title = "Price vs Sales : Residential") +
  geom_smooth(alpha = 0.25, color = "black", filled.contour = "navyblue")

#Sales v/s Price (C)
ggplot(data = data.cleaned, aes(x = Price, y = Sales_commercial)) + geom_point(alpha = 0.25, color = "royalblue4") + labs(x = "Price (cents/KWhour)", y = "Sales (MegaWattHour)", title = "Price vs Sales : Commercial") +
  geom_smooth(alpha = 0.25, color = "black", filled.contour = "navyblue")

#Price vs Years (R)
ggplot(priceformonthly, aes(x = rownames(priceformonthly), y = priceformonthly$tapply.data.cleaned.Price..data.cleaned.Year..mean.)) + geom_point(alpha = 1, color = "royalblue4") + labs(x = "Year", y = "Sales (MegaWattHour)", title = "Prices over the Years : Residential") +
  geom_smooth(color = "black", filled.contour = "black")

#Prices vs Years (C)
ggplot(priceformonthly_commercial, aes(x = rownames(priceformonthly_commercial), y = priceformonthly_commercial$tapply.data.cleaned.Price_commercial..data.cleaned.Year..mean.)) + geom_point(alpha = 1, color = "royalblue4") + labs(x = "Year", y = "Sales (MegaWattHour)", title = "Prices over the Years : Commercial") +
  geom_smooth(color = "black", filled.contour = "black")

#Sales vs Years (R)
ggplot(dataformonthly, aes(x = rownames(dataformonthly), y = dataformonthly$tapply.data.cleaned.Sales..data.cleaned.Year..mean.)) + geom_point(alpha = 1, color = "royalblue4") + labs(x = "Year", y = "Sales (MegaWattHour)", title = "Sales over the Years : Residential") +
  geom_smooth(color = "black", filled.contour = "black")

#Sales vs Years (C)
ggplot(dataformonthly_commercial, aes(x = rownames(dataformonthly_commercial), y = dataformonthly_commercial$tapply.data.cleaned.Sales_commercial..data.cleaned.Year..mean.)) + geom_point(alpha = 1, color = "royalblue4") + labs(x = "Year", y = "Sales (MegaWattHour)", title = "Sales over the Years : Commercial") +
  geom_smooth(color = "black", filled.contour = "black")

#Sales vs months (R)
ggplot(data = dataforyearly, aes(x = reorder(rownames(dataforyearly), as.numeric(rownames(dataforyearly))), y = dataforyearly$tapply.data.cleaned.Sales..data.cleaned.Month..mean.)) + geom_point(alpha = 0.25, color = "royalblue4") + labs(x = "Month", y = "Sales (MegaWattHour)", title = "Sales Trend over a Year : Residential") +
  geom_bar(stat = "identity")

#Sales vs months(C)
ggplot(data = dataforyearly_commercial, aes(x = reorder(rownames(dataforyearly_commercial), as.numeric(rownames(dataforyearly_commercial))), y = dataforyearly_commercial$tapply.data.cleaned.Sales_commercial..data.cleaned.Month..mean.)) + geom_point(alpha = 0.25, color = "royalblue4") + labs(x = "Month", y = "Sales (MegaWattHour)", title = "Sales Trend over a Year : Commercial") +
  geom_bar(stat = "identity")

#Sales v/s State (R)
ggplot(data = dataforstatewise, aes(x = reorder(rownames(dataforstatewise), dataforstatewise$tapply.data.cleaned.Sales..data.cleaned.State..mean.), y = (dataforstatewise$tapply.data.cleaned.Sales..data.cleaned.State..mean.))) + labs(x = "State", y = "Sales (MegaWattHour)", title = "Average Sales per State : Residential") +
  geom_bar(stat = "identity", position = "identity") + coord_flip() + scale_fill_brewer(palette = "Greens") + theme_minimal()

#Sales v/s State (C)
ggplot(data = dataforstatewise_commercial, aes(x = reorder(rownames(dataforstatewise_commercial), dataforstatewise_commercial$tapply.data.cleaned.Sales_commercial..data.cleaned.State..mean.), y = (dataforstatewise_commercial$tapply.data.cleaned.Sales_commercial..data.cleaned.State..mean.))) + labs(x = "State", y = "Sales (MegaWattHour)", title = "Average Sales per State : Commercial") +
  geom_bar(stat = "identity", position = "identity") + coord_flip() + scale_fill_brewer(palette = "Greens") + theme_minimal()

#SINGLE PLOT COMPARISONS BETWEEN R AND C:

#Density plot of sales - both (R) and (C) Overlap
ggplot(data.cleaned, aes(Sales, colour = "navyblue", fill = "blue")) + geom_density(alpha = 0.1)
ggplot() + geom_density(aes(Sales), color = "blue", data = data.cleaned, fill = "blue", alpha = 0.25) + geom_density(aes(Sales_commercial), color = "green", data = data.cleaned, fill = "green", alpha = 0.25)

#Pie Chart showing the contribution of each (R) and (C) towards the entire Sales Total
#ggplot(compare_sales, aes(x = factor(Type), y = Sales, fill = Type)) + geom_bar(width = 0.25, stat = "identity") + coord_polar(theta = "y") + scale_fill_manual(values = c("#999999", "#E69F00"))






#MAPPING ON SPATIAL DATA:::::::::::::
#qmap(location = "united states of america", zoom = 4)

#mapdata.clean=read.csv("States.csv")
#mapdata.clean = mapdata.clean[, 2]
#mapdata.clean = mapdata.clean[-1,]
#mapdata.clean=data.frame(mapdata.clean)
#mapdata.clean$mapdata.clean = as.character(mapdata.clean$mapdata.clean)
#for (i in 1:nrow(mapdata.clean)) {
#	latlon = geocode(mapdata.clean[i,1])
#	mapdata.clean$lon[i] = as.numeric(latlon[1])
#	mapdata.clean$lat[i] = as.numeric(latlon[2])
#}


Max_resi = max(data.cleaned$Sales)
Min_resi = min(data.cleaned$Sales)
Max_commercial = max(data.cleaned$Sales_commercial)
Min_commercial = min(data.cleaned$Sales_commercial)

#Climate aggregates 
CLDD.Climate.TX.month = aggregate(data.climate.TX$CLDD ~ data.climate.TX$STATION_NAME + data.climate.TX$DATE, data = data.climate.TX, mean)
CLDD.mean.TX = tapply(CLDD.Climate.TX.month$`data.climate.TX$CLDD`, CLDD.Climate.TX.month$`data.climate.TX$DATE`, mean, na.rm = TRUE)

HTDD.Climate.TX.month = aggregate(data.climate.TX$HTDD ~ data.climate.TX$STATION_NAME + data.climate.TX$DATE, data = data.climate.TX, mean)
HTDD.mean.TX = tapply(HTDD.Climate.TX.month$`data.climate.TX$HTDD`, HTDD.Climate.TX.month$`data.climate.TX$DATE`, mean, na.rm = TRUE)

TPCP.Climate.TX.month = aggregate(data.climate.TX$TPCP ~ data.climate.TX$STATION_NAME + data.climate.TX$DATE, data = data.climate.TX, mean)
TPCP.mean.TX = tapply(TPCP.Climate.TX.month$`data.climate.TX$TPCP`, TPCP.Climate.TX.month$`data.climate.TX$DATE`, mean, na.rm = TRUE)

#weather

#dew point 
data.dew.TX = data.wea.TX[, c(3, 5)]
data.dew.TX$YEARMODA = as.character(data.dew.TX$YEARMODA)
data.dew.TX$YEARMODA = substr(data.dew.TX$YEARMODA, 1, 6)
dew.TX.month = tapply(data.dew.TX$DEWP, data.dew.TX$YEARMODA, mean, na.rm = TRUE)
#gust point 
data.gus.TX = data.wea.TX[, c(3, 11)]
gus.TX.month = tapply(data.gus.TX$GUST, data.dew.TX$YEARMODA, mean, na.rm = TRUE)
#windspeed point 
data.wdsp.TX = data.wea.TX[, c(3, 9)]
wdsp.TX.month = tapply(data.wdsp.TX$WDSP, data.dew.TX$YEARMODA, mean, na.rm = TRUE)


texas = data.frame(data.sales, dew.TX.month, data.emply.TX$Value,
					 data.texas$Price, TPCP.mean.TX,
					 CLDD.mean.TX, HTDD.mean.TX, gus.TX.month, wdsp.TX.month)

texas_Commercial = data.frame(data.sales_commercial, dew.TX.month, data.emply.TX$Value,
								data.texas$Price, TPCP.mean.TX,
								CLDD.mean.TX, HTDD.mean.TX, gus.TX.month, wdsp.TX.month)
texas_Commercial$data.sales_commercial = texas_Commercial$data.sales_commercial / 1000



years = data.frame(rownames(texas))
texas_trend = data.frame(texas)
texas_trend = cbind(texas_trend, years)
E_hat_y = as.matrix(tapply(texas_trend$data.sales, substr(as.character(texas_trend$rownames.texas.), 1, 4), mean))
E_hat = mean(E_hat_y)
f_adjusted = E_hat_y / E_hat

f_usefinal = list()
f_use = list()
for (i in 1:26) {
	f_use = rep(f_adjusted[i], each = 12)
	f_usefinal = cbind(f_usefinal, f_use)
}
f_usefinal = unlist(f_usefinal, use.names = F)
f_usefinal[313] = f_adjusted[27]

replacement = data.frame(texas_trend$data.sales)
replacement = replacement / f_usefinal

texas[, 1] = replacement


texas$data.sales = texas$data.sales / 1000
plot(seq(1:313), t(texas$data.sales), type = "l")
#EDA----
par(mfrow = c(1, 1))
library(corrplot)
chart.Correlation(texas)

#Weather Graphs
#Feature Engineering:
illinames = rownames(texas)
texas[, 10] = illinames
texas[, 11] = illinames
texas[, 10] = as.numeric(substr(as.character(texas[, 10]), 5, 6))
texas[, 11] = as.numeric(substr(as.character(texas[, 11]), 5, 6))

for (i in 1:nrow(texas)) {
	if (texas[i, 10] == 01) {
		texas[i, 10] = "Jan"
	}
	if (texas[i, 10] == 02) {
		texas[i, 10] = "Feb"
	}
	if (texas[i, 10] == 03) {
		texas[i, 10] = "Mar"
	}
	if (texas[i, 10] == 04) {
		texas[i, 10] = "Apr"
	}
	if (texas[i, 10] == 05) {
		texas[i, 10] = "May"
	}
	if (texas[i, 10] == 06) {
		texas[i, 10] = "Jun"
	}
	if (texas[i, 10] == 07) {
		texas[i, 10] = "Jul"
	}
	if (texas[i, 10] == 08) {
		texas[i, 10] = "Aug"
	}
	if (texas[i, 10] == 09) {
		texas[i, 10] = "Sep"
	}
	if (texas[i, 10] == 10) {
		texas[i, 10] = "Oct"
	}
	if (texas[i, 10] == 11) {
		texas[i, 10] = "Nov"
	}
	if (texas[i, 10] == 12) {
		texas[i, 10] = "Dec"
	}
}


for (i in 1:nrow(texas)) {
	if (texas[i, 11] == 1) {
		texas[i, 11] = "Winter"
	}
	if (texas[i, 11] == 2) {
		texas[i, 11] = "Winter"
	}
	if (texas[i, 11] == 3) {
		texas[i, 11] = "Spring"
	}
	if (texas[i, 11] == 4) {
		texas[i, 11] = "Spring"
	}
	if (texas[i, 11] == 5) {
		texas[i, 11] = "Spring"
	}
	if (texas[i, 11] == 6) {
		texas[i, 11] = "Summer"
	}
	if (texas[i, 11] == 7) {
		texas[i, 11] = "Summer"
	}
	if (texas[i, 11] == 8) {
		texas[i, 11] = "Summer"
	}
	if (texas[i, 11] == 9) {
		texas[i, 11] = "Fall"
	}
	if (texas[i, 11] == 10) {
		texas[i, 11] = "Fall"
	}
	if (texas[i, 11] == 11) {
		texas[i, 11] = "Fall"
	}
	if (texas[i, 11] == 12) {
		texas[i, 11] = "Winter"
	}
}


ggplot(data = weathertemp_yearly, aes(x = rownames(weathertemp_yearly), y = weathertemp_yearly$tapply.data.wea.TX.TEMP..substr.as.character.data.wea.TX.YEARMODA...)) + geom_line(aes(rownames(weathertemp_yearly), weathertemp_yearly$tapply.data.wea.TX.TEMP..substr.as.character.data.wea.TX.YEARMODA...), alpha = 0.25, color = "royalblue4") + geom_point(alpha = 0.25, color = "royalblue4") + labs(x = "Year", y = "Average Temperature", title = "Temperature Trend over the Years - texas") +
  geom_hline(aes(yintercept = mean(weathertemp_yearly$tapply.data.wea.TX.TEMP..substr.as.character.data.wea.TX.YEARMODA...)))

ggplot(texas, aes(texas$data.sales)) + geom_density(alpha = 0.1)
ggplot(texas, aes(texas$data.sales, colour = texas$V11, fill = texas$V11)) + geom_density(alpha = 0.1)



#-----
texas$CLDD.mean.TX = NULL
texas$HTDD.mean.TX = NULL

chart.Correlation(texas)


#Page 9 (R)
ggplot(texas, aes(data.sales, colour = "navyblue", fill = "blue")) + geom_density(alpha = 0.1)

#Page 10 (C)
ggplot(texas_Commercial, aes(data.sales, colour = "navyblue", fill = "blue")) + geom_density(alpha = 0.1)

#Above two together:
#ggplot() + geom_density(aes(data.sales), color = "blue", fill = "blue", alpha = 0.15, data = texas) + geom_density(aes(data.sales_commercial), color = "green", fill = "green", alpha = 0.15, data = texas_Commercial)+scale_fill_continuous(guide = "legend")


ggplot() + geom_density(aes(data.sales, color = "Residential"), fill = "green", alpha = 0.15, data = texas) + geom_density(aes(data.sales_commercial, color = "Commercial"), fill = "red", alpha = 0.15, data = texas_Commercial) + scale_fill_continuous(guide = "legend")

#THIS IS ONE^^^^^^^^^^^
texas$V10 <- factor(texas$V10, levels = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'), ordered = TRUE)



#Page 10 - Boxplot (R)
ggplot(texas, aes(factor(texas$V10), texas$data.sales)) + geom_boxplot(aes(fill = texas$V10))
ggplot(texas, aes(factor(texas$V10), texas_Commercial$data.sales)) + geom_boxplot(aes(fill = texas$V10))

#Page 11 - Boxplot (C)

#Page 12 - Violinplot (Both)
ggplot(texas, aes(factor(1), texas$data.sales)) + geom_violin(alpha = 0.5, color = "midnightblue", draw_quantiles = TRUE, show.legend = TRUE, fill = "dodgerblue4") + scale_color_manual(values = c("green", "black", "red")) + geom_boxplot(width = 0.25, color = "gray31", fill = "gray41", outlier.alpha = 0.5, outlier.color = "black", outlier.shape = "X", outlier.size = 4)

#CHECKTHIS LATER^^^^^^^
#Page 14 - Correlation (R)
chart.Correlation(texas[, 2:9])

#Page 26 - SEasoanl Variation
ggplot(texas, aes(factor(texas$V10), texas$wdsp.TX.month)) + geom_boxplot(aes(fill = texas$V10))

#Page 27 _Seasonal Variation -TPCP
#texas$V10 <- factor(texas$V10,levels = c('Jan', 'Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec'), ordered = TRUE)
ggplot(texas, aes((texas$V10), texas$TPCP.mean.TX)) + geom_boxplot(aes(fill = texas$V10))


ggplot(texas, aes(texas_Commercial$data.sales, colour = texas$V11, fill = texas$V11)) + geom_density(alpha = 0.1)
