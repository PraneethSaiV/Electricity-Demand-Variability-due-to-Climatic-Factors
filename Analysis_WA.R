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
data.washington = subset(data.cleaned, State == "WA")
data.sales = data.washington[, 6]
data.sales_commercial = data.washington[, 9]

#__________________________________________
data.wea.WA = read.csv("WADEW.csv")
data.emply.WA = read.csv("WAunemployment.csv")
data.climate.WA = read.csv("WA.csv")

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

weathertemp_yearly = data.frame(tapply(data.wea.WA$TEMP, substr(as.character(data.wea.WA$YEARMODA), 1, 4), mean))
weathertemp_month = data.frame(tapply(data.wea.WA$TEMP, substr(as.character(data.wea.WA$YEARMODA), 5, 6), mean))

weatherdew_yearly = data.frame(tapply(data.wea.WA$DEWP, substr(as.character(data.wea.WA$YEARMODA), 1, 4), mean))
weatherdew_month = data.frame(tapply(data.wea.WA$DEWP, substr(as.character(data.wea.WA$YEARMODA), 5, 6), mean))

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
CLDD.Climate.WA.month = aggregate(data.climate.WA$CLDD ~ data.climate.WA$STATION_NAME + data.climate.WA$DATE, data = data.climate.WA, mean)
CLDD.mean.WA = tapply(CLDD.Climate.WA.month$`data.climate.WA$CLDD`, CLDD.Climate.WA.month$`data.climate.WA$DATE`, mean, na.rm = TRUE)

HTDD.Climate.WA.month = aggregate(data.climate.WA$HTDD ~ data.climate.WA$STATION_NAME + data.climate.WA$DATE, data = data.climate.WA, mean)
HTDD.mean.WA = tapply(HTDD.Climate.WA.month$`data.climate.WA$HTDD`, HTDD.Climate.WA.month$`data.climate.WA$DATE`, mean, na.rm = TRUE)

TPCP.Climate.WA.month = aggregate(data.climate.WA$TPCP ~ data.climate.WA$STATION_NAME + data.climate.WA$DATE, data = data.climate.WA, mean)
TPCP.mean.WA = tapply(TPCP.Climate.WA.month$`data.climate.WA$TPCP`, TPCP.Climate.WA.month$`data.climate.WA$DATE`, mean, na.rm = TRUE)

#weather

#dew point 
data.dew.WA = data.wea.WA[, c(3, 5)]
data.dew.WA$YEARMODA = as.character(data.dew.WA$YEARMODA)
data.dew.WA$YEARMODA = substr(data.dew.WA$YEARMODA, 1, 6)
dew.WA.month = tapply(data.dew.WA$DEWP, data.dew.WA$YEARMODA, mean, na.rm = TRUE)
#gust point 
data.gus.WA = data.wea.WA[, c(3, 11)]
gus.WA.month = tapply(data.gus.WA$GUST, data.dew.WA$YEARMODA, mean, na.rm = TRUE)
#windspeed point 
data.wdsp.WA = data.wea.WA[, c(3, 9)]
wdsp.WA.month = tapply(data.wdsp.WA$WDSP, data.dew.WA$YEARMODA, mean, na.rm = TRUE)


washington = data.frame(data.sales, dew.WA.month, data.emply.WA$Value,
				   data.washington$Price, TPCP.mean.WA,
				   CLDD.mean.WA, HTDD.mean.WA, gus.WA.month, wdsp.WA.month)

washington_Commercial = data.frame(data.sales_commercial, dew.WA.month, data.emply.WA$Value,
							  data.washington$Price, TPCP.mean.WA,
							  CLDD.mean.WA, HTDD.mean.WA, gus.WA.month, wdsp.WA.month)
washington_Commercial$data.sales_commercial = washington_Commercial$data.sales_commercial / 1000



years = data.frame(rownames(washington))
washington_trend = data.frame(washington)
washington_trend = cbind(washington_trend, years)
E_hat_y = as.matrix(tapply(washington_trend$data.sales, substr(as.character(washington_trend$rownames.washington.), 1, 4), mean))
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

replacement = data.frame(washington_trend$data.sales)
replacement = replacement / f_usefinal

washington[, 1] = replacement


washington$data.sales = washington$data.sales / 1000
plot(seq(1:313), t(washington$data.sales), type = "l")
#EDA----
par(mfrow = c(1, 1))
library(corrplot)
chart.Correlation(washington)

#Weather Graphs
#Feature Engineering:
illinames = rownames(washington)
washington[, 10] = illinames
washington[, 11] = illinames
washington[, 10] = as.numeric(substr(as.character(washington[, 10]), 5, 6))
washington[, 11] = as.numeric(substr(as.character(washington[, 11]), 5, 6))

for (i in 1:nrow(washington)) {
	if (washington[i, 10] == 01) {
		washington[i, 10] = "Jan"
	}
	if (washington[i, 10] == 02) {
		washington[i, 10] = "Feb"
	}
	if (washington[i, 10] == 03) {
		washington[i, 10] = "Mar"
	}
	if (washington[i, 10] == 04) {
		washington[i, 10] = "Apr"
	}
	if (washington[i, 10] == 05) {
		washington[i, 10] = "May"
	}
	if (washington[i, 10] == 06) {
		washington[i, 10] = "Jun"
	}
	if (washington[i, 10] == 07) {
		washington[i, 10] = "Jul"
	}
	if (washington[i, 10] == 08) {
		washington[i, 10] = "Aug"
	}
	if (washington[i, 10] == 09) {
		washington[i, 10] = "Sep"
	}
	if (washington[i, 10] == 10) {
		washington[i, 10] = "Oct"
	}
	if (washington[i, 10] == 11) {
		washington[i, 10] = "Nov"
	}
	if (washington[i, 10] == 12) {
		washington[i, 10] = "Dec"
	}
}


for (i in 1:nrow(washington)) {
	if (washington[i, 11] == 1) {
		washington[i, 11] = "Winter"
	}
	if (washington[i, 11] == 2) {
		washington[i, 11] = "Winter"
	}
	if (washington[i, 11] == 3) {
		washington[i, 11] = "Spring"
	}
	if (washington[i, 11] == 4) {
		washington[i, 11] = "Spring"
	}
	if (washington[i, 11] == 5) {
		washington[i, 11] = "Spring"
	}
	if (washington[i, 11] == 6) {
		washington[i, 11] = "Summer"
	}
	if (washington[i, 11] == 7) {
		washington[i, 11] = "Summer"
	}
	if (washington[i, 11] == 8) {
		washington[i, 11] = "Summer"
	}
	if (washington[i, 11] == 9) {
		washington[i, 11] = "Fall"
	}
	if (washington[i, 11] == 10) {
		washington[i, 11] = "Fall"
	}
	if (washington[i, 11] == 11) {
		washington[i, 11] = "Fall"
	}
	if (washington[i, 11] == 12) {
		washington[i, 11] = "Winter"
	}
}


ggplot(data = weathertemp_yearly, aes(x = rownames(weathertemp_yearly), y = weathertemp_yearly$tapply.data.wea.WA.TEMP..substr.as.character.data.wea.WA.YEARMODA...)) + geom_line(aes(rownames(weathertemp_yearly), weathertemp_yearly$tapply.data.wea.WA.TEMP..substr.as.character.data.wea.WA.YEARMODA...), alpha = 0.25, color = "royalblue4") + geom_point(alpha = 0.25, color = "royalblue4") + labs(x = "Year", y = "Average Temperature", title = "Temperature Trend over the Years - washington") +
  geom_hline(aes(yintercept = mean(weathertemp_yearly$tapply.data.wea.WA.TEMP..substr.as.character.data.wea.WA.YEARMODA...)))

ggplot(washington, aes(washington$data.sales)) + geom_density(alpha = 0.1)
ggplot(washington, aes(washington$data.sales, colour = washington$V11, fill = washington$V11)) + geom_density(alpha = 0.1)



#-----
washington$CLDD.mean.WA = NULL
washington$HTDD.mean.WA = NULL

chart.Correlation(washington)


#Page 9 (R)
ggplot(washington, aes(data.sales, colour = "navyblue", fill = "blue")) + geom_density(alpha = 0.1)

#Page 10 (C)
ggplot(washington_Commercial, aes(data.sales, colour = "navyblue", fill = "blue")) + geom_density(alpha = 0.1)

#Above two together:
#ggplot() + geom_density(aes(data.sales), color = "blue", fill = "blue", alpha = 0.15, data = washington) + geom_density(aes(data.sales_commercial), color = "green", fill = "green", alpha = 0.15, data = washington_Commercial)+scale_fill_continuous(guide = "legend")


ggplot() + geom_density(aes(data.sales, color = "Residential"), fill = "green", alpha = 0.15, data = washington) + geom_density(aes(data.sales_commercial, color = "Commercial"), fill = "red", alpha = 0.15, data = washington_Commercial) + scale_fill_continuous(guide = "legend")

#THIS IS ONE^^^^^^^^^^^
washington$V10 <- factor(washington$V10, levels = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'), ordered = TRUE)



#Page 10 - Boxplot (R)
ggplot(washington, aes(factor(washington$V10), washington$data.sales)) + geom_boxplot(aes(fill = washington$V10))
ggplot(washington, aes(factor(washington$V10), washington_Commercial$data.sales)) + geom_boxplot(aes(fill = washington$V10))

#Page 11 - Boxplot (C)

#Page 12 - Violinplot (Both)
ggplot(washington, aes(factor(1), washington$data.sales)) + geom_violin(alpha = 0.5, color = "midnightblue", draw_quantiles = TRUE, show.legend = TRUE, fill = "dodgerblue4") + scale_color_manual(values = c("green", "black", "red")) + geom_boxplot(width = 0.25, color = "gray31", fill = "gray41", outlier.alpha = 0.5, outlier.color = "black", outlier.shape = "X", outlier.size = 4)

#CHECKTHIS LATER^^^^^^^
#Page 14 - Correlation (R)
chart.Correlation(washington[, 2:9])

#Page 26 - SEasoanl Variation
ggplot(washington, aes(factor(washington$V10), washington$wdsp.WA.month)) + geom_boxplot(aes(fill = washington$V10))

#Page 27 _Seasonal Variation -TPCP
#washington$V10 <- factor(washington$V10,levels = c('Jan', 'Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec'), ordered = TRUE)
ggplot(washington, aes((washington$V10), washington$TPCP.mean.WA)) + geom_boxplot(aes(fill = washington$V10))


ggplot(washington, aes(washington_Commercial$data.sales, colour = washington$V11, fill = washington$V11)) + geom_density(alpha = 0.1)
