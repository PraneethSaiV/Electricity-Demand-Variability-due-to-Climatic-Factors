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
data.newyork = subset(data.cleaned, State == "NY")
data.sales = data.newyork[, 6]
data.sales_commercial = data.newyork[, 9]

#__________________________________________
data.wea.NY = read.csv("NYDEW.csv")
data.emply.NY = read.csv("NYunemployment.csv")
data.climate.NY = read.csv("NY.csv")

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

weathertemp_yearly = data.frame(tapply(data.wea.NY$TEMP, substr(as.character(data.wea.NY$YEARMODA), 1, 4), mean))
weathertemp_month = data.frame(tapply(data.wea.NY$TEMP, substr(as.character(data.wea.NY$YEARMODA), 5, 6), mean))

weatherdew_yearly = data.frame(tapply(data.wea.NY$DEWP, substr(as.character(data.wea.NY$YEARMODA), 1, 4), mean))
weatherdew_month = data.frame(tapply(data.wea.NY$DEWP, substr(as.character(data.wea.NY$YEARMODA), 5, 6), mean))

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
CLDD.Climate.NY.month = aggregate(data.climate.NY$CLDD ~ data.climate.NY$STATION_NAME + data.climate.NY$DATE, data = data.climate.NY, mean)
CLDD.mean.NY = tapply(CLDD.Climate.NY.month$`data.climate.NY$CLDD`, CLDD.Climate.NY.month$`data.climate.NY$DATE`, mean, na.rm = TRUE)

HTDD.Climate.NY.month = aggregate(data.climate.NY$HTDD ~ data.climate.NY$STATION_NAME + data.climate.NY$DATE, data = data.climate.NY, mean)
HTDD.mean.NY = tapply(HTDD.Climate.NY.month$`data.climate.NY$HTDD`, HTDD.Climate.NY.month$`data.climate.NY$DATE`, mean, na.rm = TRUE)

TPCP.Climate.NY.month = aggregate(data.climate.NY$TPCP ~ data.climate.NY$STATION_NAME + data.climate.NY$DATE, data = data.climate.NY, mean)
TPCP.mean.NY = tapply(TPCP.Climate.NY.month$`data.climate.NY$TPCP`, TPCP.Climate.NY.month$`data.climate.NY$DATE`, mean, na.rm = TRUE)

#weather

#dew point 
data.dew.NY = data.wea.NY[, c(3, 5)]
data.dew.NY$YEARMODA = as.character(data.dew.NY$YEARMODA)
data.dew.NY$YEARMODA = substr(data.dew.NY$YEARMODA, 1, 6)
dew.NY.month = tapply(data.dew.NY$DEWP, data.dew.NY$YEARMODA, mean, na.rm = TRUE)
#gust point 
data.gus.NY = data.wea.NY[, c(3, 11)]
gus.NY.month = tapply(data.gus.NY$GUST, data.dew.NY$YEARMODA, mean, na.rm = TRUE)
#windspeed point 
data.wdsp.NY = data.wea.NY[, c(3, 9)]
wdsp.NY.month = tapply(data.wdsp.NY$WDSP, data.dew.NY$YEARMODA, mean, na.rm = TRUE)


newyork = data.frame(data.sales, dew.NY.month, data.emply.NY$Value,
						data.newyork$Price, TPCP.mean.NY,
						CLDD.mean.NY, HTDD.mean.NY, gus.NY.month, wdsp.NY.month)

newyork_Commercial = data.frame(data.sales_commercial, dew.NY.month, data.emply.NY$Value,
								   data.newyork$Price, TPCP.mean.NY,
								   CLDD.mean.NY, HTDD.mean.NY, gus.NY.month, wdsp.NY.month)
newyork_Commercial$data.sales_commercial = newyork_Commercial$data.sales_commercial / 1000



years = data.frame(rownames(newyork))
newyork_trend = data.frame(newyork)
newyork_trend = cbind(newyork_trend, years)
E_hat_y = as.matrix(tapply(newyork_trend$data.sales, substr(as.character(newyork_trend$rownames.newyork.), 1, 4), mean))
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

replacement = data.frame(newyork_trend$data.sales)
replacement = replacement / f_usefinal

newyork[, 1] = replacement


newyork$data.sales = newyork$data.sales / 1000
plot(seq(1:313), t(newyork$data.sales), type = "l")
#EDA----
par(mfrow = c(1, 1))
library(corrplot)
chart.Correlation(newyork)

#Weather Graphs
#Feature Engineering:
illinames = rownames(newyork)
newyork[, 10] = illinames
newyork[, 11] = illinames
newyork[, 10] = as.numeric(substr(as.character(newyork[, 10]), 5, 6))
newyork[, 11] = as.numeric(substr(as.character(newyork[, 11]), 5, 6))

for (i in 1:nrow(newyork)) {
	if (newyork[i, 10] == 01) {
		newyork[i, 10] = "Jan"
	}
	if (newyork[i, 10] == 02) {
		newyork[i, 10] = "Feb"
	}
	if (newyork[i, 10] == 03) {
		newyork[i, 10] = "Mar"
	}
	if (newyork[i, 10] == 04) {
		newyork[i, 10] = "Apr"
	}
	if (newyork[i, 10] == 05) {
		newyork[i, 10] = "May"
	}
	if (newyork[i, 10] == 06) {
		newyork[i, 10] = "Jun"
	}
	if (newyork[i, 10] == 07) {
		newyork[i, 10] = "Jul"
	}
	if (newyork[i, 10] == 08) {
		newyork[i, 10] = "Aug"
	}
	if (newyork[i, 10] == 09) {
		newyork[i, 10] = "Sep"
	}
	if (newyork[i, 10] == 10) {
		newyork[i, 10] = "Oct"
	}
	if (newyork[i, 10] == 11) {
		newyork[i, 10] = "Nov"
	}
	if (newyork[i, 10] == 12) {
		newyork[i, 10] = "Dec"
	}
}


for (i in 1:nrow(newyork)) {
	if (newyork[i, 11] == 1) {
		newyork[i, 11] = "Winter"
	}
	if (newyork[i, 11] == 2) {
		newyork[i, 11] = "Winter"
	}
	if (newyork[i, 11] == 3) {
		newyork[i, 11] = "Spring"
	}
	if (newyork[i, 11] == 4) {
		newyork[i, 11] = "Spring"
	}
	if (newyork[i, 11] == 5) {
		newyork[i, 11] = "Spring"
	}
	if (newyork[i, 11] == 6) {
		newyork[i, 11] = "Summer"
	}
	if (newyork[i, 11] == 7) {
		newyork[i, 11] = "Summer"
	}
	if (newyork[i, 11] == 8) {
		newyork[i, 11] = "Summer"
	}
	if (newyork[i, 11] == 9) {
		newyork[i, 11] = "Fall"
	}
	if (newyork[i, 11] == 10) {
		newyork[i, 11] = "Fall"
	}
	if (newyork[i, 11] == 11) {
		newyork[i, 11] = "Fall"
	}
	if (newyork[i, 11] == 12) {
		newyork[i, 11] = "Winter"
	}
}


ggplot(data = weathertemp_yearly, aes(x = rownames(weathertemp_yearly), y = weathertemp_yearly$tapply.data.wea.NY.TEMP..substr.as.character.data.wea.NY.YEARMODA...)) + geom_line(aes(rownames(weathertemp_yearly), weathertemp_yearly$tapply.data.wea.NY.TEMP..substr.as.character.data.wea.NY.YEARMODA...), alpha = 0.25, color = "royalblue4") + geom_point(alpha = 0.25, color = "royalblue4") + labs(x = "Year", y = "Average Temperature", title = "Temperature Trend over the Years - newyork") +
  geom_hline(aes(yintercept = mean(weathertemp_yearly$tapply.data.wea.NY.TEMP..substr.as.character.data.wea.NY.YEARMODA...)))

ggplot(newyork, aes(newyork$data.sales)) + geom_density(alpha = 0.1)
ggplot(newyork, aes(newyork$data.sales, colour = newyork$V11, fill = newyork$V11)) + geom_density(alpha = 0.1)



#-----
newyork$CLDD.mean.NY = NULL
newyork$HTDD.mean.NY = NULL

chart.Correlation(newyork)


#Page 9 (R)
ggplot(newyork, aes(data.sales, colour = "navyblue", fill = "blue")) + geom_density(alpha = 0.1)

#Page 10 (C)
ggplot(newyork_Commercial, aes(data.sales, colour = "navyblue", fill = "blue")) + geom_density(alpha = 0.1)

#Above two together:
#ggplot() + geom_density(aes(data.sales), color = "blue", fill = "blue", alpha = 0.15, data = newyork) + geom_density(aes(data.sales_commercial), color = "green", fill = "green", alpha = 0.15, data = newyork_Commercial)+scale_fill_continuous(guide = "legend")


ggplot() + geom_density(aes(data.sales, color = "Residential"), fill = "green", alpha = 0.15, data = newyork) + geom_density(aes(data.sales_commercial, color = "Commercial"), fill = "red", alpha = 0.15, data = newyork_Commercial) + scale_fill_continuous(guide = "legend")

#THIS IS ONE^^^^^^^^^^^
newyork$V10 <- factor(newyork$V10, levels = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'), ordered = TRUE)



#Page 10 - Boxplot (R)
ggplot(newyork, aes(factor(newyork$V10), newyork$data.sales)) + geom_boxplot(aes(fill = newyork$V10))
ggplot(newyork, aes(factor(newyork$V10), newyork_Commercial$data.sales)) + geom_boxplot(aes(fill = newyork$V10))

#Page 11 - Boxplot (C)

#Page 12 - Violinplot (Both)
ggplot(newyork, aes(factor(1), newyork$data.sales)) + geom_violin(alpha = 0.5, color = "midnightblue", draw_quantiles = TRUE, show.legend = TRUE, fill = "dodgerblue4") + scale_color_manual(values = c("green", "black", "red")) + geom_boxplot(width = 0.25, color = "gray31", fill = "gray41", outlier.alpha = 0.5, outlier.color = "black", outlier.shape = "X", outlier.size = 4)

#CHECKTHIS LATER^^^^^^^
#Page 14 - Correlation (R)
chart.Correlation(newyork[, 2:9])

#Page 26 - SEasoanl Variation
ggplot(newyork, aes(factor(newyork$V10), newyork$wdsp.NY.month)) + geom_boxplot(aes(fill = newyork$V10))

#Page 27 _Seasonal Variation -TPCP
#newyork$V10 <- factor(newyork$V10,levels = c('Jan', 'Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec'), ordered = TRUE)
ggplot(newyork, aes((newyork$V10), newyork$TPCP.mean.NY)) + geom_boxplot(aes(fill = newyork$V10))


ggplot(newyork, aes(newyork_Commercial$data.sales, colour = newyork$V11, fill = newyork$V11)) + geom_density(alpha = 0.1)
