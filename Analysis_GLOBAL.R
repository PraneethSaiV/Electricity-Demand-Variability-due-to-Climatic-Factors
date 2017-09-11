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

data.illinois = subset(data.cleaned, State == "IL")
data.sales = data.illinois[, 6]
data.sales_commercial = data.illinois[, 9]

#__________________________________________
data.wea.IL = read.csv("ILDEW.csv")
data.emply.IL = read.csv("ILunemployment.csv")
data.climate.IL = read.csv("IL.csv")

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

weathertemp_yearly = data.frame(tapply(data.wea.IL$TEMP, substr(as.character(data.wea.IL$YEARMODA), 1, 4), mean))
weathertemp_month = data.frame(tapply(data.wea.IL$TEMP, substr(as.character(data.wea.IL$YEARMODA), 5, 6), mean))

weatherdew_yearly = data.frame(tapply(data.wea.IL$DEWP, substr(as.character(data.wea.IL$YEARMODA), 1, 4), mean))
weatherdew_month = data.frame(tapply(data.wea.IL$DEWP, substr(as.character(data.wea.IL$YEARMODA), 5, 6), mean))

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
ggplot(priceformonthly, aes(x = rownames(priceformonthly), y = priceformonthly$tapply.data.cleaned.Price..data.cleaned.Year..mean., group = rownames(priceformonthly))) + geom_point(alpha = 1, color = "royalblue4") + labs(x = "Year", y = "Sales (MegaWattHour)", title = "Prices over the Years : Residential") +
  geom_smooth(color = "black", filled.contour = "black") + geom_line()

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




#CHANGE FROM HERE:---------------------------------------

#Sales v/s State (R)
ggplot(data = dataforstatewise, aes(x = reorder(rownames(dataforstatewise), dataforstatewise$tapply.data.cleaned.Sales..data.cleaned.State..mean.), y = (dataforstatewise$tapply.data.cleaned.Sales..data.cleaned.State..mean.))) + labs(x = "State", y = "Sales (MegaWattHour)", title = "Average Sales per State : Residential") +
  geom_bar(stat = "identity", position = "identity") + coord_flip() + scale_fill_brewer(palette = "Greens") + theme_minimal()

#Sales v/s State (C)
ggplot(data = dataforstatewise_commercial, aes(x = reorder(rownames(dataforstatewise_commercial), dataforstatewise_commercial$tapply.data.cleaned.Sales_commercial..data.cleaned.State..mean.), y = (dataforstatewise_commercial$tapply.data.cleaned.Sales_commercial..data.cleaned.State..mean.))) + labs(x = "State", y = "Sales (MegaWattHour)", title = "Average Sales per State : Commercial") +
  geom_bar(stat = "identity", position = "identity") + coord_flip() + scale_fill_brewer(palette = "Greens") + theme_minimal()

#SINGLE PLOT COMPARISONS BETWEEN R AND C:

#Density plot of sales - both (R) and (C) Overlap
ggplot(data.cleaned, aes(Sales, colour = "navyblue", fill = "blue")) + geom_density(alpha = 0.1)
ggplot() + geom_density(aes(Sales, color = "Residential"), data = data.cleaned, fill = "blue", alpha = 0.35,) + geom_density(aes(Sales_commercial, color = "Commercial"), data = data.cleaned, fill = "orange", alpha = 0.25) + theme(axis.text = element_text(size = 12), axis.title = element_text(size = 20))

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
CLDD.Climate.IL.month = aggregate(data.climate.IL$CLDD ~ data.climate.IL$STATION_NAME + data.climate.IL$DATE, data = data.climate.IL, mean)
CLDD.mean.IL = tapply(CLDD.Climate.IL.month$`data.climate.IL$CLDD`, CLDD.Climate.IL.month$`data.climate.IL$DATE`, mean, na.rm = TRUE)

HTDD.Climate.IL.month = aggregate(data.climate.IL$HTDD ~ data.climate.IL$STATION_NAME + data.climate.IL$DATE, data = data.climate.IL, mean)
HTDD.mean.IL = tapply(HTDD.Climate.IL.month$`data.climate.IL$HTDD`, HTDD.Climate.IL.month$`data.climate.IL$DATE`, mean, na.rm = TRUE)

TPCP.Climate.IL.month = aggregate(data.climate.IL$TPCP ~ data.climate.IL$STATION_NAME + data.climate.IL$DATE, data = data.climate.IL, mean)
TPCP.mean.IL = tapply(TPCP.Climate.IL.month$`data.climate.IL$TPCP`, TPCP.Climate.IL.month$`data.climate.IL$DATE`, mean, na.rm = TRUE)

#weather

#dew point 
data.dew.IL = data.wea.IL[, c(3, 5)]
data.dew.IL$YEARMODA = as.character(data.dew.IL$YEARMODA)
data.dew.IL$YEARMODA = substr(data.dew.IL$YEARMODA, 1, 6)
dew.IL.month = tapply(data.dew.IL$DEWP, data.dew.IL$YEARMODA, mean, na.rm = TRUE)
#gust point 
data.gus.IL = data.wea.IL[, c(3, 11)]
gus.IL.month = tapply(data.gus.IL$GUST, data.dew.IL$YEARMODA, mean, na.rm = TRUE)
#windspeed point 
data.wdsp.IL = data.wea.IL[, c(3, 9)]
wdsp.IL.month = tapply(data.wdsp.IL$WDSP, data.dew.IL$YEARMODA, mean, na.rm = TRUE)


illinois = data.frame(data.sales, dew.IL.month, data.emply.IL$Value,
					  data.illinois$Price, TPCP.mean.IL,
					  CLDD.mean.IL, HTDD.mean.IL, gus.IL.month, wdsp.IL.month)

illinois_Commercial = data.frame(data.sales_commercial, dew.IL.month, data.emply.IL$Value,
								 data.illinois$Price, TPCP.mean.IL,
								 CLDD.mean.IL, HTDD.mean.IL, gus.IL.month, wdsp.IL.month)
illinois_Commercial$data.sales_commercial = illinois_Commercial$data.sales_commercial / 1000



years = data.frame(rownames(illinois))
illinois_trend = data.frame(illinois)
illinois_trend = cbind(illinois_trend, years)
E_hat_y = as.matrix(tapply(illinois_trend$data.sales, substr(as.character(illinois_trend$rownames.illinois.), 1, 4), mean))
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

replacement = data.frame(illinois_trend$data.sales)
replacement = replacement / f_usefinal

illinois[, 1] = replacement


illinois$data.sales = illinois$data.sales / 1000
plot(seq(1:313), t(illinois$data.sales), type = "l")
#EDA----
par(mfrow = c(1, 1))
library(corrplot)
chart.Correlation(illinois)

#Weather Graphs
#Feature Engineering:
illinames = rownames(illinois)
illinois[, 10] = illinames
illinois[, 11] = illinames
illinois[, 10] = as.numeric(substr(as.character(illinois[, 10]), 5, 6))
illinois[, 11] = as.numeric(substr(as.character(illinois[, 11]), 5, 6))

for (i in 1:nrow(illinois)) {
	if (illinois[i, 10] == 01) {
		illinois[i, 10] = "Jan"
	}
	if (illinois[i, 10] == 02) {
		illinois[i, 10] = "Feb"
	}
	if (illinois[i, 10] == 03) {
		illinois[i, 10] = "Mar"
	}
	if (illinois[i, 10] == 04) {
		illinois[i, 10] = "Apr"
	}
	if (illinois[i, 10] == 05) {
		illinois[i, 10] = "May"
	}
	if (illinois[i, 10] == 06) {
		illinois[i, 10] = "Jun"
	}
	if (illinois[i, 10] == 07) {
		illinois[i, 10] = "Jul"
	}
	if (illinois[i, 10] == 08) {
		illinois[i, 10] = "Aug"
	}
	if (illinois[i, 10] == 09) {
		illinois[i, 10] = "Sep"
	}
	if (illinois[i, 10] == 10) {
		illinois[i, 10] = "Oct"
	}
	if (illinois[i, 10] == 11) {
		illinois[i, 10] = "Nov"
	}
	if (illinois[i, 10] == 12) {
		illinois[i, 10] = "Dec"
	}
}


for (i in 1:nrow(illinois)) {
	if (illinois[i, 11] == 1) {
		illinois[i, 11] = "Winter"
	}
	if (illinois[i, 11] == 2) {
		illinois[i, 11] = "Winter"
	}
	if (illinois[i, 11] == 3) {
		illinois[i, 11] = "Spring"
	}
	if (illinois[i, 11] == 4) {
		illinois[i, 11] = "Spring"
	}
	if (illinois[i, 11] == 5) {
		illinois[i, 11] = "Spring"
	}
	if (illinois[i, 11] == 6) {
		illinois[i, 11] = "Summer"
	}
	if (illinois[i, 11] == 7) {
		illinois[i, 11] = "Summer"
	}
	if (illinois[i, 11] == 8) {
		illinois[i, 11] = "Summer"
	}
	if (illinois[i, 11] == 9) {
		illinois[i, 11] = "Fall"
	}
	if (illinois[i, 11] == 10) {
		illinois[i, 11] = "Fall"
	}
	if (illinois[i, 11] == 11) {
		illinois[i, 11] = "Fall"
	}
	if (illinois[i, 11] == 12) {
		illinois[i, 11] = "Winter"
	}
}


ggplot(data = weathertemp_yearly, aes(x = rownames(weathertemp_yearly), y = weathertemp_yearly$tapply.data.wea.IL.TEMP..substr.as.character.data.wea.IL.YEARMODA...)) + geom_line(aes(rownames(weathertemp_yearly), weathertemp_yearly$tapply.data.wea.IL.TEMP..substr.as.character.data.wea.IL.YEARMODA...), alpha = 0.25, color = "royalblue4") + geom_point(alpha = 0.25, color = "royalblue4") + labs(x = "Year", y = "Average Temperature", title = "Temperature Trend over the Years - Illinois") +
  geom_hline(aes(yintercept = mean(weathertemp_yearly$tapply.data.wea.IL.TEMP..substr.as.character.data.wea.IL.YEARMODA...)))

ggplot(illinois, aes(illinois$data.sales)) + geom_density(alpha = 0.1)
ggplot(illinois, aes(illinois$data.sales, colour = illinois$V11, fill = illinois$V11)) + geom_density(alpha = 0.1)



#-----
illinois$CLDD.mean.IL = NULL
illinois$HTDD.mean.IL = NULL

chart.Correlation(illinois)


library(vioplot)




#Page 9 (R)
ggplot(illinois, aes(data.sales, colour = "navyblue", fill = "blue")) + geom_density(alpha = 0.1)

#Page 10 (C)
ggplot(illinois_Commercial, aes(data.sales, colour = "navyblue", fill = "blue")) + geom_density(alpha = 0.1)

#Above two together:
#ggplot() + geom_density(aes(data.sales), color = "blue", fill = "blue", alpha = 0.15, data = illinois) + geom_density(aes(data.sales_commercial), color = "green", fill = "green", alpha = 0.15, data = illinois_Commercial)+scale_fill_continuous(guide = "legend")


ggplot() + geom_density(aes(data.sales, color = "Residential"), fill = "green", alpha = 0.15, data = illinois) + geom_density(aes(data.sales_commercial, color = "Commercial"), fill = "red", alpha = 0.15, data = illinois_Commercial) + scale_fill_continuous(guide = "legend")

#THIS IS ONE^^^^^^^^^^^
illinois$V10 <- factor(illinois$V10, levels = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'), ordered = TRUE)



#Page 10 - Boxplot (R)
ggplot(illinois, aes(factor(illinois$V10), illinois$data.sales)) + geom_boxplot(aes(fill = illinois$V10))
ggplot(illinois, aes(factor(illinois$V10), illinois_Commercial$data.sales)) + geom_boxplot(aes(fill = illinois$V10))

#Page 11 - Boxplot (C)

#Page 12 - Violinplot (Both)
ggplot(illinois, aes(factor(1), illinois$data.sales)) + geom_violin(alpha = 0.5, color = "midnightblue", draw_quantiles = TRUE, show.legend = TRUE, fill = "dodgerblue4") + scale_color_manual(values = c("green", "black", "red")) + geom_boxplot(width = 0.25, color = "gray31", fill = "gray41", outlier.alpha = 0.5, outlier.color = "black", outlier.shape = "X", outlier.size = 4)

#CHECKTHIS LATER^^^^^^^
#Page 14 - Correlation (R)
chart.Correlation(illinois[, 2:9])

#Page 26 - SEasoanl Variation
ggplot(illinois, aes(factor(illinois$V10), illinois$wdsp.IL.month)) + geom_boxplot(aes(fill = illinois$V10))

#Page 27 _Seasonal Variation -TPCP
#illinois$V10 <- factor(illinois$V10,levels = c('Jan', 'Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec'), ordered = TRUE)
ggplot(illinois, aes((illinois$V10), illinois$TPCP.mean.IL)) + geom_boxplot(aes(fill = illinois$V10))


ggplot(illinois, aes(illinois_Commercial$data.sales, colour = illinois$V11, fill = illinois$V11)) + geom_density(alpha = 0.1)


]
library(cluster)
data.cleaned_cluster = subset(data.cleaned, data.cleaned$State == c("WA", "NY", "IL", "CA", "TX"))
R = data.frame(cbind(data.cleaned_cluster$Sales, data.cleaned_cluster$Price, data.cleaned_cluster$State))
C = data.frame(cbind(data.cleaned_cluster$Sales_commercial, data.cleaned_cluster$Price_commercial, data.cleaned_cluster$State))
colnames(R) = c("Sales", "Price", "State")
colnames(C) = c("Sales", "Price", "State")
k_r = kmeans(R[, 1:2], 5)
k_c = kmeans(C[, 1:2], 5)

k_c$cluster = as.factor(k_c$cluster)
center_c = as.data.frame(k_c$centers)

C$State[C$State == 45] = "TX"
C$State[C$State == 36] = "NY"
C$State[C$State == 16] = "IL"
C$State[C$State == 49] = "WA"
C$State[C$State == 6] = "CA"

R$State[R$State == 45] = "TX"
R$State[R$State == 36] = "NY"
R$State[R$State == 16] = "IL"
R$State[R$State == 49] = "WA"
R$State[R$State == 6] = "CA"


ggplot(C, aes(Sales, Price, color = as.factor(C$State))) + geom_point()


k_r$cluster = as.factor(k_r$cluster)
center_r = as.data.frame(k_r$centers)
abba=k_r$centers
ggplot(R, aes(Sales, Price, color = as.factor(R$State))) + geom_point() #+ geom_point(data = abba, aes(R$Sales,abba, color = as.factor(R$State)))


clusplot(R, k_r$cluster, labels = 5, color = TRUE, cex.txt = 0.5)



R = data.frame(cbind(data.cleaned_cluster$Sales, data.cleaned_cluster$Price, data.cleaned_cluster$State))
C = data.frame(cbind(data.cleaned_cluster$Sales_commercial, data.cleaned_cluster$Price_commercial, data.cleaned_cluster$State))
colnames(R) = c("Sales", "Price", "State")
colnames(C) = c("Sales", "Price", "State")

mega=cbind(R,C)
mega = unlist(mega)

R[, 4] = "Residential"
C[, 4] = "Commercial"

mega=data.frame(R$Sales,C$Sales,R$V4,C$V4)
mega = data.frame(cbind(stack(mega(Sales=Sales,)),stack(R$V4,C$V4)))
colnames(mega)=c("Residential","Commercial")


vioplot(mega$Sales, names = "Residential Sales")
ggplot(mega,aes(as.factor(colnames(mega),mega))) + geom_violin()

