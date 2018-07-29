library(rpart)				        # Popular decision tree algorithm
#library(rattle)					# Fancy tree plot
library(rpart.plot)				# Enhanced tree plots
library(RColorBrewer)				# Color selection for fancy tree plot
#library(party)					# Alternative decision tree algorithm
#library(partykit)				# Convert rpart object to BinaryTree
library(caret)					# Just a data source for this script
#library(mvnormtest)
library(plotly)
# but probably one of the best R packages ever. 
data(segmentationData)				# Get some data
data <- segmentationData[,-c(1,2)]
library(maptools)
library(MASS)
library(lavaan) #latent variable analysis/SEM
#------

panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...) 
{ 
  usr <- par("usr"); on.exit(par(usr)) 
  par(usr = c(0, 1, 0, 1)) 
  r <- abs(cor(x, y)) 
  txt <- format(c(r, 0.123456789), digits = digits)[1] 
  txt <- paste0(prefix, txt) 
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt) 
  text(0.5, 0.5, txt, cex = cex.cor * r) 
} 

#-Loading all commodities for the palouse 1989 - 2015

palouse_sumloss_allcomm <- read.csv("/dmine/data/USDA/agmesh-scenarios/Allstates/summaries/palouse_summary_all.csv")
palouse_sumloss_allcomm2  <- aggregate(loss ~ year + damagecause + county + commodity,  palouse_sumloss_allcomm, sum)
palouse_sumloss_allcomm2_07_15 <- subset(palouse_sumloss_allcomm2, year <= 2015 & year >= 2007)

palouse_count_allcomm2  <- aggregate(count ~ year + damagecause + county + commodity,  palouse_sumloss_allcomm, sum)

palouse_sumloss_allcomm2 <- palouse_sumloss_allcomm2[palouse_sumloss_allcomm2$loss >= 1, ]


#-Loading all WHEAT claims for the palouse from 1989-2015

palouse_sumloss <- read.csv("/dmine/data/USDA/agmesh-scenarios/Allstates/summaries/Palouse_summary_sumloss.csv")
palouse_counts <- read.csv("/dmine/data/USDA/agmesh-scenarios/Allstates/summaries/Palouse_summary_counts.csv")
#--drought claim counts all counties 
pc_drought <- subset(palouse_counts, damagecause == "Drought")

palouse_sumloss <- aggregate(loss ~ year + damagecause + county,  palouse_sumloss, sum)
palouse_counts <- aggregate(count ~ year + damagecause + county,  palouse_counts, sum)
pc_drought <- aggregate(count ~ year + damagecause + county,  pc_drought, sum)


palouse_sumloss_aggregate <- aggregate(palouse_sumloss$loss, list(palouse_sumloss$damagecause), FUN = "sum")


palouse_sumloss_2007_2015_aggregate <- subset(palouse_sumloss, year >= 2007 & year <= 2015 )
#-is there a normal signal using just wheat, drought claims across all of the pacific northwest
palouse_sumloss_2007_2015_aggregate <- aggregate(palouse_sumloss_2007_2015_aggregate$loss, list(palouse_sumloss_2007_2015_aggregate$damagecause), FUN = "sum")

palouse_sumloss_2015 <- subset(palouse_sumloss, year == 2015 )
palouse_sumloss_2015 <- aggregate(palouse_sumloss_2015$loss, list(palouse_sumloss_2015$damagecause), FUN = "sum")

palouse_sumloss_2009 <- subset(palouse_sumloss, year == 2009 )
palouse_sumloss_2009 <- aggregate(palouse_sumloss_2009$loss, list(palouse_sumloss_2009$damagecause), FUN = "sum")

palouse_sumloss_2011 <- subset(palouse_sumloss, year == 2011 )
palouse_sumloss_2011 <- aggregate(palouse_sumloss_2011$loss, list(palouse_sumloss_2011$damagecause), FUN = "sum")

palouse_sumloss_drought <- subset(palouse_sumloss, damagecause == "Drought")
qqnorm(palouse_sumloss_drought$loss)

#load wheat pricing

#barley <- read.csv("/dmine/data/USDAprices/barleyprices_1988_2017.csv", header=TRUE, strip.white =TRUE)
wheatprice <- read.csv("/dmine/data/USDAprices/wheatprices_1998_2017.csv", header=TRUE, strip.white =TRUE)
wheatprice_year <- aggregate(wheatprice$Price, list(wheatprice$Year), FUN="mean")
colnames(wheatprice_year) <- c("year", "price")

#merge wheat pricing with palouse_sumloss

palouse_sumloss <- merge(palouse_sumloss, wheatprice_year, by = "year")

#use a cube transformation on loss for WHEAT claims

Math.cbrt <- function(x) {
  sign(x) * abs(x)^(1/3)
}

#palouse_sumloss2 <- subset(palouse_sumloss, loss > 0)

palouse_sumloss$cube_loss <- Math.cbrt(palouse_sumloss$loss)
palouse_counts$cube_counts <- Math.cbrt(palouse_counts$count)

#use a cube transformation on loss for all commodity claims

palouse_sumloss_allcomm2$cube_loss <- Math.cbrt(palouse_sumloss_allcomm2$loss)


#-use a log transform on the same WHEAT claims data

palouse_sumloss$log_loss <- log(which(!is.na(palouse_sumloss$loss)))

# - plot some qqplots to see how normal the data is

#qqnorm(palouse_sumloss$loss)
#qqnorm(palouse_sumloss$cube_loss)
#qqnorm(palouse_sumloss$log_loss)
#qqnorm(palouse_sumloss_allcomm2$cube_loss)
#qqnorm(palouse_counts$count)

#box cox transformation


#-factor counties
palouse_sumloss$county = factor(palouse_sumloss$county,
                                levels=unique(palouse_sumloss$county))

#-factor years
#palouse_sumloss$year = factor(palouse_sumloss$year,
#                                levels=unique(palouse_sumloss$year))

#-plot basic interaction plots for WHEAT cube root loss using year as x and damagecause as the line
palouse_sumloss_onlydrought <- subset(palouse_sumloss, damagecause == "Drought")

palouse_sumloss_drought <- subset(palouse_sumloss, damagecause == "Drought" | damagecause == "Heat" | damagecause == "Decline in Price" | damagecause == "Cold Winter")
palouse_sumloss_drought$damagecause <- factor(palouse_sumloss_drought$damagecause)
data_loss_county <- subset(palouse_sumloss_onlydrought, county == "Latah" | county == "Umatilla" | county == "Whitman" | county == "Spokane" | county == "Adams" | county == "Lincoln" )
data_loss_county$county <- factor(data_loss_county$county)
options(scipen = 999)


#--accessing output of design matrix/time lag data based on monthly selection from dashboard runs

var1 <- read.csv("/dmine/data/USDA/agmesh-scenarios/Allstates/climatematrix_outputs/pr_jun2_cube_root_loss_climatecorrelation.csv")
colnames(var1)[9] <- paste(colnames(var1)[2], "_zscore", sep="")


var2 <- read.csv("/dmine/data/USDA/agmesh-scenarios/Allstates/climatematrix_outputs/pet_jul3_cube_root_loss_climatecorrelation.csv")
colnames(var2)[9] <- paste(colnames(var2)[2], "_zscore", sep="")
var3 <- read.csv("/dmine/data/USDA/agmesh-scenarios/Allstates/climatematrix_outputs/tmmx_jul2_cube_root_loss_climatecorrelation.csv")
colnames(var3)[9] <- paste(colnames(var3)[2], "_zscore", sep="")

var4 <- read.csv("/dmine/data/USDA/agmesh-scenarios/Allstates/climatematrix_outputs/pr_jun2_cube_root_acres_climatecorrelation.csv")
colnames(var4)[9] <- paste(colnames(var4)[2], "_zscore", sep="")


var5 <- read.csv("/dmine/data/USDA/agmesh-scenarios/Allstates/climatematrix_outputs/pet_jun2_loss_per_acre_climatecorrelation.csv")
colnames(var5)[9] <- paste(colnames(var5)[2], "_zscore", sep="")
var6 <- read.csv("/dmine/data/USDA/agmesh-scenarios/Allstates/climatematrix_outputs/tmmx_jun1_cube_root_acres_climatecorrelation.csv")
colnames(var6)[9] <- paste(colnames(var6)[2], "_zscore", sep="")


var7 <- read.csv("/dmine/data/USDA/agmesh-scenarios/Allstates/climatematrix_outputs/pr_sep5_loss_climatedata.csv")
colnames(var4)[9] <- paste(colnames(var7)[2], "_zscore", sep="")


var8 <- read.csv("/dmine/data/USDA/agmesh-scenarios/Allstates/climatematrix_outputs/pet_sep5_loss_climatedata.csv")
colnames(var5)[9] <- paste(colnames(var8)[2], "_zscore", sep="")
var9 <- read.csv("/dmine/data/USDA/agmesh-scenarios/Allstates/climatematrix_outputs/tmmx_jul2_loss_climatedata.csv")
colnames(var6)[9] <- paste(colnames(var9)[2], "_zscore", sep="")

var7a <- read.csv("/dmine/data/USDA/agmesh-scenarios/Allstates/climatematrix_outputs/pr_sep5_loss_climatecorrelation.csv")
colnames(var4)[9] <- paste(colnames(var7)[2], "_zscore", sep="")


var8a <- read.csv("/dmine/data/USDA/agmesh-scenarios/Allstates/climatematrix_outputs/pet_sep5_loss_climatecorrelation.csv")
colnames(var5)[9] <- paste(colnames(var8)[2], "_zscore", sep="")
var9a <- read.csv("/dmine/data/USDA/agmesh-scenarios/Allstates/climatematrix_outputs/tmmx_jul2_loss_climatecorrelation.csv")
colnames(var6)[9] <- paste(colnames(var9)[2], "_zscore", sep="")







data1 <- cbind(var1, var2[9], var3[9])
data2 <- cbind(var1[1:6], var2[2], var3[2])

data3 <- cbind(var4[1:6], var5[2], var6[2])

data3 <- plyr::join(data3, wheatprice_year, by = "year")

data4 <- cbind(var7[1:6], var8[2], var9[2])

data4a <- left_join(data4, var7a, by = c("year" = "year", "county" = "county"))
data4aa <- na.omit(data4a)

fit <- ('county ~ pr
        county ~ tmmx
        loss ~  = data2)
