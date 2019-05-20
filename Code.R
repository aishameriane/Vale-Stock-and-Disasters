
chooseCRANmirror(graphics = FALSE, ind = 10)
if (!require("pacman")) install.packages("pacman")
pacman::p_load(ggplot2, forecast, BETS, seasonal, seasonalview, lubridate, zoo, stargazer, gridExtra, reshape2, ggfortify, RColorBrewer, scales, quantmod, PerformanceAnalytics, strucchange, knitr, grid, ggpubr, gdata, vars, urca, compiler, DescTools, kableExtra, readxl, tseries, Synth)

sheet <- read.csv("https://raw.githubusercontent.com/aishameriane/Vale-Stock-and-Disasters/master/Mining%20companies%20listed%20on%20NYSE.csv?token=AAVGJTQXJQADIEF7Z27VUIK45PV7E")
colnames(sheet)[1] <- "Name"
sheet <- sheet[-c(18,19,5,21),] # for some reson the thing does not understand the -P notation for the preferential stocks, so for now I removed them
# I also removed NexGen Energy (21) because it has more than 600 missing points
symbols <- as.character(sheet[,2])


getSymbols(symbols, src = 'yahoo', from = "2014-01-01", to = "2019-01-01", 
             auto.assign = TRUE, warnings = FALSE) 
prices <- merge(BHP, BBL, RIO, VALE, SCCO, FCX, ACH, ARNC, VEDL, AA, CCJ, TRQ, CLF, CMC, HBM, HSC, MTL, LAC, UEC, PLM, TGB, URG, PLG, UAMY, GMO, XPL)
volume <- prices[,grepl( "Volume" , names(prices) )]
prices <- prices[,grepl( "Close" , names(prices) )]
colnames(volume) <- as.character(sheet[,1])
colnames(prices) <- as.character(sheet[,1])

returns <- diff(log(prices))
n <- length(returns)
returns <- returns[-1,]
volume  <- volume[-1,]

colnames(volume) <- c("BHP", "BBL", "RIO", "VALE", "SCCO", "FCX", "ACH", "ARNC", "VEDL", "AA", "CCJ", "TRQ",
				 "CLF", "CMC", "HBM", "HSC", "MTL", "LAC", "UEC", "PLM", "TGB", "URG", "PLG", "UAMY", "GMO", "XPL")
colnames(returns) <- c("BHP", "BBL", "RIO", "VALE", "SCCO", "FCX", "ACH", "ARNC", "VEDL", "AA", "CCJ", "TRQ",
				 "CLF", "CMC", "HBM", "HSC", "MTL", "LAC", "UEC", "PLM", "TGB", "URG", "PLG", "UAMY", "GMO", "XPL")

Data <- time(returns)
returns <- data.frame(returns)
volume  <- data.frame(volume)
returns$Data  <- Data
volume$Data   <- Data

returns_melt <- melt(data = returns, id.vars = "Data")
volume_melt  <- melt(data = volume, id.vars = "Data")

names(returns_melt) <- c("Data", "Company", "Return")
names(volume_melt) <- c("Data", "Company", "Volume")

data_frame <- merge(returns_melt, volume_melt, by = c("Data", "Company"))
head(data_frame)

Company_index <- unique(data_frame[,2])
Company_index <- data.frame(Company_index, seq(1, length(Company_index), by = 1))
head(Company_index)
names(Company_index) <- c("Company", "Company_index")

Date_index <- unique(returns[,27])
Date_index <- data.frame(Date_index, seq(1, length(Date_index), by = 1))
head(Date_index)
names(Date_index) <- c("Data", "Date_index")

data_frame2 <- merge(data_frame, Company_index, by = c("Company"))
head(data_frame2)
# Vale is the company number 24

data_frame3 <- merge(data_frame2, Date_index, by = c("Data"))

# Now I need a new vector with the controls

Controles <- Company_index[-24,2]

data_frame4 <- cbind(data_frame3[,6], data_frame3[,1:5])
names(data_frame4)[1] <- ("Time")
rm(data_frame3, data_frame2, data_frame)
data_frame4$Company <- as.character(data_frame4$Company)

# Ok, good to go!

vale_controle <- dataprep(
  foo=data_frame4,
  predictors = 'Volume', 
  predictors.op = 'mean',
  time.predictors.prior = 1:491,
  dependent = 'Return',
  unit.variable = 'Company_index',
  unit.names.variable = 'Company',
  time.variable = 'Time',
  treatment.identifier = 24,
  controls.identifier = Controles,
  time.optimize.ssr = 1:491,
  time.plot = 1:1257)

out <- synth(data.prep.obj = vale_controle,optimxmethod = 'BFGS')

tabelas <- synth.tab(synth.res = out,dataprep.res = vale_controle,round.digit = 3)
path_plot<-path.plot(synth.res = out,dataprep.res = vale_controle,Ylab = 'Retornos',Xlab='')
gaps.plot(synth.res = out,dataprep.res = vale_controle,Main = '',Xlab = '',Ylab = 'gap entre Vale e Controle sintético')

stargazer(tabelas$tab.pred,type = 'latex',title = 'estimado controle e tratado')
stargazer(tabelas$tab.v,type = 'latex',title = 'Pesos das variáveis')
pesos_cidades <- matrix(ncol = 1,nrow = 3)


row.names(pesos_cidades) <- c('Itapevi','Taubaté','São Vicente')
colnames(pesos_cidades) <- c('Pesos das Cidades')
pesos_cidades[,1]<- c(0.714,0.11,0.176)
stargazer(pesos_cidades)


