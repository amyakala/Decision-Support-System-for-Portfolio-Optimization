library(timeSeries)
library(fPortfolio)
library(caTools)
library(quantmod)
library(tseries)

# Getting Market Portfolio using S&P 500 Data
getSymbols("^GSPC",from="2012-10-01",to="2014-11-26",src="yahoo")
SP500.Clo=GSPC[,6]
SP500.Ret=periodReturn(SP500.Clo,period="daily")
head(SP500.Ret)
adf.test(SP500.Ret)
mean(SP500.Ret)
market.returns=mean(SP500.Ret)*253 # Converting daily rteurns to yearly returns of S & P 500
market.returns
market.risk=sd(SP500.Ret)*sqrt(253) # Converting daily risk to yearly returns of S & P 500
market.risk


# Getting Different Stocks Data.

getSymbols("GOOG",from="2012-10-01",to="2014-11-26",src="google")
GOOG.Clo=GOOG[,4]
head(GOOG.Clo)
tail(GOOG.Clo)
GOOG.Ret=periodReturn(GOOG.Clo,period="daily")
head(GOOG.Ret)
tail(GOOG.Ret)
plot(GOOG.Ret) # Both plot() & acf() are used to see whether the returns are stationary 
acf(GOOG.Ret) 
adf.test(GOOG.Ret) # This says that google returns are stationary
GOOG.Mean=mean(GOOG.Ret)
GOOG.StdDev=sd(GOOG.Ret)

getSymbols("WMT",from="2012-10-01",to="2014-11-26",src="google")
WMT.Clo=WMT[,4]
WMT.Ret=periodReturn(WMT.Clo,period="daily")

getSymbols("XOM",from="2012-10-01",to="2014-11-26",src="google")
XOM.Clo=XOM[,4]
XOM.Ret=periodReturn(XOM.Clo,period="daily")

getSymbols("CVX",from="2012-10-01",to="2014-11-26",src="google")
CVX.Clo=CVX[,4]
CVX.Ret=periodReturn(CVX.Clo,period="daily")

getSymbols("GM",from="2012-10-01",to="2014-11-26",src="google")
GM.Clo=GM[,4]
GM.Ret=periodReturn(GM.Clo,period="daily")

getSymbols("BAC",from="2012-10-01",to="2014-11-26",src="google")
BAC.Clo=BAC[,4]
BAC.Ret=periodReturn(BAC.Clo,period="daily")

getSymbols("WFC",from="2012-10-01",to="2014-11-26",src="google")
WFC.Clo=WFC[,4]
WFC.Ret=periodReturn(WFC.Clo,period="daily")

getSymbols("C",from="2012-10-01",to="2014-11-26",src="google")
C.Clo=C[,4]
C.Ret=periodReturn(C.Clo,period="daily")

getSymbols("BA",from="2012-10-01",to="2014-11-26",src="google")
BA.Clo=BA[,4]
BA.Ret=periodReturn(BA.Clo,period="daily")

getSymbols("MSFT",from="2012-10-01",to="2014-11-26",src="google")
MSFT.Clo=MSFT[,4]
MSFT.Ret=periodReturn(MSFT.Clo,period="daily")

getSymbols("AMZN",from="2012-10-01",to="2014-11-26",src="google")
AMZN.Clo=AMZN[,4]
AMZN.Ret=periodReturn(AMZN.Clo,period="daily")

getSymbols("BAH",from="2012-10-01",to="2014-11-26",src="google")
BAH.Clo=BAH[,4]
BAH.Ret=periodReturn(BAH.Clo,period="daily")

getSymbols("^IRX",from="2012-10-01",to="2014-11-26")
IRX1=IRX
head(IRX1)
IRX1=IRX1[,6]
head(IRX1)
IRX1=IRX1/(100*253)
head(IRX1)
mean(IRX1)
riskFreeRate=mean(IRX1)*253
riskFreeRate # Yearly Rate

Returns.Portfolio.Stocks=as.timeSeries(cbind(GOOG.Ret,WMT.Ret,XOM.Ret,CVX.Ret,
                                             GM.Ret,BAC.Ret,WFC.Ret,C.Ret,BA.Ret,MSFT.Ret,AMZN.Ret,BAH.Ret))
colnames(Returns.Portfolio.Stocks)=c("GOOG.Ret","WMT.Ret","XOM.Ret","CVX.Ret",
                                     "GM.Ret","BAC.Ret","WFC.Ret","C.Ret","BA.Ret",
                                     "MSFT.Ret","AMZN.Ret","BAH.Ret")
head(Returns.Portfolio.Stocks)

# Longonly Portfolio

portfolio.spec=portfolioSpec(portfolio=list(riskFreeRate=mean(IRX1),nFrontierPoints=50))

Frontier=portfolioFrontier(Returns.Portfolio.Stocks,spec=portfolio.spec)
plot(Frontier,2)

riskReturnPoints1=frontierPoints(Frontier)
annualizedPoints1=data.frame(targetRisk=riskReturnPoints1[,"targetRisk"]*sqrt(253),
                            targetReturn=riskReturnPoints1[,"targetReturn"]*253)

allocations=getWeights(Frontier@portfolio)
colnames(allocations)=StocksList
allocations

tangency.portfolio=tangencyPortfolio(Returns.Portfolio.Stocks,spec=portfolio.spec)
tangency.portfolio

mean.tangency.portfolio=getPortfolio(tangency.portfolio)$targetReturn
mean.tangency.portfolio

return.TP=mean.tangency.portfolio[1]*253 # Converting it to yearly return
return.TP

sigma.tangency.portfolio=getPortfolio(tangency.portfolio)$targetRisk
sigma.tangency.portfolio

volatality.TP=sigma.tangency.portfolio[2]*sqrt(253) # Converting it to yearly risk
volatality.TP

# You can short in this portfolio

StocksList=c("GOOG.Ret","WMT.Ret","XOM.Ret","CVX.Ret",
             "GM.Ret","BAC.Ret","WFC.Ret","C.Ret","BA.Ret",
             "MSFT.Ret","AMZN.Ret","BAH.Ret")
constraint=c("minW[1:length(StocksList)]=-1")
portfolio.spec=portfolioSpec(portfolio=list(riskFreeRate=mean(IRX1),nFrontierPoints=50))
Frontier1=portfolioFrontier(Returns.Portfolio.Stocks,spec=portfolio.spec,constraints=constraint)
plot(Frontier1,1)

riskReturnPoints2=frontierPoints(Frontier1)
annualizedPoints2=data.frame(targetRisk=riskReturnPoints2[,"targetRisk"]*sqrt(253),
                             targetReturn=riskReturnPoints2[,"targetReturn"]*253)

allocations=getWeights(Frontier1@portfolio)
colnames(allocations)=StocksList
allocations

tangency.portfolio=tangencyPortfolio(Returns.Portfolio.Stocks,spec=portfolio.spec,
                                     constraints=constraint)
tangency.portfolio

mean.tangency.portfolio=getPortfolio(tangency.portfolio)$targetReturn
mean.tangency.portfolio

return.TP=mean.tangency.portfolio[1]*253 # Converting it to yearly return
return.TP

sigma.tangency.portfolio=getPortfolio(tangency.portfolio)$targetRisk
sigma.tangency.portfolio

volatality.TP=sigma.tangency.portfolio[2]*sqrt(253) # Converting it to yearly risk
volatality.TP


# Having a constraint that minimum of 5% in each and maximum of 20% in each stock

constraint=c("minW[1:length(StocksList)]=0.05","maxW[1:length(StocksList)]=0.2")
portfolio.spec=portfolioSpec(portfolio=list(riskFreeRate=mean(IRX1),nFrontierPoints=50))
Frontier2=portfolioFrontier(Returns.Portfolio.Stocks,spec=portfolio.spec,constraints=constraint)
plot(Frontier2,1)

riskReturnPoints3=frontierPoints(Frontier2)
annualizedPoints3=data.frame(targetRisk=riskReturnPoints3[,"targetRisk"]*sqrt(253),
                             targetReturn=riskReturnPoints3[,"targetReturn"]*253)

allocations=getWeights(Frontier2@portfolio)
colnames(allocations)=StocksList
allocations

tangency.portfolio=tangencyPortfolio(Returns.Portfolio.Stocks,spec=portfolio.spec,
                                     constraints=constraint)
tangency.portfolio

mean.tangency.portfolio=getPortfolio(tangency.portfolio)$targetReturn
mean.tangency.portfolio

return.TP=mean.tangency.portfolio[1]*253 # Converting it to yearly return
return.TP

sigma.tangency.portfolio=getPortfolio(tangency.portfolio)$targetRisk
sigma.tangency.portfolio

volatality.TP=sigma.tangency.portfolio[2]*sqrt(253) # Converting it to yearly risk
volatality.TP

xlimit=range(annualizedPoints1[,1],annualizedPoints2[,1])
ylimit=range(annualizedPoints1[,2],annualizedPoints2[,2])

plot(annualizedPoints1,xlim=xlimit,ylim=ylimit,pch=16,col="blue")
points(annualizedPoints2,pch=16,col="red")
points(annualizedPoints3,pch=16,col="green")
legend("right",legend=c("long only","Short Selling","5% to 20%"),col=c("blue","red","green"),pch=16)



