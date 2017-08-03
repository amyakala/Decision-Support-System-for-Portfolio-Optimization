getSymbols("^GSPC",from="2012-10-01",to="2014-11-26",src="yahoo")
SP500.Clo=GSPC[,6]
SP500.Ret=periodReturn(SP500.Clo,period="daily")
head(SP500.Ret)
adf.test(SP500.Ret)
mean(SP500.Ret)
market.returns=mean(SP500.Ret)*253 
market.returns
market.risk=sd(SP500.Ret)*sqrt(253) 
market.risk

getSymbols("GOOG",from="2012-10-01",to="2014-11-26",src="google")
GOOG.Clo=GOOG[,4]
GOOG.Ret=periodReturn(GOOG.Clo,period="daily")

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

portfolio.spec=portfolioSpec(portfolio=list(riskFreeRate=mean(IRX1),nFrontierPoints=50))


StocksList=c("GOOG.Ret","WMT.Ret","XOM.Ret","CVX.Ret",
             "GM.Ret","BAC.Ret","WFC.Ret","C.Ret","BA.Ret",
             "MSFT.Ret","AMZN.Ret","BAH.Ret")
constraint=c("minW[1:length(StocksList)]=0.05","maxW[1:length(StocksList)]=0.2")

Frontier=portfolioFrontier(Returns.Portfolio.Stocks,spec=portfolio.spec,constraints=constraint)
plot(Frontier,1)

MVP.weights=efficientPortfolio(Returns.Portfolio.Stocks,spec=portfolio.spec,constraints=constraint)
MVP.weights

tangency.portfolio=tangencyPortfolio(Returns.Portfolio.Stocks,spec=portfolio.spec,
                                     constraints=constraint)
tangency.portfolio
tangency.portfolio.weights=getPortfolio(tangency.portfolio)$weights
tangency.portfolio.weights

mean.Individual.Stocks=getStatistics(Frontier)$mean
mean.Individual.Stocks

# Converting individual stocks means to yearly ones

mean.Individual.Stocks=(getStatistics(Frontier)$mean)*253
mean.Individual.Stocks

mean.tangency.portfolio=getPortfolio(tangency.portfolio)$targetReturn
mean.tangency.portfolio

return.TP=mean.tangency.portfolio[1]*253 # Converting it to yearly return
return.TP

sigma.tangency.portfolio=getPortfolio(tangency.portfolio)$targetRisk
sigma.tangency.portfolio

volatality.TP=sigma.tangency.portfolio[2]*sqrt(253) # Converting it to yearly risk
volatality.TP

cov.matrix.Stocks=getStatistics(tangency.portfolio)$Cov
cov.matrix.Stocks

# Calculating Weights of Tangency Portfolio & Risk Free Asset given risk=10%/year
# Or
# Calculating different Efficeint Portfolios given portfolio risk.

sigma.portfolio=0.10
riskFreeRate

weight.at.tangency.portfolio=sigma.portfolio/volatality.TP
names(weight.at.tangency.portfolio)=c("Weight at Tangency Portfolio")
weight.at.tangency.portfolio

tangency.portfolio.return1=getPortfolio(tangency.portfolio)$targetReturn
tangency.portfolio.return1

tangency.portfolio.return=tangency.portfolio.return1[1]*253
tangency.portfolio.return

return.portfolio=(weight.at.tangency.portfolio*tangency.portfolio.return)+
  ((1-weight.at.tangency.portfolio)*riskFreeRate)
names(return.portfolio)=c("portfolio return")
return.portfolio

Weight.at.riskfreeassest=1-weight.at.tangency.portfolio
names(Weight.at.riskfreeassest)=c("Weight.at.riskfreeassest")
Weight.at.riskfreeassest

Weights.of.stocks.TP=weight.at.tangency.portfolio*tangency.portfolio.weights
Weights.of.stocks.TP

# Calculating Weights of Tangency Portfolio & Risk Free Asset given return=23.40%/Year

mean.portfolio=0.2340
riskFreeRate

tangency.portfolio.return1=getPortfolio(tangency.portfolio)$targetReturn
tangency.portfolio.return1
tangency.portfolio.return=tangency.portfolio.return1[1]*253 # Yearly Return
tangency.portfolio.return

weight.at.tangency.portfolio=(mean.portfolio-riskFreeRate)/(tangency.portfolio.return-riskFreeRate)
names(weight.at.tangency.portfolio)=c("weight of Tangency Portfolio")
weight.at.tangency.portfolio

Weight.at.riskfreeassest=1-weight.at.tangency.portfolio
names(Weight.at.riskfreeassest)=c("Weight.at.riskfreeassest")
Weight.at.riskfreeassest


Weights.of.stocks.TP=weight.at.tangency.portfolio*tangency.portfolio.weights
Weights.of.stocks.TP

# Calculating Beta of each Stock and the portfolio which tells you how aggressive your portfolio

Beta.Google=(GOOG.Mean-riskFreeRate)/(mean(SP500.Ret)-riskFreeRate)
Beta.Google

Beta.Portfolio=((mean.portfolio/253)-riskFreeRate)/(mean(SP500.Ret)-riskFreeRate)
Beta.Portfolio

# Calculating Capital Market Line

retur.efficient.portfolio=(riskFreeRate)+((sigma.portfolio/market.risk)*
                                            (market.returns-(riskFreeRate)))
retur.efficient.portfolio

sharpes.ratio=(market.returns-(riskFreeRate))/market.risk
sharpes.ratio