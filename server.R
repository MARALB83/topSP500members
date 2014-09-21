require(tseries)
require(XML)
require(ggplot2)
require(reshape2)
require(tcltk)
require(scales)

##Get current S&P 500 members from Wikipedia
url<-'http://en.wikipedia.org/wiki/List_of_S%26P_500_companies'
htmlTable<-data.frame(readHTMLTable(url)[1])
sp500Members<-as.character(htmlTable$NULL.Ticker.symbol)
sp500Members<-gsub(pattern = '.',replacement = '-',x = sp500Members,fixed = TRUE)

##Get Adjusted Close pricing data for all 500 members
#Get most recent date where market data is available
today<-Sys.Date()
if(weekdays(today)=='Sunday'){today<-today-2}else if(weekdays(today)=='Monday'){today<-today-3}else{today<-today-1}
#Get previous year
previousYear<-as.character(as.numeric(substr(today,start = 1,stop = 4))-1)
#Get last date of previous year
start_date <- as.Date(paste(previousYear,'-12-31',sep = ''))
#Adjust to trading day
if(weekdays(start_date)=='Saturday'){start_date<-start_date-1}else if(weekdays(start_date)=='Sunday'){start_date<-start_date-2}
#Get pricing data
options(warn=-1)
index<-as.Date(row.names(as.data.frame(get.hist.quote(instrument = 'SPY',start = start_date,end=today,provider = 'yahoo',quote = 'AdjClose',quiet = TRUE))))
numberDays<-length(index)
priceData<-data.frame(matrix(nrow = numberDays,ncol = length(sp500Members)),row.names = index)
names(priceData)<-sp500Members
for(i in 1:length(sp500Members)){
    temp<-as.data.frame(get.hist.quote(instrument = sp500Members[i],start = start_date,end=today,provider = 'yahoo',quote = 'AdjClose',quiet = TRUE))
    tempSize<-length(temp$AdjClose)
    if(tempSize<numberDays){
        for(j in 1:length(row.names(priceData))){
            for(k in 1:length(row.names(temp))){
                if(row.names(temp)[k]==row.names(priceData)[j]){priceData[j,i]=temp$AdjClose[k]}
            }
        }
    }else{priceData[,i]<-temp}
}
options(warn=0)

##Compute daily rate of returns 
N<-nrow(priceData)
returnData<-priceData[2:N,]/priceData[1:N-1,]-1
##Compute performance metrics
performanceMetrics<-data.frame(matrix(nrow = length(sp500Members),ncol = 4),row.names=sp500Members)
names(performanceMetrics)<-c('Cumulative_RoR','Annualized_RoR','Annualized_Volatility','Return2Volatility_Ratio')
#Cumulative rate of return
performanceMetrics[,1]<-apply(1+returnData,MARGIN = 2,FUN = prod,na.rm=TRUE)-1
#Annualized rate of return
validEntries<-N-colSums(apply(priceData,MARGIN = 2,FUN = is.na))-1
performanceMetrics[,2]<-(1+performanceMetrics[,1])^(252/(validEntries))-1
#Risk - Annualized Standard Deviation (measure of volatility)
performanceMetrics[,3]<-apply(returnData,MARGIN = 2,FUN=sd,na.rm=TRUE)*sqrt(252)
#Return2Volatility Ratio = Annualized rate of return divided by annualized volatility
performanceMetrics[,4]<-performanceMetrics[,2]/performanceMetrics[,3]

##Sort stocks by each performance Metric
cumulativeRoRSorted<-performanceMetrics[order(-performanceMetrics$Cumulative_RoR),]
AnnualizedVolSorted<-performanceMetrics[order(-performanceMetrics$Annualized_Volatility),]
Return2VolSorted<-performanceMetrics[order(-performanceMetrics$Return2Volatility_Ratio),]

##Create daily cumulative return for all stocks
dailyCumulativeReturn<-apply(X = (1+returnData),MARGIN = 2,FUN = cumprod)-1

shinyServer(function(input, output) {
    #Generate instructions
    #output$instructions <- renderText({
    #})
    
    #Generate text for Performance List tab
    output$performanceList<- renderText({paste('You selected the top ',input$members,' S&P 500 Members, in terms of',input$metric,'.')})
    #Generate table
    output$performanceTable<- renderTable({
        if(input$metric=='Cumulative Rate of Return'){
            t<-cumulativeRoRSorted[1:input$members,]
        }
        else if(input$metric=='Annualized Standard Deviation'){
            t<-AnnualizedVolSorted[1:input$members,]
        }
        else {
            t<-Return2VolSorted[1:input$members,]
        }
        t
    })
    #Generate text for Plot tab
    output$textPlot<-renderText({paste('The following plot shows cumulative year-to-date performance for the top ',input$members,' S&P 500 Members.')})
    #Generate plot
    output$plot<-renderPlot({
        #Grab names of top input$members in terms of cumulative performance
        topNnames<-row.names(cumulativeRoRSorted[1:input$members,])
        #Subset daily daily cumulative return
        subsetdailyCumulativeReturn<-data.frame(subset(dailyCumulativeReturn,select=topNnames))*100
        maxRoR<-ceiling(max(subsetdailyCumulativeReturn))
        minRoR<-floor(min(subsetdailyCumulativeReturn))
        subsetdailyCumulativeReturn['Date']<-index[2:length(index)]
        meltedsubsetdailyCumulativeReturn<-melt(data = subsetdailyCumulativeReturn,id='Date')
        p<-ggplot(meltedsubsetdailyCumulativeReturn,aes(x=Date,y=value,colour=variable))+geom_line()+scale_x_date(breaks='1 month',minor_breaks = '1 week',labels=date_format('%d-%B-%Y'))+theme(axis.text.x = element_text(angle = 45, hjust = 1))+ylab('Cumulative YTD Return (%)')+scale_y_continuous(breaks=seq(minRoR,maxRoR,by=5))+scale_colour_discrete(name="Ticker")
        print(p)
        })
})
