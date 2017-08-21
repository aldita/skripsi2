library(arules)
library(arulesViz)

#Getting data
dat <- function() {
  data <- read.csv("C:/Users/aldita/Documents/Skripsi/2/databps01.csv")
  return(data)
}

rawdata <- function(){
  dfVardata <- as.matrix(dat()[,5])
  dfVarId <- as.matrix(dat()[,3])
  df <- as.data.frame(cbind(dfVarId,dfVardata))
  return(df)
}

dataTrans <- function()
{
  tr_basket <- rawdata()
  
  #if (!is.null(input$chooseId)){
  tr_basket <- split(tr_basket[,2],tr_basket[,1]) #aggregat
  
  
  #}
  
  hasil <- as(tr_basket,"transactions")
  return(hasil)
}

#chooseFreq <- function() {
# frequency <- itemFrequency(dataTrans())
#frequency <- as.data.frame(frequency)
#  item<-rownames(frequency)
#  datfram <- as.data.frame(cbind(item,frequency))
#  minsup <- minSupEclat()
#  minsupCount <- minsup 
#  datfram <- datfram[datfram$frequency >= minsupCount,] #prune!
#  return(datfram)
#}

#tableItemset <- function(itemset) {
#  d <- inspect(dataTrans())[1]  #item pada transaksi
#  f <- sapply(1:nrow(d), function(x) gsub("\\{|\\}","",as.character(d[x,1])))
#  item2 <- chooseFreq()[,1]
#  b <- combn(item2,itemset)
#  combination <- sapply(1:ncol(b),function(i)  paste(b[,i], collapse = ","))
#  names(data) <- NULL
#  dfs <- lapply(list(f, combination), function(x)  melt(strsplit(x, ",")))
#  m <- merge(dfs[[2]], dfs[[1]], by = 1)
#  f <- function(n) sum(aggregate(value ~ L1.y, m[m$L1.x == n,], function(x) length(unique(x)) == itemset)$value)
#  frequency <- sapply(1:length(combination), f)
#  frequency <- as.data.frame(as.numeric(frequency/nrow(dataTrans())))
#  datfram <- as.data.frame(cbind(combination, frequency))
#  colnames(datfram) <- c("combination","frequency")
#  minsupCount <- minSupEclat() 
#  datfram <- datfram[datfram$frequency >= minsupCount,]
#  return(datfram)
#}

#transaksi vertikal
transver <- function() {
  tl <- as(dataTrans(), "tidlists")
  return(inspect(tl))
}

#fungsi frequent itemsets
frequent <- function (){
  f <- ecl()
  tl <- tidLists(f)
  tl2 <- inspect(tl)
  return(tl2)
}

minSupEclat <- function() {
  minSup <- 0.01 
  if (minSup < 0.001) {
    minSup <- 0.1
    #updateNumericInput(session,"minSupEclat", value = 0.1)
  }
  return(minSup)
}

minConfEclat <- function() {
  minConf <- 0.9
  if (minConf < 0.0001) {
    minConf <- 0.1
    #updateNumericInput(session,"minConfEclat", value = 0.01)
  }
  return(minConf)
}

maxIEclat <- function() {
  maxI <- 5
  if (maxI < 2) {
    maxI <- 2
    #updateNumericInput(session,"maxIEclat",2)
  }
  return(maxI)
}

#fungsi eclat
ecl <- function (){
  itemsets <-eclat(
    dataTrans(), parameter = list(
      supp = minSupEclat(), minlen = 2, maxlen = maxIEclat(), tidLists= TRUE
    ))
  return(itemsets)
}

#fungsi rules
fungsi <- function() {
  rules<- ruleInduction(ecl(),dataTrans(),confidence = minConfEclat())
  #if(input$removeRedundant==TRUE){
  #  rules.pruned <- rules[!is.redundant(rules)]
  #  rules<-rules.pruned
  #}
  return(rules)
}

#fungsi tampil rules
dataRules <- function() {
  data<- fungsi()
  #if (!is.null(input$intmeasure)) {
    quality(data) <-
      cbind(
        quality(data), RPF()
      )
  #  }
  data2 <- inspect(data)  
  return(data2)
}


RPF <- function(){
  rp <- interestMeasure(fungsi(), "support", dataTrans()) * interestMeasure(fungsi(), "confidence", dataTrans())
  return(rp)
}

numberRuleEclat <- function() {
  nr <- 5
  if (nr > length(fungsi())) {
    nr <- length(fungsi())
    #updateNumericInput(session,"numberRuleApr", value = length(fungsi()))
  }
  return(nr)
}

#visualisasi
sampel <- function(){
  sam <- sample(fungsi(),numberRuleEclat())
  return(sam)
}

splot<- function(){
  plot <- plot(sampel())
  return(plot)
}

goplot<- function(){
  plot <- plot(sampel(), method = "grouped")
  return(plot)
}

gaplot<- function(){
  plot <- plot(sampel(), method = "graph")
  return(plot)
}

pplot<- function(){
  plot <- plot(sampel(), method = "paracoord", control=list(reorder=TRUE))
  return(plot)
}