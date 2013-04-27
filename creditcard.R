read.cc <-function(out=FALSE, myfile="Activity.CSV"){
  # To run, just type read.cc()
  # If you want to rename and output the file,
  # type read.cc(T)
  
  setwd("/Users/mariafinison/kevin/R/creditcard") 
  
  cc <- read.csv(myfile, header=TRUE) 
  cc.sub <- subset(cc, Type != "Payment") 
  cc.sub.sales <- subset(cc, Type == "Sale")
  
  cc.sub$nmonth <- substr(cc.sub$Trans.Date,1,2)
  
  amnt.spent <- -1*tapply(cc.sub$Amount,cc.sub$Description,sum)
  print(as.data.frame(sort(amnt.spent)))
  
  balance <- -1*sum(cc.sub.sales$Amount)
  cat("\n\nNew credit card balance is: $", balance, sep="")
  
  cat("\n\nThe balance for each half-month is: \n")
  print(-1*tapply(cc.sub$Amount,cc.sub$nmonth,sum))
  cat("(enter these into gnucash)")
  
  # I want to write the csv file back out, but 
  # append the year and month of the bill
  # onto the file name. Example:
  # If the bill came out on March 16, 2013, then
  # name file ccactivity-2013-03.csv
  
  if (out) {
    d <- format(Sys.Date(), "%Y-%m")
    f1 <- paste0("ccactivity-", d, ".csv") 
    write.csv(cc, file = f1)
    cat("\n\nfile", f1, "written to output")
  }
}