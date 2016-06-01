
install.packages("RDCOMClient")
library("RDCOMClient")

cls <- function() { 
        require(RDCOMClient) 
        wsh <- COMCreate("Wscript.Shell") 
        wsh$SendKeys("\014") 
        invisible(wsh) 
} 
cls()  # invoke 


