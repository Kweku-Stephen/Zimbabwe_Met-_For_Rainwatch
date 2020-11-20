colnames(t(Data_T)) == starts_with("Month")

#Trying to print available months from the binded data to make sure the dataframe to be 
#created captures only the available months
rep(colnames(Data),times=nrow(unique(Data[,1]))) -> Label

Data_T2 <- data.frame(Data_T,Label)

rep(
    unique((Data_T2[Data_T2[,length(Data_T2)] == "Month", ])[,1]),
    each =length(Data[1,])
    )


#Trying to make sure the added columns have the same month number and their respective day number 
if(length(Month_2) < length(Month_1)){
    Month_2[,(ncol(Month_2)+1):31] <- c(2,31,rep(NA,times=length(start_year:end_year)))
    colnames(Month_2) <- names
    Month_2
}

Data_T %>% data.frame(rep(colnames(Data),times=nrow(unique(Data[,1])))) -> Data_T2
