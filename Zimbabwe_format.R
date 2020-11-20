
#########################################################################################################
setwd("C:/Users/Qet/Documents")

library(tidyverse)
library(readxl)

Data <- read_excel(
    "Book1.xlsx" #Path and name of data
)

#Start and end years
start_year = 1980
end_year = 2018
mis_val = -9988
#Taking care of empty cells
Data[,1:ncol(Data)] <- replace(Data[,1:ncol(Data)],is.na(Data[,1:ncol(Data)]),mis_val)

#Slicing the data into 12 transposed chunks,based on the 12 Months 
Month_1 <- data.frame(t(Data[Data[,1] == 1, ])) #extracting and transposing all Januarys
Month_2 <- data.frame(t(Data[Data[,1] == 2, ])) #extracting and transposing all Februarys
Month_3 <- data.frame(t(Data[Data[,1] == 3, ])) #extracting and transposing all Marchs
Month_4 <- data.frame(t(Data[Data[,1] == 4, ])) #extracting and transposing all Aprils
Month_5 <- data.frame(t(Data[Data[,1] == 5, ])) #extracting and transposing all Mays
Month_6 <- data.frame(t(Data[Data[,1] == 6, ])) #extracting and transposing all Junes
Month_7 <- data.frame(t(Data[Data[,1] == 7, ])) #extracting and transposing all Julys
Month_8 <- data.frame(t(Data[Data[,1] == 8, ])) #extracting and transposing all Augusts
Month_9 <- data.frame(t(Data[Data[,1] == 9, ])) #extrActing and transposing all Septembers
Month_10 <- data.frame(t(Data[Data[,1] == 10, ])) #extracting and transposing all Octobers
Month_11 <- data.frame(t(Data[Data[,1] == 11, ])) #extracting and transposing all November
Month_12 <- data.frame(t(Data[Data[,1] == 12, ])) #extracting and transposing all Decembers
#
names = paste("X",1:31,sep="")

# Bind the individual Months above rowise
if (ncol(Month_1) < 31){Month_1[,(ncol(Month_1)+1):31]<-mis_val 
colnames(Month_1)<- names
Month_1 
} else {Month_1} %>% 
    rbind.data.frame(
        if (ncol(Month_2) < 31){
            Month_2[,(ncol(Month_2)+1):31]<-mis_val 
            colnames(Month_2)<- colnames(Month_1)
            Month_2 
        } else {Month_2}
    ) %>% 
    rbind.data.frame(
        if (ncol(Month_3) < 31){
            Month_3[,(ncol(Month_3)+1):31]<-mis_val 
            colnames(Month_3)<- colnames(Month_1)
            Month_3 
        } else {Month_3}
    ) %>% 
    rbind(
        if (ncol(Month_4) < 31){
            Month_4[,(ncol(Month_4)+1):31]<- mis_val
            colnames(Month_4) <- colnames(Month_1)
            Month_4
        } else{Month_4}
    ) %>% 
    rbind.data.frame(
        if (ncol(Month_5) < 31){
            Month_5[,(ncol(Month_5)+1):31]<- mis_val
            colnames(Month_5) <- colnames(Month_1)
            Month_5
        } else{Month_5}
    ) %>% 
    rbind.data.frame(
        if (ncol(Month_6) < 31){
            Month_6[,(ncol(Month_6)+1):31]<- mis_val
            colnames(Month_6) <- colnames(Month_1)
            Month_6
        } else{Month_6}
    ) %>% 
    rbind.data.frame(
        if (ncol(Month_7) < 31){
            Month_7[,(ncol(Month_7)+1):31]<- mis_val
            colnames(Month_7) <- colnames(Month_1)
            Month_7
        } else{Month_7}
    ) %>% 
    rbind.data.frame(
        if (ncol(Month_8) < 31){
            Month_8[,(ncol(Month_8)+1):31]<- mis_val
            colnames(Month_8) <- colnames(Month_1)
            Month_8
        } else{Month_8}
    ) %>% 
    rbind.data.frame(
        if (ncol(Month_9) < 31){
            Month_9[,(ncol(Month_9)+1):31] <- mis_val
            colnames(Month_9) <- colnames(Month_1)
            Month_9
        } else{Month_9}
    ) %>% 
    rbind.data.frame(
        if (ncol(Month_10) < 31){
            Month_10[,(ncol(Month_10)+1):31]<- mis_val
            colnames(Month_10) <- colnames(Month_1)
            Month_10
        } else{Month_10}
    ) %>% 
    rbind.data.frame(
        if (ncol(Month_11) < 31) {
            Month_11[,(ncol(Month_11)+1):31] <- mis_val
            colnames(Month_11) <- colnames(Month_1)
            Month_11
        } else{Month_11}
    ) %>% 
    rbind.data.frame(
        if (ncol(Month_12) < 31){
            Month_12[,(ncol(Month_12)+1):31]<- mis_val
            colnames(Month_12) <- colnames(Month_1)
            data.frame(Month_12)
        } else{Month_12}
    ) -> Data_T  

# Removes rownames of the above Bind or Data_T
rownames(Data_T) <- NULL
#identifying available months within the data 
Data_T %>% data.frame(rep(colnames(Data),times=nrow(unique(Data[,1])))) -> Data_T2
#Creates a dataframe (Final Result)
data.frame(
    Year=rep(c("Month","Day",start_year:end_year),times = 12),
    Month=rep(unique((Data_T2[Data_T2[,length(Data_T2)] == "Month", ])[,1]),
              each =length(Data[1,])),
    Data_T
) %>% arrange(Year) %>% filter(Year >= start_year & Year <= end_year) -> Data_TT
replace(Data_TT[,1:ncol(Data_TT)], Data_TT[,1:ncol(Data_TT)] == -9988,-99) %>% 
    write.csv(file = "Wrangled Data.csv")

##############################################################################################
