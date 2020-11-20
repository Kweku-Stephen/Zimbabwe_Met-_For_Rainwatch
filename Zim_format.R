#########################################################################################################
#set working directory - Insert the part to your file into the bracket below
setwd("")
#NB. This algorithm assumes the months Jan,Mar,May,July,Aug,Oct,Dec of the 
#imported data are all of 31 days, the rest are 30 days except february 
#which it assumes is either 28 or 29 Days 
library(tidyverse)
library(readxl)

Data <- read_excel(
    "F:/Book1.xlsx"
)

#Start and end years
start_year = 1980
end_year = 2018
mis_val = -9988
#Slicing the data into 12 transposed chunks,thus selecting the individual
#months in the data after you have transposed the entire data
Month_1 <- data.frame(t(Data[Data[,1] == 1, ]))
Month_2 <- data.frame(t(Data[Data[,1] == 2, ])) 
Month_3 <- data.frame(t(Data[Data[,1] == 3, ])) 
Month_4 <- data.frame(t(Data[Data[,1] == 4, ]))
Month_5 <- data.frame(t(Data[Data[,1] == 5, ]))
Month_6 <- data.frame(t(Data[Data[,1] == 6, ])) 
Month_7 <- data.frame(t(Data[Data[,1] == 7, ])) 
Month_8 <- data.frame(t(Data[Data[,1] == 8, ]))
Month_9 <- data.frame(t(Data[Data[,1] == 9, ]))
Month_10 <- data.frame(t(Data[Data[,1] == 10, ])) 
Month_11 <- data.frame(t(Data[Data[,1] == 11, ])) 
Month_12 <- data.frame(t(Data[Data[,1] == 12, ]))


# Bind the individual Months above rowise
Month_1 %>% 
    rbind.data.frame(
        if (ncol(Month_2) == 28) {
            data.frame(Month_2,X29=mis_val,X30=mis_val,X31=mis_val)
        } else {
            data.frame(Month_2,X30=mis_val,X31=mis_val)
        }
    ) %>% 
    rbind.data.frame(
        Month_3
    ) %>% 
    rbind.data.frame(
        if (ncol(Month_4) == 30) {
            data.frame(Month_4, X31=mis_val)
        } 
    ) %>% 
    rbind.data.frame(
        Month_5
    ) %>% 
    rbind.data.frame(
        if (ncol(Month_6) == 30) {
            data.frame(Month_6, X31=mis_val)
        } 
    ) %>% 
    rbind.data.frame(
        Month_7
    ) %>% 
    rbind(
        Month_8
    ) %>% 
    rbind.data.frame(
        if (ncol(Month_9) == 30) {
            data.frame(Month_9, X31=mis_val)
        }
    ) %>% 
    rbind.data.frame(
        Month_10
    ) %>% 
    rbind.data.frame(
        if (ncol(Month_11) == 30) {
            data.frame(Month_11, X31=mis_val)
        }
    ) %>% 
    rbind.data.frame(
        Month_12
    ) -> Data_T  

# Removes rownames of the above Bind or Data_T
rownames(Data_T) <- NULL
#Creates a dataframe (Final Result)
data.frame(
    Year=rep(c("Month","Day",start_year:end_year),times = 12),
    Month=rep(1:12, each = length(Data[1,])),
    Data_T
) %>% arrange(Year) %>% filter(Year >= start_year & Year <= end_year) %>% 
    write.csv(file="Wrangled_Data.csv")


####################################################################################
