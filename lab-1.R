# chernoff faces
library(aplpack)

##(1)
library(readr)
eco_dev_data <- read.csv("eco_dev_data.csv")
countries <- eco_dev_data[,1]
eco_dev_data <- eco_dev_data[,-1]
faces(eco_dev_data[1:20,],face.type=1)
# clusters, 101 countries left for facing

##(2)
wine <- read.csv("Wine_data.csv")
wine <- wine[,-15:-33]
type <- factor(wine[,1])
wine <- wine[,-1]
faces(wine[1:100,],face.type=1)
# type depends on hair size

##(3)
bank <- read.csv("PS_bank_fin_ratio.csv")
bank <- bank[-1,]
x <- bank[,1]
bank <- bank[,-1]
faces(bank[1:10,],face.type = 1)
