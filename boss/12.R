install.packages("plotly")
install.packages("dplyr")
install.packages("tidyverse")
library(tidyverse)
library(dplyr)
library(plotly)
setwd('C:\\Users\\ad\\boss\\')
data <- read.csv("output2.csv",encoding = "UTF-8" , header = T, stringsAsFactors = F)
#I.lam sach du lieu.
data1 <- data
data1[data1 == "" ] <- NA
data1[is.na(data1)]= 0
summary(data1)
data1$price <- as.numeric(as.factor(data1$price))
data1$positive_rating  <- as.numeric(as.factor(data1$positive_rating))
data1$new_price <- as.numeric(as.factor(data1$new_price))
data1$delivered_on_time <- as.numeric(as.factor(data1$delivered_on_time))
data1$positive_rating <- as.numeric(as.factor(data1$positive_rating))
data1$discount <- as.numeric(as.factor(data1$discount))
data1$response_rate <- as.numeric(as.factor(data1$response_rate))
#. Bar Plot.
ggplot(data1, aes(x=danh_gia, y=danh_gia_tich_cuc)) +
  geom_bar(stat = "identity", fill = "red", color = "blue") +
  labs(x = "danh_gia", y = "danh_gia_tich_cuc", title = "BIEU DO BAR PLOT SO SANH GIUA DANH GIA VA DANH GIA TICH CUC ")
View(data1)
#. Bar Plot.
ggplot(data1, aes(x=price, y=new_price)) +
  geom_bar(stat = "identity", fill = "red", color = "blue") +
  labs(x = "price", y = "new_price", title = "BIEU DO BAR PLOT SO SANH GIUA GIA GOC VA GIA SAU KHI GIAM ")
#. Bar Plot.
ggplot(data1, aes(x=price, y=new_price)) +
  geom_bar(stat = "identity", fill = "red", color = "blue") +
  labs(x = "price", y = "new_price", title = "BIEU DO BAR PLOT SO SANH GIUA GIA GOC VM ")
#. Bar Plot.
ggplot(data1, aes(x=price, y=new_price)) +
  geom_bar(stat = "identity", fill = "red", color = "blue") +
  labs(x = "price", y = "new_price", title = "BIEU DO BAR PLOT SO SANH IA GOC VM ")
#plot
ggplot(data1,aes(x = name,y = price))+
  geom_bar(stat="identity", width=0.5, color="blue", fill = "green")+
  labs(title = "Bi???u d??? ph???n b??? gi??? c???a c???c s???n ph???m")+
  xlab("Name")+ylab("Price")+
  theme(text = element_text(size = 4),
        axis.text.x = element_text(angle=50, hjust=1))
data2 = data1[order(-data$price),]
data2 = data1[order(-data1$price),]
data2[1:10,]
#plot
ggplot(data2[1:10,],aes(x = name,y = price))+
  geom_bar(stat="identity", width=0.5, color="green", fill = "blue")+
  labs(title = "Bi???u d??? top 10 s???n ph???m c??? gi??? cao nh???t")+
  xlab("Name")+ylab("price")+
  theme(text = element_text(size = 6),
        axis.text.x = element_text(angle=50, hjust=1))
# Box plots.
ggplot(data1, aes(x = discount, y = brand)) +
  geom_boxplot() +
  labs(title = "BIEU DO BOX PLOTS THE HIEN SU GIAM GIA CUA CAC H???ng SAN PHAN AO QUAN NAM", x = "discount", y = "brand")
ggplot(data1, aes(x=price)) +
  geom_histogram(fill = "pink", color = "yellow", bins = 6) +
  labs(title = "BIEU DO HISTOGRAM THE HIEN PHI VAN CHUYEN CUA SAN PHAM GIAY NAM", x = "price")
'-------------'

tab1 <- table(data1$brand)
tab1 <- data.frame(tab1)
colnames(tab1) <- c('brand', 'Freq')
p1 <- tab1 %>%
  mutate(Trade = fct_reorder(brand, Freq)) %>%
  ggplot(aes(x = Trade, 
             y = Freq,
             fill = brand)) +
  geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
  labs(title = "S??? lu???ng áo dang bán ??? m???i hãng",
       x = "Brand",
       y = "Count") + 
  coord_flip() +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5), 
        panel.background = element_rect(fill = 'white'), 
        panel.border = element_rect(colour = '#990000', fill = NA, size = 1.5))
ggplotly(p1, dynamicTicks = TRUE) %>%
  rangeslider() %>%
  layout(hovermode = "x")
```
# Danh m???c s???n ph???m
mode(data1$tag)
describe(data$tag)
qplot(data$tag, xlab = 'Danh m???c', ylab = 'S??? lu???ng s???n ph???m', main = 'Bi???u d??? danh m???c s???n ph???m')
