#实例1 ggplot2 画图
#install.packages("ggplot2")
library("ggplot2")
theta <- seq(0,24*pi, len=2000)
radius <- exp(cos(theta)) - 2*cos(4*theta) + sin(theta/12)^5
dd <- data.frame(x=radius*sin(theta), y=radius*cos(theta))
p <- ggplot(dd, aes(x, y))+geom_path()+xlab("")+ylab("") + theme_classic()
p


#示例2 词频分析及词云
# install.packages("wordcloud2")
library(wordcloud2)
wordcloud2(demoFreqC, size = 0.7, shape = 'diamond')


#示例3 实时查看我国各地空气质量
install.packages("rvest")
install.packages("leafletCN")
install.packages("rgeos")
###
Sys.setlocale("LC_CTYPE", "eng")
library(rvest)
library(leafletCN)
library(rgeos)
doc = read_html("http://www.pm25s.com/cn/rank/")
cities = doc %>% html_nodes(".cityrank a") %>%
  html_text()
cities = iconv(cities, "UTF-8", "UTF-8")
AQI = doc %>% html_nodes("span[class^='lv']") %>%
  html_text() %>% .[c(F,F,T)] %>% as.numeric
dat = data.frame(city = cities, AQI = AQI)
geojsonMap(dat, "city",
           popup =  paste0(dat$city,":",dat$AQI),
           palette = "Reds", legendTitle = "AQI")