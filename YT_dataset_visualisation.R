library(dplyr)
library(readxl)
library(tidyverse)
library(wesanderson)
library(RColorBrewer)
library(gghighlight)
library(ggbreak)
library(treemapify) 

#Status as of mid-2023
GYT <- read.csv("Global_YouTube_Statistics.csv",header=TRUE, sep=',', dec='.',encoding = 'Latin-1')

#removing uncoded signs from ytbers names. I've tried many encodings
GYT$Youtuber <- iconv(GYT$Youtuber, from = "UTF-8", to = "ASCII", sub = "")
GYT$Title <- iconv(GYT$Title, from = "UTF-8", to = "ASCII", sub = "")
GYT$Youtuber <- gsub("/","",GYT$Youtuber)
GYT$Title <- gsub("/","",GYT$Title)
str(GYT)

table(GYT$category)

GYT_withoutNaN <- subset(GYT, video.views > 50000000)

top10_ytbers <- GYT_withoutNaN[1:10,]
top50_ytbers <- GYT_withoutNaN[1:50,]

top10_ytbers_byViews <- GYT_withoutNaN %>%                                      
  arrange(desc(video.views)) %>% 
  slice(1:10)


# Top 10 youtubers by subscribers
#Na prezce dodać adnotację o PewdiePie - kanał na wykresie jest z Japonii, bo chociaż sam autor Felix Kjellberg pochodzi z Szwecji, 
#to w 2022 roku przeprowadził się
#z żoną do Japonii i od teraz jego kanał w oficjalnych danych jest klasyfikowany jako pochodzący z Japonii

ggplot(data = top10_ytbers, aes(x = reorder(Youtuber, +subscribers), y=subscribers, fill=Country)) + 
  geom_bar(stat = 'identity', color = 'darkgreen') +
  scale_fill_brewer(palette="Set1") +
  theme_minimal() +
   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(title = 'Top 10 biggest Youtube channels in 2023',
      x="Youtube channel",
       y="Subscriber count",
        fill="Country") +
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6))
  

#Top 10 yt channels by channel type

ggplot(data = top10_ytbers, aes(x = reorder(Youtuber, +subscribers), y=subscribers, fill=category)) + 
  geom_bar(stat = 'identity', color = 'darkgreen') +
  scale_fill_brewer(palette="Set1") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(title = 'Top 10 biggest Youtube channels in 2023',
       x="Youtube channel",
       y="Subscriber count",
       fill="Category") +
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6))

#wykres ilość subów do ilości wyświetleń. Pokazanie, nie zawsze najwięcej obserwujących = najwięcej wyświetleń. 

ggplot(data = GYT_withoutNaN, aes(x =  subscribers, y = video.views)) + labs(x = "Subscriber count",
               y = "Total views",
               title = "Views vs Subs",
               caption = "\nHighlighted points are top 10 channels by total views")  +
  geom_point()  + 
  geom_point(data=top10_ytbers_byViews, 
             aes(x=subscribers,y=video.views), 
             color='gold',
             size=3) +geom_smooth() +
 theme_minimal() + 
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6), trans="log") +
  scale_x_continuous(labels = unit_format(unit = "M", scale = 1e-6), trans="log")

#ilość przedstawicieli państw w top 1000 (prawie 1000) youtuba

channels_by_country <- GYT_withoutNaN %>% count(Country)
channels_by_country$Country <- ifelse(channels_by_country$Country=="nan","Other",channels_by_country$Country)

## Może zrobić taki wykres (albo podobny, ale dla tych danych)
## ale z podziałem na kontynenty. Ogolnie ograniczyłem do 15 państw, bo 50 źle wygląda.
## Można pokazać wszystkie, ale ograniczyć na Kontynenty i dodać podgrupy, żeby np pokazywać, że
## wiekszość Azji to Indie, a większość Europy to UK i Hiszpania

ggplot(channels_by_country%>%arrange(desc(n)) %>% 
         slice(1:15),aes(area=n,fill=Country,label=Country))+ 
  geom_treemap(layout="squarified")+ 
  geom_treemap_text(place = "centre",size = 12)+ 
  labs(title="How many top accounts by country")


#wykres ilość wyświetleń do ilości zuploadowanych filmów. Pokazanie, że ilość nie zawsze znaczy jakość