library(dplyr)
library(readxl)
library(tidyverse)
library(wesanderson)
library(RColorBrewer)
library(gghighlight)
library(ggbreak)
library(treemapify) 
library(scales)
library(knitr)
library(wesanderson)
library(ggpubr)
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

####################

english_speaking_countries <- c("Australia", "Canada", "United Kingdom", "United States", "India", "Singapore")


GYT_withoutNaN <- GYT_withoutNaN %>% 
  mutate(category_gr = case_when(
    category %in% c('Education', 'News & Politics', 'Science & Technology', 'Nonprofits & Activism') ~ 'Education',
    category == 'nan' ~ 'Unknown',
    category == 'Gaming' ~ 'Gaming',
    category == 'Music' ~ 'Music',
    category == 'Entertainment' ~ 'Entertainment',
    category %in% c('Comedy', 'Shows', 'Movies', 'Film & Animation', 'Sports', 'Trailers') ~ 'Shows',
    category %in% c('People & Blogs', 'Travel & Events', 'Howto & Style', 'Pets & Animals', 'Autos & Vehicles') ~ 'Lifestyle'),
    region = case_when(
      Country %in% c("Afghanistan", "Bangladesh", "China", "India", "Indonesia", "Iraq","United Arab Emirates", "Japan", "Jordan", "Kuwait", "Malaysia", "Pakistan", "Philippines", "Saudi Arabia", "Singapore", "South Korea", "Thailand", "Vietnam") ~ "Asia",
      Country %in% c("Andorra", "Finland", "France", "Germany", "Italy", "Latvia", "Netherlands", "Russia", "Spain", "Sweden", "Switzerland", "Ukraine", "United Kingdom") ~ "Europe",
      Country %in% c("Canada", "Cuba", "Mexico", "United States") ~ "North America",
      Country %in% c("Argentina", "Brazil", "Chile", "Colombia", "Ecuador", "Peru", "Venezuela") ~ "South America",
      Country %in% c("Australia", "Samoa") ~ "Oceania",
      Country %in% c("Egypt", "El Salvador", "Morocco") ~ "Africa",
      TRUE ~ "Other"),
    english_offical_lang = ifelse(Country %in% english_speaking_countries, TRUE, FALSE),
    mean_yearly_earnings = (highest_yearly_earnings+lowest_yearly_earnings)/2,
    Youtuber = case_when(
      Youtuber == '\xfd\xfd\xfd Kids Diana Show' ~ 'Kids Diana Show',
      TRUE ~ Youtuber),
    Title = case_when(
      Title == '\xfd\xfd\xfd Kids Diana Show' ~ 'Kids Diana Show',
      TRUE ~ Title)
  )

table(GYT_withoutNaN$category_gr)
kable(table(GYT_withoutNaN$Country))

top10_ytbers <- GYT_withoutNaN[1:10,]
top50_ytbers <- GYT_withoutNaN[1:50,]

top10_ytbers_byViews <- GYT_withoutNaN %>%                                      
  arrange(desc(video.views)) %>% 
  slice(1:10)

## intro - general info
GYT_agg <- GYT_withoutNaN %>% 
  filter(!category_gr=='Unknown') %>% 
  dplyr::count(region, category_gr)

ggplot(GYT_agg, aes(x = region, y = category_gr)) + 
  geom_tile(aes(fill = n), color = 'black', show.legend = F) +
  theme_minimal() + 
  geom_text(aes(label = n), size = 5, fontface = 'bold', color = 'white') +
  labs(title = 'Youtubers in Regions', x='', y='') +
  scale_fill_gradient(low = wes_palette("Moonrise2")[4], high = wes_palette("Moonrise2")[2])

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



top10_ytbers <- top10_ytbers %>% 
  mutate(region = as.factor(region),
         name = rownames(.))

ggdotchart(top10_ytbers, x = "Youtuber", y = "subscribers", color = "region",                                
           palette = wes_palette(n=3, "BottleRocket2"), 
           sorting = "descending",                       
           rotate = TRUE,                                
           dot.size = 2,                                
           y.text.col = TRUE) + 
  labs(y="Subscriber count",
       color="Region") +
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6)) +
  theme_cleveland()  




# Does high number of subsribers mean that rest of metrics are also high?
library(GGally)

ggparcoord(top10_ytbers, columns=c(3,4,7,32), 
           groupColumn = "Youtuber",
           mapping = aes(size = 1)) +
  scale_size_identity() +
  theme_minimal() +
  theme(panel.grid.major.x = element_line(colour="grey70"))



##Radar of features for top 10 youtubers. Different approach as graph above

devtools::install_github("ricardo-bion/ggradar")
library(ggradar)

ytTop10_radar <- subset(top10_ytbers_byViews, select=c("Youtuber","lowest_yearly_earnings","highest_yearly_earnings","mean_yearly_earnings","uploads","video.views","subscribers"))
ytTop10_radar <- ytTop10_radar %>% mutate_at(vars(-Youtuber), scales::rescale)

ggradar(ytTop10_radar) 




#wykres ilość subów do ilości wyświetleń. Pokazanie, nie zawsze najwięcej obserwujących = najwięcej wyświetleń. 

ggplot(data = GYT_withoutNaN, aes(x =  subscribers, y = video.views)) + 
  labs(x = "Subscriber count",
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

# Rozkład w podziale na kategorie
# żaden z regionów się nie wyróżnia
# dodac wskaźnik korelacji? 
ggplot(GYT_withoutNaN %>% filter(!category_gr=='Unknown'), 
       aes(x=video.views, y=mean_yearly_earnings)) + 
  geom_point(aes(color = region), size=1, alpha=0.6) +
  geom_smooth(color=wes_palette("Moonrise2")[1], cex=0.6, alpha=0.3) +
  scale_x_continuous(name = "Video Views [billions]",
                     breaks = seq(0, 1e+11, by = 1e+10), # use function seq() - operate on original scale values
                     labels = paste0(format(seq(0, 100, by = 10)),"B"),
                     limits = c(0,6e+10)) +
  scale_y_continuous(name = "Yearly Earnings [millions]",
                     breaks = seq(0, 3e+7, by = 5e+6), # use function seq() - operate on original scale values
                     labels = paste0(format(seq(0, 30, by = 5)),"M"),
                     limits = c(0,3e+7)) +
  facet_wrap(~ category_gr, ncol=3) 


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


#waffle chart, the same as above but with regions

# 6 classes, 234 obs.

# Preparing grid with 100 rectangles:
df <- expand.grid(y = 1:10, x = 1:10)
table(GYT_withoutNaN$region)
# Now we need to rescale 234 obs into 100 (carefully)
categ_table <- round(table(GYT_withoutNaN$region) * ((10*10)/(length(GYT_withoutNaN$region))))
categ_table['Africa'] <- 1 #to show Africa
categ_table
sum(categ_table)

# Now we know, how many rectangles each region occupies:
df$Regions <- factor(rep(names(categ_table), categ_table))  
ggplot(df, aes(x = x, y = y, fill = Regions)) + 
  geom_tile(color = "black", size = 0.5) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), trans = 'reverse') +
  scale_fill_brewer(palette = "Set3") +
  labs(title="YT channels regions distribution") + 
  theme_void() +
  theme(plot.margin = unit(c(0.2, 0.1, 0.2, .1), units = 'in'))







#wykres ilość wyświetleń do ilości zuploadowanych filmów. Pokazanie, że ilość nie zawsze znaczy jakość



# Boxplot, rozkład zarobków na kategorie i english - non-english

ggplot(data = GYT_withoutNaN %>% filter(!category_gr=='Unknown'), 
       aes(y = mean_yearly_earnings, x = category_gr, color = english_offical_lang)) +
  geom_boxplot(width = .75) +
  stat_summary(geom = 'point', shape = 15, fun = mean, size = 2, 
               position = position_dodge(.75)) +
  geom_text(data = GYT_withoutNaN %>% filter(!category_gr=='Unknown' & 
                                               mean_yearly_earnings > 45e+06), 
            aes(label = Youtuber),
            hjust = 0.5, vjust = 0.5, size = 2.8) + 
  labs(x='Category') +
  scale_y_continuous(name = "Yearly Earnings [milions of USD]",
                     breaks = seq(0, 1e+08, by = 1e+07), # use function seq() - operate on original scale values
                     labels = paste0(format(seq(0, 100, by = 10)),"M")
                     ) +
  scale_color_manual(name = "Official Language",
                     values = wes_palette(n=2, name="Moonrise2"),
                     labels = c("TRUE" = "English", "FALSE" = "Non-English")) +
  theme(legend.position = "bottom") +
  coord_flip() 


# Sprawdzić relacje miedzy wyswietlenia vs zarobki a zarobki vs region (unemployement.rate)
# czy zarobjki bardziej zaleza od wysiwtlen, czy potencjalnego bogactwa ogladajacych
# dodatkowo wysiwetlenia a populacja 


# czas zalozenia kanalu, a wyswietlenia/dochody
