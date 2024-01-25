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
library(ggrepel)
library(GGally)
#devtools::install_github("ricardo-bion/ggradar")
library(ggradar)


#Status as of mid-2023
GYT <- read.csv("Global_YouTube_Statistics.csv",header=TRUE, sep=',', dec='.',encoding = 'Latin-1')

#removing uncoded signs from ytbers names
GYT$Youtuber <- iconv(GYT$Youtuber, from = "UTF-8", to = "ASCII", sub = "")
GYT$Title <- iconv(GYT$Title, from = "UTF-8", to = "ASCII", sub = "")
GYT$Youtuber <- gsub("/","",GYT$Youtuber)
GYT$Title <- gsub("/","",GYT$Title)
str(GYT)


GYT_withoutNaN <- subset(GYT, video.views > 50000000)


english_speaking_countries <- c("Australia", "Canada", "United Kingdom", "United States", "India", "Singapore")

## Data preparation
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

##Created_year_range variable
#Ranges: (-inf, 2014>; <2015,+inf) Why? Since in 2015 YT launched YouTube premium
GYT_withoutNaN$CreatedYear_Interval <- ifelse(GYT_withoutNaN$created_year <= 2014, "2005 - 2014", "2015 - 2023")


## Data preparation
top10_ytbers <- GYT_withoutNaN[1:10,]
top50_ytbers <- GYT_withoutNaN[1:50,]

top10_ytbers_byViews <- GYT_withoutNaN %>%                                      
  arrange(desc(video.views)) %>% 
  slice(1:10)




## Plot: Distribution of channels by continents in the dataset
df <- expand.grid(y = 1:10, x = 1:10)

# Now we need to rescale all obs into 100
categ_table <- round(table(GYT_withoutNaN$region) * ((10*10)/(length(GYT_withoutNaN$region))))
categ_table['Africa'] <- 1 #to show Africa

df$Regions <- factor(rep(names(categ_table), categ_table))  
ggplot(df, aes(x = x, y = y, fill = Regions, color = NA)) + 
  geom_tile(color = "black", size = 0.5) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), trans = 'reverse') +
  scale_fill_brewer(palette = "Set3") +
  labs(title="YT channels regions distribution") + 
  theme_void() +
  theme(plot.margin = unit(c(0.2, 0.1, 0.2, .1), units = 'in'), plot.title=element_text(size=22),legend.text = element_text(size=15),legend.title = element_text(size=15))


## Outcome: North America is more than a quater of all channels in top 1000. Europe is also huge part.


## Plot: Channels by category and regions
GYT_agg <- GYT_withoutNaN %>% 
  filter(!category_gr=='Unknown') %>% 
  dplyr::count(region, category_gr)

ggplot(GYT_agg, aes(x = region, y = category_gr)) + 
  geom_tile(aes(fill = n), color = 'black', show.legend = F) +
  theme_minimal() + 
  geom_text(aes(label = n), size = 5, fontface = 'bold', color = 'white') +
  labs(title = 'TOP 1000 Youtubers in Regions', x='', y='') +
  scale_fill_gradient(low = wes_palette("Moonrise2")[4], high = wes_palette("Moonrise2")[2])

## Outcome: The most popular category is general entertainment although in both Americas the most popular
##          category in top 1000 channels is Music. In Europe and Asia the most popular is entartainment. 
##          Oceania and Africa have a small group of representatives in TOP 1000. It's worth to mention that
##          Education category is popular in Asia and North America what is a positive thing.


## Plot: Distribution of channels by countries in the dataset

channels_by_country <- GYT_withoutNaN %>% subset(Country != "nan")%>% count(Country)
channels_by_country$Country <- ifelse(channels_by_country$Country=="nan","Other",channels_by_country$Country)

ggplot(channels_by_country%>%arrange(desc(n)) %>% 
         slice(1:20),aes(area=n,fill=Country,label=Country))+ 
  geom_treemap(layout="squarified", show.legend = FALSE)+ 
  geom_treemap_text(place = "centre",size = 12)+ 
  labs(title="How many top accounts by country") +
  scale_fill_manual(values = c(
    "#1f78b4", "#33a02c", "#e31a1c", "#ff7f00", "#6a3d9a",
    "#a6cee3", "#b2df8a", "#fb9a99", "#fdbf6f", "#cab2d6",
    "#ffff99", "#b15928", "#8dd3c7", "#fb8072", "#80b1d3",
    "#bebada", "#ffed6f", "#bc80bd", "#ccebc5", "#ffed6f"
  )) + 
  theme(plot.margin = unit(c(0.2, 0.1, 0.2, .1), units = 'in'), plot.title=element_text(size=15))

## Outcome: USA is more than a quater of all set. The second biggest is India. In South America
## Brazil is on the top. From Europe UK has the most channels in top 1000 of Youtube. 



## Plot: number of views vs. number of subs

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

## Outcome: not always most watchers = most views

## Plot: Distributions by category

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
  facet_wrap(~ category_gr, ncol=3) +
  theme_minimal()

## Outcome: None of the regions stand out

## Plot: Distribution of earnings by English/non-English speaking categories and countries
ggplot(data = GYT_withoutNaN %>% filter(!category_gr=='Unknown'), 
       aes(y = mean_yearly_earnings, x = category_gr, color = english_offical_lang)) +
  geom_boxplot(width = .75) +
  stat_summary(geom = 'point', shape = 15, fun = mean, size = 2, 
               position = position_dodge(.75)) +
  geom_text(data = GYT_withoutNaN %>% filter(!category_gr=='Unknown' & 
                                               mean_yearly_earnings > 45e+06), 
            aes(label = Youtuber,fontface = "bold"),
            hjust = 0.7, vjust = 0.5, size = 3.5) + 
  labs(x='Category') +
  scale_y_continuous(name = "Yearly Earnings [milions of USD]",
                     breaks = seq(0, 1e+08, by = 1e+07), # use function seq() - operate on original scale values
                     labels = paste0(format(seq(0, 100, by = 10)),"M")
  ) +
  scale_color_manual(name = "Official Language",
                     values = wes_palette(n=2, name="Moonrise2"),
                     labels = c("TRUE" = "English", "FALSE" = "Non-English")) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  coord_flip() 

## Outcome: In almost all categories English speaking countries earns more than non-English. The only difference makes
## Music category. As we can see music does not have a language.

## Plot: Mean earnings by category of channel - comparison of old and young youtubers

GYT_MeanEarnings_YearCategory <- GYT_withoutNaN %>% group_by(CreatedYear_Interval,category_gr) %>%
  summarise_at(vars(mean_yearly_earnings), list(AvgYearlyIncome = mean))
GYT_MeanEarnings_YearCategory <- na.omit(GYT_MeanEarnings_YearCategory)

pivoted_df<- pivot_wider(GYT_MeanEarnings_YearCategory,names_from=CreatedYear_Interval, values_from = AvgYearlyIncome )
pivoted_df <- pivoted_df %>% filter(!category_gr =='Unknown')
left_label <- paste(pivoted_df$category_gr, paste0(format(round(pivoted_df$`2005 - 2014`), big.mark=",",scientific=FALSE)," $"),sep=", ")
right_label <- paste(pivoted_df$category_gr, paste0(format(round(pivoted_df$`2015 - 2023`), big.mark=",", scientific=FALSE)," $"),sep=", ")
pivoted_df$class <- ifelse((pivoted_df$`2015 - 2023` - pivoted_df$`2005 - 2014`) < 0, "red", "green")


ggplot(pivoted_df) + 
  geom_segment(aes(x = 1, xend = 2, y = `2005 - 2014`, yend = `2015 - 2023`, col=class), 
               size = .9, show.legend = F) + 
  geom_vline(xintercept = 1, linetype = "dashed", size = .5) + 
  geom_vline(xintercept = 2, linetype = "dashed", size = .5) +
  scale_color_manual(labels = c("Up", "Down"), 
                     values = c("green" = "#00ba38", "red" = "#f8766d")) +
  xlim(.5, 2.5) + 
  ylim(0, (1.1 * max(pivoted_df$`2005 - 2014`, pivoted_df$`2015 - 2023`))) + 
  geom_text_repel(label = left_label, y=pivoted_df$`2005 - 2014`, x=rep(1, NROW(pivoted_df)), hjust=1.1, size=4) + 
  geom_text_repel(label = right_label, y=pivoted_df$`2015 - 2023`, x=rep(2, NROW(pivoted_df)), hjust=-0.1, size=4) + 
  geom_text(label =" created in 2005 - 2014", x=1, y=1.1 * max(pivoted_df$`2005 - 2014`, pivoted_df$`2015 - 2023`), hjust=1.2, size=5) + 
  geom_text(label ="created in 2015 - 2023", x=2, y=1.1 * max(pivoted_df$`2005 - 2014`, pivoted_df$`2015 - 2023`), hjust=-0.1, size=5) + 
  labs(title="Mean 2023 earnings by category of channel - comparision of old and young authors", hjust=0.5) +
  theme_void()

## Outcome: Old guard makes more money in only 2 categories - Education and Music. 


## Plot: Top 10 Youtubers
ggplot(data = top10_ytbers, aes(x = reorder(Youtuber, +subscribers), y=subscribers, fill=category_gr)) + 
  geom_bar(stat = 'identity', color = NA,width = 0.7, position = position_dodge(width = 0.8)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1)) +
  labs(title = 'Top 10 biggest Youtube channels in 2023',
       x="Youtube channel",
       y="Subscriber count",
       fill="Category") +
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6))+
  scale_fill_manual(values = palette("Set1")) +
  coord_flip() +
  geom_text(
    aes(label = Country),
    position = position_stack(vjust = 0.5),
    size = 4,
    fontface = "bold",
    color = "black" 
  )


## Plot: A little bit different approach to the subject above
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


## Outcome: T-Series from India has much more subs than the others. There is only one gaming channel
## on the chart and it is PewDiePie. 4 of 10 channels are for children.


## Plot: Does high number of subsribers mean that rest of metrics are also high?
ggparcoord(top10_ytbers, columns=c(3,4,7,32), 
           groupColumn = "Youtuber",
           mapping = aes(size = 1.3)) +
  scale_size_identity() +
  theme_minimal() +
  theme(panel.grid.major.x = element_line(colour="grey70"),
        axis.text = element_text(size = 10),  # Adjust axis label size
        axis.title = element_text(size = 10) ) +
  labs(x='',y='')
## Outcome: T-series channel with them most subs is 3rd in uploads for example. It's interesting that
## 6 channels uploaded similar number of videos. MrBeast for example has the least views but he is
## second in subs.


## Plot: simmilar approach like above but with radar plot
ytTop10_radar <- subset(top10_ytbers_byViews, select=c("Youtuber","lowest_yearly_earnings","highest_yearly_earnings","mean_yearly_earnings","uploads","video.views","subscribers"))
ytTop10_radar <- ytTop10_radar %>% mutate_at(vars(-Youtuber), scales::rescale)

ggradar(ytTop10_radar) 


