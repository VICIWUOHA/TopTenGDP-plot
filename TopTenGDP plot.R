#simple bar plot
install.packages('readxl')
library('readxl')
library('ggplot2')
library('scales')
# get data and assign variables to data-frame columns
gdp<-read_excel("Top 10 GDP 2020.xlsx", sheet='Top Ten GDP-2020',col_names = TRUE)
View(gdp) #preview data
country<- gdp$Country
gdp_value<-gdp$GDP

# generate plot, round gdp to 1d.p and separate using the comma function
g<-ggplot(gdp,aes(x=reorder(country, gdp_value),y=gdp_value,
               fill=-gdp_value,label=comma(round(gdp_value),1)))+
  geom_col() +
  # avoid overlap of text and bar to make text visible, set data label formats
  geom_text(nudge_y = 1,size=4,hjust=-0.05,fontface='bold')+
  # set title and subtitle
  ggtitle(label = 'Top 10 Countries by GDP',subtitle = '-IMF,2020')+
  # adjust position of bar and figure limits to avoid plot going out of the canvas
  scale_y_continuous(expand=c(0,0),limits = c(0,24500))+
  labs(y='GDP (Billions USD$)', x='',
       caption='Source: International Monetary Fund')+#set label and caption
  # adjust sizes and alignment of axis, titles & labels
  theme(plot.title = element_text(size = 20,face='bold'))+
  theme(plot.caption = element_text(face = 'italic'))+
  theme(axis.text.x = element_text(hjust =0.05))+
  theme(axis.text.y=element_text(size = 11, hjust = 1))+
  coord_flip()+#flip x & Y co-ordinates
  theme(legend.position = "none",panel.background = element_blank())+
  # remove x-axis ticks and tick labels,set size for x-axis title
  theme(axis.text.x = element_blank())+
  theme(axis.ticks.x=element_blank(),
        axis.title.x = element_text(vjust =5, size =9.5))+
#add all annotations, watermark , body text and curve
  annotate(
    "text", label = "@viciwuoha",
    y = 20000, x = 8, size = 4.5, colour = "darkgrey", fontface='bold')+
  annotate( "text", label = "-Asia: 40%\n Europe: 40%\n North-America: 20%",
            y = 18000, x = 3, size = 3.5, colour = "darkgrey", fontface='bold')+
 #curvature determines curve direction, other parameters determine arrow position
   annotate(
    geom = "curve", x = 2, y = 12000, xend = 3, yend = 14000, 
    curvature = -.2, arrow = arrow(length = unit(2, "mm")))
g   

