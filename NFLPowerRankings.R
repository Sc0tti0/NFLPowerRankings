library(rvest)
library(ggplot2)
library(ggimage)

#Use week 1 URL to initialize the list of teams and rankings
url <- paste0("https://www.nfl.com/news/nfl-power-rankings-week-1-buccaneers-chiefs-begin-2021-season-on-top")
webpage<- read_html(url)
teams<-html_nodes(webpage,"#main-content > article > div:nth-child(3) > div > div:nth-child(2) > section")

#Create list of URLs by week
url.list <- rev(paste0("https://www.nfl.com",html_attr(html_nodes(teams,"a"),"href")))
url.list[1] <- url


for (i in 1:length(url.list)){
  url <- url.list[i] #Set which URL to read
  webpage<- read_html(url) #Convert webpage to xml_document
  teams<-html_nodes(webpage,"div.nfl-o-ranked-item__title") #Get ordered list of teams
  images <- html_attr(html_nodes(webpage,"div.nfl-o-ranked-item__image > figure > picture > img"),"data-src") #Get ordered list of logos
  df <- data.frame(trimws(html_text(teams))) #Initialize df with list of teams in the first column
  df[,2]<-paste0("Week",i) #Set the second column of df to the week number
  df[,3] <- 1:length(html_text(teams)) #Rank the teams in the third column
  df[,4] <- images #Set the fourth column to the image source
  names(df)<- c("Team","Week","Rank","Image") #Rename the columns of df
  #If it is the first iteration, initialize final.df with df, else append df to final.df
  if(i==1){
    final.df<-df
  } else {
    final.df<-rbind(final.df,df)
  }
}

#Clean up final.df to help with ggplot
ls<-paste0("Week",1:length(url.list))
final.df$Team <- factor(final.df$Team, levels = final.df[1:32,"Team"])
final.df$Week <- factor(final.df$Week, levels = ls)
final.df$Image <- stringr::str_replace(final.df$Image,"/t_lazy","")

color_group <- c("#d50a0a","#e31837","#00338d","#203731","#241773","#002244","#002244","#fb4f14","#aa0000","#ffb612","#002244","#002244","#002244","#008e97","#9f8958","#002c5f","#4f2683","#97233f","#002244","#773141","#0b162a","#002244","#a5acaf","#0b2265","#004953","#a71930","#0085ca","#125740","#006778","#fb4f14","#005a8b","#03202f")

q <- ggplot(final.df,aes(y=Rank,x=Week,group=Team))+
  geom_line(show.legend = FALSE,aes(color=Team),size = 1)+
  scale_y_reverse(limits=c(32, 1),breaks=1:32,minor_breaks = seq(1, 32, 1))+
  theme(axis.title.x=element_blank())+geom_image(aes(image=Image),size=0.035)+
  theme_bw()+
  scale_colour_manual(values=color_group)+
  labs(title = "NFL.com Power Rankings by Week")+
  xlab("")+
  ylab("Ranking")+
  theme(plot.title = element_text(size=20,face = "bold"),
        axis.text=element_text(size=10,color = "black"),
        axis.title=element_text(size=14),
        panel.grid.major.x = element_blank())

ggsave(plot = q, width = 8, height = 8, dpi = 300, filename = "NflGraph.png")
