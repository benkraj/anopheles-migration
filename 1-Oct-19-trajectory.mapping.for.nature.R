#### Plotting of flight paths for Tovi
library('tidyverse')
library('lubridate')
library('ggmap')
#devtools::install_github('oswaldosantos/ggsn')
library('ggsn')
library('ggthemes')
library('ggrepel')
library('geosphere')

#Setting up background with google maps/ggmap package
register_google('insertyourAPIkeyhere')
mali.simplified <- get_googlemap(center=c(-7.21471, lat=14.4), zoom=7, maptype='hybrid', style = 'feature:road|element:all|visibility:simplified&style=feature:administrative.locality|element:labels|visibility:off&style=feature:poi.park|element:labels|visibility:off') ## trying a different map remoing features, see https://stackoverflow.com/questions/35018703/adjust-font-size-in-map-plots-in-ggmap
#ggmap(mali.simplified)

##incorporating scalebars -- code from https://github.com/oswaldosantos/ggsn/issues/4
close_up <- ggmap(mali.simplified)
x_lim <- close_up$data[c(1, 4), 1] * c(1, .9998)
y_lim <- close_up$data[c(1, 4), 2] * c(1.003, 1)

#and making village location dataframe for plotting start points
df.balloon.spot <- data.frame(Vil=c("T", "S", "D", "M", "BD"), Long=c(-7.21471, -7.22793, -7.03689, -6.34382,  -4.2979), Lat=c(13.6586, 14.16762, 13.61576, 13.91442,  11.17715), name=c("Thierola", "Siguima", "Dallowere", "Markabougou", "Bobo-Dioulasso"))

countries <- data.frame(country=c("Guinea", "Mauritania", "Mali", "Burkina F"), Long=c(-9.6, -7.5, -7.3,-4.75), Lat=c(11.8, 16, 11.8, 11.8))

bamako <- data.frame(country=c("Bamako"), Long=c(-7.971547), Lat=c(12.63590))


##Building a dataset from the HYSPLIT trajectories:
df.ben <- read.csv("ToMapAno1.csv") %>% 
  filter(!is.na(YR)) %>% #removing initial NAs from dataset
  mutate(intercepDT=as.POSIXct(paste(as.Date(substr(intercepDT, 1, 7), format="%d%b%y"), paste0(substr(intercepDT,9,10), ":00"), sep=" "), format="%Y-%m-%d %H:%M"),
         startDT=as.POSIXct(paste(as.Date(substr(startDT, 1, 7), format="%d%b%y"), paste0(substr(startDT,9,10), ":00"), sep=" "), format="%Y-%m-%d %H:%M"), ## datetime reformatting
         month=as.factor(MO),
         int.hour = hour(intercepDT),
         season=case_when(month %in% c("10", "11", "12") ~ "Oct-Dec",
                          month %in% c("8", "9") ~ "Aug-Sept",
                          month %in% c("6", "7") ~ "June-July",
                          TRUE ~ "WHATMONTHISTHIS"), #define season
         season=factor(season, levels=c("June-July", "Aug-Sept", "Oct-Dec", "WHATMONTHISTHIS")),
         track.length=(intercepDT-startDT)/3600,
         catch.success = case_when(TotMosi>0 ~ "Y", TRUE ~ "N"),
         tid.group=case_when(tid == "An. squamosus" ~ "An. squamosus",
                             tid == "An. pharoensis" ~ "An. pharoensis",
                             tid == "An. rufipes" ~ "An. rufipes",
                             tid == "An. coustani" ~ "An. coustani",
                             tid == "An. coluzzii" ~ "An. coluzzii",
                             TRUE ~ "Anopheles spp. (other)")) %>%
  add_count(tid.group, int.hour, Tind) %>%
  filter(n >= 8) %>% #This is filtering to entries with 8 hour or greater tracks. I was having issues getting the >6am intercept tracks to be full (they were just a few points---I'm not sure)
  mutate(keep.track = case_when(TotMosi > 0 & int.hour == "6" ~ "Y",
                                TotMosi > 1 & int.hour == "5" ~ "Y",
                                TotMosi > 2 & int.hour == "4" ~ "Y",
                                TotMosi > 3 & int.hour == "3" ~ "Y",
                                TotMosi > 4 & int.hour == "2" ~ "Y",
                                TRUE ~ "N"),
         tid.group = factor(tid.group, levels=c("An. squamosus", "An. pharoensis", "An. coustani", "An. rufipes", "An. coluzzii", "Anopheles spp. (other)"))) %>%
  filter(track.length < 10) #Filter to just 9 hour paths

df.ben <- df.ben %>% mutate(tid=case_when(tid=="An. cf. coustani 1" ~ "An. namibiensis",
                                          TRUE ~ as.character(tid)))

#Define convex hulls (line around the points)

df.for.hulls <- df.ben %>% filter(catch.success=="Y") %>%
  mutate(id=paste(Vil, tid.group, DateDown, sep="-")) %>%
  select(id=id, x=Long, y=Lat, Vil=Vil, season=season, tid.group=tid.group, MO=MO)

find_hull <- function(df) df[chull(df$x, df$y), ]
hulls <- plyr::ddply(df.for.hulls, "id", find_hull) %>%
  mutate(tid.group = factor(tid.group, levels=c("An. squamosus", "An. pharoensis", "An. coustani", "An. rufipes", "An. coluzzii", "Anopheles spp. (other)")))


#Plotting the flight paths
p1 <- ggmap(mali.simplified) +
  geom_polygon(data = hulls, aes(x, y, group=id), fill="black", alpha=0.3) + 
  geom_path(data=df.ben %>% filter(keep.track=="Y"), 
            aes(Long,Lat,group=Tind, color=season)) + 
  # geom_point(data=df.ben %>% filter(keep.track=="Y" & track.length=="10"),
  #       aes(Long,Lat, fill=season), colour="black", pch=21, size=1) + # This is if you want all end points of tracks
  geom_point(data=df.balloon.spot %>% filter(!Vil %in% c("B", "BD")), aes(Long, Lat), colour="black", fill='white', pch=21, size=1.5) +
  geom_point(data=bamako, aes(Long, Lat), color="black", size=1) +
  geom_text_repel(data=df.ben %>% filter(tid.group=="Anopheles spp. (other)" & keep.track=="Y" & track.length=="9"), 
                  aes(Long,Lat,label=tid), color="#d3e3e3", size=3, min.segment.length=0, fontface = "italic") +
  geom_text_repel(data=df.balloon.spot %>% filter(!Vil %in% c("B", "BD")), aes(Long, Lat, label=Vil), color="white", size=4) +
  geom_text(data=df.balloon.spot %>% filter(Vil %in% c("B", "BD")), aes(Long, Lat-0.1, label=name), color="white", size=3) +
  geom_text(data=countries, aes(Long, Lat, label=country), color="#f2ff42", size=3.5) +
  geom_text_repel(data=bamako, aes(Long, Lat, label=country), color="#f2ff42", size=3.5, alpha=0.7) +
  scale_color_manual(values=c('#b2df8a','#1f78b4','#a6cee3')) +
  scale_fill_manual(values=c('#b2df8a','#1f78b4','#a6cee3')) +
  scale_x_continuous(limits=c(-10, -4.3), expand=c(0,0)) +
  scale_y_continuous(limits=c(11.2, 16.65), expand=c(0,0)) + 
  facet_wrap(~tid.group, ncol=3) +
  guides(fill=FALSE, size=FALSE, alpha=FALSE, color=guide_legend(title="Season")) + 
  theme(axis.text.x=element_text(size=10), axis.text.y=element_text(size=10), 
        legend.position = c(0.06, 0.92),legend.background = element_rect(fill = "white", colour = NA),
        legend.text=element_text(size=8), legend.title=element_blank(), legend.key.size = unit(0.2, 'cm'),
        strip.text = element_text(face = "italic")) +
  labs(x="Longitude", y="Latitude", fill="Season") + 
  scalebar(x.min = -10, x.max = -4.5, #I do a bit of shifting of the scale bar to get it in a better spot
           y.min = 11.4, y.max = 16.45, # more shifting
           dist = 50, model = 'WGS84',
           location = 'bottomright',
           st.bottom = F, #text on top
           height = .02, #scalebar size as proportion of box
           st.size=3, #text size
           transform=T, 
           dist_unit = 'km') +
  guides(color = guide_legend(override.aes = list(size=5)))

#ggsave("11-Sept-18-track.count.jpg", dpi=600, width=12, height=10)
#ggsave("11-Sept-18-mosquito.tracks.by.season.and.species.jpg", dpi=600, width=12, height=10)


###Windrose stuff
# WindRose.R
## windrose plotting code is modified from Andy Clifton here: https://stackoverflow.com/questions/17266780/wind-rose-with-ggplot-r 
require(tidyverse)
require(RColorBrewer)

plot.windrose <- function(data,
                          spd,
                          dir,
                          spdres = 2,
                          dirres = 30,
                          spdmin = 2,
                          spdmax = 20,
                          spdseq = NULL,
                          palette = "Paired",
                          countmax = NA,
                          debug = 0)
{
  
  
  # Look to see what data was passed in to the function
  if (is.numeric(spd) & is.numeric(dir)){
    # assume that we've been given vectors of the speed and direction vectors
    data <- data.frame(spd = spd,
                       dir = dir)
    spd = "spd"
    dir = "dir"
  } else if (exists("data")){
    # Assume that we've been given a data frame, and the name of the speed 
    # and direction columns. This is the format we want for later use.    
  }  
  
  # Tidy up input data ----
  n.in <- NROW(data)
  dnu <- (is.na(data[[spd]]) | is.na(data[[dir]]))
  data[[spd]][dnu] <- NA
  data[[dir]][dnu] <- NA
  
  # figure out the wind speed bins ----
  if (missing(spdseq)){
    spdseq <- seq(spdmin,spdmax,spdres)
  } else {
    if (debug >0){
      cat("Using custom speed bins \n")
    }
  }
  # get some information about the number of bins, etc.
  n.spd.seq <- length(spdseq)
  n.colors.in.range <- n.spd.seq - 1
  
  # create the color map
  spd.colors <- colorRampPalette(brewer.pal(min(max(3,
                                                    n.colors.in.range),
                                                min(9,
                                                    n.colors.in.range)),                                               
                                            palette))(n.colors.in.range)
  
  if (max(data[[spd]],na.rm = TRUE) > spdmax){    
    spd.breaks <- c(spdseq,
                    max(data[[spd]],na.rm = TRUE))
    spd.labels <- c(paste(c(spdseq[1:n.spd.seq-1]),
                          '-',
                          c(spdseq[2:n.spd.seq])),
                    paste(spdmax,
                          "-",
                          max(data[[spd]],na.rm = TRUE)))
    spd.colors <- c(spd.colors, "grey50")
  } else{
    spd.breaks <- spdseq
    spd.labels <- paste(c(spdseq[1:n.spd.seq-1]),
                        '-',
                        c(spdseq[2:n.spd.seq]))    
  }
  data$spd.binned <- cut(x = data[[spd]],
                         breaks = spd.breaks,
                         labels = spd.labels,
                         ordered_result = TRUE)
  # clean up the data
  data. <- na.omit(data)
  
  # figure out the wind direction bins
  dir.breaks <- c(-dirres/2,
                  seq(dirres/2, 360-dirres/2, by = dirres),
                  360+dirres/2)  
  dir.labels <- c(paste(360-dirres/2,"-",dirres/2),
                  paste(seq(dirres/2, 360-3*dirres/2, by = dirres),
                        "-",
                        seq(3*dirres/2, 360-dirres/2, by = dirres)),
                  paste(360-dirres/2,"-",dirres/2))
  # assign each wind direction to a bin
  dir.binned <- cut(data[[dir]],
                    breaks = dir.breaks,
                    ordered_result = TRUE)
  levels(dir.binned) <- dir.labels
  data$dir.binned <- dir.binned
  
  # Run debug if required ----
  if (debug>0){    
    cat(dir.breaks,"\n")
    cat(dir.labels,"\n")
    cat(levels(dir.binned),"\n")       
  }  
  
  # deal with change in ordering introduced somewhere around version 2.2
  if(packageVersion("ggplot2") > "2.2"){    
    cat("WindRose\n")
    data$spd.binned = with(data, factor(spd.binned, levels = rev(levels(spd.binned))))
    spd.colors = rev(spd.colors)
  }
  
  df.output <- data
  # create the plot ----
  p.windrose <- ggplot(data = data,
                       aes(x = dir.binned,
                           fill = spd.binned)) +
    geom_bar() + 
    scale_x_discrete(drop = FALSE,
                     labels = waiver()) +
    coord_polar(start = -((dirres/2)/360) * 2*pi) +
    scale_fill_manual(name = "Wind Speed (m/s)", 
                      values = spd.colors,
                      drop = FALSE) +
    theme(axis.title.x = element_blank())
  
  # adjust axes if required
  if (!is.na(countmax)){
    p.windrose <- p.windrose +
      ylim(c(-500,countmax))
  }
  
  ## print the plot
  #print(p.windrose)  
  
  # return the handle to the wind rose
  return(p.windrose)
  
}


#Defining groups of months for broad "seasons" based on early plots
may.july <- c("May", "June", "July")
june.july <- c("June", "July")
aug.sept <- c("August", "September")
oct.dec <- c("October", "November", "December")

#pulling in all the ERA5 data I have for thierola -- 2010 to 2015, and adding a few grouping variables

data.in <- bind_rows(readRDS("../thierola.10.11.rds"), readRDS("../thierola.2012.2015.RDS")) %>%
  mutate(Year=year(Date),
         date.alone=as.Date(Date),
         year.week=paste(Year, week(Date), sep='.'),
         month=factor(format(as.Date(Date, tz="Africa/Bamako"), "%B"), levels=c("January", "February", "March", "April",
                                                                                "May", "June", "July", "August", 
                                                                                "September", "October", "November", 
                                                                                "December")), 
         season=case_when(month %in% june.july ~ "June-July", 
                          month %in% aug.sept ~ "Aug-Sept",
                          month %in% oct.dec ~ "Oct-Dec"),
         season=factor(season, levels=c("June-July", "Aug-Sept", "Oct-Dec"))) %>%
  filter(Year %in% c("2013", "2014", "2015"))


#the actual plotting, I've changed a few things here for wind speed bins, and other various plotting aspects
p2 <- plot.windrose(data=data.in %>% filter(!is.na(season)), spd="ws180", dir="wd180", spdseq = c(0,2.5,5,7.5,10,12.5,15)) + 
  facet_wrap(~season, ncol = 1) + theme_minimal() +
  theme(axis.text.x=element_text(size=8), axis.text.y = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank(), legend.text=element_text(size=10), legend.title = element_text(size=10), legend.key.size = unit(0.3, 'cm'), legend.position='bottom', legend.box='horizontal') + ylim(-400, 1900) + scale_x_discrete(drop=FALSE, labels=seq(0,330,30)) +
  guides(fill = guide_legend(title.position="top", title.hjust = 0.5, nrow=6)) 
library('patchwork')
#p1 + p2 + plot_layout(ncol=2, width=c(6,1))

p1 + {
  p2 +
    plot_spacer() +
    plot_layout(ncol=1, height=c(8,1)) 
} +
  plot_layout(ncol = 2, width=c(6,1))
ggsave("10-Jul-19-mosquito.tracks.inc.namibiensis.color.edit.jpg", dpi=600, width=12, height=7.5)

ggsave("29-Aug-19-mosquito.tracks.eps", dpi=600, width=12, height=7.5)
ggsave("29-Aug-19-mosquito.tracks.pdf", dpi=600, width=12, height=7.5, device=cairo_pdf)



















###Possible summary table (to check path #s) -- first using tovi's:
df.mosq.count <- df.orig %>% mutate(TotMosi = as.numeric(as.character(TotMosi))) %>% filter(TotMosi>0) %>% group_by(Vil, tid, DateDown) %>% summarise(sum=sum(TotMosi)/93)
df.mosq.count <- df.mosq.count %>% group_by(tid) %>% summarise(count=sum(sum))


df.ben2 <- read.csv("ToMapAno1.csv") %>% 
  filter(!is.na(YR)) %>% #removing initial NAs from dataset
  mutate(intercepDT=as.POSIXct(paste(as.Date(substr(intercepDT, 1, 7), format="%d%b%y"), paste0(substr(intercepDT,9,10), ":00"), sep=" "), format="%Y-%m-%d %H:%M"),
         startDT=as.POSIXct(paste(as.Date(substr(startDT, 1, 7), format="%d%b%y"), paste0(substr(startDT,9,10), ":00"), sep=" "), format="%Y-%m-%d %H:%M"), ## datetime recormatting
         month=as.factor(MO),
         int.hour = hour(intercepDT),
         season=case_when(month %in% c("10", "11", "12") ~ "Oct-Dec",
                          month %in% c("8", "9") ~ "Aug-Sept",
                          month %in% c("5", "6", "7") ~ "May-July",
                          TRUE ~ "WHATMONTHISTHIS"), #define season
         season=factor(season, levels=c("May-July", "Aug-Sept", "Oct-Dec", "WHATMONTHISTHIS")),
         track.length=(intercepDT-startDT)/3600,
         catch.success = case_when(TotMosi>0 ~ "Y", TRUE ~ "N"),
         tid.group=case_when(tid == "An. squamosus" ~ "An. squamosus",
                             tid == "An. pharoensis" ~ "An. pharoensis",
                             tid == "An. rufipes" ~ "An. rufipes",
                             tid %in% c("An. coustani", "An. cf. coustani 1") ~ "An. coustani",
                             tid == "An. coluzzii" ~ "An. coluzzii",
                             TRUE ~ "other")) %>%
  add_count(tid, int.hour, Tind) %>%
  filter(n == 9) %>% #This is filtering to entries with 8 hour or greater tracks. I was having issues getting the >6am intercept tracks to be full (they were just a few points---I'm not sure)
  mutate(keep.track = case_when(TotMosi > 0 & int.hour == "6" ~ "Y",
                                TotMosi > 1 & int.hour == "5" ~ "Y",
                                TotMosi > 2 & int.hour == "4" ~ "Y",
                                TotMosi > 3 & int.hour == "3" ~ "Y",
                                TotMosi > 4 & int.hour == "2" ~ "Y",
                                TRUE ~ "N"))

df.mosq.count.ben.df <- df.ben2 %>% mutate(TotMosi = as.numeric(as.character(TotMosi))) %>% filter(TotMosi>0) %>% group_by(Vil, tid, DateDown) %>% summarise(sum=sum(TotMosi)/9)
df.mosq.count.ben.df <- df.mosq.count.ben.df %>% group_by(tid) %>% summarise(count=sum(sum))










