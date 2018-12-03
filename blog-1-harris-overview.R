library(rgeos)
library(rgdal)
library(raster)
library(leaflet)
library(leaflet.extras)
library(tidyverse)
library(sp)
library(mapview)
library(png)

library(sf)
library(tidyverse)

library(maptools)
library(here)
library(stringr)
library(janitor)
library(readr)

library(plotly)
library(dplyr)


#Sets up projections
wgs84<-CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
utm14n<-CRS("+proj=utm +zone=14 +ellps=GRS80 +datum=NAD83 +units=m +no_defs +towgs84=0,0,0")

#Census planning data ######
#Download from here: https://www.census.gov/research/data/planning_database/2018/
dfbg <- read_csv(here("pdb2018bgv4_us.csv")) %>% 
  dplyr::select(GIDBG:`Block_group`, 
         contains("ACS_12_16"), -contains("pct"),
         MailBack_Area_Count_CEN_2010:Low_Response_Score)


#Fix characters and take mean across census tracts
state.cty.names <- dfbg %>% 
  dplyr::select(State, State_name, County, County_name) %>% 
  group_by(State, County) %>% 
  mutate(
    dup = row_number()
  ) %>% 
  ungroup() %>% 
  dplyr::filter(dup==1)

county_avg <- dfbg %>% 
  mutate_at(vars(Med_HHD_Inc_BG_ACS_12_16:avg_Agg_House_Value_ACS_12_16), parse_number) %>% 
  dplyr::select(State, County, Tot_Occp_Units_ACS_12_16, Low_Response_Score, avg_Tot_Prns_in_HHD_ACS_12_16
                ) %>% 
  mutate(
    below73_cnt = ifelse(Low_Response_Score>=28, 1, 0),
    below70_cnt = ifelse(Low_Response_Score>=35, 1, 0)
  ) %>% 
  group_by(State, County) %>% 
  summarise(
    lr.score.avg = mean(Low_Response_Score, na.rm=T),
    lr.score.wavg = weighted.mean(Low_Response_Score, Tot_Occp_Units_ACS_12_16, na.rm=T),
    lr.below73.cnt = mean(below73_cnt, na.rm=T),
    lr.below70 = mean(below70_cnt, na.rm=T),
    
    avg_Tot_prns_in_HHD = mean(avg_Tot_Prns_in_HHD_ACS_12_16, na.rm=T)
  )




#Compile county-level file######
county <- dfbg %>% 
  mutate_at(vars(Med_HHD_Inc_BG_ACS_12_16:avg_Agg_House_Value_ACS_12_16), parse_number) %>% 
  dplyr::select(State, County, State_name, County_name,
                Tot_Population_ACS_12_16, Males_ACS_12_16,
                Pop_under_5_ACS_12_16, Pop_5_17_ACS_12_16, Pop_18_24_ACS_12_16, Pop_25_44_ACS_12_16, Pop_45_64_ACS_12_16, Pop_65plus_ACS_12_16,
                Hispanic_ACS_12_16, NH_White_alone_ACS_12_16, NH_Blk_alone_ACS_12_16, 
                Pop_25yrs_Over_ACS_12_16, College_ACS_12_16, Not_HS_Grad_ACS_12_16,
                Pov_Univ_ACS_12_16, Prs_Blw_Pov_Lev_ACS_12_16, 
                Diff_HU_1yr_Ago_ACS_12_16, 
                Rel_Family_HHD_ACS_12_16, Female_No_HB_ACS_12_16, Sngl_Prns_HHD_ACS_12_16,
                Tot_Prns_in_HHD_ACS_12_16, Rel_Child_Under_6_ACS_12_16,
                Med_HHD_Inc_TR_ACS_12_16, Med_House_Value_TR_ACS_12_16,
                Tot_Housing_Units_ACS_12_16:No_Plumb_ACS_12_16,
                avg_Tot_Prns_in_HHD_ACS_12_16, ENG_VW_ACS_12_16,
                MailBack_Area_Count_CEN_2010:Mail_Return_Rate_CEN_2010) %>% 
  group_by(State, County) %>% 
  summarise_if(is.numeric, sum, na.rm=T) %>% 
  left_join(., county_avg, by=c("State", 'County')) %>% 
  left_join(., state.cty.names, by=c("State", 'County')) %>% 
  mutate(
    `% Renting` = Renter_Occp_HU_ACS_12_16 / Tot_Occp_Units_ACS_12_16,
    #males = Males_ACS_12_16 / Tot_Population_ACS_12_16,
    `% Ages 18-24` = Pop_18_24_ACS_12_16 / Tot_Population_ACS_12_16,
    `% HH female head, no husb.` = Female_No_HB_ACS_12_16 / Rel_Family_HHD_ACS_12_16,
    `% Non-White` = 1 - (NH_White_alone_ACS_12_16 / Tot_Population_ACS_12_16),
    `% Under age 65` = 1 - (Pop_65plus_ACS_12_16 / Tot_Population_ACS_12_16),
    `% HH with child under 6` = Rel_Child_Under_6_ACS_12_16 / Rel_Family_HHD_ACS_12_16,
    #ages_2544 = Pop_25_44_ACS_12_16 / Tot_Population_ACS_12_16,
    `% Vacant units` = Tot_Vacant_Units_ACS_12_16 / Tot_Housing_Units_ACS_12_16,
    `% Lacking college degree` = 1 - (College_ACS_12_16 / Pop_25yrs_Over_ACS_12_16),
    #med.inc
    #ages_4464 = Pop_45_64_ACS_12_16 / Tot_Population_ACS_12_16,
    `No. persons per HH` = avg_Tot_prns_in_HHD,
    #moved 2005-2009
    `% Hispanic` = Hispanic_ACS_12_16 / Tot_Population_ACS_12_16,
    `% Single housing units` = Single_Unit_ACS_12_16 / Tot_Occp_Units_ACS_12_16,
    #pop density
    `% Below poverty` = Prs_Blw_Pov_Lev_ACS_12_16 / Pov_Univ_ACS_12_16, 
    `% Diff. housing 1yr ago` = Diff_HU_1yr_Ago_ACS_12_16 / Tot_Population_ACS_12_16,
    `% Ages 5-17` = Pop_5_17_ACS_12_16 / Tot_Population_ACS_12_16,
    `% Black` = NH_Blk_alone_ACS_12_16 / Tot_Population_ACS_12_16,
    `% HH single person` = Sngl_Prns_HHD_ACS_12_16 / Rel_Family_HHD_ACS_12_16,
    `% Lacking HS degree` = Not_HS_Grad_ACS_12_16 / Pop_25yrs_Over_ACS_12_16,
    #median hosue value
    #pub_assist = 
    `% Crowded housing units` = Crowd_Occp_U_ACS_12_16 / Tot_Occp_Units_ACS_12_16,
    `% HH w/ Limited English` = ENG_VW_ACS_12_16 / Tot_Occp_Units_ACS_12_16,
    `% Without phone service` = Occp_U_NO_PH_SRVC_ACS_12_16 / Tot_Occp_Units_ACS_12_16,
    
    county_fips = paste0(State, County),
    harris01 = ifelse(State=="48" & County=="201", "1", "0")
  ) %>% 
  dplyr::select(State, County, State_name, County_name, county_fips, harris01, Tot_Population_ACS_12_16, Tot_Occp_Units_ACS_12_16,
         lr.score.wavg:lr.below70, `% Renting`:`% Without phone service`)


#Top 25 only ####
top25 <- county %>% 
  arrange(desc(Tot_Population_ACS_12_16)) %>% 
  ungroup() %>% 
  mutate(
    poprank = row_number()
  ) %>% 
  filter(poprank<=25)

top25.fips <- top25$county_fips

county.long <- county %>% 
  gather(indicator, cty.value, `% Renting`:`% Without phone service`) %>% 
  group_by(indicator) %>% 
  mutate(
    nat.value = mean(cty.value, na.rm=T),
    z.value = (cty.value-nat.value)/sd(cty.value)
  )


top25.long <- county.long %>% 
  filter(county_fips %in% top25.fips) %>% 
  group_by(county_fips) %>% 
  mutate(
    harris1 = ifelse(State=="48" & County=="201", z.value, NA)
  ) %>% 
  ungroup() %>% 
  group_by(indicator) %>% 
  mutate(
    harrismax = max(harris1, na.rm = T)
  ) %>% 
  ungroup() %>% 
  mutate(
    var.order = as.factor(indicator)
  )
  


# Bar graph #####
us <- data.frame(State_name = "National", County_name = "National",  county_fips = "999999",
                 harris01="2", lr.score.wavg = 18.6, label_name = "US County Average")

barchart <- top25 %>%
  dplyr::select(State_name, County_name, county_fips, harris01, lr.score.wavg) %>% 
  mutate(
    label_name = as.factor(paste0(County_name, ", ", State_name))
  ) %>% 
  rbind(., us)


library(gridExtra)
library(grid)


#logo <- readPNG("/Users/davidmcclendon/Documents/January_Advisors/Marketing\ materials/ja-square.png")
rast <- grid::rasterGrob(logo, interpolate = T)

g <- grid.arrange(
  textGrob("Harris County Is Hard To Count",
           gp=gpar(fontsize=18, col="#2b2b2b", fontfamily = "Playfair Display"), 
           x=unit(0.005, "npc"), just=c("left", "bottom")),
  
  textGrob("Census Low Response Scores for Top 25 Counties", 
           gp=gpar(fontsize=10, col="#2b2b2b", fontfamily = "Roboto", fontface="bold"), 
           x=unit(0.005, "npc"), just=c("left", "bottom")),

  ggplot(data = barchart, aes(x=reorder(label_name, lr.score.wavg), y = lr.score.wavg, fill=harris01)) +
    geom_bar(stat = "identity") +
    geom_text(aes(x=label_name, y = lr.score.wavg, color=harris01,
                  label = ifelse(lr.score.wavg<0, "", paste0(round(lr.score.wavg), "%"))), size=3, hjust = -0.3) +
    scale_fill_manual(values = c("lightgray", "#FF7F05", "#2A92F2")) +
    scale_color_manual(values = c("#2a2a2a", "#FF7F05", "#2A92F2")) +
    coord_flip() +
    guides(fill=F, color=F) +
    ylim(0, 30) +
    annotation_custom(rast, xmin = 0, xmax = 4, ymin = 23) +
    theme(
      plot.title = element_text(family = "Playfair Display", size=20, hjust=-1.03),
      plot.subtitle = element_text(family = "Roboto", face="bold", size=10, hjust=-0.7),
      text=element_text(family="Roboto", size=12),
      legend.title = element_blank(),
      legend.position = "top",
      legend.background = element_rect(fill="transparent", colour="transparent"),
      legend.key = element_blank(),
      panel.background = element_rect(fill = "transparent", colour = "transparent"),
      plot.background = element_rect(fill = "transparent", colour = NA),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_text(size=9, color = "#2a2a2a"),
      plot.caption = element_text(family="Roboto", face = "plain", size = 8)
    ),
  
  textGrob("Note: Estimates based on American Community Survey, 2012-2016",
           gp=gpar(fontsize=9, col="#2b2b2b", fontfamily = "Roboto"), 
           x=unit(0.005, "npc"), just=c("left", "bottom")),
  
  ncol=1,
  heights=c(0.085, 0.015, 0.85, 0.05)
)


ggsave(here("blog1_barchart.png"), g, width = 8, height = 5)




#Dotplot graphic ######

long <- top25.long %>% 
  ggplot(aes(x = reorder(indicator, harrismax), y = z.value, group = county_fips, size=Tot_Population_ACS_12_16,
             text = paste0(County_name, ", ", State_name, ": ", round(cty.value*100), "%\n",
                           "National average: ", round(nat.value*100), "%\n",
                           "Standard deviation from mean: ", round(z.value, 1)))) +
  geom_point(aes(fill=harris01), color="white", stroke=0.1, alpha=0.8) +
  coord_flip() +
  scale_fill_manual(values = c("#2A92F2", "#FF7F05")) +
  scale_y_continuous("Standard deviations (sd) from US county average",
                     breaks = c(-4, -3, -2, -1, 0, 1, 2, 3, 4, 5),
                     labels = c("-4sd", "-3", "-2", "-1", "Average", "+1", "+2", "+3", "+4", "+5sd")) +
  scale_x_discrete("") +
  ggtitle("Key Predictors of Low Response for 25 Largest Counties") +
  theme(
    plot.title = element_text(family = "Playfair Display", size=18, hjust=0.5),
    text=element_text(family="Roboto", size=12, color="black"),
    axis.text.x = element_text(size=9),
    axis.title.x = element_text(size=10, face="bold"),
    panel.background = element_rect(fill = "transparent", colour = "transparent")
    )

gglong <- ggplotly(long, tooltip = c("text")) %>% 
  hide_legend() %>% 
  layout(xaxis = list(zeroline = TRUE),
         
         images = list(source = "http://januaryadvisors.com/wp-content/uploads/2013/05/ja-og.png",
                       xref = "paper",
                       yref = "paper",
                       x= 0.5,
                       y= 1,
                       sizex = 0.5,
                       sizey = 0.5,
                       opacity = 0.8),
         
         annotations = list(text = 'Source: U.S. Census Bureau.',
                              font = list(size = 10),
                              showarrow = FALSE,
                              xref = 'paper', x = 0,
                              yref = 'paper', y = -0.3)
         ) %>% 
  config(displayModeBar = F) 
  

gglong


#library(htmlwidgets)
htmlwidgets::saveWidget(gglong, here("dotplot.html"))






# SCATTERPLOT: non-citizens ########
#tract low response score: Citizenship only available at TRACT level
#DOwnload here: https://www.census.gov/research/data/planning_database/2018/

dft <- read_csv(here("pdb2018trv4_us.csv")) %>% 
  dplyr::select(GIDTR, State, County, Tract, Low_Response_Score) %>% 
  filter(State=="48" & County=="201")

#import and merge citizen data
names <- names(read_csv(here("Harris_tract_data", "ACS_16_5YR_B05001_citizenship",
                             "ACS_16_5YR_B05001_with_ann.csv"), 
                        n_max = 0))

citizen <- read_csv(here("Harris_tract_data", "ACS_16_5YR_B05001_citizenship",
                         "ACS_16_5YR_B05001_with_ann.csv"), 
                    col_names = names, skip=2) %>% 
  mutate(
    non_citizen_pct = HD01_VD06/HD01_VD01,
    GIDTR = as.character(GEO.id2)
  ) %>% 
  left_join(., dft, by="GIDTR") %>% 
  mutate(
    above_avg = ifelse(Low_Response_Score<18.58918, "Below national average", "Above national average")
  )

#Scatterplot of non-citizens and low response score #####
cit <- citizen %>% 
  filter(!is.na(above_avg)) %>% 
  ggplot(aes(x = non_citizen_pct*100 , y = Low_Response_Score, group=GIDTR,
             text = paste0("Tract ", Tract, "\n",
                           "Low Response Score: ", Low_Response_Score, "%\n",
                           "% Non-Citizens: ", round(non_citizen_pct*100), "%"))) + 
  geom_point(aes(fill = above_avg, size = HD01_VD01), color="white", stroke=0.2, alpha=0.8) +
  scale_x_continuous("% Not US citizens",
                     breaks = c(0, 20, 40, 60),
                     labels = c("0%", "20%", "40%", "60%")) +
  scale_y_continuous("",
                     breaks = c(0, 10, 20, 30, 40),
                     labels = c("0%", "10%", "20%", "30%", "40%"),
                     limits = c(0, 45)) +
  scale_fill_manual(values = c("#FF7F05", "#2A92F2")) +
  ggtitle("Low Response Scores of Harris County Census Tracts\nby Citizenship Status") +
  annotate("text", x=-Inf, y=Inf, label="Low Response Score", hjust=-.2, vjust=2) +
  theme(
    plot.title = element_text(family = "Playfair Display", size=18, hjust=0.5),
    text=element_text(family="Roboto", size=12, color="black"),
    axis.text = element_text(size=10),
    axis.title = element_text(size=12),
    panel.background = element_rect(fill = "transparent", colour = "transparent"),
    axis.title.y = element_blank(),
    legend.title = element_blank()
  )



cit

cit_y <- ggplotly(cit, tooltip = c("text")) %>% 
  layout(annotations= list(x=5, y=47, text = "Low Response Score (%)", 
                           showarrow = F)) %>% 
  layout(legend = list(x = 0.7, y=0.1)) %>% 
  config(displayModeBar = F) 

cit_y

htmlwidgets::saveWidget(cit_y, here("blog-series", "scatterplot.html"))




# LOW RESPONSE MAP ########

#Prepare census data
harris_bg <- dfbg %>% 
  filter(State=="48" & County=="201") %>% 
  mutate(
    GEOID = GIDBG
  )

#Import census blocks
library(tigris)
options(tigris_use_cache = FALSE)
blockg_sp <- block_groups(state = '48', county = '201',refresh = TRUE)

blockg_sp <- spTransform(blockg_sp, utm14n)

blockg_sf <- st_as_sf(blockg_sp) %>% 
  left_join(., harris_bg) %>% 
  mutate(
    `% Renting` = Renter_Occp_HU_ACS_12_16 / Tot_Occp_Units_ACS_12_16,
    `% Ages 18-24` = Pop_18_24_ACS_12_16 / Tot_Population_ACS_12_16,
    `% HH female head, no husband` = Female_No_HB_ACS_12_16 / Rel_Family_HHD_ACS_12_16,
    `% Non-White` = 1 - (NH_White_alone_ACS_12_16 / Tot_Population_ACS_12_16),
    `% Under age 65` = 1 - (Pop_65plus_ACS_12_16 / Tot_Population_ACS_12_16),
    `% HH with child under 6` = Rel_Child_Under_6_ACS_12_16 / Rel_Family_HHD_ACS_12_16,
    `% Vacant units` = Tot_Vacant_Units_ACS_12_16 / Tot_Housing_Units_ACS_12_16,
    `% Lacking college degree` = 1 - (College_ACS_12_16 / Pop_25yrs_Over_ACS_12_16),
    `% Hispanic` = Hispanic_ACS_12_16 / Tot_Population_ACS_12_16,
    `% Single housing units` = Single_Unit_ACS_12_16 / Tot_Occp_Units_ACS_12_16,
    `% Below poverty` = Prs_Blw_Pov_Lev_ACS_12_16 / Pov_Univ_ACS_12_16, 
    `% in diff. housing unit 1yr ago` = Diff_HU_1yr_Ago_ACS_12_16 / Tot_Population_ACS_12_16,
    `% Ages 5-17` = Pop_5_17_ACS_12_16 / Tot_Population_ACS_12_16,
    `% Black` = NH_Blk_alone_ACS_12_16 / Tot_Population_ACS_12_16,
    `% HH single person` = Sngl_Prns_HHD_ACS_12_16 / Rel_Family_HHD_ACS_12_16,
    `% Lacking HS degree` = Not_HS_Grad_ACS_12_16 / Pop_25yrs_Over_ACS_12_16,
    `% Crowded housing units` = Crowd_Occp_U_ACS_12_16 / Tot_Occp_Units_ACS_12_16,
    `% Without phone service` = Occp_U_NO_PH_SRVC_ACS_12_16 / Tot_Occp_Units_ACS_12_16,
    
    lrs25 = quantile(Low_Response_Score, probs=.25, na.rm=TRUE),
    lrs50 = quantile(Low_Response_Score, probs=.5, na.rm=TRUE),
    lrs75 = quantile(Low_Response_Score, probs=.75, na.rm=TRUE),
    
    lrs.bins = ifelse(Low_Response_Score<=10, "<10%",
                      ifelse(Low_Response_Score>10 & Low_Response_Score<19, "11%-18.9%",
                             ifelse(Low_Response_Score>=19 & Low_Response_Score<25, "19%-24.9%",
                                    ifelse(Low_Response_Score>=25 & Low_Response_Score<30, "25%-29.9%",
                                           ifelse(Low_Response_Score>=30 & Low_Response_Score<35, "30%-34.9%", 
                                                  ifelse(Low_Response_Score>=35, "35%+", NA)))))),
    
    high_rentals = ifelse(`% Renting`>=quantile(`% Renting`, probs=.75, na.rm=T) , 1, 0),
    high_kids6 = ifelse(`% HH with child under 6`>=quantile(`% HH with child under 6`, probs=.75, na.rm=T) , 1, 0),
    high_hispanic = ifelse(`% Hispanic`>=quantile(`% Hispanic`, probs=.75, na.rm=T) , 1, 0),
    high_black = ifelse(`% Black`>=quantile(`% Black`, probs=.75, na.rm=T) , 1, 0)

  )

tabyl(blockg_sf, high_hispanic)

#Convert data back to SF and proper projection for leaflet
mapdf <- spTransform(as(blockg_sf, "Spatial"), wgs84)

rental <- subset(mapdf, mapdf@data$high_rentals==1)
kids <- subset(mapdf, mapdf@data$high_kids6==1)
hispanic <- subset(mapdf, mapdf@data$high_hispanic==1)
black <- subset(mapdf, mapdf@data$high_black==1)


#Tract citizenship layer
tract_sp <- tracts(state = '48', county = '201',refresh = TRUE)
tract_sp <- spTransform(tract_sp, utm14n)

tract_sf <- st_as_sf(tract_sp) %>% 
  mutate(GEO.id2 = as.numeric(GEOID)) %>% 
  left_join(., citizen) %>% 
  mutate(
    lrs.bins = ifelse(Low_Response_Score<=10, "<10%",
                      ifelse(Low_Response_Score>10 & Low_Response_Score<19, "11%-18.9%",
                             ifelse(Low_Response_Score>=19 & Low_Response_Score<25, "19%-24.9%",
                                    ifelse(Low_Response_Score>=25 & Low_Response_Score<30, "25%-29.9%",
                                           ifelse(Low_Response_Score>=30 & Low_Response_Score<35, "30%-34.9%", 
                                                  ifelse(Low_Response_Score>=35, "35%+", NA)))))),
    
    high_noncitizen = ifelse(non_citizen_pct>=quantile(non_citizen_pct, probs=.75, na.rm=T) , 1, 0)
  ) %>% 
  filter(high_noncitizen==1)

tract_sp <- spTransform(as(tract_sf, "Spatial"), wgs84)



#Make map

pal <- colorFactor(c("#094d8c",  "#2A92F2", "#ffd6af", "#ffaa5a","#FF7F05", "#af5500", "gray" ), 
                   domain = c("NA", "<10%", "11%-18.9%", "19%-24.9%", "25%-29.9%", "30%-34.9%", "35%+"))


#Popup box
popup<-paste0("<b>Block Group</b>: #", mapdf@data$GEOID, "<br/>",
              "<b>Low Response Rate</b>: ", mapdf@data$Low_Response_Score, "%<br/>",
              "<b>% Renters</b>: ",  round(mapdf@data$X..Renting*100), "%<br/>",
              "<b>% HH with children <6</b>: ", round(mapdf@data$X..HH.with.child.under.6*100), "%<br/>",
              "<b>% Hispanic</b>: ", round(mapdf@data$X..Hispanic*100), "%<br/>",
              "<b>% Black</b>: ", round(mapdf@data$X..Black*100), "%"
              )

popup2 <- paste0("<b>Tract</b>: #", tract_sp@data$GEOID, "<br/>",
                 "<b>Low Response Rate</b>: ", tract_sp@data$Low_Response_Score, "%<br/>",
                 "<b>% Non-Citizens</b>: ",  round(tract_sp@data$non_citizen_pct*100), "%"
)

map <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>% 
  addPolygons(
    group = "All block groups",
    data = mapdf ,
    fillColor = ~pal(mapdf@data$lrs.bins),
    popup = popup,
    weight = 0.5,
    opacity = 1,
    color = 'white',
    fillOpacity = 0.8
  ) %>% 
  addPolygons(
    group = "High % Rentals",
    data = rental,
    fillColor = ~pal(rental@data$lrs.bins),
    popup = popup,
    weight = 0.5,
    opacity = 1,
    color = 'white',
    stroke = 0.1,
    fillOpacity = 0.8
  ) %>% 
  addPolygons(
    group = "High % HH kids <6",
    data = kids,
    fillColor = ~pal(kids@data$lrs.bins),
    popup = popup,
    weight = 0.5,
    opacity = 1,
    color = 'white',
    stroke = 0.1,
    fillOpacity = 0.8
  ) %>% 
  addPolygons(
    group = "High % Hispanic",
    data = hispanic,
    fillColor = ~pal(hispanic@data$lrs.bins),
    popup = popup,
    weight = 0.5,
    opacity = 1,
    color = 'white',
    stroke = 0.1,
    fillOpacity = 0.8
  ) %>% 
  addPolygons(
    group = "High % Black",
    data = black,
    fillColor = ~pal(black@data$lrs.bins),
    popup = popup,
    weight = 0.5,
    opacity = 1,
    color = 'white',
    stroke = 0.1,
    fillOpacity = 0.8
  ) %>% 
  addPolygons(
    group = "High % Non-Citizens (tract)",
    data = tract_sp,
    fillColor = ~pal(tract_sp@data$lrs.bins),
    popup = popup2,
    weight = 0.5,
    opacity = 1,
    color = 'white',
    stroke = 0.1,
    fillOpacity = 0.8
  ) %>%
  addLegend("bottomright", pal = pal,
            values = mapdf@data$lrs.bins,
            title = "Low Response Score<br/>by Block Group",
            opacity = 0.8) %>%
  addSearchOSM(options = searchOSMOptions(zoom=12, position = 'topleft', hideMarkerOnCollapse=T)) %>% 
  addLogo(img = "http://januaryadvisors.com/wp-content/uploads/2013/05/ja-og.png", 
          position = c("bottomleft"), offset.x = 10, offset.y = 10, width = 90, height = 90, alpha = 0.9) %>% 
  addLayersControl(
    baseGroups = c("All block groups", "High % Rentals", "High % HH kids <6", 
                   "High % Hispanic", "High % Black", "High % Non-Citizens (tract)"),
    options = layersControlOptions(collapsed = FALSE)
  )

map

# Export Map #####
saveWidget(map, here("lowresponse_harris.html"))









#Scatterplot: LR score vs. % block groups below avg (or below 60%?)

what <- county %>%
  filter(Tot_Population_ACS_12_16>=100000) %>% 
  mutate(
    county_fips = paste0(State, County),
    harris01 = ifelse(State=="48" & County=="201", 1, 0)
  ) %>% 
  ggplot(aes(x = lr.score.wavg, y = Tot_Population_ACS_12_16, group = county_fips)) +
  geom_point(aes(color = harris01, size = Tot_Population_ACS_12_16)) +
  geom_smooth()

what

#Calculate percentages by county level
#Compare Harris County to other big metro counties (top 10? 20?)
  #On census mailback rates in 2010
  #% of census tracts with low response rates (look at website def)
  #Key predictors of low response rates nationwide (What's distrinct about Harris County?)



# Renter occupied units  
# Ages 18-24 
# Female head 
# Hispanic
# NOT non-Hispanic White (reverse)
# UNDER Age 65 (reverse)
# Related child <6 
# Non-married households (add, reverse)
# Vacant units
# Non-college graduates (reverse)
# Median household income (add)
# Persons per hh
# Single person hh
# Single-unit structures (reverse to multi-unit structures)
# Below poverty
# Under 18 (add)
# Black
# Not hs grad
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 

