# Set a working directory on your local machine
setwd("")
# Ensure strings come in as character types
options(stringsAsFactors = FALSE)
# Install packages
library(tidyverse)
library(grid)
library(gridExtra)
library(reshape2)
library(jsonlite)
library(sf)
library(rgeos)

#################################
# Function Definitions
######

## @knitr download_unzip_files
download_unzip_files <- function(url_,file_name=NULL){
  #' download_unzip_datafiles
  #'
  #' Downloads zipped files, unzips file and loads them into
  #' a readable dataframe format
  #' 
  # @return
  #' dataframe
  #' 
  
  #create a couple temp files
  temp_zipped <- tempfile()
  temp_unzip <- tempfile()
  #download the zip folder from the internet save to 'temp_zipped' 
  download.file(url = url_,temp_zipped)
  #unzip the contents in 'temp' and save unzipped content in 'temp_unzip'
  unzip(zipfile = temp_zipped, exdir = temp_unzip)
  if (is.null(file_name)==TRUE) {
    #finds the filepath of the shapefile (.shp) file in the temp_unzip folder
    shp_file<-list.files(temp_unzip, pattern = ".shp$",full.names=TRUE)
    #read the shapefile
    data_df <- sf::read_sf(shp_file)
  }else if (grepl(".shp$",file_name) == TRUE){
    #read the shapefile
    data_df <- sf::read_sf(file_name)
  }else{data_df = read.csv(file_name)}
  
  unlink(c(temp_zipped,temp_unzip))
  
  return(data_df)
}

## @knitr get_seattle_population
get_seattle_population <- function() {
  #' get_seattle_population
  #'
  #' creates a dataframe with the population estimates for 2020
  #' for Seattle with gender and race breakdown 
  #' 
  # @return
  #' population dataframe
  #' 
  
  # setting temporary file for excel datafile from census source
  temp = tempfile(fileext = ".xlsx")
  SEATLE_CENSUS_TRACT_POP_URL <- "https://ofm.wa.gov/sites/default/files/public/dataresearch/pop/asr/sade/ofm_pop_sade_tract_2015_to_2020.xlsx"
  download.file(SEATLE_CENSUS_TRACT_POP_URL, destfile=temp, mode='wb')
  # create dataframes for sheet 2 (Totals) and sheet 4 (Non-hispanic breakdown)
  test_total <- readxl::read_xlsx(temp, sheet =2, range = cellranger::cell_cols("A:G"))
  test_nonhisp <- readxl::read_excel(temp, sheet =4)
  # only keep values for 'Totals' and for the year '2020'
  test_total <- test_total[(grepl("Total$", test_total$`Age Group`)&(test_total$Year=='2020')),]
  test_nonhisp <- test_nonhisp[(grepl("Total", test_nonhisp$`Age Group`)&(test_nonhisp$Year=='2020')),]
  # merging dataframes to mutate
  test <- merge(test_total, test_nonhisp, by=c("Area Name","Area ID","Year","Age Group"))
  # converting value columns to numberic
  cols_toNum <- names(test)[5:length(colnames(test))]
  test[cols_toNum] <- lapply(test[cols_toNum], as.numeric)
  # Creating dataframe to return, subtract 'Total' and 'Non-hispanic totals' 
  # to get a value for 'Hispanic' population. 
  pop.df <- test %>%
    mutate('Hispanic Total' = .$Total-.$`Non-Hispanic Total`,
           'Hispanic Male' = .$Male-.$`Non-Hispanic Male`,
           'Hispanic Female' = .$Female-.$`Non-Hispanic Female`) %>%
    select(-c(`Non-Hispanic Total`,`Non-Hispanic Male`,`Non-Hispanic Female`)) %>%
    setNames(tolower(gsub("Non-Hispanic ","",names(.)))) %>% 
    mutate_if(is.numeric, round)
  # releasing memory from temp file link
  unlink(temp)
  
  return(pop.df)
}

#################################
# Defining Parameters
######

# Data can be directly downloaded from the API for each of the datasets
# An API KEY will be needed to download entire dataset or data can be 
# downloaded as a CSV. I had the csv files already downloaded in the gitrepo
# so I'm simply loading the data from those files with max date '2020-11-14'
## @knitr url_links
SHOOTING_API_20201114 = 'https://raw.githubusercontent.com/JUAN-SOLORIO/data-512/main/data-512-final/1-Data/spd_officer_shooting_20201114.csv'
FORCE_USE_API_20201114 = 'https://raw.githubusercontent.com/JUAN-SOLORIO/data-512/main/data-512-final/1-Data/spd_use_of_force_20201114.csv'
SEATTLE_CENSUS_TRACTS = 'https://www.seattle.gov/Documents/Departments/OPCD/Demographics/GeographicFilesandMaps/KingCountyTractsShapefiles.zip'
POLICE_BEATS_URL = 'https://opendata.arcgis.com/datasets/45495a3ab3a141dcb3277dc91c52ec5f_1.zip'

# Shape files
SPB_2015_2017 = 'Seattle_Police_Beats_2015-2017.shp'
KC_TRACTS = 'kc_tract_10.shp'

#################################
# Loading Data
#####

# loading data into dataframes
# These are taken from old github repo
# datafiles were downloaded '2020-11-14'
## @knitr police_data_files
shooting.df <- read.csv(SHOOTING_API_20201114)
use.of.force.df <- read.csv(FORCE_USE_API_20201114)


# Seattle Police Beats shapefile for mapping
## @knitr scpd.beats
scpd.beats <- download_unzip_files(POLICE_BEATS_URL)


## @knitr seattle.census.tracts.shp
# Loading Seattke Census tracts shapes
seattle.census.tracts.shp <- download_unzip_files(SEATTLE_CENSUS_TRACTS)
#plot(seattle.census.tracts.shp['TRACTCE10'])

#################################
# Create Dataframes
######

## @knitr shooting.df
# Adding the police precinct and beat where the shooting took place
# package 'sf' has st_intersects that finds the intersection of
# a provided lon lat coordinate on a given polygon geometry
shooting.df <- shooting.df %>%
  st_as_sf(coords = c("Longitude", "Latitude"),
           crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0") %>%
  mutate(
    intersection = as.integer(st_intersects(geometry, scpd.beats))
    , beat = if_else(is.na(intersection), '', scpd.beats$beat[intersection])
    , first_prec = if_else(is.na(intersection), '', scpd.beats$first_prec[intersection])
  ) %>% filter(!is.na(intersection))%>% 
  mutate(Officer.Race = recode(Officer.Race, 
                               'Hispanic or Latino' = 'Hispanic/Latino',
                               'AI/AN' = 'American-Indian/Alaska-Native', 
                               'Black or African American' = 'Black',
                               'Two or More Races' = 'Multi-Racial', 
                               'Nat Hawaiian/Oth Pac Islander' = 'Asian/Pacific-Islander',
                               'Asian' = 'Asian/Pacific-Islander'))

## @knitr use.of.force.df
# Adding the police precinct and beat polygon and spatial data
# mostly for mapping purposes but also reduces the dataframe
# to only those samples with actual precinct and beat available (not NA)
use.of.force.df <- use.of.force.df %>%
merge(y=scpd.beats, by.x='Beat', by.y='beat', all.x = T) %>%
  filter(!is.na(first_prec)) %>%
  st_as_sf()
# need to change a specific cell with wrong Precinct for beat U3
use.of.force.df$Precinct[(use.of.force.df$Beat == 'U3')&(use.of.force.df$Precinct=='South')] = 'North'

## @knitr seattle.ct.centroids.latlon
# Get the centroids ('center') for each tract
# we'll use this to find which tracts fall inside a police beat
seattle.ct.centroids.latlon <- st_centroid(seattle.census.tracts.shp) %>%
  st_transform("+proj=longlat +ellps=WGS84 +datum=WGS84")

## @knitr seattle.scpd.bct
# getting the census tracts that fall in each police beat by
# location of tract centroid
seattle.scpd.bct <- seattle.ct.centroids.latlon %>% mutate(
  intersection = as.integer(st_intersects(geometry, scpd.beats))
  , beat = if_else(is.na(intersection), '', scpd.beats$beat[intersection])
  , first_prec = if_else(is.na(intersection), '', scpd.beats$first_prec[intersection])
) %>% filter(!is.na(intersection)) %>%
  select(c(GEOID10,beat,first_prec))

## @knitr seattle.pop.data
# Get the census tract population data
seattle.pop.data <- get_seattle_population()

## @kniter seattle.scpd.pop.data
# population data for each police precinct and beat
# by merging the census tract - police beat location datafram
# and population demographics for each tract
seattle.scpd.pop.data <- merge(x=seattle.scpd.bct,y=seattle.pop.data,
      by.x = 'GEOID10', by.y = 'area id', all.x = T) %>%
  st_set_geometry(NULL) %>%
  select(-c('area name','age group', 'year')) %>% 
  pivot_longer(
    cols=names(seattle.pop.data[5:length(names(seattle.pop.data))])
    , names_to = "Demographic Group", values_to = "Count") %>%
  filter(!is.na(first_prec)) 

## @kniter seattle.scpd.pop.total
# population totals for each police precinct and beat
# reassigns labels to reduce demographics to match police data
seattle.scpd.pop.total <- seattle.scpd.pop.data %>%
  filter(str_detect(`Demographic Group`, " total$")) %>%
  mutate(`Demographic Group` = case_when(grepl("asian|nhopi", `Demographic Group`, ignore.case = TRUE) ~ "Asian/Pacific-Islander",
                                         grepl("two|races", `Demographic Group`, ignore.case = TRUE) ~ "Multi-Racial",
                                         grepl("black", `Demographic Group`, ignore.case = TRUE) ~ "Black",
                                         grepl("hispanic", `Demographic Group`, ignore.case = TRUE) ~ "Hispanic/Latino",
                                         grepl("aian", `Demographic Group`, ignore.case = TRUE) ~ "American-Indian/Alaska-Native",
                                         TRUE                                         ~ "White")) 

## @knitr seattle.scpd.pop.race.gender
# population totals split for race and gender columns
# specifically for easy visualization with ggplot
seattle.scpd.pop.race.gender <- seattle.scpd.pop.data %>%
  filter(str_detect(`Demographic Group`, "male$")) %>%
  mutate(`Demographic Group` = case_when(grepl("asian male|nhopi male", `Demographic Group`, ignore.case = TRUE) ~ "Asian/Pacific-Islander Male",
                                         grepl("two|races male", `Demographic Group`, ignore.case = TRUE) ~ "Multi-Racial Male",
                                         grepl("black male", `Demographic Group`, ignore.case = TRUE) ~ "Black Male",
                                         grepl("hispanic male", `Demographic Group`, ignore.case = TRUE) ~ "Hispanic/Latino Male",
                                         grepl("aian male", `Demographic Group`, ignore.case = TRUE) ~ "American-Indian/Alaska-Native Male",
                                         grepl("asian male|nhopi female", `Demographic Group`, ignore.case = TRUE) ~ "Asian/Pacific-Islander Female",
                                         grepl("two|races female", `Demographic Group`, ignore.case = TRUE) ~ "Multi-Racial Female",
                                         grepl("black female", `Demographic Group`, ignore.case = TRUE) ~ "Black Female",
                                         grepl("hispanic female", `Demographic Group`, ignore.case = TRUE) ~ "Hispanic/Latino Female",
                                         grepl("aian female", `Demographic Group`, ignore.case = TRUE) ~ "American-Indian/Alaska-Native Female",
                                         grepl("white female", `Demographic Group`, ignore.case = TRUE) ~ "White Female",
                                         grepl("white male", `Demographic Group`, ignore.case = TRUE) ~ "White Male",
                                         TRUE                                         ~ "Other"))  %>%
  filter(!`Demographic Group` == 'Other') %>%
  separate(col = `Demographic Group`, into = c("Race", "Gender"), sep = "\\ ")

## @knitr

#################################
# Statistical Summaries
######

## @knitr officer.shootings.race
officer.shootings.race <- shooting.df %>%
  st_drop_geometry() %>%
  group_by(Officer.Race) %>%
  summarize('Number of Officers Involved in Shootings' = 
n()) %>%
  mutate(Proportion = paste0(round(100 * `Number of Officers Involved in Shootings`/sum(`Number of Officers Involved in Shootings`), 0), "%")) %>%
  arrange(desc(`Number of Officers Involved in Shootings`))

officer.shootings.race %>% 
  DT::datatable()

## @knitr officer.shootings.gender
shooting.df %>%
  st_drop_geometry() %>%
  group_by(Officer.Gender) %>%
  summarize('Number of Officers Involved in Shootings' = 
              n()) %>%
  mutate(Proportion = paste0(round(100 * `Number of Officers Involved in Shootings`/sum(`Number of Officers Involved in Shootings`), 0), "%")) %>%
  arrange(desc(`Number of Officers Involved in Shootings`)) %>%
  DT::datatable()

## @knitr shooting.subject.race
# Given that we know White Officers to make up most of the police
# Creating a table to see the proportions of White officers involved
# in shootings in the data
shooting.df %>%
  st_drop_geometry() %>%
  group_by(Subject.Race) %>%
  filter(Officer.Race == 'White') %>%
  summarize('Number of White Officers Involved in Shootings' = 
              n()) %>%
  mutate(Proportion = `Number of White Officers Involved in Shootings`/sum(`Number of White Officers Involved in Shootings`)) %>%
  arrange(desc(`Number of White Officers Involved in Shootings`)) %>%
  DT::datatable()


#################################
# Visualizations
######

## @knitr barplot_officer_shot_race
# barplot for police related shooting proportions
officer.shootings.race %>%
  ggplot(aes(y=reorder(Officer.Race,`Number of Officers Involved in Shootings`),
             x=`Number of Officers Involved in Shootings`, fill=Officer.Race)) +
  geom_bar(stat="identity")  +
  scale_fill_brewer(palette="Spectral")+
  theme_minimal() + theme(legend.position="none",axis.title.y=element_blank(),
                     panel.grid.minor = element_blank(),axis.line.y = element_blank())  +
  labs(title="Officer Involved in Shooting Numbers",
       subtitle="Totals by Officer Race",
       caption="Source: Seattle Open Data API\n@Juan Solorio")

## @knitr barplot_pop_race_gender
# barplot for seattle population for race and gender
# seattle.scpd.pop.race.gender %>%
#   group_by(Race, Gender) %>%
#   summarize(Counts = sum(Count)) %>%
#   mutate(`Proportion of Race` = Counts/sum(Counts))
#   DT::datatable()

seattle.scpd.pop.race.gender %>%
  group_by(Race, Gender) %>%
  summarize(n = sum(Count)) %>%
  ggplot(aes(y=Race, x=n, fill=Gender)) +
  geom_bar(stat = 'identity', position=position_dodge())+ 
  scale_fill_brewer(palette="Set2") + theme_minimal() +
  theme(axis.title.y=element_blank(),panel.grid.minor = element_blank(),
        axis.line.y = element_blank())  +
  labs(title="Seattle Population Demographics",
       subtitle="Totals by Race and Gender",
       caption="Source: Seattle Open Data API\n@Juan Solorio")


# If we dont specify a column, R will plot each of the columns to the
# shape of the given row (beat) entry
## @knitr map_beats
map_beats <- scpd.beats %>%
  filter(!is.na(first_prec)) %>%
  ggplot() +
  geom_sf(aes(fill = first_prec),alpha=0.4) +
  scale_fill_brewer(palette="Spectral",name = "Pricinct") + 
  theme_bw() + theme(panel.border = element_blank(), 
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), 
                     axis.line = element_blank(),
                     axis.text.x=element_blank(),axis.text.y=element_blank(),
                     axis.ticks=element_blank(),axis.title.x=element_blank(),)+
  labs(title="SCPD Beats Color by Precinct")
map_beats
# 
# map_beats  +
#   labs(title="SCPD Beats",
#        subtitle="Color by First Pricinct Location",
#        caption="Source: Seattle Open Data API\n@Juan Solorio")

## @knitr map_census_tracts
# map of census tracts colored by police precinct
# with location of police shootings
seattle.scpd.bct %>%
  st_drop_geometry() %>%
  merge(y=seattle.census.tracts.shp, by='GEOID10',all.x=T) %>%
  filter(!is.na(first_prec))%>%
  st_as_sf()%>%
  ggplot() +
  geom_sf(aes(fill = first_prec)) +
  scale_fill_brewer(palette="Spectral",name = "Pricinct") +
  geom_sf(data = shooting.df,fill = "darkred",alpha=.4)
  

## @knitr map_shootings_beats_by_precinct
# map of beats color by precinct 
# with police shooting locations
map_shootings_beats_by_precinct <- map_beats +
  geom_sf(data = shooting.df,fill = "darkred",alpha=.4) +
  labs(subtitle="Location of Police Shooting",
       caption="Source: Seattle Open Data API\n@Juan Solorio")

## @knitr map_use_of_force_by_prec
# map of use of force cases shown by fill strength 
# with police beats colors on borders
map_use_of_force_by_prec <- use.of.force.df %>%
  group_by(Beat, Precinct) %>%
  summarize(n=n()) %>%
  ggplot() +
  geom_sf(aes(color = Precinct, fill = n),size =.95) +
  scale_color_brewer(palette = "Spectral") +
  scale_fill_viridis_c(trans = "sqrt", alpha = .5, name='Number of Incidents') +
  theme_bw() + theme(panel.border = element_blank(), 
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), 
                     axis.line = element_blank(),
                     axis.text.x=element_blank(),axis.text.y=element_blank(),
                     axis.ticks=element_blank(),axis.title.x=element_blank()) +
  labs(title="Number of Use of Force Incidents per Beat")


## @knitr maps_shooting_use_force
# map of use of force cases shown by fill strength 
grid.arrange(map_shootings_beats_by_precinct,map_use_of_force_by_prec, ncol=2)

#################################
# Statistical Tests
######

## @knitr chi_sq_tests
# chi-square test for comparing observed vs expected values
# for the shooting demographics and use of force between precincts

# calculating observed values for chi-square test from shooting data
# on subject Race
observed_freq <- shooting.df %>%
  st_drop_geometry() %>%
  mutate(Subject.Race = case_when(grepl("Asian|Nat Hawaiian/Oth Pac Islander", Subject.Race, ignore.case = TRUE) ~ "Asian/Pacific-Islander",
                                         grepl("Not Specified", Subject.Race, ignore.case = TRUE) ~ "Multi-Racial",
                                         grepl("^Black", Subject.Race, ignore.case = TRUE) ~ "Black",
                                         grepl("^Hispanic", Subject.Race, ignore.case = TRUE) ~ "Hispanic/Latino",
                                         grepl("^Native", Subject.Race, ignore.case = TRUE) ~ "American-Indian/Alaska-Native",
                                         TRUE                                         ~ "White")) %>%
  group_by(first_prec, Subject.Race) %>%
  summarise(`Number of Shot Subjects` = n()) %>%
  mutate(`Frequency Shot` = `Number of Shot Subjects` / sum(`Number of Shot Subjects`))

# calculating expected values for chi-square test from population data
# on seattle population demographics
expected_freq <- seattle.scpd.pop.race.gender %>%
  group_by(first_prec, Race) %>%
  summarise(`Number of People in Race Demographic` = sum(Count)) %>%
  mutate(`Frequency of Race` = `Number of People in Race Demographic` / sum(`Number of People in Race Demographic`))

# creating one dataframe to loop through for each precinct
obs_exp_df <- merge(expected_freq, observed_freq, by.x=c('first_prec','Race'),
      by.y=c('first_prec','Subject.Race'), all.x=T) %>%
  mutate(`Number of Shot Subjects` = replace_na(`Number of Shot Subjects`, 0),
         `Frequency Shot`= replace_na(`Frequency Shot`, 0)) 

# initiating list values for looping through precincts and getting
# chi-square test results
p.vals <- c()
exp.vals.valid <- c()
for (i in unique(obs_exp_df$first_prec)) {
   res <- chisq.test(obs_exp_df[obs_exp_df$first_prec==i,]$`Number of Shot Subjects`,
             p=obs_exp_df[obs_exp_df$first_prec==i,]$`Frequency of Race`)
    p.vals<-append(p.vals, res$p.value)
    exp.vals.valid<-append(exp.vals.valid,mean(res$expected >= 5) == T)
  }

# displaying results
# note that according to chisq.test docummentation
# since the expected calculated values in the test were not >5
# our results might not be valid or accurate
data.frame('Precinct' = unique(obs_exp_df$first_prec),
           'P-Values'=p.vals,
           'Are Calculated Expected Values Valid?'=exp.vals.valid) %>%
  DT::datatable()
