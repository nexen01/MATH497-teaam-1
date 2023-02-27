####  Read in required pacakges. ######
library(readr)
library(dplyr)
library(sf)
library(leaflet)
library(stringr)
library(stringdist)
library(party)
library(tigris)
library(ggmap)

### Work though following stages.
##  1. Read in data. If there's a state provided file, then create basic stats about it 
##     and Census VTDs and record on spreadsheet.
##  2. Map voter precincts to geos using geocoded voter data.
##  3. Compare precinct names from the two files. Are names very different from each other?
##  4. Identify counties with lower proportions of matches (Lower CI < .75).


### Read in required data #####
##  1. state-provided shapefile (if available)
##  2. List of precincts from state
##  3. Census VTD file 
##  4. File with sample of geocoded voters

### Read in State's file of precincts
## No file yet for Ohio
# state_precs <- st_read("")
# state_precs <- st_transform(state_precs,"+proj=longlat +datum=WGS84")

### Read in state shapefile

## Read in list of precincts
#precincts <- read_csv("OH_precincts.csv")


### Read in Census VTDs
vtds <- voting_districts(state="IL")
vtds <- st_transform(vtds, "+proj=longlat +datum=WGS84")
base_projection <- "+proj=longlat +datum=WGS84"

cntys <- dplyr::filter(acs::fips.county, State == "IL")
cntys$County.ANSI <- str_pad(cntys$County.ANSI,width=3, pad="0")

## Add county name to file.
vtds <- dplyr::left_join(vtds, 
                         cntys[,c("County.ANSI","County.Name")],
                         by=c("COUNTYFP20" = "County.ANSI")
                         ) %>% 
  rename(county = County.Name)

vtds$county <- str_to_upper(vtds$county) %>% str_remove(" COUNTY")


## Map to compare VTD and state provided data if latter exists
leaflet(vtds) %>% addPolygons(data=state_precs,color="red",weight=3.5) %>% 
  addPolygons(color="blue",weight=2) %>% addProviderTiles("CartoDB.Positron")


  
### Read in sample of voters, and use geocodes to link county/precincts from voter file to shapes. #####
##  Will use vtds if a state file is not available.
vote_samp <- read_csv("https://raw.githubusercontent.com/nexen01/MATH497-teaam-1/main/csvs/Assessor-1.csv?token=GHSAT0AAAAAAB7BNVTA55XC4IL4VUXPXZA2Y7T5FGA")
head(vote_samp)
tail(vote_samp)
names(vote_samp) ## Look at names in voter sample to get proper names for precinct field

## Trim whitespace from precinct names and count names.
vote_samp$county <- str_trim(vote_samp$county)
vote_samp$precinct <- str_trim(vote_samp$precinct)

nrow(vote_samp) ## Check count of rows.
head(vote_samp) ## quick view of the data

## Need to create an sf object with point geometry information.  
##  Not that there might be a better way, but this is the way I do it.  I got it to work and haven't bothered exploring alternatives.
lead_shape <- vote_samp[,c("Latitude","")] %>%  
  split(1:nrow(vote_samp)) %>% ##split each row into its own list, 
  lapply(FUN=function(x){as.numeric(x) %>% st_point()}) %>%  ##lapply applies a functions to each object in the list. In this case, create a point geom for each row.
  st_sfc(crs = "+proj=longlat +datum=WGS84")

## At this point vote_geos has just the geometry information.  Will attach it to other voter information  

voter_geos <- st_sf(select(vote_samp,voter_id,voters, sampsize,address:precinct), ## Select voter_id through precinct
                    geometry=voter_geos) ## This might about 1-2 minutes.
head(voter_geos)

voter_geos$county <- str_to_upper(voter_geos$county)
voter_geos$precinct <- str_to_upper(voter_geos$precinct)

## Look at count of unique county/precinct values
st_set_geometry(voter_geos,NULL) %>% select(county,precinct) %>% unique() %>% nrow()

head(voter_geos)  ## See how the data has changed compared to vote_samp
class(vote_samp)  
class(voter_geos)  ## Look at the class before and after.  Now the data is an sf or simple feature.

### Match voter precincts to state precinct geos 
cntys_vec <- voter_geos$county %>% unique() %>% sort()
x <- "COOK"
geo_match <- split(cntys_vec, ## Split the final_matches
                   1:length(cntys_vec)
                   ) %>% ## End split 
  lapply(FUN=function(x){
    cnty_out <<- x ## For debugging. Outputs county that's in processing.
    cnty <- unique(x)
    cat(cnty," ")
    dplyr::filter(voter_geos, county ==  cnty) %>%
      prec_via_geocode(shapes=vtds, cnty_name=cnty,
                             state_name = "GEOID20", vote_name = "precinct",
                             prop=.05)
  }
)


## Bind all results together
geo_match <- do.call(rbind,geo_match)

## join with original shape file to gather names onto geo_match
##  For Ohio, we only have VTDs, so this join is designed with that data in mind. Alter depending the shape file applied.

geo_match <- left_join(geo_match,
                       st_set_geometry(vtds,NULL) %>% ## Need to strip off geometries first, otherwise it gives an error
                         select(NAME20, GEOID20), ## Match back to shape file, but keep NAME20 only
                       by="GEOID20")

View(geo_match)

## Look at cases when voters didn't match to a geo in the county (these will have an NA in GEOID20.
filter(geo_match, is.na(GEOID20) ) %>% select(relFreq) %>% summary()

## Numbers are pretty high, so will remove NAs and recalculate relFreq
geo_match_noNA <- filter(geo_match, !is.na(GEOID20) ) %>%
  group_by(county, precinct) %>% mutate(tot_precinct = sum(Freq),
                                        relFreq_noNA = Freq/tot_precinct)
View(geo_match_noNA)

## Take GEOID20 w/ the highest relFreq value in the precinct, and declare it a match.
geo_match_noNA <- ungroup(geo_match_noNA) %>% 
  group_by(precinct) %>% mutate(maxp = max(relFreq_noNA))
View(geo_match_noNA)

filter(vtds, str_detect(NAME20,"MILTON TWP #1")) %>% leaflet() %>%
  addPolygons(label=~NAME20) %>% addProviderTiles("OpenStreetMap")

#Milton 1 Map: https://www.boe.ohio.gov/ashland/c/precmaps/24.pdf?637890108622589592
## General ASHLAND maps: https://lookup.boe.ohio.gov/vtrapp/ashland/precandpoll.aspx
## For Cuyahoga county: https://boe.cuyahogacounty.gov/maps-and-data/interactive-maps-by-city-and-cleveland-ward

## Model and visualize any counties with lower match rates than others. #####
county_tree <- ctree(relFreq_noNA ~ as.factor(county), 
                     data=filter(geo_match_noNA,maxp==relFreq_noNA)
                       )
plot(county_tree)
county_tree

### Compare names for geo-matched data. #######
# 
## Create shape precinct name


## Create voter precinct name 
geo_match_noNA$voter_precinct <- geo_match_noNA$precinct %>% 
  str_to_upper() %>%
  str_replace_all("[,.']"," ") %>%
  str_replace_all("  "," ") %>% ## Replace double-blanks with a single blank
  str_trim()

geo_match_noNA$state_precinct <- geo_match_noNA$NAME20 %>% 
  str_to_upper() %>%
  str_replace_all("[,.]"," ") %>%
  str_replace_all("  "," ") %>%
  str_trim()

geo_match_noNA <- ungroup(geo_match_noNA)
word_counts <- bind_rows( ## bind_rows combines rows from different datasets. Those data sets should have the same column names.
  select(geo_match_noNA, county, state_precinct)  %>%  ## will parse precinct_format by county and NAME20, so select all or those vars.
    unnest_tokens(word, state_precinct) %>% group_by(county) %>% ## unnest() collects the words into a single column, and other variables are added as columns
    count(word) %>% mutate(source = "State"), ## want to count word use by county, then add variable for source. 
  ## perform same operations for voter precincts
  select(geo_match_noNA,county, precinct, voter_precinct) %>% 
    unnest_tokens(word,voter_precinct) %>% group_by(county) %>%
    count(word) %>% mutate(source="Voter") 
)

nrow(word_counts)
sum(word_counts$n) 

## Subset data to those words that occur with greater frequency. Words that occur just once aren't worth our attention.
##  This also lessens the dataset size and makes the model faster.
word_sub <- filter(word_counts, n >= 5)
nrow(word_sub)  ## Subset

firsttree <- ctree(as.factor(source) ~ as.factor(word), data=word_sub, 
                   weights = word_sub$n,
                   control = ctree_control(minbucket = 20,maxdepth=10)
)

plot(firsttree)
firsttree

word_sub$prob_state <- (predict(firsttree, type="prob") %>% simplify2array() %>% t())[,1]
View((word_sub))

## Construct list of words to delete.  This code gives a bad set fo Ohio, b/c of so many abbreviations, so may need to determine own set.
deletes <- filter(word_sub, (prob_state <=.3 | prob_state >=.7 ) & 
                    n >=40 &
                    !(str_detect(word,"[0-9]+")) & 
                    nchar(word)>=2) %>% select(word) %>% 
  simplify2array() %>% 
  str_to_upper() %>% paste0(" ",.) %>% unique()

## I settled on this list.
deletes <- c("PREC","PRECINCT","TP","WARD")

## Reorders delete-able words by desencing length. Removes larger words first
deletes <- deletes[order(nchar(deletes))] %>% rev()

## Remove words from state_precinct and voter_precinct
##  The paste(delect_vec, collapse="|") function creates single string with an or command to regex, 
##    so it'll delete any of the strings in delete_vec
geo_match_noNA$voter_precinct <- geo_match_noNA$voter_precinct %>% 
  str_remove_all(paste(deletes, collapse="|")) %>% 
  str_trim()

geo_match_noNA$state_precinct <- geo_match_noNA$state_precinct %>% 
  str_remove_all(paste(deletes, collapse="|")) %>%
  str_trim()
  
## Calculate and compare string distances. #####
var_names <- c("state_precinct","voter_precinct")

## Logical for Match and then distance based on Levenshtein metric
geo_match_noNA$match <-  base::apply(geo_match_noNA[,var_names],1,function(y){y <- y[order(nchar(y))]; agrepl(y[1],y[2])}) 
geo_match_noNA$dist <-  base::apply(geo_match_noNA[,var_names],1,function(y){y <- y[order(nchar(y))]; adist(y[1],y[2])}) 


## Calculate Jaccard distance
geo_match_noNA$jaccard_dist <-  base::apply(geo_match_noNA[,var_names],1,
                                       function(y){y <- y[order(nchar(y))]; 
                                       stringdist(paste(" ",y[1]," "),paste(" ",y[2]," "),
                                                  method="jaccard",q=2)})
## Calculate Cosine distance
geo_match_noNA$cosine_dist <-  base::apply(geo_match_noNA[,var_names],1,
                                      function(y){y <- y[order(nchar(y))]; 
                                      stringdist(paste(" ",y[1]," "),paste(" ",y[2]," "),
                                                 method="cosine",q=2)})  

## Calculate Jaro-Winkler distance
geo_match_noNA$jw_dist <-  base::apply(geo_match_noNA[,var_names],1,
                                  function(y){y <- y[order(nchar(y))]; 
                                  stringdist(paste(y[1]),paste(y[2]),
                                             method="jw")}) 

## Plot the jw distance and the jaccard distance, and see how simlar they are.
##  Note what appears to be a structural break around .6 on the x-axis.
plot(jw_dist ~ jaccard_dist, data=geo_match_noNA)

View(geo_match_noNA)
 