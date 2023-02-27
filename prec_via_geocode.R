## Function requires the following inputs  
#    -dataset with sf point geometry (vote_file) 
#    -and a shape file (shapes)

prec_via_geocode <- function(vote_file,shapes,cnty_name,prop = .10) {
  require(sf); require(dplyr)
  ## Provide function a cnty_name, file of counties in a state (from fips.county),
  ##  Shape file of geos, and file of voters.
  ## 
  
  ## For case of voters in a specific precinct, provide voters in precinct and name of 
  ##  county.  
  ##  -Create sfc_point object by breaking all voters into separate st_point objects
  ##    and piping into st_sfc
  
  voter_sub <- dplyr::filter(vote_file, county == cnty_name)
  ## nrow(voter_sub)
  
  
  ## Get fips code for county to reference and extract shapes from Census data
  
  ##  -Extract precinct geos from the specified county
  precs_geos <- dplyr::filter(shapes,shapes$county %in% cnty_name)
  precs_geos <- precs_geos[order(precs_geos$GEOID20),]
  
  voter_within <- st_join(voter_sub, 
                          select(precs_geos, NAME20, GEOID20),
                          join=st_within)
  ## Want % of a precincts voters within a particular GEOID20 (Census polygon shape)
  voter_summary <- st_set_geometry(voter_within,NULL) %>% ## Remove geometry, as we don't need it for summary.
    group_by(county, precinct, NAME20, GEOID20) %>% ## Want counts by Census GEOID20 within state precinct
    summarise(Freq = n() ) %>% ungroup() %>% ## Summarise, b/c just want counts
    group_by(county, precinct) %>% mutate(relFreq = Freq/sum(Freq)) ## Regroup by county/precinct, and calculate %


  voter_summary <- filter(voter_summary, relFreq >= prop)

  ## at to voter summary info on # of voters and sample size. Need for confidence intervals later.
  dplyr::left_join(voter_summary,
                   voter_sub %>% st_set_geometry(NULL) %>% 
                     select(total_voters, sampsize, precinct) %>% unique(),
                               by="precinct"
                               
  ) %>% return()
  
}


