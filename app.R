# Construct an interactive map for the South Bend area.
# Clickable map outlining the demographics of a selected census tract
# Purpose: to inform efforts to address social issues such as homelessness
# and poverty. 

# Schools panel - created by Roger Goldsmith
# Census panel - created by Elicia Dennis
# Abandoned properties panel - created by Zodo Agadi



library(shiny)
library(leaflet)
library(rgdal)
library(rgeos)
library(ggmap)
library(sf)
library(leaflet)
library(tidyverse)
library(dplyr)
library(ggplot2)



# Read in census data
census <- st_read("2020_CensusData.shp") %>%
  st_as_sf(coords = c("Lon", "Lat")) %>%
  st_set_crs(value = 4326)

# Add a measure of percent of people living in poverty or struggling to make ends meet
census$pov_status <- 100 - (((census$B13004_5) / census$A00001_1) * 100)

# Rename columns so they're easier to work with
census <- census %>% rename (total_pop = A02001_1,
                             total_male = A02001_2,
                             total_female = A02001_3,
                             under_5 = A01001_2, 
                             age_5_9 = A01001_3,
                             age_10_14 = A01001_4,
                             age_15_17 = A01001_5, 
                             age_18_24 = A01001_6,
                             age_25_34 = A01001_7, 
                             age_35_44 = A01001_8,
                             age_45_54 = A01001_9, 
                             age_55_64 = A01001_10,
                             age_65_74 = A01001_11,
                             age_75_84 = A01001_12,
                             age_85_older = A01001_13,
                             race_white = A03001_2,
                             race_black = A03001_3,
                             race_native = A03001_4,
                             race_asian = A03001_5, 
                             race_pacificisland = A03001_6,
                             race_other = A03001_7,
                             race_multi = A03001_8,
                             family_married = A10008_3,
                             family_male_nowife = A10008_5,
                             family_female_nohus = A10008_6,
                             non_family = A10008_7,
                             median_income = A14006_1,
                             owner_occupied = A10060_2,
                             renter_occupied = A10060_3
)

schools <- st_read("School_Boundaries.shp") %>%
  st_as_sf(coords = c("Lon", "Lat")) %>%
  st_set_crs(value = 4326)

# Dense matrix of intersections between the school areas and the census.
# index by census district index, school index. This data is static so we don't
# need to keep recalculating it.
census_school_intersection <- st_intersects(census$geometry, schools$geometry, sparse = FALSE)


if (TRUE)
{
sh<-st_read("Abandoned_Property_Parcels.shp", stringsAsFactors = FALSE)
#extracting the lon and lat
dh2<-sh%>% dplyr::mutate(lon = list(sf::st_coordinates(.)[,1]),
                         lat = list(sf::st_coordinates(.)[,2]))%>%tidyr::unnest(substr(lon,3,6), substr(lat,3,7))

dh2$lon<-as.numeric(dh2$`substr(lon, 3, 6)`)
dh2$lat<-as.numeric(dh2$`substr(lat, 3, 7)`)

facilities.spatial <-as.data.frame( dh2 %>%
                                      st_as_sf(coords  = c("Lon","Lat"))%>%
                                      st_set_crs(value = 4326))

dT<-facilities.spatial%>%
  dplyr::filter(!is.na( Outcome_St))%>%
  dplyr::count(Zip_Code, Outcome_St)
}


# Define UI for application that draws a histogram
ui <- fluidPage(

  titlePanel("South Bend Data Explorer"),
  #textOutput(outputId = "instructions"),
  
  tabsetPanel (
    # The map is partitioned by census district. Census districts
    # are clickable, and when clicked, school zones that in the selected
    # census zone are added to the map.
    tabPanel ("Schools",
      sidebarLayout (

        sidebarPanel("School Info", width = 4 ,
                     dataTableOutput(outputId = "schooldata"),
                     actionButton(inputId = "clear", "Clear Map Selection"),
                     textOutput(outputId = "selected_census_zone")),
        mainPanel(width = 8, leafletOutput("zone"))
      )
    ),
    tabPanel( "Census", 
              mainPanel(width = 9, leafletOutput("track"),
                        textOutput("percent_struggling", container = tags$h3),
                        plotOutput("bar_chart_gender"),
                        plotOutput("bar_chart_race"),
                        plotOutput("bar_chart_age"),
                        plotOutput("bar_chart_ownership"),
                        plotOutput("bar_chart_household"))
    ), 
    tabPanel("Abandoned Buildings",
             sidebarLayout(
      sidebarPanel(
        selectInput("outcome",
                    "Type of Abandonment:",
                    c("Demolished" ="Demolished" ,"Deconstructed"="Deconstructed",
                      "Repaired"="Repaired","Repaired & Occupied" ="Repaired & Occupied" ,
                      "Occupied & Not Repaired"="Occupied & Not Repaired"
                    ) )
        ## we can also use unique(sh$outcome) rather hardcodding
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        leafletOutput("map"),
        plotOutput("bar")
      )
    ))
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # Here is where we can observe the clicks on the map.
  observeEvent(input$zone_shape_click,
    {
      p <- input$zone_shape_click
      
      #print(paste("Clicked Census Zone : ", p$id))
      
      output$selected_census_zone <- renderText(paste("Selected Census Zone : ", p$id))
      
      # Avoid redrawing the map with the proxy.
      m2 <- leafletProxy("zone", session = session)
      
      m2 %>% removeShape("school")
      
      schools_in_click <- which ( census_school_intersection[which(census$GEOID == p$id),])
      
      
      # This replaces the content of the latter "school", and "census_zone"
      school_subset <- schools[schools_in_click,]
      
      
      m2 %>% addPolygons(data = school_subset$geometry , color = "blue", layerId = "school") %>%
        addPolygons(data = census[census$GEOID == p$id,],
                    color = "red", opacity = 0.4 ,
                    layerId = "census_zone") # highlight selected census zone.
      
      output$schooldata <- renderDataTable(as.data.frame(school_subset)[c("School", "SchoolType")] )
      
    }
  )
  
  # Want a mechanism to clear all selected zones.
  observeEvent(input$clear,
               {
                 #print ("clearing")
                 m2 <- leafletProxy("zone", session = session)
                 m2 %>% removeShape("school") %>% removeShape("census_zone")
                 output$schooldata <- renderDataTable(data.frame())
                 output$selected_census_zone <- renderText("")
               })

  output$zone <- renderLeaflet(
    {
      m <- leaflet() %>% addTiles() %>%
        addPolygons(data = census, layerId = ~GEOID, label = census$GEOID, color = "blue", opacity = 0.1,
                    highlight = highlightOptions(color = "red",weight = 1, bringToFront = T, opacity = 0.7))
    }
  )
  
  ###
  
  
  plotData<-reactive({
    facilities.spatial%>%dplyr::filter(Outcome_St %in%c(input$outcome))
    
  })
  plotData1<-reactive({
    sh%>%dplyr::filter(Outcome_St %in%c(input$outcome))
    
  })
  plotData2<-reactive({
    dT%>%dplyr::filter(Outcome_St %in%c(input$outcome))
    
  })
  
  output$map <- renderLeaflet({
    pal <- colorFactor(palette = 'Set1', domain =sh$Outcome_St)
    leaflet( plotData()) %>%
      addTiles() %>%
      addPolygons(data= plotData1(),weight=5,col = ~pal(as.factor(plotData1()$Outcome_St)),label = plotData1()$Outcome_St) %>% 
      #addMarkers(data = facilities.spatial,  popup = ~as.factor(Outcome_St))
      addCircleMarkers(lng =~ plotData()$lon, lat=~ plotData()$lat, weight = 2,  
                       radius = 4, popup = ~plotData()$Code_Enfor, color = ~pal(plotData()$Outcome_St),label = plotData()$Code_Enfor) #%>%
      
      #addCircles(lng =~ plotData()$lon, lat=~ plotData()$lat, weight = 1,
      #           radius = ~sqrt(dT$n) * 30, popup = ~dT$Zip_Code,label = dT$Zip_Code
      #)
    
  })
  
  output$bar <- renderPlot({
    ggplot(data=plotData2(), aes(x=Zip_Code, y=n)) +
      geom_bar(stat="identity",  fill="steelblue")+
      xlab('Abandoned Property Zip Code')+
      ylab('Total of Abandoned Property')
    
    
  })
  
  ## Text and graphs
  # Percentage text
  output$percent_struggling <- renderText({
    paste0(toString(round(census_poverty(), digits = 0)), "% of people in this census tract live below the povery line or struggle to make ends meet")
  }) %>% bindEvent(input$zone_shape_click)
  
  # Create bar plot of the gender composition in the census tract
  output$bar_chart_gender <- renderPlot({
    ggplot(census_pop_selected(), aes(x=GEOID, y=value, fill=variable)) +
      geom_bar(stat='identity', position='dodge') + scale_fill_discrete(labels=c('Total female', 'Total male')) + ggtitle("Number of men and women in the selected census tract") +
      xlab("Census tract") + ylab("Number of people")
  }) %>% bindEvent(input$zone_shape_click)
  
  # Create bar plot of the racial composition of the census tract
  
  output$bar_chart_race <- renderPlot({
    ggplot(census_race_selected(), aes(x=GEOID, y=value, fill=variable)) +
      geom_bar(stat='identity', position='dodge') + scale_fill_discrete(labels=c('Asian', 'Black', "Multiple races", "Native/Indiginous", "Other race", "Pacific Island/Hawaiian", "White")) + ggtitle("Number of people in the census tract by race") +
      xlab("Census tract") + ylab("Number of people")
  }) %>% bindEvent(input$zone_shape_click)
  
  # Create bar plot of the age distributions of the census tract
  
  output$bar_chart_age <- renderPlot({
    ggplot(census_age_selected(), aes(x=GEOID, y=value, fill=variable)) +
      geom_bar(stat='identity', position='dodge') + scale_fill_discrete(labels=c('Under 5 years', '5-9 years', "10-14 years", "15-17 years", "18-24 years", "25-34 years", "35-44 years", "45-54 years", "55-64 years", "65-74 years", "75-84 years", "85 and older")) + ggtitle("Number of people in the census tract by age") +
      xlab("Census tract") + ylab("Number of people")
  }) %>% bindEvent(input$zone_shape_click)
  
  # Create bar plot of the ownership and renters within the census tract
  
  output$bar_chart_ownership <- renderPlot({
    ggplot(census_ownership_selected(), aes(x=GEOID, y=value, fill=variable)) +
      geom_bar(stat='identity', position='dodge') + scale_fill_discrete(labels=c("Owns home", "Rents home")) + ggtitle("Number of households in the census tract by type of occupancy") +
      xlab("Census tract") + ylab("Number of people")
  }) %>% bindEvent(input$zone_shape_click)
  
  # Create bar plot of the household types within the census tract
  
  output$bar_chart_household <- renderPlot({
    ggplot(census_household_selected(), aes(x=GEOID, y=value, fill=variable)) +
      geom_bar(stat='identity', position='dodge') + scale_fill_discrete(labels=c("Married Family", "Family - female, no husband", "Family - male, no wife", "Non-family")) + ggtitle("Number of households in the census tract by head of household") +
      xlab("Census tract") + ylab("Number of people")
  }) %>% bindEvent(input$zone_shape_click)
  
  ## Calculations  
  # Calculate the percentage of people in poverty in the census tract
  census_poverty <- reactive(
    {
      p <- input$zone_shape_click
      census_poverty <- census %>% filter(GEOID == p$id)
      census_poverty <- census_poverty$pov_status
      print(census_poverty)
      return(census_poverty)
    }
  )
  
  # Calculate the number of people of each sex in the census tract
  census_pop_selected <- reactive(
    {
      p <- input$zone_shape_click
      census_pop <- census %>% select(c(GEOID, total_male, total_female, total_pop, pov_status)) %>% filter(GEOID == p$id)
      census_pop_selected <- tidyr::pivot_longer(census_pop, cols=c('total_male', 'total_female'), names_to='variable', values_to="value")
      return(census_pop_selected)
    }
  )
  
  # Calculate the number of people of each race in the census tract
  
  census_race_selected <- reactive(
    {
      p <- input$zone_shape_click
      census_race <- census %>% select(c(GEOID, race_white, race_black, race_native, race_asian, race_pacificisland, race_other, race_multi)) %>% filter(GEOID == p$id)
      
      census_race_selected <- tidyr::pivot_longer(census_race, cols=c("race_white", "race_black", "race_native", "race_asian", "race_pacificisland", "race_other", "race_multi"), names_to='variable', values_to="value")
      return(census_race_selected)
      
    }
  )
  
  
  # Calculate the number of people of each age group in the census tract
  
  census_age_selected <- reactive(
    {
      p <- input$zone_shape_click
      
      census_race <- census %>% select(c(GEOID,A03001_1, under_5, age_5_9, age_10_14, age_15_17, age_18_24, age_25_34, age_35_44, age_45_54, age_55_64, age_65_74, age_75_84, age_85_older)) %>% filter(GEOID == p$id)
      
      census_age_selected <- tidyr::pivot_longer(census_race, cols=c("under_5", "age_5_9", "age_10_14", "age_15_17", "age_18_24", "age_25_34", "age_35_44", "age_45_54", "age_55_64", "age_65_74", "age_75_84", "age_85_older"), names_to='variable', values_to="value")
      return(census_age_selected)
      
    }
  )
  
  # Calculate the number of owners and renters in each census tract
  
  census_ownership_selected <- reactive(
    {
      p <- input$zone_shape_click
      
      census_ownership <- census %>% select(c(GEOID,A03001_1, owner_occupied, renter_occupied)) %>% filter(GEOID == p$id)
      
      census_ownership_selected <- tidyr::pivot_longer(census_ownership, cols=c("owner_occupied", "renter_occupied"), names_to='variable', values_to="value")
      return(census_ownership_selected)
      
    }
  )
  
  # Calculate the number of households of each type within the census tract
  
  census_household_selected <- reactive(
    {
      p <- input$zone_shape_click
      
      census_household <- census %>% select(c(GEOID,A03001_1, family_married, family_female_nohus, family_male_nowife, non_family)) %>% filter(GEOID == p$id)
      
      census_household_selected <- tidyr::pivot_longer(census_household, cols=c("family_married", "family_female_nohus", "family_male_nowife", "non_family"), names_to='variable', values_to="value")
      return(census_household_selected)
      
    }
  )
  
  # Here is where we can observe the clicks on the map.
  observeEvent(input$track_shape_click,
               {
                 p <- input$track_shape_click
                 
                 output$selected_census_track <- renderText(paste("Selected Census track : ", p$id))
                 
                 m2 <- leafletProxy("track", session = session)
                 
                 m2 %>% addPolygons(data = census[census$GEOID == p$id,], color = "red", opacity = 0.4 ,
                                    layerId = "census_track") # highlight selected census track.
                 
               }
  )
  
  # Want a mechanism to clear all selected tracks.
  observeEvent(input$clear,
               {
                 m2 <- leafletProxy("track", session = session)
                 output$selected_census_track <- renderText("")
               })
  ## Maps
  # Create the initial map
  output$track <- renderLeaflet(
    {
      m <- leaflet() %>% addTiles() %>%
        addPolygons(data = census, layerId = ~GEOID, label = census$GEOID, color = "blue", opacity = 0.1,
                    highlight = highlightOptions(color = "black",weight = 2, bringToFront = T, opacity = 0.7))
    }
  )
  
  ## Text and graphs
  # Percentage text
  output$percent_struggling <- renderText({
    paste0(toString(round(census_poverty(), digits = 0)), "% of people in this census tract live below the povery line or struggle to make ends meet")
  }) %>% bindEvent(input$track_shape_click)
  
  # Create bar plot of the gender composition in the census tract
  output$bar_chart_gender <- renderPlot({
    ggplot(census_pop_selected(), aes(x=GEOID, y=value, fill=variable)) +
      geom_bar(stat='identity', position='dodge') + scale_fill_discrete(labels=c('Total female', 'Total male')) + ggtitle("Number of men and women in the selected census tract") +
      xlab("Census tract") + ylab("Number of people")
  }) %>% bindEvent(input$track_shape_click)
  
  # Create bar plot of the racial composition of the census tract
  
  output$bar_chart_race <- renderPlot({
    ggplot(census_race_selected(), aes(x=GEOID, y=value, fill=variable)) +
      geom_bar(stat='identity', position='dodge') + scale_fill_discrete(labels=c('Asian', 'Black', "Multiple races", "Native/Indiginous", "Other race", "Pacific Island/Hawaiian", "White")) + ggtitle("Number of people in the census tract by race") +
      xlab("Census tract") + ylab("Number of people")
  }) %>% bindEvent(input$track_shape_click)
  
  # Create bar plot of the age distributions of the census tract
  
  output$bar_chart_age <- renderPlot({
    ggplot(census_age_selected(), aes(x=GEOID, y=value, fill=variable)) +
      geom_bar(stat='identity', position='dodge') + scale_fill_discrete(labels=c('Under 5 years', '5-9 years', "10-14 years", "15-17 years", "18-24 years", "25-34 years", "35-44 years", "45-54 years", "55-64 years", "65-74 years", "75-84 years", "85 and older")) + ggtitle("Number of people in the census tract by age") +
      xlab("Census tract") + ylab("Number of people")
  }) %>% bindEvent(input$track_shape_click)
  
  # Create bar plot of the ownership and renters within the census tract
  
  output$bar_chart_ownership <- renderPlot({
    ggplot(census_ownership_selected(), aes(x=GEOID, y=value, fill=variable)) +
      geom_bar(stat='identity', position='dodge') + scale_fill_discrete(labels=c("Owns home", "Rents home")) + ggtitle("Number of households in the census tract by type of occupancy") +
      xlab("Census tract") + ylab("Number of people")
  }) %>% bindEvent(input$track_shape_click)
  
  # Create bar plot of the household types within the census tract
  
  output$bar_chart_household <- renderPlot({
    ggplot(census_household_selected(), aes(x=GEOID, y=value, fill=variable)) +
      geom_bar(stat='identity', position='dodge') + scale_fill_discrete(labels=c("Married Family", "Family - female, no husband", "Family - male, no wife", "Non-family")) + ggtitle("Number of households in the census tract by head of household") +
      xlab("Census tract") + ylab("Number of people")
  }) %>% bindEvent(input$track_shape_click)
  
  ## Calculations  
  # Calculate the percentage of people in poverty in the census tract
  census_poverty <- reactive(
    {
      p <- input$track_shape_click
      census_poverty <- census %>% filter(GEOID == p$id)
      census_poverty <- census_poverty$pov_status
      print(census_poverty)
      return(census_poverty)
    }
  )
  
  # Calculate the number of people of each sex in the census tract
  census_pop_selected <- reactive(
    {
      p <- input$track_shape_click
      census_pop <- census %>% select(c(GEOID, total_male, total_female, total_pop, pov_status)) %>% filter(GEOID == p$id)
      census_pop_selected <- tidyr::pivot_longer(census_pop, cols=c('total_male', 'total_female'), names_to='variable', values_to="value")
      return(census_pop_selected)
    }
  )
  
  # Calculate the number of people of each race in the census tract
  
  census_race_selected <- reactive(
    {
      p <- input$track_shape_click
      census_race <- census %>% select(c(GEOID, race_white, race_black, race_native, race_asian, race_pacificisland, race_other, race_multi)) %>% filter(GEOID == p$id)
      
      census_race_selected <- tidyr::pivot_longer(census_race, cols=c("race_white", "race_black", "race_native", "race_asian", "race_pacificisland", "race_other", "race_multi"), names_to='variable', values_to="value")
      return(census_race_selected)
      
    }
  )
  
  
  # Calculate the number of people of each age group in the census tract
  
  census_age_selected <- reactive(
    {
      p <- input$track_shape_click
      
      census_race <- census %>% select(c(GEOID,A03001_1, under_5, age_5_9, age_10_14, age_15_17, age_18_24, age_25_34, age_35_44, age_45_54, age_55_64, age_65_74, age_75_84, age_85_older)) %>% filter(GEOID == p$id)
      
      census_age_selected <- tidyr::pivot_longer(census_race, cols=c("under_5", "age_5_9", "age_10_14", "age_15_17", "age_18_24", "age_25_34", "age_35_44", "age_45_54", "age_55_64", "age_65_74", "age_75_84", "age_85_older"), names_to='variable', values_to="value")
      return(census_age_selected)
      
    }
  )
  
  # Calculate the number of owners and renters in each census tract
  
  census_ownership_selected <- reactive(
    {
      p <- input$track_shape_click
      
      census_ownership <- census %>% select(c(GEOID,A03001_1, owner_occupied, renter_occupied)) %>% filter(GEOID == p$id)
      
      census_ownership_selected <- tidyr::pivot_longer(census_ownership, cols=c("owner_occupied", "renter_occupied"), names_to='variable', values_to="value")
      return(census_ownership_selected)
      
    }
  )
  
  # Calculate the number of households of each type within the census tract
  
  census_household_selected <- reactive(
    {
      p <- input$track_shape_click
      
      census_household <- census %>% select(c(GEOID,A03001_1, family_married, family_female_nohus, family_male_nowife, non_family)) %>% filter(GEOID == p$id)
      
      census_household_selected <- tidyr::pivot_longer(census_household, cols=c("family_married", "family_female_nohus", "family_male_nowife", "non_family"), names_to='variable', values_to="value")
      return(census_household_selected)
      
    }
  )
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

