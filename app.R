library(leaflet)
library(shiny)
library(bslib)
library(readr)
library(stringr)
library(sf)
library(dplyr)
library(ggplot2)
library(scales)
library(shinydashboard)
library(htmltools)
library(tigris)

house_districts <- read_sf("data/house.geojson") %>% 
  st_transform(4326) # This converts NAD83 to WGS84

senate_districts <- read_sf("data/senate.geojson") %>% 
  st_transform(4326)

mhvillage_df <- read_csv("data/MHvillage_IL_addedDistricts.csv")
mhvillage_df$Number_of_Sites <- as.integer(mhvillage_df$Number_of_Sites)

# make infographic tab with total site by county
build_infographics1 <- function(mhvillage_df) {
  total_sites_by_county <- mhvillage_df %>%
  filter(!is.na(`Number_of_Sites`)) %>%
    group_by(County) %>%
  summarise(Total_Sites = sum(Number_of_Sites, na.rm = TRUE)) %>%
   arrange(desc(Total_Sites))

  top_20 <- total_sites_by_county %>%
  slice_head(n = 20)  # Keep only top 20

  plot <- ggplot(top_20, aes(x = Total_Sites, y = reorder(County, Total_Sites))) +
    geom_bar(stat = "identity", fill = "cadetblue4", color = "cadetblue4") +
     labs(
       title = "Top 20 Counties by Number of MHCs (MHVillage)",
       x = "Total Number of Sites",
       y = "County"
     ) +
     theme_minimal() +
     scale_x_continuous(labels = scales::comma)
 
  return(plot)
 }

# make the marker layer
build_marker_layer <- function(map, layer_name) {
  if (layer_name == "MHVillage Markers (includes site info)") {
    mhvillage_df$label <- paste(
      "Name: ", as.character(mhvillage_df$Name),
      br(),
      "Sites: ", as.character(mhvillage_df$Number_of_Sites),
      br(),
      "Address: ", mhvillage_df$full_address, 
      br(),
      "House District: ", as.character(mhvillage_df$`House.district`),
      br(),
      "Senate District: ", as.character(mhvillage_df$`Senate.district`),
      br(),
      "Source: MHVillage"
    )
    # Add your specific code to build the Marker MHVillage layer
    map <- map %>% addMarkers(lng = as.numeric(mhvillage_df$long), 
                              lat = as.numeric(mhvillage_df$lat),
                              group = "MHVillage Markers (includes site info)",
                              popup = mhvillage_df$label,
                              clusterOptions = markerClusterOptions())
   } else if (layer_name == "MHVillage Circles (location only)") {
    # Add your specific code to build the Circle MHVillage layer
    map <- map %>% addCircleMarkers(lng = as.numeric(mhvillage_df$long), 
                                    lat = as.numeric(mhvillage_df$lat),
                                    color = "orange",
                                    opacity = 0.9,
                                    radius = 1,
                                    fillOpacity = 1,
                                    group = "MHVillage Circles (location only)")
      }
  return(map)
}

ui <- navbarPage(
  imageOutput("mhaction_logo", inline = TRUE),
  # First tab with existing content
  
  # Additional tabs
  tabPanel(
    "Map Tool",
    fluidPage(
      titlePanel("Illinois Manufacturing Housing Communities Map Tool"),
      fluidRow(
        column(
          width = 5,
          card(
            card_header(h6("Instructions")),
            HTML("
              <ol>
                <li>Select the layers you would like to view.
                  <ul>
                    <li>Markers will include additional information in a Pop-up message.</li>
                    <li>Circle layers will only pinpoint the coordinates of each MHC.</li>
                  </ul>
                </li>
              </ol>
            "),
            selectizeInput("layerlist", "Choose a Layer:", choices = c("Base Map Only", 
                                                                       "MHVillage Circles (location only)",
                                                                       "MHVillage Markers (includes site info)"),
                           
                           selected = "MHVillage Circles (location only)",
                           multiple = FALSE),
            HTML("
              <ol start='2'>
                <li>Select a district layer in the upper-right corner of the map.</li>
                <br> Note: To visualize circle sites clearly, please add a House or Senate districting layer before selecting a marker.
              </ol>
            "),
            height = "500px"
          )
        ),
        column(
          width = 7,
          card(
            leafletOutput("leafletMap"),
            full_screen = TRUE,
            height = "800px")
        )
      )
    )),
  
  tabPanel(
    "About",
    page_fillable(
      titlePanel(
        "Manufactured Housing Communities in Illinois"
      ),
      p("By ", 
        a("INFORMS", href = "https://informs.engin.umich.edu/", target = "_blank"), 
        " and ", 
        a("CTAC", href = "https://ginsberg.umich.edu/ctac", target = "_blank"),
        " at the University of Michigan")), br(),
    layout_columns(
      card(
        card_title("About"),
        p("The MHAction Mapping Tool is a visualization application designed to highlight the distribution of manufactured housing communities (MHC's) across the state of Michigan. The project began in late December 2023, initially between INFORMs and MHAction.
            In May 2024, INFORMs transitioned the project to the Community Technical Assistance Collaborative under the Ginsberg Center for Community Service and Learning. 
            The new development team aimed to improve the number of users allowed on the webpage, transitioning the webpage to GitHub pages. This allows for easy access to source data and code files in the future, as well as improved deployment capacity.
             Additional functional capabilities were added, including downloadable .csv files, user interface (UI) improvements, and data visibility. Package versioning was not optimized under the Python Shiny GitHub Pages website, so the app was re-written in RShiny using the automatic deployment tool."),
        p("Two sources of data were used to create this application. Michigan Department of Licensing and Regulatory Affairs (LARA) data was obtained by a FOIA request January 2025. This contains a complete list of MHC's in the state of Michigan, per state regulatory guidelines for registration. 
            MHVillage data was obtained from the website in February 2025. The data sources can differ significantly, including by MHC name and management, and MHVillage data is typically incomplete. However, it is important to include data from both sources, as 
            this enhances the reliability and accuracy of the map visualization tool by allowing users to cross-verify information and identify discrepancies."),
        p("Please note that data is updated annually."),
        p("State House and Senate districting information is essential to help identify legislators and communities most significantly impacted by laws surrounding Manufactured Housing Communities. State districting information was based off of the 2021 Linden (State Senate) and Hickory (State House) maps. 
            These were drafted by the Michigan Independent Citizens Redistricting Commission following the decennial US Census results in 2020."),
        p("For more information, please visit ",
          a("MHAction.org", href = "https://www.mhaction.org/", target = "_blank"))
      )),
    layout_columns(
      card(
        card_title("Navigation"),
        "About: Background information for this MHAction mapping project.",
        br(), "Map Tool: Visualize Manufactured Housing Communities and view data for the state of Michigan.",
        br(), "Infographics: Static bar graphs demonstrating MHVillage and LARA dataset capabilities. Also allows user to download full table data.",
        br(), "Tables: Dynamic tables that output MHC rows based on data source, geographic boundary type, and boundary selections. Users can download a full table with County, House, and Senate districting numbers.",
        br(), "Other: Credits, source files, and additional information."
      )
    ),
    layout_columns(
      card(
        card_title("Key Terms"),
        "MHC: Manufactured Housing Community",
        br(), "MHVillage: Online marketplace for buying and selling manufactured homes. Data may be incomplete."))),
  
  
  tabPanel(
    "Infographics",
    fluidPage(
      titlePanel("Infographics"),
      fluidRow(
        column(
          width = 4,
          card(
            "Infographics show selected county information from the MHVillage dataset. A csv file is available for all rows of data.",
            downloadLink("info1", "Download all MHVillage county site counts as .csv file."),
            # downloadLink("info2", "Download all MHVillage average rents by county as .csv file.")
          )
        ),
        column(
          width = 8,
          card(
            plotOutput("infographic1"),
            # plotOutput("infographic2")
          )
        )
      )
    )),
  
  tabPanel(
    "MHC Site List Tables",
    fluidPage(
      titlePanel("Tables"),
      fluidRow(
        column(
          width = 4,
          wellPanel(
            selectInput("main_category", "Select a Geographic Boundary Type:", choices = c("","County", "House District", "Senate District")),
            uiOutput("sub_category_ui"),
            h6("Site List Summary"),
            tableOutput("site_list_summary"),
            downloadButton("site_list_download", "Download Site List"),
            br(), br(),
            h5("Interested in the full district communities?"),
            downloadLink("mhvillage_all", "Download MHVillage data"),
            br())
        ),
        column(
          width = 8,
          tableOutput("site_list")
        )
      )
    )),
  
  tabPanel(
    "Other",
    page_fillable(
      titlePanel("Other Information"),
      layout_columns(
        card(
          card_title("Credits"),
          p("This website was built by Vicky Wang and Jiwon Suh with the Community Technical Assistance Collaborative in partnership with MHAction."),
          p("A previous version, hosted on GitHub Pages with limited functionality is available ",
            a("here.", href = "https://viwaumich.github.io/mhc/", target = "_blank")),
          p("Inspired by a ",
            a("project ", href = "https://hessakh.shinyapps.io/michigan_housing1/", target = "_blank"),
            "created by INFORMs at the University of Michigan."),
          br())),
      layout_columns(
        card(card_title("Reference Files"),
             p("Illinois Municipal League"),
             p("https://iecam.illinois.edu/data-descriptions/geographic-region-state-house-and-senate-districts"),
             p(
               downloadLink("mhvillage_raw", "MHVillage Raw Data (.csv)"),
               br(),
               downloadLink("house_geojson", "Illinois House Districts (.json)"),
               br(),
               downloadLink("senate_geojson", "Illinois Senate Districts (.json)")
             )
        )
      ),
      " Please reach out to Vicky Wang (viwa@umich.edu) or Jiwon Suh (jiwonsuh@umich.edu) with questions.",
      br(),
      imageOutput("ctac_logo",inline = TRUE),
      imageOutput("mhaction_logo_large", inline = TRUE),
      imageOutput("informs_logo", inline = TRUE)))
)


server <- function(input, output, session) {
  col_map <- c(
    "County"         = "County",
    "House District" = "House.district",
    "Senate District"= "Senate.district"
  )
  
  circlelist_mh <- list()
  mklist_mh <- list()

  output$mhvillage_all <- downloadHandler(
    filename = function() {
      paste("mhvillage_all", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(mhvillage_df, file, row.names = FALSE)
    }
  )

  output$ctac_logo <- renderImage({
    
    list(src = "www/ctac_logo.png",
         height = 60)
    
  }, deleteFile = F)
  
  output$mhaction_logo <- renderImage({
    
    list(src = "www/mhaction_logo.png",
         height = 50)
    
  }, deleteFile = F)
  
  output$mhaction_logo_large <- renderImage({
    
    list(src = "www/mhaction_logo.png",
         height = 80)
    
  }, deleteFile = F)
  
  output$informs_logo <- renderImage({
    
    list(src = "www/informs_logo.png",
         height = 70)
    
  }, deleteFile = F)
  
  output$leafletMap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -89.4, lat = 40, zoom = 7) %>%
      addPolygons(
        data = house_districts,
        weight = 1,
        opacity = 1,
        color = "green",
        fillOpacity = 0.4,
        label = ~paste0("House District: ", DISTRICT),
        labelOptions = labelOptions(
          style = list("color" = "black", "font-size" = "12px"),
          textOnly = TRUE
        ),
        highlightOptions = highlightOptions(
          weight = 1,
          color = "yellow",
          fillColor =  "orange",
          bringToFront = FALSE
        ),
        group = "House Districts"
      ) %>%
      addPolygons(
        data = senate_districts,
        weight = 1,
        opacity = 1,
        color = "purple",
        fillOpacity = 0.4,
        label = ~paste0("Senate District: ", DISTRICT),
        labelOptions = labelOptions(
          style = list("color" = "black", "font-size" = "12px"),
          textOnly = TRUE
        ),
        highlightOptions = highlightOptions(
          weight = 1,
          color = "yellow",
          fillColor =  "orange",
          bringToFront = FALSE
        ),
        group = "Senate Districts"
      ) %>%
      addLayersControl(
        overlayGroups = c("House Districts", "Senate Districts"),
        options = layersControlOptions(collapsed = TRUE)
      ) %>%
      hideGroup(c("House Districts", "Senate Districts"))
  })
  
  observeEvent(input$layerlist, {
    req(input$layerlist)
    leafletProxy("leafletMap") %>%
      clearMarkers() %>% 
      clearMarkerClusters()
    
    map <- leafletProxy("leafletMap")
    
    for (layer in input$layerlist) {
      if (layer != " ") {
        map <- build_marker_layer(map, layer)
      }
    }
  })
  
   output$infographic1 <- renderPlot({
     build_infographics1(mhvillage_df)
   })

   total_sites_by_county <- mhvillage_df %>%
     filter(!is.na(`Number_of_Sites`)) %>%
     group_by(County) %>%
     summarise(Total_Sites = sum(`Number_of_Sites`, na.rm = TRUE)) %>%
     arrange(desc(Total_Sites))
   
   output$info1 <- downloadHandler(
     filename = function() {
       paste("mhc_counts_", Sys.Date(), ".csv", sep = "")
     },
     content = function(file) {
       write.csv(total_sites_by_name_count, file, row.names = FALSE)
     }
   )
   
# Table tab
  observeEvent(input$main_category, {
    req(input$main_category != "")
    target_col <- col_map[input$main_category]
    
    # Get unique values from the correct column
    subcategory_choices <- sort(unique(mhvillage_df[[target_col]]))
    
    # Update the dropdown
    updateSelectInput(session, "sub_category", choices = subcategory_choices)
  })
  
  output$sub_category_ui <- renderUI({
    selectInput("sub_category", "Select County/District Boundary:", choices = NULL)
  })
    
  
  # make the sub df of 3 cols based on main_category
  reactive_site_list <- reactive({
    req(input$main_category,input$sub_category)

    target_col <- col_map[input$main_category]

    df <- mhvillage_df %>%
      filter(!!sym(target_col) == (input$sub_category)) %>%
      select(Name, 'Number of Sites' = Number_of_Sites, 'Address' = full_address) %>%
      arrange(desc('Number of Sites'))

    return(df)
  })

  # Update table based on selected inputs
  output$site_list <- renderTable({
    req(input$sub_category)
    reactive_site_list()
  })

  output$site_list_summary <- renderTable({
    df <- reactive_site_list()
    if (nrow(df) > 0) {
      summary_df <- data.frame(
        "Number of MHC's" = nrow(df),
        "Total Sites" = as.integer(sum(df$`Number of Sites`, na.rm = TRUE))
      )
    } else {
      summary_df <- data.frame(
        "Number of MHC's" = 0,
        "Total Sites" = 0
      )
    }
    summary_df %>%
      rename_with(~ gsub("\\.", " ", .))
  })


  
  
  # download available
  {
  output$site_list_download <- downloadHandler(
    filename = function() {
      paste("site_list_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(reactive_site_list(), file, row.names = FALSE)
    }
  )

  output$mhvillage_raw <- downloadHandler(
    filename = "mhvillage_raw.csv",
    content = function(file) {
      write.csv(mhvillage_df, file, row.names = FALSE)
    }
  )

  # output$house_geojson <- downloadHandler(
  #   filename = "house.json",
  #   content = function(file) {
  #     write_json(house_districts, file, row.names = FALSE)
  #   }
  # )
  # 
  # output$senate_geojson <- downloadHandler(
  #   filename = "senate.json",
  #   content = function(file) {
  #     write_json(senate_districts, file, row.names = FALSE)
  #   }
  # )
  }

}

shinyApp(ui = ui, server = server)