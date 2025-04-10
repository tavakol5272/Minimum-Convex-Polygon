library(move2)
library(move)
library(ggmap)
library(adehabitatHR)
library(shiny)
library(shinycssloaders)
library(fields)
library(scales)
library(lubridate)
library(zip)
library(shinyBS)
library(sf)
library(mapview)
library(pals)
library(leaflet)
library(leaflet.extras)
library(htmlwidgets)
library(webshot2)
library(dplyr)


shinyModuleUserInterface <- function(id, label) {
  ns <- NS(id)
  
  tagList(
    titlePanel("Minimum Convex Polygon(MCP) on a Map"),
    sidebarLayout(
      sidebarPanel(
        
        sliderInput(ns("perc"), 
                    "Percentage of points the MCP should overlap",  
                    min = 0, max = 100,value = 90, width = "100%" ),
        
        checkboxGroupInput(ns("animal_selector"), "Select Animal:", choices = NULL),
        downloadButton(ns("save_html"),"Save map as HTML", class = "btn-sm"),
        downloadButton(ns("save_png"), "save Map as PNG", class = "btn-sm"),
        downloadButton(ns("download_geojson"), "Download MCP as GeoJSON", class = "btn-sm"),
        downloadButton(ns("download_kmz"), "Download MCP as KMZ", class = "btn-sm"),
        downloadButton(ns("download_gpkg"), "Download MCP as GPKG", class = "btn-sm"),
        downloadButton(ns("download_mcp_table"), "Download MCP Table (CSV)", class = "btn-sm"),
        
      
        ,width = 4
      ),
      
      mainPanel(
        leafletOutput(ns("leafmap"), height = "85vh")
        ,width = 12)
    )
  )
}



#####server######

shinyModule <- function(input, output, session, data) {
  # all IDs of UI functions need to be wrapped in ns()
  ns <- session$ns
  current <- reactiveVal(data)
  dataObj <- reactive({ data })
  
 
  # exclude all individuals with less than 5 locations
  data_filtered <- reactive({
    req(data)
    #filter_ids <- names(which(table(mt_track_id(data)) >= 5))  
    #data[mt_track_id(data) %in% filter_ids, ]
    data %>%
      group_by(individual_name_deployment_id) %>%
      filter(n() >= 5) %>%
      ungroup()
    
    
    #mt_as_move2(filtered, time_column = "timestamp", track_id_column = "individual_name_deployment_id")
  })
  
  ##select desired animal among animal with more than 5 location
  observe({
    req(data_filtered())
    animal_choices <- unique(mt_track_id(data_filtered()))
    updateCheckboxGroupInput(session = session,
                             inputId = "animal_selector",
                             choices = animal_choices,
                             selected = animal_choices)  })
  
  selected_data <- reactive({
    req(input$animal_selector)
    df <- data_filtered()
    selected <- df[mt_track_id(df) %in% input$animal_selector, ]
    validate(need(nrow(selected) > 0, "No data for selected animals."))
    selected
  })
  
 
  
  
  # Compute the MCP for the filtered tracks
  mcp_cal <- reactive({
    req(input$perc)
    data_sel <- selected_data()
    #print(class(data_sel))
    move_obj <- mt_as_move2(
      data_sel,
      time_column = "timestamp",  
      track_id_column = "individual_name_deployment_id"  
    )
    print(class(move_obj))
    move_stack <- to_move(move_obj)
    
    
    sf_data <- st_as_sf(move2ade(move_stack))
    #sf_data <- st_as_sf(move2ade(move_stack))
    print(class(sf_data))
    crs_proj <- mt_aeqd_crs(sf_data, center = "centroid", units = "m")
    #sf_data_proj <- st_transform(sf_data, crs_proj)
    sf_data_proj <- st_transform(sf_data, 4326)
    print(class(sf_data_proj))
    sp_data_proj <- as(sf_data_proj, "Spatial")
    print(class(sp_data_proj))
    sf_data_proj$id = sf_data$individual_name_deployment_id
    
    data_mcp <- tryCatch({
      adehabitatHR::mcp(sp_data_proj, percent = input$perc, unin = "m", unout = "km2")
    }, error = function(e) {
      warning("MCP computation failed: ", e)
      return(NULL)
    })
    #data_mcp <- adehabitatHR::mcp(sp_data_proj, percent = input$perc, unin = "m", unout = "km2")
    
    print(class(data_mcp))
    
    if (!is.null(data_mcp)) {
      if (any(data_mcp@data$area == 0)) {
        warning("Some MCPs have zero area.")
      }
    }
    
    
    return(list(data_mcp = data_mcp,track_lines = mt_track_lines(data_sel)))
    
  })
  
  
  
  ##leaflet map####
  mmap <- reactive({
      req(mcp_cal())
      mcp <- mcp_cal()
      bounds <- as.vector(st_bbox(dataObj()))
      
      # Safely handle if data_mcp is NULL
      #if (is.null(mcp$data_mcp)) {
        # Return an empty map or a map with just the base tiles
        #return(leaflet() %>%
                 #fitBounds(bounds[1], bounds[2], bounds[3], bounds[4]) %>%
                 #addTiles())
      #}
      
      # Safely handle cases where there are no valid polygons
      #if (length(mcp$data_mcp) == 0) {
        #return(leaflet() %>%
                 #fitBounds(bounds[1], bounds[2], bounds[3], bounds[4]) %>%
                 #addTiles())
      #}
      
      
      
      sf_mcp <- st_as_sf(mcp$data_mcp)
      track_lines <- mcp$track_lines
      #ids <- unique(mt_track_id(track_lines))
      ids <- unique(mcp$track_ids)
      pal <- colorFactor(palette = pals::cols25(), domain = ids)
      
      leaflet(options = leafletOptions(minZoom = 2)) %>% 
          fitBounds(bounds[1], bounds[2], bounds[3], bounds[4]) %>%       
          addTiles() %>%
          addProviderTiles("Esri.WorldTopoMap", group = "TopoMap") %>%
          addProviderTiles("Esri.WorldImagery", group = "Aerial") %>%
          addTiles(group = "Default") %>%
          addScaleBar(position = "topleft") %>%
          
          #addPolylines(data = track_lines,color = ~pal(mt_track_id(track_lines)),weight = 2,group = "Tracks") %>%
          addPolylines(data = track_lines, color = ~pal(track_lines$individual_name_deployment_id),
                     weight = 3, group = "Tracks") %>%
          addPolygons(data = sf_mcp, fillColor = ~pal(rownames(sf_mcp)),color = "black",fillOpacity = 0.4,
          weight = 2,label = ~rownames(sf_mcp) ,group = "MCPs") %>%
        
          addLegend(position = "topleft",pal = pal,values = ids,title = "Animal") %>%
        
          addLayersControl(
              baseGroups = c("Default", "TopoMap", "Aerial"),
              overlayGroups = c("Tracks", "MCPs"),
              options = layersControlOptions(collapsed = FALSE)
          )
     })
                 
  output$leafmap <- renderLeaflet({mmap()})
  
  ###download the table of mcp
  output$download_mcp_table <- downloadHandler(
    filename = function() {
      paste0("mcp_table_", Sys.Date(), ".csv")
    },
    content = function(file) {
      mcp <- mcp_cal()$data_mcp
      
      # Check if mcp is NULL or has no rows
      if (is.null(mcp) || length(mcp) == 0) {
        # Create a data frame with a message
        df <- data.frame(message = "No MCP data available.")
      } else {
        # Convert to data frame safely
        mcp_df <- as.data.frame(mcp)
        
        # Prepare the data frame with TrackID and Area_km2
        df <- data.frame(TrackID = rownames(mcp_df), Area_km2 = round(mcp_df$area, 3))
      }
      
      write.csv(df, file, row.names = FALSE)
    }
  )

  ### save map as HTML
  output$save_html <- downloadHandler(
    filename = "LeafletMap.html",
    content = function(file) {
      saveWidget(widget = mmap(),file=file) })
  
  ### save map as PNG
  output$save_png <- downloadHandler(
    filename = "LeafletMap.png",
    content = function(file) {
      temp_html <- tempfile(fileext = ".html")
      saveWidget(mmap(), file = temp_html, selfcontained = TRUE)
      webshot2::webshot(url = temp_html, file = file, vwidth = 1000, vheight = 800) })
    
  
  ###download shape as kmz  
  output$download_kmz <- downloadHandler(
    filename = "MCP_shapefile.kmz",
    content = function(file) {
      temp_kmz <- tempdir()
      mcp_shape <- st_as_sf(mcp_cal()$data_mcp)
      kml_path <- file.path(temp_kmz, "mcp.kml")
      st_write(mcp_shape, kml_path, driver="KML", delete_dsn = TRUE)
      zip::zip(zipfile = file, files = kml_path, mode = "cherry-pick")})
  
  ###download shape as GeoJSON
  output$download_geojson <- downloadHandler(
    filename = "MCP_shape.geojson",
    content = function(file) {
      mcp_shape <- st_as_sf(mcp_cal()$data_mcp)
      st_write(mcp_shape, file, driver = "GeoJSON", delete_dsn = TRUE) } )
  
  ###download shape as GeoPackage (GPKG)
  output$download_gpkg <- downloadHandler(
    filename = "MCP_shape.gpkg",
    content = function(file) {
      mcp_shape <- st_as_sf(mcp_cal()$data_mcp)
      st_write(mcp_shape, file, driver = "GPKG", delete_dsn = TRUE)} )
  
  return(reactive({ current() }))
}



