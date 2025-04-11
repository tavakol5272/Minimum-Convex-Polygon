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
  ns <- session$ns
  current <- reactiveVal(data)
  dataObj <- reactive({ data })
  
  
  # exclude all individuals with less than 5 locations
  data_filtered <- reactive({
    req(data)
    data %>%
      group_by(individual_name_deployment_id) %>%
      filter(n() >= 5) %>%
      ungroup()
  })
  
  ##select animalin slide bar
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
  
  
  
  
  # Compute the MCP 
  mcp_cal <- reactive({
    req(input$perc)
    data_sel <- selected_data()
    #print(class(data_sel))
    
    move_obj <- mt_as_move2(
      data_sel,
      time_column = "timestamp",  
      track_id_column = "individual_name_deployment_id"  
    )
    #print(class(move_obj))
    
    move_stack <- to_move(move_obj)
    #print(class(move_stack))
    
    sf_data <- st_as_sf(move2ade(move_stack))
    #print(class(sf_data))
    
    #crs_proj <- mt_aeqd_crs(sf_data, center = "centroid", units = "m")
    #sf_data_proj <- st_transform(sf_data, crs_proj)
    
    sf_data_proj <- st_transform(sf_data, 4326)
    #print(class(sf_data_proj))
    
    sp_data_proj <- as(sf_data_proj, "Spatial")
    #print(class(sp_data_proj))
    
    sf_data_proj$id = sf_data$individual_name_deployment_id
    
    data_mcp <- adehabitatHR::mcp(sp_data_proj, percent = input$perc, unin = "m", unout = "km2")
    #print(class(data_mcp))
    
    return(list(data_mcp = data_mcp,track_lines = mt_track_lines(data_sel)))
    
  })
  
  
  
  ##leaflet map####
  mmap <- reactive({
    req(mcp_cal())
    mcp <- mcp_cal()
    print(mcp)
    bounds <- as.vector(st_bbox(dataObj()))
    
    sf_mcp <- st_as_sf(mcp$data_mcp)
    track_lines <- mcp$track_lines
    ids <- unique(mcp$track_ids)
    pal <- colorFactor(palette = pals::cols25(), domain = ids)
    
    leaflet(options = leafletOptions(minZoom = 2)) %>% 
      fitBounds(bounds[1], bounds[2], bounds[3], bounds[4]) %>%       
      addTiles() %>%
      addProviderTiles("Esri.WorldTopoMap", group = "TopoMap") %>%
      addProviderTiles("Esri.WorldImagery", group = "Aerial") %>%
      addTiles(group = "Default") %>%
      addScaleBar(position = "topleft") %>%
      
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
    filename = "mcp_table.csv",
    content = function(file) {
      mcp <- mcp_cal()$data_mcp
      mcp_df <- as.data.frame(mcp)
      df <- data.frame(TrackID = rownames(mcp_df), Area = mcp_df$area)
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
