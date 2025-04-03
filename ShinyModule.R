library('move2')
library('move')
library('ggmap')
library('adehabitatHR')
library('shiny')
library('shinycssloaders')
library('fields')
library('scales')
library('lubridate')
library('rgeos')
library('zip')
library("shinyBS")

## ToDo:
## final product:
# - have the MCPs + tracks displayed on a leaflet map - this substitutes the current one
# - be able to adjust the %, exactly like in the option "Percentage of points the MCP should overlap" in the current version
# - create a table with an area per track similar to the current one,
# - enable downloading the MCP as a GeoJSON, kmz, and GeoPackage (GPKG) - all possible to do via sf::st_write() - discart the current options
# - optional: enable downloading map as a png, probably with webshot or similar. Not sure how straight forward this is
## additional changes that are needed
# replace functions of rgeos, as this library is deprecated
# if leaflet turns out very complicated, use same approach as in OSM App, so no API-key is needed
## a more current MCP code: in the script "mcp_code.R"




shinyModuleUserInterface <- function(id, label) {
  ns <- NS(id)
  
  tagList(
    titlePanel("Minimum Convex Polygon(s) on a Map"),
    sidebarLayout(
      sidebarPanel(
        sliderInput(inputId = ns("num"), 
                    label = "Choose a margin size", 
                    value = 0.001, min = 0, max = 30, step=0.01),
        sliderInput(inputId = ns("perc"), 
                    label = "Percentage of points the MCP should overlap", 
                    value = 95, min = 0, max = 100),
        sliderInput(inputId = ns("zoom"), 
                    label = "Resolution of background map", 
                    value = 5, min = 3, max = 18, step=1),
        bsTooltip(id=ns("zoom"), title="Zoom of background map (possible values from 3 (continent) to 18 (building)). Depending on the data, high resolutions might not be possible.", placement = "bottom", trigger = "hover", options = list(container = "body")),
        textInput(ns("api"), "Enter your stadia API key. (This is required until MoveApps provides its OSM mirror. Register with stamen, it is free: https://stadiamaps.com/stamen/onboarding/create-account", value = ""),
        downloadButton(ns("act"),"Save map"),
        downloadButton(ns("act2"),"Save MCP as shapefile")
        ,width = 2),
      mainPanel(
        withSpinner(plotOutput(ns("map"),height="85vh"))
      ,width = 10)
    )
  )
}

shinyModule <- function(input, output, session, data) {
  current <- reactiveVal(data)
    
  n.all <- length(mt_time(data))
  data <- data[!duplicated(paste0(round_date(mt_time(data), "1 mins"), mt_track_id(data))),]
  logger.info(paste0("For better performance, the data have been thinned to max 1 minute resolution. From the total ",n.all," positions, the algorithm retained ",length(mt_time(data))," positions for calculation."))
  
  # exclude all individuals with less than 5 locations
  data5 <- data[mt_track_id(data) %in% names(which(table(mt_track_id(data))>=5)),]
  if (any(table(mt_track_id(data))<5)) logger.info(paste("It is only possible to calculate Minimum Convex Polygons for tracks with at least 5 locations. In your data set the individual(s):",names(which(table(mt_track_id(data))<5)),"do not fulfill this requirement and are removed from the MCP analysis. They are still available in the output data set that is passed on to the next App."))
  
  mcpmap.re <- reactiveVal()
  mcpgeo.data.re <- reactiveVal()
  
  output$map <- renderPlot({
  data5_move1 <- moveStack(to_move(data5))
  data.sp <- move2ade(data5_move1)
  data.spt <- spTransform(data.sp,CRSobj=paste0("+proj=aeqd +lat_0=",round(mean(coordinates(data5_move1)[,2]),digits=1)," +lon_0=",round(mean(coordinates(data5_move1)[,1]),digits=1)," +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
    
  data.geo.all <- as.data.frame(data5_move1) #hier trackId nur für MoveStacks, oben gezwungen
  names(data.geo.all) <- make.names(names(data.geo.all),allow_=FALSE)
  if(!any(names(data.geo.all)=="location.long")) data.geo.all$location.long <- data.geo.all$coords.x1
  if(!any(names(data.geo.all)=="location.lat")) data.geo.all$location.lat <- data.geo.all$coords.x2
  data.geo <- data.geo.all[,c("location.long","location.lat","trackId")] #track is already a valid name (validNames()), so no need to adapt; note that track comes from the move2 object

  mcp.data <- mcp(data.spt,percent=input$perc,unin="m",unout="km2")  #mcp() need at least 5 locations per ID, is projected to aeqd in metre!
  mcpgeo.data <- spTransform(mcp.data,CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +towgs84=0,0,0")) 

  if(input$api=="") logger.info("no API key entered") else register_stadiamaps(input$api)
  
  # map <- get_map(bbox(extent(data5)+c(-input$num,input$num,-input$num,input$num)),source="osm",force=TRUE,zoom=input$zoom)
  map <- get_stadiamap(bbox(extent(data5_move1)+c(-input$num,input$num,-input$num,input$num)),source="stamen_terrain",force=TRUE,zoom=input$zoom)

  mcpmap <- ggmap(map) +
      geom_point(data=data.geo, 
                 aes(x=location.long, y=location.lat, col=trackId,shape='.'),show.legend=FALSE) +
      geom_path(data=data.geo, 
                 aes(x=location.long, y=location.lat, col=trackId),show.legend=FALSE) +
      geom_polygon(data=fortify(mcpgeo.data),
                     aes(long,lat,colour=id,fill=id),
                     alpha=0.3) +
      theme(legend.justification = "top") +
      labs(x="Longitude", y="Latitude") +
      scale_fill_manual(name="Animal", values=tim.colors(length(namesIndiv(data5_move1))),aesthetics=c("colour","fill"))
    
  mcp.data.df <- data.frame(mcp(data.spt,percent=input$perc,unin="m",unout="km2"))
  mcp.data.df$area <- round(mcp.data.df$area,digits=3)
  names(mcp.data.df)[2] <- paste0("area (km2) - ",input$perc,"% MCP")
  write.csv(mcp.data.df,file=appArtifactPath("MCP_areas.csv"),row.names=FALSE)
  #write.csv(mcp.data.df,"MCP_areas.csv",row.names=FALSE)
  
  mcpgeo.data.re(mcpgeo.data)
  mcpmap.re(mcpmap)
  
  print(mcpmap)
  })
  
  output$act <- downloadHandler(
    filename="MCP_map.png",
    content = function(file) {
      png(file)
      print(mcpmap.re())
      dev.off()
    }
  )
  
  output$act2 <- downloadHandler(
    filename="MCP_shapefile.zip", # for the browser / user
    content = function(file) {
      # file: is a file path (string) of a nonexistent temp file, and writes the content to that file path
      
      # our working directory
      temp_shp <- tempdir()
      writeOGR(mcpgeo.data.re(),dsn=temp_shp,layer="mcp",driver="ESRI Shapefile",overwrite_layer=TRUE)
      # zip everything in our temp working directory and store the result in the expected file target
      zip::zip(
        zipfile=file, # write into the file the shiny download-handler expects it
        files = list.files(temp_shp,"mcp",full.names=TRUE), # list all files matching the given pattern 'mcp'
        mode = "cherry-pick"
      )
      # provide the generated zip file also as an app artefact
      #file.copy(file, file=appArtifactPath("MCP_shapefile-artifact.zip"))
    }
  )
  
  return(reactive({ current() }))
}
