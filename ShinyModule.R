library('move')
library('ggmap')
library('adehabitatHR')
library('shiny')
library('fields')
library('scales')
library('lubridate')

shinyModuleUserInterface <- function(id, label, num=0.001, perc=95) {
  ns <- NS(id)

  tagList(
    titlePanel("Minimum Convex Polygon(s) on a Map"),
    sliderInput(inputId = ns("perc"), 
                label = "Percentage of points the MCP should overlap", 
                value = perc, min = 0, max = 100),
    plotOutput(ns("map"),height="80vh"),
    actionButton(ns("act"),"Add shapefile to output (does not work yet)") #can change to downloadButton
  )
}

shinyModuleConfiguration <- function(id, input) {
  ns <- NS(id)
  
  configuration <- list()
  
  configuration
}

shinyModule <- function(input, output, session, data, num, perc) {
  current <- reactiveVal(data)
    
  n.all <- length(timestamps(data))
  data <- data[!duplicated(paste0(round_date(timestamps(data), "5 mins"), trackId(data))),]
  logger.info(paste0("For better performance, the data have been thinned to max 5 minute resolution. From the total ",n.all," positions, the algorithm retained ",length(timestamps(data))," positions for calculation."))
    
  data.sp <- move2ade(data)
  data.spt <- spTransform(data.sp,CRSobj=paste0("+proj=aeqd +lat_0=",round(mean(coordinates(data)[,2]),digits=1)," +lon_0=",round(mean(coordinates(data)[,1]),digits=1)," +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
    
  data.geo.all <- as.data.frame(data)
  names(data.geo.all) <- make.names(names(data.geo.all),allow_=FALSE)
  data.geo <- data.geo.all[,c("location.long","location.lat","trackId")] #trackId is already a valid name (validNames()), so no need to adapt

  mcp.data <- reactive({ mcp(data.spt,percent=input$perc,unin="m",unout="km2") })
  mcpgeo.data <- reactive({ spTransform(mcp.data(),CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +towgs84=0,0,0")) })
    
  map <- get_map(bbox(extent(data)+c(-num,num,-num,num)))

  mcpmap <- reactive({
    out <- ggmap(map) +
      geom_point(data=data.geo, 
                 aes(x=location.long, y=location.lat, col=trackId,shape='.'),show.legend=FALSE) +
       geom_polygon(data=fortify(mcpgeo.data()),
                     aes(long,lat,colour=id,fill=id),
                     alpha=0.3) +
      theme(legend.justification = "top") +
      labs(x="Longitude", y="Latitude") +
      scale_fill_manual(name="Animal", values=tim.colors(length(namesIndiv(data))),aesthetics=c("colour","fill"))
    out
  })
    
  mcp.data.df <- data.frame(mcp(data.spt,percent=perc,unin="m",unout="km2"))
  mcp.data.df$area <- round(mcp.data.df$area,digits=3)
  names(mcp.data.df)[2] <- paste0("area (km2) - ",perc,"% MPC")
  write.csv(mcp.data.df,paste0(Sys.getenv(x = "APP_ARTIFACTS_DIR", "/tmp/"),"MCP_areas.csv"),row.names=FALSE)
  #write.csv(mcp.data.df,"MCP_areas.csv",row.names=FALSE)

  observeEvent(input$act, {
    writeOGR(obj=mcpgeo.data(), dsn=Sys.getenv(x = "APP_ARTIFACTS_DIR", "/tmp/"), layer="mcp", 
             driver="ESRI Shapefile", overwrite_layer=TRUE)
  })
  # if downloadButton then have to change this to downloadHandler and save a zip file of the shp files (help: https://stackoverflow.com/questions/47591070/r-download-shapefile-from-a-shiny-app)
    
  output$map <- renderPlot({
    mcpmap()
  })
  
  return(reactive({ current() }))
}
