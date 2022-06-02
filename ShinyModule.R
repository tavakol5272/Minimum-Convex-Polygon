library('move')
library('ggmap')
library('adehabitatHR')
library('shiny')
library('fields')
library('scales')
library('lubridate')

shinyModuleUserInterface <- function(id, label, num=0.001, perc=95, zoom=10) {
  ns <- NS(id)

  tagList(
    titlePanel("Minimum Convex Polygon(s) on a Map"),
    sliderInput(inputId = ns("perc"), 
                label = "Percentage of points the MCP should overlap", 
                value = perc, min = 0, max = 100),
    sliderInput(inputId = ns("zoom"), 
                label = "Zoom of background map (possible values from 3 (continent) to 18 (building)). \n Depending on the data, high resolutions might not be possible.", 
                value = zoom, min = 3, max = 18, step=1),
    plotOutput(ns("map"),height="80vh"),
    downloadButton(ns("act"),"Save map")
  )
}

shinyModuleConfiguration <- function(id, input) {
  ns <- NS(id)
  
  configuration <- list()
  
  configuration
}

shinyModule <- function(input, output, session, data, num, perc, zoom) {
  current <- reactiveVal(data)
    
  n.all <- length(timestamps(data))
  data <- data[!duplicated(paste0(round_date(timestamps(data), "1 mins"), trackId(data))),]
  logger.info(paste0("For better performance, the data have been thinned to max 1 minute resolution. From the total ",n.all," positions, the algorithm retained ",length(timestamps(data))," positions for calculation."))
  
  # exclude all individuals with less than 5 locations
  data5 <- moveStack(data[[which(n.locs(data)>=5)]])
  if (any(n.locs(data)<5)) logger.info(paste("It is only possible to calculate Minimum Convex Polygons for tracks with at least 5 locations. In your data set the individual(s):",names(which(n.locs(data)<5)),"do not fulfill this requirement and are removed from the MCP analysis. They are still available in the output data set that is passed on to the next App."))
  
  data.sp <- move2ade(data5)
  data.spt <- spTransform(data.sp,CRSobj=paste0("+proj=aeqd +lat_0=",round(mean(coordinates(data5)[,2]),digits=1)," +lon_0=",round(mean(coordinates(data5)[,1]),digits=1)," +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
    
  data.geo.all <- as.data.frame(data5) #hier trackId nur für MoveStacks, oben gezwungen
  names(data.geo.all) <- make.names(names(data.geo.all),allow_=FALSE)
  data.geo <- data.geo.all[,c("location.long","location.lat","trackId")] #trackId is already a valid name (validNames()), so no need to adapt

  mcp.data <- reactive({ mcp(data.spt,percent=input$perc,unin="m",unout="km2") }) #mcp() need at least 5 locations per ID
  mcpgeo.data <- reactive({ spTransform(mcp.data(),CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +towgs84=0,0,0")) })
    
  map <- reactive({ get_map(bbox(extent(data5)+c(-num,num,-num,num)),source="osm",force=TRUE,zoom=input$zoom) })

  mcpmap <- reactive({
    out <- ggmap(map()) +
      geom_point(data=data.geo, 
                 aes(x=location.long, y=location.lat, col=trackId,shape='.'),show.legend=FALSE) +
      geom_path(data=data.geo, 
                 aes(x=location.long, y=location.lat, col=trackId),show.legend=FALSE) +
      geom_polygon(data=fortify(mcpgeo.data()),
                     aes(long,lat,colour=id,fill=id),
                     alpha=0.3) +
      theme(legend.justification = "top") +
      labs(x="Longitude", y="Latitude") +
      scale_fill_manual(name="Animal", values=tim.colors(length(namesIndiv(data5))),aesthetics=c("colour","fill"))
    out
  })
    
  mcp.data.df <- data.frame(mcp(data.spt,percent=perc,unin="m",unout="km2"))
  mcp.data.df$area <- round(mcp.data.df$area,digits=3)
  names(mcp.data.df)[2] <- paste0("area (km2) - ",perc,"% MPC")
  write.csv(mcp.data.df,paste0(Sys.getenv(x = "APP_ARTIFACTS_DIR", "/tmp/"),"MCP_areas.csv"),row.names=FALSE)
  #write.csv(mcp.data.df,"MCP_areas.csv",row.names=FALSE)

 
  output$act <- downloadHandler(
    filename="MCP_map.png",
    content = function(file) {
      png(file)
      print(mcpmap())
      dev.off()
    }
  )
  
  output$map <- renderPlot({
    print(mcpmap())
  })
  
  return(reactive({ current() }))
}
