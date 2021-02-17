library('move')
library('ggmap')
library('adehabitatHR')
library('shiny')
library('fields')
library('scales')
library('lubridate')

shinyModuleUserInterface <- function(id, label, num, perc=95) {
  ns <- NS(id)

  tagList(
    titlePanel("Minimum Convex Polygon(s) on a Map"),
    sliderInput(inputId = ns("perc"), 
                label = "Percentage of points the MCP should overlap", 
                value = perc, min = 0, max = 100),
    plotOutput(ns("map"),height="80vh")
  )
}

shinyModuleConfiguration <- function(id, input) {
  ns <- NS(id)
  
  configuration <- list()
  
  configuration
}

shinyModule <- function(input, output, session, data, num=0.01, perc=95) {
    current <- reactiveVal(data)

    n.all <- length(timestamps(data))
    data <- data[!duplicated(paste0(round_date(timestamps(data), "5 mins"), trackId(data))),]
    logger.info(paste0("For better performance, the data have been thinned to max 5 minute resolution. From the total ",n.all," positions, the algorithm retained ",length(timestamps(data))," positions for calculation."))
    
    data.sp <- move2ade(data)
    data.spt <- spTransform(data.sp,CRSobj=paste0("+proj=aeqd +lat_0=",round(mean(coordinates(data)[,2]),digits=1)," +lon_0=",round(mean(coordinates(data)[,1]),digits=1)," +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
    
    data.geo <- as.data.frame(data)[,c("location_long","location_lat","local_identifier")]
    data.geo$local_identifier <- gsub(" ",".",data.geo$local_identifier)
    
    mcp.data <- reactive({ mcp(data.spt,percent=input$perc,unin="m",unout="km2") })
    mcpgeo.data <- reactive({ spTransform(mcp.data(),CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +towgs84=0,0,0")) })
    id <- reactive({ mcp.data()$id  })
    cols <- reactive({ tim.colors(length(id())) })
      
    map <- get_map(bbox(extent(data)+c(-num,num,-num,num)))
    
    mcpmap <- reactive({
      out <- ggmap(map) +
        geom_polygon(data=fortify(mcpgeo.data()),
                     aes(long,lat,colour=id,fill=id),
                     alpha=0.3) +
        geom_point(data=data.geo,
                   aes(x=location_long, y=location_lat, colour=factor(local_identifier),shape='.'),show.legend=FALSE) +
        theme(legend.position=c(0.15,0.80)) +
        labs(x="Longitude", y="Latitude") +
        scale_fill_manual(name="Animal", values=cols(), breaks=id()) +
        scale_colour_manual(name="Animal",values=cols(), breaks=id())
      out
    })
    
    mcp.data.df <- data.frame(mcp(data.spt,percent=perc,unin="m",unout="km2"))
    mcp.data.df$area <- round(mcp.data.df$area,digits=3)
    names(mcp.data.df)[2] <- "area (km2)"
    write.csv(mcp.data.df,paste0(Sys.getenv(x = "APP_ARTIFACTS_DIR", "/tmp/"),"MCP_areas.csv"),row.names=FALSE)

  output$map <- renderPlot({
    mcpmap()
  })
  
  return(reactive({ current() }))
}

