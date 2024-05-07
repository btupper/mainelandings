#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
suppressPackageStartupMessages({
  library(shiny)
  library(DT)
  library(dplyr)
  library(rlang)
  library(readr)
  library(ggplot2)
})

#' Create a pretty set of plotting breaks
#' @param n number of intervals
#' @param ... for \code{pretty}
#' @return a function for computing pretty breaks
integer_breaks <- function(n = 5, ...) {
    fxn <- function(x) {
        breaks <- floor(pretty(x, n, ...))
        names(breaks) <- attr(breaks, "labels")
        breaks
    }
    return(fxn)
}

#' List the newest by name CSV file
#' @param path the path to the files
#' @return filename
most_recent_csv <- function(path = "."){
    ff <- list.files(pattern = "^.*\\.csv", full.names = TRUE)
    ff[length(ff)]
}
#' Read inn the data set
#' @param filename the name of the file to read
#' @return tibble
read_data <- function(filename = most_recent_csv()){
 readr::read_csv(filename,
                 col_types = readr::cols(
                   year = readr::col_double(),
                   species = readr::col_character(),
                   port = readr::col_character(),
                   county = readr::col_character(),
                   lob_zone = readr::col_character(),
                   weight_type = readr::col_character(),
                   weight = readr::col_double(),
                   value = readr::col_double(),
                   trip_n = readr::col_double(),
                   harv_n = readr::col_double()
                 )) |>
        dplyr::filter(!is.na(species)) |>
        dplyr::arrange(port)
}
DATA <- read_data()
ALL <- rep(TRUE, nrow(DATA))
LOBSTER <- DATA$species == "Lobster American" 


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Maine Marine Commerical Landings"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectizeInput("port",
                        "Port:",
                        choices = DATA$port,
                        selected = 1),
            radioButtons("lobster",
                         "Include lobster?",
                         choices = c("yes", "no")),
            radioButtons("by", 
                         "Show by Weight or Value:",
                         choices = c("weight", "value")),
            radioButtons("transform",
                         "Data Transform:",
                         choices = c("none", 
                                     "per trip", 
                                     "per harvester"
                                     #"per harvester per trip"
                                     ))
        ),
        

        # Show a plot of the generated distribution
        mainPanel(
          DT::DTOutput("table"),
          plotOutput("plot")
        )
        
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$table <- renderDataTable(
        DATA |> 
          dplyr::filter(port == input$port, 
                        if(input$lobster == "no"){
                          !LOBSTER
                        } else {
                          ALL
                        }),
        options = list(paging = TRUE, pageLength = 5)
    )
    output$plot <- renderPlot({

        x <- DATA |> 
            dplyr::filter(port == input$port, 
                          if(input$lobster == "no"){
                            !LOBSTER
                          } else {
                            ALL
                          }) |>
            dplyr::mutate(data = switch(input$transform,
                          "per trip" = .data[[!!input$by]]/.data$trip_n,
                          "per harvester" = .data[[!!input$by]]/.data$harv_n,
                          #"per harvester per trip" = .data[[!!input$by]]/.data$harv_n/.data$trip_n,
                          .data[[!!input$by]]))
        
        
        ylab = paste(gsub("\\b(\\w)",    "\\U\\1", input$by, perl=TRUE), 
                     switch(input$transform,
                            "per trip" = "per trip",
                            "per harvester" = "per harvester",
                            #"per harvester per trip" = "per harvester per trip",
                            ""))
        
        ylab <- switch(input$by,
                       "none" = ylab,
                       "weight" = sprintf("%s (%s)", ylab, x$weight_type[1]),
                       "value" = sprintf("%s ($)", ylab))
        
        p <- ggplot(data = x, aes(x = year, y = data, shape = species)) + 
            geom_point(size = 2) + 
            scale_x_continuous(breaks = integer_breaks()) + 
            scale_y_continuous(labels = scales::comma) + 
            expand_limits(y = 0) + 
            labs(title = input$port,
                 caption = "https://mainedmr.shinyapps.io/Landings_Portal",
                 x = 'Year',
                 y = ylab) + 
            scale_shape_manual(values=seq(0,15)) 
        print(p)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
