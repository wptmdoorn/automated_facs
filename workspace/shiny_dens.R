library(shiny)
library(ggplot2)
library(flowCore)
library(flowViz)
library(flowDensity)

s <- 'granu <- flowDensity(
    obj = singlets, channels = c("V500-A", "SSC-A"),
    position = c(FALSE, TRUE),
    percentile = c(.75, .30), use.percentile = c(T, T),
)

granu2 <- flowDensity(
    obj = granu, channels = c("V500-A", "SSC-A"),
    position = c(TRUE, FALSE)
)'

ui <- fluidPage(
    titlePanel("Flow Cytometry Analysis"),
    sidebarLayout(
        sidebarPanel(
            fileInput("fcsFile", "Choose an FCS file"),
            textAreaInput("lymphoDefinition", "Lympho Definition", value = s),
            actionButton("runAnalysis", "Run Analysis")
        ),
        mainPanel(
            plotOutput("densityPlot")
        )
    )
)

server <- function(input, output) {
    data <- reactiveVal(NULL)

    observeEvent(input$fcsFile, {
        inFile <- input$fcsFile
        print(inFile)
        if (!is.null(inFile)) {
            data_file <- read.FCS(inFile$datapath)
            comp <- keyword(data_file)[["SPILL"]]

            data_compensated <- compensate(data_file, comp)
            transformList <- estimateLogicle(data_compensated,
                channels = colnames(comp)
            )

            data(transform(data_compensated, transformList))
        }
    })

    output$densityPlot <- renderPlot(
        {
            req(data())

            data <- data()

            channels <- c("V500-A", "SSC-A")

            singlets <- flowDensity(data,
                channels = c("FSC-A", "FSC-H"),
                position = c(FALSE, FALSE),
                percentile = c(0.99999, 0.99999),
                use.percentile = c(TRUE, TRUE),
                ellip.gate = TRUE, scale = 0.99
            )

            lympho_definition <- parse(text = input$lymphoDefinition)
            eval(lympho_definition)

            print("hello!!")

            plotDens(data(), channels = c("V500-A", "SSC-A"))
            points(granu@filter, type = "l", col = "blue", lwd = 2)
            points(granu2@filter, type = "l", col = "red", lwd = 2)
        },
        height = 500,
        width = 500
    )
}

shinyApp(ui, server)
