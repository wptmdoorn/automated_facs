library(ggcyto)
library(flowCore)
library(flowWorkspace)
library(gridExtra)
library(grid)

source("src/plot.R", local = plot <- new.env())
source("src/data.R", local = dataenv <- new.env())


function(input, output) {
    data <- reactive({
        print("data reactive called")
        dataenv$read_and_compensate_data("data/fcs/A.fcs")
    })

    output$mdsTube1 <- renderPlot({
        plot$get_plots_for_shiny("MDS_01", data())
    })

    output$binPlotOutput <- renderPlot({
        print("called!!!")
        print(is.null(input$binPlot))
        if (is.null(input$binPlot)) {
            return()
        }

        print("selected!!")
        print(input$binPlot)

        .strings <- strsplit(input$binPlot, " vs ")[[1]]

        plot$bin_example_plot(data(),
            xstring = .strings[2],
            ystring = .strings[1]
        )
    })
}
