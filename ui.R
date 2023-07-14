source("src/panel.R", local = panel <- new.env())

fluidPage(
    titlePanel("Automated flow cytometry"),
    sidebarLayout(
        sidebarPanel(
            selectInput(
                inputId = "selectedFile",
                label = "Select FACS file",
                choices = list.files("data/fcs",
                    pattern = ".fcs",
                    full.names = FALSE
                )
            ),
            hr(),
            hr(style = "border-top: 1px solid #000000;"),
            hr(),
            checkboxInput(
                inputId = "applyDensity",
                label = "Apply density filter",
                value = FALSE
            ),
        ),
        mainPanel(
            tabsetPanel(
                type = "tabs",
                tabPanel(
                    "MDS-1",
                    br(),
                    plotOutput("mdsTube1", height = "950px"),
                ),
                tabPanel(
                    "Binning",
                    br(),
                    selectInput("binPlot",
                        "Select plot to bin",
                        choices = panel$get_panel_plot_list("MDS_01")
                    ),
                    hr(),
                    plotOutput("binPlotOutput", height = "950px")
                ),
                tabPanel("Gating", HTML("None"))
            )
        )
    )
)
