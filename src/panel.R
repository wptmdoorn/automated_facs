library(rjson)

.graph_settings <- fromJSON(file = "src/plot.json")

get_panel_information <- function(panel_name) {
    if (!(panel_name %in% names(.graph_settings$panels))) {
        stop("Panel not found in graph setting - please recheck spelling
              or otherwise add panel to plot.json")
    }

    .graph_settings$panels[[panel_name]]
}

get_panel_plot_list <- function(panel_name) {
    .f <- get_panel_information(panel_name)$layout

    result <- rapply(.f, function(x) paste0(x[1], " vs ", x[2]))
    result[result != " vs NA"]
}
