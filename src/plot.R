library(ggplot2)
library(rjson)
library(grid)
library(gridExtra)

source("src/panel.R", local = panel <- new.env())
source("src/bin.R", local = bin <- new.env())
source("src/data.R", local = dataenv <- new.env())

.label_at <- function(n) function(x) ifelse(x %% n == 0, x / 1000, "")

get_plots_for_shiny <- function(panel_name, data) {
    .plots <- panel$get_panel_information(panel_name)

    plots <- list()
    i <- 1

    for (op in .plots$layout) {
        for (ip in op) {
            if (length(ip) == 1 && ip == "") {
                plots[[i]] <- grid.rect(gp = gpar(col = "white"))
                i <- i + 1
            } else {
                plots[[i]] <- auto_plot(data,
                    xstring = ip[2], ystring = ip[1]
                ) +
                    theme(plot.margin = unit(
                        c(-0.30, 0, 0, 0),
                        "null"
                    ))
                i <- i + 1
            }
        }
    }

    grid.arrange(grobs = plots, ncol = .plots$ncol, nrow = .plots$nrow)
}

auto_plot <- function(data, xstring, ystring) {
    if (!(xstring %in% names(panel$.graph_settings$layers)) ||
        !(ystring %in% names(panel$.graph_settings$layers))) {
        stop("Layer not found in graph setting - please recheck spelling
              or otherwise add layer to plot.json")
    }

    .xlayer <- panel$.graph_settings$layers[[xstring]]
    .ylayer <- panel$.graph_settings$layers[[ystring]]

    # base graph object
    g <- ggcyto::ggcyto(data, aes(
        x = !!sym(xstring), y = !!sym(ystring),
    )) + geom_point()

    # add x and y limits
    g <- g + ggcyto::ggcyto_par_set(limits = list(
        x = if (.xlayer$scale == "linear") c(0, 260000) else c(10, 100000),
        y = if (.ylayer$scale == "linear") c(0, 260000) else c(10, 100000)
    ))

    # add scales
    if (.xlayer$scale == "linear") {
        g <- g + scale_x_continuous(
            breaks = seq(0, 260000, 10000),
            labels = .label_at(50000),
            expand = c(0, 0)
        )
    } else {
        g <- g + ggcyto::scale_x_flowjo_fasinh()
    }

    if (.ylayer$scale == "linear") {
        g <- g + scale_y_continuous(
            breaks = seq(0, 260000, 10000),
            labels = .label_at(50000),
            expand = c(0, 0)
        )
    } else {
        g <- g + ggcyto::scale_y_flowjo_fasinh()
    }

    # general theme additions
    g <- g + theme_bw() +
        theme(
            aspect.ratio = 1,
            plot.margin = unit(c(-0.30, 0, 0, 0), "null")
        ) +
        facet_null()

    as.ggplot(g)
}

bin_example_plot <- function(data, xstring, ystring) {
    before <- auto_plot(data, xstring, ystring)

    # scale data if neccessary
    .xlayer <- panel$.graph_settings$layers[[xstring]]
    .ylayer <- panel$.graph_settings$layers[[ystring]]

    if (.xlayer$scale == "fasinh" || .ylayer$scale == "fasinh") {
        data <- dataenv$transform_data(data)
    }

    .data <- as.data.frame(data@exprs[, c(xstring, ystring)])

    # get bins
    binned_data <- bin$custom_count_bin(.data, bins = c(100, 100))

    after <- ggplot(binned_data, aes(x = x, y = y, fill = z)) +
        geom_tile() +
        scale_fill_gradientn(
            trans = "log",
            colors = c("white", "orange", "red")
        ) +
        xlab("Bins (x)") +
        ylab("Bins (y)") +
        theme_bw() +
        theme(
            aspect.ratio = 1,
            plot.margin = unit(c(-0.30, 0, 0, 0), "null")
        )

    grid.arrange(before, after, ncol = 2)
}
