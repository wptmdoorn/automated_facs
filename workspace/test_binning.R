library(ash)
library(ggplot2)
library(gridExtra)
library(ggcyto)
source("src/plot.R")

data <- flowCore::read.FCS("data/fcs/E.fcs")
comp <- flowCore::keyword(data)[["SPILL"]]

data <- flowWorkspace::compensate(
    data,
    comp
)

transformList <- flowCore::estimateLogicle(data, channels = colnames(comp))
data2 <- flowWorkspace::transform(data, transformList)

custom_count_bin <- function(df, bins = c(100, 100)) {
    xrange <- max(df[, 1]) / bins[1]
    yrange <- max(df[, 2]) / bins[2]

    x_bins <- as.integer(cut(df[, 1],
        breaks = seq(0, max(df[, 1]) + xrange, by = xrange),
        include.lowest = TRUE
    ))
    y_bins <- as.integer(cut(df[, 2],
        breaks = seq(0, max(df[, 2]) + yrange, by = yrange),
        include.lowest = TRUE
    ))

    count_matrix <- table(x_bins, y_bins)

    count_matrix
}

bin_data_example <- function(data, bins = c(100, 100)) {
    binned_data <- custom_count_bin(data, bins = bins)
    # Convert matrix to data frame for ggplot
    df_plot <- as.data.frame(as.table(binned_data))

    # Rename the variables
    names(df_plot) <- c("x", "y", "z")

    # Convert the count to numeric
    df_plot$z <- as.numeric(df_plot$z)

    browser()

    before <- ggplot() +
        geom_point(aes(x = data[, 1], y = data[, 2]), alpha = 0.1) +
        coord_fixed()

    after <- ggplot(df_plot, aes(x = x, y = y, fill = z)) +
        geom_tile() +
        scale_fill_gradientn(
            trans = "log",
            colors = c("white", "orange", "red")
        ) +
        theme_minimal() +
        xlab("x") +
        ylab("y") +
        ggtitle("2D Histogram") +
        coord_fixed()

    grid.arrange(before, after, ncol = 2)
}

bin_data_example_log <- function(data, bins = c(100, 100)) {
    binned_data <- custom_count_bin(data, bins = bins)
    # Convert matrix to data frame for ggplot
    df_plot <- as.data.frame(as.table(binned_data))

    # Rename the variables
    names(df_plot) <- c("x", "y", "z")

    # Convert the count to numeric
    df_plot$z <- as.numeric(df_plot$z)
    df_plot$x <- as.numeric(df_plot$x)

    before <- ggplot() +
        geom_point(aes(x = data[, 1], y = data[, 2]), alpha = 0.1) +
        scale_x_logicle() +
        scale_y_logicle() +
        theme(aspect.ratio = 1)

    after <- ggplot(df_plot, aes(
        x = x, y = y, fill = z
    )) +
        geom_tile() +
        scale_fill_gradientn(
            trans = "log",
            colors = c("white", "orange", "red")
        ) +
        theme_minimal() +
        xlab("x") +
        ylab("y") +
        ggtitle("2D Histogram") +
        # scale_y_continuous(
        #    breaks = seq(0, 260000, 10000),
        #    labels = label_at(50000),
        #    expand = c(0, 0)
        # ) +
        # ggcyto::scale_x_flowjo_fasinh() +
        theme_bw() +
        theme(
            aspect.ratio = 1,
            plot.margin = unit(c(-0.30, 0, 0, 0), "null")
        ) +
        facet_null()

    grid.arrange(before, after, ncol = 2)
}




subset_data <- as.data.frame(data@exprs[, c("FSC-A", "FSC-H")])

bin_data_example(subset_data, bins = c(200, 200))


subset_data <- as.data.frame(data2@exprs[, c("V500-A", "SSC-A")])

bin_data_example_log(subset_data, bins = c(300, 300))
