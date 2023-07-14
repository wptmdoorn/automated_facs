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

    count_matrix <- as.data.frame(table(x_bins, y_bins))
    names(count_matrix) <- c("x", "y", "z")

    count_matrix$z <- as.numeric(count_matrix$z)

    # normalize $z to [0, 1]
    count_matrix$z <- log(count_matrix$z)

    count_matrix
}
