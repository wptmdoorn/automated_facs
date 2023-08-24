library(ggplot2)

data <- flowCore::read.FCS("data/fcs/A.fcs")
comp <- flowCore::keyword(data)[["SPILL"]]

data <- flowWorkspace::compensate(
    data,
    comp
)

transformList <- flowCore::estimateLogicle(data, channels = colnames(comp))
data <- flowWorkspace::transform(data, transformList)

source("src/bin.R", local = bin <- new.env())

subset_data <- as.data.frame(data2@exprs[, c("V500-A", "SSC-A")])
subset_data2 <- as.data.frame(data@exprs[, c("V500-A", "SSC-A")])


bins <- 200
binned_data <- bin$custom_count_bin(subset_data, bins = c(bins, bins))

binned_data$x <- as.numeric(binned_data$x)
binned_data$y <- as.numeric(binned_data$y)
# binned_data$z <- log(as.numeric(binned_data$z))

de <- xtabs(z ~ x + y, binned_data)

image(de)
cl <- contourLines(de, nlevels = 10)
lapply(cl, lines)

# Perform k-means clustering with 5 clusters
data_for_clustering <- binned_data[, c("x", "y", "z")]
start <- matrix(c(
    0.3 * bins, 0.45 * bins, 0.7 * bins,
    0.05 * bins, 0.22 * bins, 0.05 * bins,
    100, 100, 100
), 3, 3)
kmeans_result <- kmeans(data_for_clustering, centers = start)

# Add the cluster labels back to the original dataframe
data_for_clustering$cluster <- as.factor(kmeans_result$cluster)

# Plot the clusters on a graph
ggplot(data_for_clustering, aes(x = x, y = y, color = z, shape = cluster)) +
    geom_point(size = 1) +
    scale_color_gradient(low = "blue", high = "red") + # Adjust color gradient
    labs(
        title = "K-Means Clustering with 5 Clusters",
        x = "X-axis", y = "Y-axis",
        color = "Z Value", shape = "Cluster"
    ) +
    theme_minimal()
