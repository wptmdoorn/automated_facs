library(ggplot2)
library(flowCore)
library(flowViz)

data <- flowCore::read.FCS("data/fcs/A.fcs")
comp <- flowCore::keyword(data)[["SPILL"]]

data <- flowWorkspace::compensate(
    data,
    comp
)

transformList <- flowCore::estimateLogicle(data, channels = colnames(comp))
data <- flowWorkspace::transform(data, transformList)

subset_data <- as.data.frame(data@exprs[, c("V500-A", "SSC-A")])
names(subset_data) <- c("V500", "SSC")

subset_data_norm <- subset_data
subset_data_norm$V500 <- subset_data_norm$V500 / max(subset_data_norm$V500)
subset_data_norm$SSC <- subset_data_norm$SSC / max(subset_data_norm$SSC)

centers <- matrix(c(
    0.24, 0.48, 0.58, 0.65,
    0.05, 0.25, 0.125, 0.05
), 4, 2)

kmeans.result <- kmeans(subset_data_norm, centers = centers, algorithm = "Forgy", iter.max = 200)
kmeans.result <- kmeans(subset_data_norm, centers = 4)
subset_data_norm$cluster <- as.factor(kmeans.result$cluster)

ggplot(subset_data_norm, aes(V500, SSC, color = cluster)) +
    geom_point()

# Perform DBSCAN clustering
dbscan_result <- dbscan(subset_data_norm, eps = 0.5, minPts = 5)

# Add cluster labels to the original dataframe
subset_data_norm$cluster <- as.factor(dbscan_result$cluster)

ggplot(subset_data_norm, aes(x = V500, y = SSC, color = cluster)) +
    geom_point()

sngl <- flowDensity(data,
    channels = c("FSC-A", "FSC-H"), position = c(F, F),
    percentile = c(.99999, .99999), use.percentile = c(T, T),
    ellip.gate = T, scale = .99
)

plotDens(data, c("FSC-A", "FSC-H"))
lines(sngl@filter, type = "l")


granu <- flowDensity(
    obj = data, channels = c("V500-A", "SSC-A"),
    position = c(NA, TRUE),
)

ery <- flowDensity(
    obj = data, channels = c("V500-A", "SSC-A"),
    position = c(FALSE, FALSE),
    use.percentile = c(TRUE, FALSE),
    percentile = c(0.75, 0.5),
    bimodal = TRUE,
    tinypeak.removal = c(0.1, 0.9)
)

lymph <- flowDensity(
    obj = data, channels = c("V500-A", "SSC-A"),
    position = c(TRUE, FALSE),
)


plotDens(data, channels = c("V500-A", "SSC-A"))
points(leuko@filter, type = "l", col = 2, lwd = 2)
points(ery@filter, type = "l", col = 3, lwd = 2)
points(lymph@filter, type = "l", col = 4, lwd = 2)

f2 <- data
thresholds <- deGate(obj = f2, channel = "V500-A")
# Percentile default is .95, which can be changed
thresholds.prcnt <- deGate(f2, channel = "V500-A", use.percentile = T, percentile = .3)
thresholds.lo <- deGate(f2, channel = "V500-A", use.upper = T, upper = F, alpha = .9)
thresholds.hi <- deGate(f2, channel = "V500-A", use.upper = T, upper = T, alpha = .9)

plotDens(f2, c("V500-A", "SSC-A"))
abline(v = c(
    thresholds,
    thresholds.prcnt, thresholds.lo, thresholds.hi
), col = c("red", "blue", "green", "black"))
