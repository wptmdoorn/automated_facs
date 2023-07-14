data <- flowCore::read.FCS("data/fcs/A.fcs")
comp <- flowCore::keyword(data)[["SPILL"]]

data <- flowWorkspace::compensate(
    data,
    comp
)

transformList <- flowCore::estimateLogicle(data, channels = colnames(comp))
data2 <- flowWorkspace::transform(data, transformList)

source("src/bin.R", local = bin <- new.env())


subset_data <- as.data.frame(data2@exprs[, c("V500-A", "SSC-A")])

binned_data <- bin$custom_count_bin(subset_data, bins = c(200, 200))

binned_data$x <- as.numeric(binned_data$x)
binned_data$y <- as.numeric(binned_data$y)
binned_data$z <- log(as.numeric(binned_data$z))

de <- xtabs(z ~ x + y, binned_data)

image(de)
cl <- contourLines(de, levels = c(6))
lapply(cl, lines)
