library(flowWorkspace)

read_and_compensate_data <- function(filepath) {
    data <- flowCore::read.FCS(filepath)

    flowWorkspace::compensate(
        data,
        flowCore::keyword(data)[["SPILL"]]
    )
}

transform_data <- function(data) {
    comp <- flowCore::keyword(data)[["SPILL"]]

    flowWorkspace::transform(
        data,
        flowCore::estimateLogicle(data, channels = colnames(comp))
    )
}
