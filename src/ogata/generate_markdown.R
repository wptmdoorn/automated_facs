library(flowCore)
library(flowWorkspace)

source("src/ogata/ogata_calculations.R")
files <- list.files("data/fcs", pattern = "*.fcs", full.names = TRUE)

for (filename in files) {
    data <- flowCore::read.FCS(filename)
    comp <- flowCore::keyword(data)[["SPILL"]]

    data <- flowWorkspace::compensate(
        data,
        comp
    )
    transformList <- flowCore::estimateLogicle(data, channels = colnames(comp))
    data <- flowWorkspace::transform(data, transformList)

    print(data@description$`EXPERIMENT NAME`)

    rmarkdown::render("src/ogata/ogata.Rmd",
        params = list(data = data, gates = process_ogata(data, filename, transformList)),
        output_file = sprintf(
            "../../output/ogata_%s.pdf",
            tools::file_path_sans_ext(basename(filename))
        )
    )
}
