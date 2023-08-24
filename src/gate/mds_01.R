library(ggplot2)
library(flowCore)
library(flowViz)
library(flowDensity)

gate <- function(data) {
    channels <- c("V500-A", "SSC-A")

    singlets <- flowDensity(data,
        channels = c("FSC-A", "FSC-H"), position = c(F, F),
        percentile = c(.99999, .99999), use.percentile = c(T, T),
        ellip.gate = T, scale = .99
    )

    granu <- flowDensity(
        obj = singlets, channels = c("V500-A", "SSC-A"),
        position = c(TRUE, TRUE),
        gates = c(1.5, mean(data@exprs[, "SSC-A"] - 7000)),
        upper = TRUE,
        percentile = c(.4, .5), use.percentile = c(T, T),
    )

    granu <- flowDensity(
        obj = granu, channels = c("V500-A", "SSC-A"),
        position = c(FALSE, TRUE),
        percentile = c(0.9, 0.05), use.percentile = c(T, T),
        ellip.gate = T,
    )

    ery <- flowDensity(
        obj = singlets, channels = c("V500-A", "SSC-A"),
        position = c(FALSE, FALSE),
        ellip.gate = TRUE, scale = .95
        # tinypeak.removal = c(0.1, 0.9)
    )

    lymph_and_mono <- flowDensity(
        obj = singlets,
        channels = channels,
        position = c(TRUE, FALSE),
        use.percentile = c(T, F),
        percentile = c(.75, NA), gates = c(2.5, 50000),
    )

    # Second call to flowDensity
    mono <- flowDensity(
        obj = lymph_and_mono, channels = channels,
        position = c(TRUE, TRUE), gates = c(FALSE, NA),
        upper = c(NA, TRUE),
        percentile = c(NA, 0.1)
    )

    mono <- flowDensity(
        obj = mono, channels = channels,
        position = c(TRUE, TRUE), gates = c(
            lymph_and_mono@gates[1],
            mono@gates[2]
        ), ellip.gate = TRUE, scale = .99
    )

    lympho <- flowDensity(
        obj = lymph_and_mono, channels = channels,
        position = c(TRUE, FALSE), gates = c(0, NA),
        ellip.gate = TRUE, scale = .975,
    )

    lympho <- flowDensity(
        obj = lympho, channels = channels,
        position = c(TRUE, FALSE), gates = c(
            lymph_and_mono@gates[1],
            lympho@gates[2]
        ), ellip.gate = TRUE, scale = .99
    )

    c(
        "data" = data,
        "singlets" = singlets,
        "granu" = granu,
        "ery" = ery,
        "lympho" = lympho,
        "mono" = mono
    )
}
