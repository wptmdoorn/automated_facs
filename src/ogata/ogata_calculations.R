library(ggplot2)
library(flowCore)
library(flowViz)
library(flowDensity)

process_ogata <- function(data, file, transformList) {
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
        percentile = c(.95, 0.01), use.percentile = c(T, T),
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

    blast.gate1 <- flowDensity(
        obj = singlets, channels = c("PerCP-Cy5-5-A", "SSC-A"),
        position = c(TRUE, FALSE),
    )

    b_inc <- ifelse(length(getPeaks(data,
        channel = "PerCP-Cy5-5-A"
    )$Peaks) == 2, 0, .5)

    blast <- flowDensity(
        obj = singlets, c("PerCP-Cy5-5-A", "SSC-A"),
        position = c(TRUE, FALSE), gates = c(
            blast.gate1@gates[1] + b_inc,
            35000
        ), ellip.gate = T,
    )

    low_ssc <- flowDensity(
        obj = singlets,
        channels = c("FSC-A", "SSC-A"),
        position = c(TRUE, FALSE),
        gates = c(8000, NA),
    )

    cd34_cd45_pos <- flowDensity(
        obj = singlets,
        channels = c("V500-A", "PerCP-Cy5-5-A"),
        position = c(NA, TRUE),
        percentile = c(NA, .9975)
    )

    bprog <- flowDensity(
        obj = cd34_cd45_pos,
        channels = c("PerCP-Cy5-5-A", "APC-H7-A"),
        position = c(TRUE, TRUE),
        gates = c(.1, NA),
        filter = matrix(c(
            2, 2, 4, 4, # x
            5, 3.0, 3.0, 5 # y
        ), 4, 2)
    )

    myeloblast <- flowDensity(
        obj = cd34_cd45_pos,
        channels = c("PerCP-Cy5-5-A", "APC-H7-A"),
        position = c(TRUE, FALSE),
        gates = c(.1, NA),
    )

    myeloblast.ssc <- flowDensity(
        obj = cd34_cd45_pos,
        channels = c("SSC-A", "V500-A"),
        position = c(FALSE, TRUE),
        gates = c(.1, NA),
        filter = matrix(c(
            0, 40000, 40000, 0, # x
            1.8, 1.8, 4, 4 # y
        ), 4, 2)
    )
    
    inv.trans <- inverseLogicleTransform(transformList)
    
    lympho.inv <- transform(lympho@flow.frame, inv.trans)
    myeloblast.inv <- transform(myeloblast.ssc@flow.frame, inv.trans)
    granu.inv <- transform(granu@flow.frame, inv.trans)

    ogata_myeloblast <- (myeloblast.ssc@cell.count / singlets@cell.count) * 100
    ogata_bprog <- (bprog@cell.count / cd34_cd45_pos@cell.count) * 100
    ogata_lymph_myeloblast <- (mean(exprs(lympho.inv)[, "V500-A"],
        na.rm = TRUE
    ) / mean(exprs(myeloblast.inv)[, "V500-A"], na.rm = TRUE))
    ogata_granulo_lymph <- (mean(exprs(granu.inv)[, "SSC-A"],
        na.rm = TRUE
    ) / mean(exprs(lympho.inv)[, "SSC-A"], na.rm = TRUE))

    total <- (ogata_myeloblast > 2) + (ogata_bprog < 5) +
        (ogata_lymph_myeloblast <= 4 | ogata_lymph_myeloblast >= 7.5) +
        (ogata_granulo_lymph <= 6)

    ogata_raw_table <- data.frame(
        "Parameter" = c(
            "Number of CD34/CD45 pos cells",
            "Number of B-progenitor cells",
            "Number of myeloblast cells",
            "Lymphocyte CD45 intensity",
            "Myeloblast CD45 intensity",
            "Granulocyte SSC intensity",
            "Lymphocyte SSC intensity"
        ),
        "Result" = c(
            cd34_cd45_pos@cell.count,
            bprog@cell.count,
            myeloblast.ssc@cell.count,
            mean(exprs(lympho.inv)[, "V500-A"], na.rm = TRUE),
            mean(exprs(myeloblast.inv)[, "V500-A"], na.rm = TRUE),
            mean(exprs(granu.inv)[, "SSC-A"], na.rm = TRUE),
            mean(exprs(lympho.inv)[, "SSC-A"], na.rm = TRUE)
        )
    )

    ogata_table <- data.frame(
        Parameter = c(
            "Myeloblast (% of CD45+ cells)",
            "B-progenitor-related cluster size (% of CD34+ cells)",
            "Lymphocyte to myeloblast CD45 ratio",
            "Granulocyte to lymphocyte SSC ratio",
            "**Total score**"
        ),
        "Cut-off values" = c("> 2%", "< 5%", "<= 4 or >= 7.5", "<= 6", ">= 2"),
        "Result" = c(
            ogata_myeloblast,
            ogata_bprog, ogata_lymph_myeloblast, ogata_granulo_lymph, total
        ), "Score" = c(
            as.numeric(ogata_myeloblast > 2), 
            as.numeric(ogata_bprog < 5), 
            as.numeric((ogata_lymph_myeloblast <= 4 | ogata_lymph_myeloblast >= 7.5)),
            as.numeric(ogata_granulo_lymph <= 6), total
        )
    )

    # round each value in the Value column
    ogata_table$Result <- round(ogata_table$Result, 2)

    list(
        "filename" = sprintf("Ogata score calculations (%s)", file),
        "data" = data,
        "singlets" = singlets,
        "granu" = granu,
        "ery" = ery,
        "lympho" = lympho,
        "mono" = mono,
        "blast" = blast,
        "low_ssc" = low_ssc,
        "cd34_cd45_pos" = cd34_cd45_pos,
        "bprog" = bprog,
        "myeloblast" = myeloblast,
        "myeloblast.ssc" = myeloblast.ssc,
        "ogata_raw_table" = ogata_raw_table,
        "ogata_table" = ogata_table
    )
}
