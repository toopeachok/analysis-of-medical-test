library(ggplot2)
library(dplyr)
rawData = read.csv(file = "test_results.csv")

analysisTypes = colnames(rawData[, 4:10])

# Fill data
fillData = function(rawData, analysisType) {
  filledData = data.frame(rawData)
  for (i in row.names(rawData[which(is.na(rawData[[analysisType]])),])) {
    sampleData = rawData[which(
      !is.na(rawData[[analysisType]]) &
        abs(rawData$age - rawData[i,]$age) <= 5 &
        rawData$gender == rawData[i, ]$gender &
        rawData$Type == rawData[i, ]$Type
    ), ]
    if (nrow(sampleData) > 0) {
      filledData[i, analysisType] = median(sampleData[[analysisType]])
    }
  }
  
  return(filledData)
}

filledData = data.frame(rawData)
for (analysisType in analysisTypes) {
  filledData = data.frame(fillData(filledData, analysisType))
}

write.csv(filledData, "filled_test_results.csv", row.names = FALSE)

# Comparison with norms
norms = read.csv(file = "norms.csv")
# To get proper structure we can just copy filled data,
# later we will put actual delta values:
# zero if value in norm,
# value - upper_norm_value, if excess (will be positive num),
# value - bottom_norm_value, if reduction (will be negative num)
deltaNorms = data.frame(filledData)

getNormDelta = function(norms, analysisType, value) {
  delta = 0
  # norms is a data frame with two rows:
  # norms[2, ] upper limit of the norm,
  # norms[1, ] bottom
  if (value > norms[2, analysisType]) {
    delta  = value - norms[2, analysisType]
  } else if (value < norms[1, analysisType]) {
    delta = value - norms[1, analysisType]
  }
  return(delta)
}

# Filling of deltaNorms
for (i in row.names(deltaNorms)) {
  for (analysisType in analysisTypes) {
    delta = NA
    value = deltaNorms[i, analysisType]
    if (!is.na(value)) {
      currentNorms = norms %>% filter(gender == deltaNorms[i, "gender"])
      delta = getNormDelta(currentNorms, analysisType, value)
    }
    deltaNorms[i, analysisType] = delta
  }
}

# Statistics and plots for each disease according to deltas of norms
diseases = unique(deltaNorms[["Type"]])
plotsReductionNorms = vector(length(diseases), mode = 'list')
plotsExcessNorms = vector(length(diseases), mode = 'list')

for (i in 1:length(diseases)) {
  disease = diseases[[i]]
  # diseaseData is a data frame with two rows and length(analysisTypes) columns.
  # Columns are a analysis types,
  # rows contains frequency of
  # excess of norms - diseaseData[2, ],
  # reduction of norms - diseaseData[1, ]
  diseaseData = as.data.frame(matrix(0, 2, length(analysisTypes)))
  colnames(diseaseData) = analysisTypes
  
  for (analysisType in analysisTypes) {
    reduction = nrow(deltaNorms[which(deltaNorms[[analysisType]] < 0 &
                                        deltaNorms[["Type"]] == disease), ])
    excess = nrow(deltaNorms[which(deltaNorms[[analysisType]] > 0 &
                                     deltaNorms[["Type"]] == disease), ])
    diseaseData[1, analysisType] = reduction
    diseaseData[2, analysisType] = excess
  }
  
  dotchart(
    as.numeric(diseaseData[1, ]),
    labels = colnames(diseaseData),
    main = disease,
    xlab = "Frequency of reduction norms",
    color = "blue"
  )
  plotsReductionNorms[[i]] = recordPlot()
  graphics.off()
  
  dotchart(
    as.numeric(diseaseData[2, ]),
    labels = colnames(diseaseData),
    main = disease,
    xlab = "Frequency of excess norms",
    color = "red"
  )
  plotsExcessNorms[[i]] = recordPlot()
  graphics.off()
}

pdf("plotsReductionNorms.pdf", onefile = TRUE)
for (plot in plotsReductionNorms) {
  replayPlot(plot)
}
graphics.off()

pdf("plotsExcessNorms.pdf", onefile = TRUE)
for (plot in plotsExcessNorms) {
  replayPlot(plot)
}
graphics.off()

write.csv(deltaNorms, "deltas_of_norms.csv", row.names = FALSE)
