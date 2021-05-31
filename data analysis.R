library(dplyr)
rowData =
  read.csv(file = "test_results.csv")

analysisTypes = colnames(rowData[, 4:10])

completeTestResults = rowData %>%
  filter_at(vars(all_of(analysisTypes)), all_vars(!is.na(.)))

# Statistics
getmode = function(v) {
  uniqv = unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

getStatistics = function(rowData,
                         analysisType,
                         statisticsFileName = "statistics.txt") {
  data = rowData[which(is.na(rowData[[analysisType]])),]
  
  if (nrow(data) > 0) {
    resultStr = paste("*********************", "\n", sep = "")
    resultStr = paste(resultStr, "Statistics for ", analysisType, "\n", sep = "")
    cat(resultStr, file = statisticsFileName, append = TRUE)
    resultStr = ""
    genderStatistics = as.data.frame(table(data$gender))
    colnames(genderStatistics) = c("Gender", "Frequency")
    write.table(
      genderStatistics,
      file = statisticsFileName,
      append = TRUE,
      quote = FALSE,
      row.names = FALSE,
      col.names = TRUE,
      sep = "\t"
    )
    resultStr = paste(resultStr, "Median of age: ", median(data$age), "\n", sep = "")
    resultStr = paste(resultStr, "Mode of age: ", getmode(data$age), "\n", sep = "")
    # Append Results To Text File
    cat(resultStr, file = statisticsFileName, append = TRUE)
    # Disease Statistics
    typeTable = as.data.frame(table(data$Type))
    colnames(typeTable) = c("Type", "Frequency")
    # Append Disease Statistics To Text File
    write.table(
      typeTable,
      file = statisticsFileName,
      append = TRUE,
      quote = FALSE,
      row.names = FALSE,
      col.names = TRUE,
      sep = "\t"
    )
  }
}

statisticsFileName = "statistics.txt"
if (file.exists(statisticsFileName)) {
  file.remove(statisticsFileName)
}

for (analysisType in analysisTypes) {
  getStatistics(rowData, analysisType)
}

# Plots
drawHist = function(rowData, analysisType) {
  data = rowData[which(is.na(rowData[[analysisType]])),]
  if (nrow(data) > 0) {
    hist(data$age,
         main = paste("Histogram for", analysisType),
         xlab = "age")
  }
}

pdf("plots.pdf")
hist(completeTestResults$age, main = "Histogram for complete test results", xlab = "age")
for (analysisType in analysisTypes) {
  drawHist(rowData, analysisType)
}

dev.off()

# Filled Data
fillData = function(rowData, analysisType) {
  filledData = data.frame(rowData)
  for (i in row.names(rowData[which(is.na(rowData[[analysisType]])),])) {
    sampleData = rowData[which(
      !is.na(rowData[[analysisType]]) &
        abs(rowData$age - rowData[i,]$age) <= 5 &
        rowData$gender == rowData[i, ]$gender &
        rowData$Type == rowData[i, ]$Type
    ), ]
    if (nrow(sampleData) > 0) {
      filledData[i, ][[analysisType]] = median(sampleData[[analysisType]])
    }
  }
  
  return(filledData)
}

filledData = data.frame(rowData)
for (analysisType in analysisTypes) {
  filledData = data.frame(fillData(filledData, analysisType))
}

write.csv(filledData,
          "filled_test_results.csv",
          row.names = FALSE)