library(dplyr)
rowData =
  read.csv(file = '/home/toopeachok/Documents/homeworks-R/MedicalDataAnalysis/test_results.csv')

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
                         statisticsFileName = "/home/toopeachok/Documents/homeworks-R/MedicalDataAnalysis/statistics.txt") {
  data = rowData[which(is.na(rowData[[analysisType]])), ]
  
  if (nrow(data) > 0) {
    resultStr = ""
    resultStr = paste(resultStr, "*********************", "\n")
    resultStr = paste(resultStr, "Statistics for", analysisType, "\n")
    resultStr = paste(resultStr, "Female quantity:", length(which(data$gender == "female")), "\n")
    resultStr = paste(resultStr, "Male quantity:", length(which(data$gender == "male")), "\n")
    resultStr = paste(resultStr, "Median of age:", median(data$age), "\n")
    resultStr = paste(resultStr, "Mode of age:", getmode(data$age), "\n")
    resultStr = paste(resultStr, "\n")
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
      row.names = FALSE,
      col.names = TRUE,
      sep = "\t"
    )
  }
}

statisticsFileName = "/home/toopeachok/Documents/homeworks-R/MedicalDataAnalysis/statistics.txt"
if (file.exists(statisticsFileName)) {
  file.remove(statisticsFileName)
}

for (analysisType in analysisTypes) {
  getStatistics(rowData, analysisType)
}

# Plots
drawHist = function(rowData, analysisType) {
  data = rowData[which(is.na(rowData[[analysisType]])), ]
  if (nrow(data) > 0) {
    hist(data$age,
         main = paste("Histogram for", analysisType),
         xlab = "age")
  }
}

pdf("/home/toopeachok/Documents/homeworks-R/MedicalDataAnalysis/plots.pdf")
hist(completeTestResults$age, main = "Histogram for Complete test results", xlab = "age")
for (analysisType in analysisTypes) {
  drawHist(rowData, analysisType)
}

dev.off()

# Filled Data
fillData = function(rowData, analysisType) {
  filledData = data.frame(rowData)
  for (i in row.names(rowData[which(is.na(rowData[[analysisType]])), ])) {
    sampleData = rowData[which(
      !is.na(rowData[[analysisType]]) &
        abs(rowData$age - rowData[i, ]$age) <= 5 &
        rowData$gender == rowData[i,]$gender &
        rowData$Type == rowData[i,]$Type
    ),]
    if (nrow(sampleData) > 0) {
      filledData[i,][[analysisType]] = median(sampleData[[analysisType]])
    }
  }
  
  return(filledData)
}

filledData = data.frame(rowData)
for (analysisType in analysisTypes) {
  filledData = data.frame(fillData(filledData, analysisType))
}

write.csv(
  filledData,
  "/home/toopeachok/Documents/homeworks-R/MedicalDataAnalysis/filled_test_results.csv",
  row.names = FALSE
)