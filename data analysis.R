library(dplyr)
rowData =
  read.csv(file = '/home/toopeachok/Documents/homeworks-R/MedicalDataAnalysis/test_results.csv')

completeTestResults = rowData %>%
  filter_at(vars(colnames(rowData[, 4:10])), all_vars(!is.na(.)))

emptyWBC = rowData[which(is.na(rowData$WBC)),]
emptyRBC = rowData[which(is.na(rowData$RBC)),]
emptyHGB = rowData[which(is.na(rowData$HGB)),]
emptyHCT = rowData[which(is.na(rowData$HCT)),]
emptyPLT = rowData[which(is.na(rowData$PLT)),]
emptyLYM = rowData[which(is.na(rowData$LYM)),]
emptyMCV = rowData[which(is.na(rowData$MCV)),]

# Statistics
getmode = function(v) {
  uniqv = unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

getStatistics = function(data, label) {
  if (nrow(data) > 0) {
    sink(
      paste(
        "/home/toopeachok/Documents/homeworks-R/MedicalDataAnalysis/statistics.txt"
      ),
      TRUE
    )
    cat(paste("*********************"), sep = "\n")
    cat(paste("Statistics for", label), sep = "\n")
    cat(paste("\n"))
    cat(paste("Female quantity:", length(which(
      data$gender == "female"
    ))), sep = "\n")
    cat(paste("Male quantity:", length(which(
      data$gender == "male"
    ))), sep = "\n")
    cat(paste("Median of age:", median(data$age)), sep = "\n")
    cat(paste("Mode of age:", getmode(data$age)), sep = "\n")
    cat(paste("\n"))
    sink()
    typeTable = as.data.frame(table(data$Type))
    colnames(typeTable) = c("Type", "Frequency")
    write.table(
      typeTable,
      file = "/home/toopeachok/Documents/homeworks-R/MedicalDataAnalysis/statistics.txt",
      append = TRUE,
      row.names = FALSE,
      col.names = TRUE,
      sep = "\t"
    )
  }
}

getStatistics(completeTestResults, "Complete test results")
getStatistics(emptyHCT, "Empty HCT")
getStatistics(emptyHGB, "Empty HGB")
getStatistics(emptyLYM, "Empty LYM")
getStatistics(emptyMCV, "Empty MCV")
getStatistics(emptyPLT, "Empty PLT")
getStatistics(emptyRBC, "Empty RBC")
getStatistics(emptyWBC, "Empty WBC")

# Plots
pdf("/home/toopeachok/Documents/homeworks-R/MedicalDataAnalysis/plots.pdf")
# Complete test results
hist(completeTestResults$age)
# WBC
hist(emptyWBC$age)
# RBC
hist(emptyRBC$age)
# HGB
hist(emptyHGB$age)
# HCT
hist(emptyHCT$age)
# PLT
hist(emptyPLT$age)
# LYM
hist(emptyLYM$age)
# MCV
hist(emptyMCV$age)

dev.off()

# Filled data
fillData = function(rowData, analysisType) {
  filledData = data.frame(rowData)
  for (i in row.names(rowData[which(is.na(rowData[[analysisType]])),])) {
    sampleData = rowData[which(
      !is.na(rowData[[analysisType]]) &
        abs(rowData$age - rowData[i,]$age) <= 5 &
        rowData$gender == rowData[i, ]$gender &
        rowData$Type == rowData[i, ]$Type
    ), ]
    print(sampleData)
    print("-----------------")
    if (nrow(sampleData) > 0) {
      filledData[i, ][[analysisType]] = median(sampleData[[analysisType]])
    }
  }
  
  return(filledData)
}

analysisTypes = c("HCT", "HGB", "LYM", "MCV", "PLT", "RBC", "WBC")
filledData = data.frame(rowData)
for (analysisType in analysisTypes) {
  filledData = data.frame(fillData(filledData, analysisType))
}

write.csv(
  filledData,
  "/home/toopeachok/Documents/homeworks-R/MedicalDataAnalysis/filled_test_results.csv",
  row.names = FALSE
)