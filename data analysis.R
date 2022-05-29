filledData = read.csv(file = "filled_test_results.csv")

analysisTypes = colnames(filledData[, 4:10])

# Comparison with norms
norms = read.csv(file = "norms.csv")
# To get proper structure we can just copy filled data,
# later we will put actual delta values:
# zero if value in norm,
# value - upper_norm_value, if excess (will be positive num),
# value - bottom_norm_value, if reduction (will be negative num)
deltaNorms = data.frame(filledData)

# Fill with real delta norms
getNormDelta = function(value, norms) {
  delta = 0
  if (value > norms[2]) {
    delta  = value - norms[2]
  } else if (value < norms[1]) {
    delta = value - norms[1]
  }
  return(delta)
}

fillDeltaNorms = function(gender, deltaNorms) {
  deltaNorms[deltaNorms$gender == gender, 4:10] = sapply(colnames(filledData[filledData$gender == gender, 4:10]), function(analysisType) {
    currentNorms = norms[norms$gender == gender, analysisType]
    # currentNorms[1] is a bottom_norm_value
    # currentNorms[2] is a upper_norm_value
    filledData[filledData$gender == gender, analysisType] = sapply(filledData[filledData$gender == gender, analysisType], function(x) {
      delta = NA
      if (!is.na(x)) {
        delta = getNormDelta(x, currentNorms)
      }
      x = delta
      return(x)
    })
    return(filledData[filledData$gender == gender, analysisType])
  })
  return(deltaNorms)
}

deltaNorms = fillDeltaNorms("male", deltaNorms)
deltaNorms = fillDeltaNorms("female", deltaNorms)