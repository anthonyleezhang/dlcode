
clean_up = function(dists) {
  finalOutput = data.table()
  for (d in dists) {
    fn = paste("results/", d, "_sumstats.csv", sep = "")
    print(fn)
    data = fread(fn)

    finalOutput = rbindlist(list(finalOutput, data))
  }
  finalOutput[, V1 := NULL]
  write.csv(finalOutput, file = "results/sumstats.csv")
}