

col1 <- rep("a", 4)
col2 <- c(0.60, 1234.55, 678.90, -999.00)
df <- data.frame(col1,col2)

out <- paste(capture.output(print(df, row.names=F)), collapse = "\n")
writeLines(out, "df.dat")

df$col2 <- sprintf("%0.02f", df$col2)
df$col2 <- sprintf(paste0("%0", max(nchar(df$col2)), "s"), df$col2)

df
