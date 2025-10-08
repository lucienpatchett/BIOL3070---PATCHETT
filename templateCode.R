# Day 1 Example Plot

viremia <- read.csv("viremia_data_full.csv")
viremia
view(viremia)
colnames(viremia) <- c("Bird","n","Species","Family","Order","1","3","4","6")

cols <- c("black","grey",rainbow(26)[4:26])

plot(c(1,3,4,6),as.numeric(viremia[1,6:9]),
     type = "1", 1wd =2, ylim  = range(viremia[,6:9],na.rm =TRUE),
     xlab = "Day Postinfection"
     ylab = "Log PFU/ml Serum")
