


report = as.matrix(read.csv("/Users/cray/Desktop/R/Turn/turn-report-0808.csv", header=TRUE, sep = ",", quote = "\""))

# Notice: report[1,] is the name of columns                    
colnames(report) <- report[1,]
report <- report[-1,]
colnames(report)[1]  <- "imp_date"
colnames(report)[4]  <- "pkg_name"
colnames(report)[9]  <- "convsions"
colnames(report)[13] <- "cost"

report <- as.data.frame(report)
#attach(report)

#sum groups
sum_imp  <- tapply(as.numeric(as.character(report$Impressions)), factor(report$imp_date, levels=levels(report$imp_date)) , FUN=sum)
sum_clk  <- tapply(as.numeric(as.character(report$Clicks)), factor(report$imp_date, levels=levels(report$imp_date)) , FUN=sum)
sum_conv <- tapply(as.numeric(as.character(report$convsions)), factor(report$imp_date, levels=levels(report$imp_date)) , FUN=sum)
sum_cost <- tapply(as.numeric(as.character(report$cost)), factor(report$imp_date, levels=levels(report$imp_date)) , FUN=sum)

#compute statistics
ctr <- sum_clk  / sum_imp
cvr <- sum_conv / sum_clk
imr <- sum_conv / sum_imp
cpm <- sum_cost / sum_imp * 1000
cpc <- sum_cost / sum_clk
cpl <- sum_cost / sum_conv

#draw graphics
# options(digits = 1)
plot.new()
plot(cpl, type="b", ylim=c(0, 250), xaxt = "n", xlab="", main="CPL by Date" )
text(1:length(cpl), as.vector(cpl), round(as.vector(cpl),1),cex=1, pos=3, col="green")
axis(1, at=c(1:length(cpl)), labels=names(cpl), col.axis="black", las=2)
 
final_table <- cbind(sum_imp, sum_clk, sum_conv, sum_cost, ctr, cvr, imr, cpm, cpc, cpl)
final_table
write.table(final_table, "/Users/cray/Desktop/R/Turn/final_table.txt", sep="\t")

