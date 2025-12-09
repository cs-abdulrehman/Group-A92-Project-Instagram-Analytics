
df <- read.csv("most_followed_ig.csv")
# fix encoding issues ER column
df$ER <- iconv(df$ER, from = "latin1", to = "UTF-8", sub = "byte")

# cleanup
df$ER <- gsub("[^0-9.]", "", df$ER)  # Keep only numbers and decimal points
df$ER <- as.numeric(df$ER)
head(df$ER)

# Categorization
df$CATEGORY <- ifelse(grepl("celebrities", df$CATEGORIES.1, ignore.case = TRUE), "Celebrity", "Non-Celebrity")

#Boxplot
png("Boxplot.png")
boxplot(ER ~ CATEGORY, data = df, 
        main = "Comparison of Engagement Rate (ER): Celebrities vs Non-Celebrities",
        xlab = "Category", ylab = "Engagement Rate (ER)",
        col = c("skyblue", "orange"))
dev.off()

# histogram
png("Histogram.png")
hist(df$ER[df$CATEGORY == "Celebrity"], 
     col = rgb(0, 0, 1, 0.5), 
     xlim = range(df$ER, na.rm = TRUE),
     main = "Histogram of Engagement Rate (ER) by Category",
     xlab = "Engagement Rate (ER)", 
     ylab = "Frequency",
     breaks = 10)
# non-celebrity histogram
hist(df$ER[df$CATEGORY == "Non-Celebrity"], 
     col = rgb(1, 0, 0, 0.5), 
     add = TRUE, 
     breaks = 10)
legend("topright", legend = c("Celebrity", "Non-Celebrity"),
       fill = c(rgb(0, 0, 1, 0.5), rgb(1, 0, 0, 0.5)))
dev.off()

# Student T-Test
celebrity_group <- df[df$CATEGORY == 'Celebrity', 'ER']
non_celebrity_group <- df[df$CATEGORY == 'Non-Celebrity', 'ER']
t_test_result <- t.test(celebrity_group, non_celebrity_group)
print(t_test_result)