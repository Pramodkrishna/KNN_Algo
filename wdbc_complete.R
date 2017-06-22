wdbc <- read.csv("wdbc.csv.csv",header = T,stringsAsFactors = F)
wdbc <- wdbc[-33]
prop.table(table(wdbc$diagnosis))

"Modelling and preparing data sets"

data_index <- createDataPartition(wdbc$diagnosis, p=0.7, list = FALSE)
train_data <- wdbc[data_index, -2]
test_data <- wdbc[-data_index, -2]

"Perform PCA"
pca_res <- prcomp(wdbc[,2:ncol(wdbc)], center = TRUE, scale = TRUE)

pca_wdbc <- prcomp(x = wdbc[,3:ncol(wdbc)])
head(summary(pca_wdbc))
