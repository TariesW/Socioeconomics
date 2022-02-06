#The purpose of LCA is to classify your participants into latent classes/groups. 
#It is more advantageous compared to traditional cluster analysis due to its output for model fit. 
#Then, you can judge whether your classification is appropriate.

#Load data
library(readxl)
latent_class_data <- read_excel("Documents/Accunique/10-/latent class data.xlsx")
View(latent_class_data)     


latent_class_data[latent_class_data == "1"] <- 2
latent_class_data[latent_class_data == "0"] <- 1


income <- latent_class_data$income
health_cover <- latent_class_data$health_cover
house <- latent_class_data$house
wc <- latent_class_data$wc
phone <- latent_class_data$phone
car <- latent_class_data$car
consumption <- latent_class_data$consumption

f <- cbind(income, health_cover, house, wc, phone, car, consumption) ~ 1

install.packages("poLCA")
library("poLCA")


#nclass represents how many class we want to have in this model
M1 <- poLCA(f, data = latent_class_data, nclass = 1, graphs = TRUE, na.rm = TRUE)
M2 <- poLCA(f, data = latent_class_data, nclass = 2, graphs = TRUE, na.rm = TRUE)
M3 <- poLCA(f, data = latent_class_data, nclass = 3, graphs = TRUE, na.rm = TRUE)
M4 <- poLCA(f, data = latent_class_data, nclass = 4, graphs = TRUE, na.rm = TRUE)
M5 <- poLCA(f, data = latent_class_data, nclass = 5, graphs = TRUE, na.rm = TRUE)

colMeans(M1$posterior)
colMeans(M2$posterior)
colMeans(M3$posterior)
colMeans(M4$posterior)
colMeans(M5$posterior)

table(M1$predclass)
table(M2$predclass)
table(M3$predclass)
table(M4$predclass)
table(M5$predclass)

poLCA.entropy(M1)
poLCA.entropy(M2)
poLCA.entropy(M3)
poLCA.entropy(M4)
poLCA.entropy(M5)




entropy <- function(p) sum(-p*log(p))

#MeanPP for M1
error_prior <- entropy(M1$P)
error_post <- mean(apply(M1$posterior, 1, entropy), na.rm = TRUE)
(error_prior - error_post) / error_prior

#MeanPP for M2
error_prior2 <- entropy(M2$P)
error_post2 <- mean(apply(M2$posterior, 1, entropy), na.rm = TRUE)
(error_prior2 - error_post2) / error_prior2

#MeanPP for M3
error_prior3 <- entropy(M3$P)
error_post3 <- mean(apply(M3$posterior, 1, entropy), na.rm = TRUE)
(error_prior3 - error_post3) / error_prior3

#MeanPP for M4
error_prior4 <- entropy(M4$P)
error_post4 <- mean(apply(M4$posterior, 1, entropy), na.rm = TRUE)
(error_prior4 - error_post4) / error_prior4

#MeanPP for M5
error_prior5 <- entropy(M5$P)
error_post5 <- mean(apply(M5$posterior, 1, entropy), na.rm = TRUE)
(error_prior5 - error_post5) / error_prior5





