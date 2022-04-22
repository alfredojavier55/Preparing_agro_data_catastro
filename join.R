setwd("/media/alfredo/Backup/USP/Projeto fapesp/Dados/Cadastro/2019")

c1 <- read.csv("1de4.xls0.csv", colClasses = "character")
c2 <- read.csv("2de4.xls0.csv", colClasses = "character")
c3 <- read.csv("3de4.xls0.csv", colClasses = "character")
c4 <- read.csv("4de4.xls0.csv", colClasses = "character")

cad19 <- rbind(c1,c2,c3,c4)
write.csv(cad19, file="cad2019.csv")
# Last update (Riems) 30/nov/2020