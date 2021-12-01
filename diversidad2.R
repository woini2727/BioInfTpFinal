setwd("C:/Users/Usuario/Desktop/BIO")
metadata<-read.delim("MetadataASPO_v7.csv")
colnames(metadata)[1] <- "SampleID"

library(qiime2R)
library(tidyverse)
library(microbiome)
library(phyloseq)
#Shannon
shannonFilogenia <- read_qza("C:/Users/Usuario/Desktop/BIO/GG/Merged_ASPO/Core_metrics/diversity/shannon_vector.qza")$data %>% rownames_to_column("SampleID") 
shannonFilogenia<-shannonFilogenia[order(shannonFilogenia$SampleID),]
vector_ShanonFilo <- shannonFilogenia['shannon_entropy']

#Shannon
shannonRutas <- read_qza("C:/Users/Usuario/Desktop/BIO/GG/Merged_ASPO/Picrust/Predict.EC_metrics/shannon_vector.qza")$data %>% rownames_to_column("SampleID") 
shannonRutas<-shannonRutas[order(shannonRutas$SampleID),]
vector_ShanonRutas <- shannonRutas['shannon_entropy']
vector_ShanonRutas <- vector_ShanonRutas[-c(81), ]

print(vector_ShanonRutas)
print(vector_ShanonFilo)

v_div <- vector_ShanonFilo/vector_ShanonRutas 
v_div_total <- cbind(shannonFilogenia$SampleID,v_div$shannon_entropy)
#Filo/Rutas
#shanon_merge <- joined <- full_join(vector_ShanonFilo, vector_ShanonRutas)


#lo pego en la metadata
metadata2<-metadata[-c(77),]
metadata2<-metadata2[order(metadata2$SampleID),]
total <- cbind(metadata2,v_div_total)

plot(total$`2`)
boxplot(as.numeric(total$`2`),horizontal = TRUE,notch = TRUE)
barplot(as.numeric(total$`2`),xlab ="Sample ID",ylab = "div result = E_Filo//E_Rutas")
boxplot(total$`Description`,horizontal = TRUE,notch = TRUE)

total$Description<-factor(total$Description, levels=c("Buenos-Ayres-microbiome","ASPO"))

ggplot(total, aes(x = Description, y = as.numeric(total$`2`)))   +  geom_boxplot()
ggplot(total, aes(x = Description, y =vector_ShanonRutas ))   + geom_boxplot()

cols <- rainbow(3, s = 0.5)
boxplot(x ~ z + y, data = DF2,
        at = c(1:3, 5:7), col = cols,
        names = c("", "A", "", "", "B", ""), xaxs = FALSE)
legend("topleft", fill = cols, legend = c(1,2,3), horiz = T)

