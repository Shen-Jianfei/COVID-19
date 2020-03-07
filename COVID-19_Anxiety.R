########## Network Analysis of Anxiety during COVID-19 ##########

library(qgraph)
library(readr)
library(bootnet)
library(ggplot2)

##### Scores of some items are reversed #####
Data_SAS_Reversed <- read_csv("F:/Scientific Research/Experiment Materials/COVID-19_Anxiety/SAS_Reversed.csv")
Cormat_SAS_Reversed <- cor_auto(Data_SAS_Reversed)
# γ = 0
SpringGraph_SAS_Reversed_0 <- qgraph(Cormat_SAS_Reversed,
                            layout = "spring",
                            graph = "glasso", tuning = 0,
                            sampleSize = nrow(Data_SAS_Reversed),
                            usePCH = TRUE,
                            title = "SAS_Reversed γ=0", title.cex = 1,
                            vsize = 12, esize = 15)
Centrality_SAS_Reversed_0 <- centrality(SpringGraph_SAS_Reversed_0)
Centrality_SAS_Reversed_0$Betweenness
Centrality_SAS_Reversed_0$Closeness
Centrality_SAS_Reversed_0$OutDegree
Plot_SAS_Reversed_0 <- centralityPlot(SpringGraph_SAS_Reversed_0)
write.csv(SpringGraph_SAS_Reversed_0[["Edgelist"]], "F:/Scientific Research/Experiment Results/COVID-19_Anxiety/SAS_Reversed_0_edgelist.csv")
summary(SpringGraph_SAS_Reversed_0)
# γ = 0.5
SpringGraph_SAS_Reversed_05 <- qgraph(Cormat_SAS_Reversed,
                            layout = "spring",
                            graph = "glasso", tuning = 0.5,
                            sampleSize = nrow(Data_SAS_Reversed),
                            usePCH = TRUE,
                            title = "SAS_Reversed γ=0.5", title.cex = 1,
                            vsize = 12, esize = 15)
Centrality_SAS_Reversed_05 <- centrality(SpringGraph_SAS_Reversed_05)
Centrality_SAS_Reversed_05$Betweenness
Centrality_SAS_Reversed_05$Closeness
Centrality_SAS_Reversed_05$OutDegree
Plot_SAS_Reversed_05 <- centralityPlot(SpringGraph_SAS_Reversed_05)
write.csv(SpringGraph_SAS_Reversed_05[["Edgelist"]], "F:/Scientific Research/Experiment Results/COVID-19_Anxiety/SAS_Reversed_05_edgelist.csv")
summary(SpringGraph_SAS_Reversed_05)

###### Raw data(some scores of items are not reversed) #####
Data_SAS <- read_csv("F:/Scientific Research/Experiment Materials/COVID-19_Anxiety/SAS.csv")
Cormat_SAS <- cor_auto(Data_SAS)
# γ = 0
SpringGraph_SAS_0 <- qgraph(Cormat_SAS,
                            layout = "spring",
                            graph = "glasso", tuning = 0,
                            sampleSize = nrow(Data_SAS),
                            usePCH = TRUE,
                            title = "SAS γ=0", title.cex = 1,
                            vsize = 12, esize = 15)
Centrality_SAS_0 <- centrality(SpringGraph_SAS_0)
Centrality_SAS_0$Betweenness
Centrality_SAS_0$Closeness
Centrality_SAS_0$OutDegree
Plot_SAS_0 <- centralityPlot(SpringGraph_SAS_0)
write.csv(SpringGraph_SAS_0[["Edgelist"]], "F:/Scientific Research/Experiment Results/COVID-19_Anxiety/SAS_0_edgelist.csv")
summary(SpringGraph_SAS_0)
# γ = 0.5
SpringGraph_SAS_05 <- qgraph(Cormat_SAS,
                            layout = "spring",
                            graph = "glasso", tuning = 0.5,
                            sampleSize = nrow(Data_SAS),
                            usePCH = TRUE,
                            title = "SAS γ=0.5", title.cex = 1,
                            vsize = 13, esize = 15,label.cex = 1.35)
Centrality_SAS_05 <- centrality(SpringGraph_SAS_05)
Centrality_SAS_05$Betweenness
Centrality_SAS_05$Closeness
Centrality_SAS_05$OutDegree
Plot_SAS_05 <- centralityPlot(SpringGraph_SAS_05, orderBy = "Strength")
write.csv(SpringGraph_SAS_05[["Edgelist"]], "F:/Scientific Research/Experiment Results/COVID-19_Anxiety/SAS_05_edgelist.csv")
summary(SpringGraph_SAS_05)

###### Testing for Centrality Stability of SAS ######
# γ = 0.5
bootnet_SAS_05_centrality <- bootnet(Data_SAS,
                                     nBoots = 10000,
                                     default = "EBICglasso",
                                     tuning = 0.5,
                                     type = "case",
                                     statistics = c("strength", "closeness","betweenness"),
                                     nCores = 2)
plot(bootnet_SAS_05_centrality, statistics = c("strength","closeness","betweenness"), "area")
CS_SAS_05 <- corStability(bootnet_SAS_05_centrality)

###### Testing for Edge Accuracy of SAS ######
bootnet_SAS_05_edges <- bootnet(Data_SAS,
                                nBoots = 10000,
                                default = "EBICglasso",
                                tuning = 0.5,
                                type = "nonparametric",
                                statistics = c("edge","strength","closeness","betweenness"),
                                nCores = 2)
plot(bootnet_SAS_05_edges, labels = FALSE, order = "sample")

###### Testing for Edge Significant Differences of SAS ######
plot(bootnet_SAS_05_edges, "edge", plot = "difference", onlyNonZero = TRUE, order = "sample", labels = FALSE)

###### Testing for Centrality Indices Significant Differences of SAS ######
plot(bootnet_SAS_05_edges, "strength", plot = "difference", order = "sample")