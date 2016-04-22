#-----------Install Package ggplot2 to run the code----------
library('ggplot2')

#-----------Normal entries (m = 10, n = 15)----------------------------
#-----------Data construction-------------------------
load('Normal10.Rdata')
data_mc_normal10 <- as.vector(t(MCNormal10))
data_bi_normal10 <- as.vector(t(biPermuNormal10))
data_uni_normal10 <- as.vector(t(uniPermuNormal10))
data_uni_rank10 <- as.vector(t(uniRanked10))
data_bi_rank10 <- as.vector(t(biRanked10))
data_collect_normal10 <- c(data_mc_normal10, data_bi_normal10, data_bi_rank10, data_uni_normal10, data_uni_rank10)
tep_mean <- (1:8) * 0.125 + 0.5
ele_mean <- rep(rep(tep_mean, each = length(data_mc_normal10) / 8, ), 5)
perm_method <- factor(rep(c('MC', 'Bidimensional', 'Bidimensional rank', 'Unidimensional', 'Unidimensional rank'), each = length(data_mc_normal10)), c('MC', 'Bidimensional', 'Bidimensional rank', 'Unidimensional', 'Unidimensional rank'))
data_normal10 <- data.frame('mean' = as.factor(ele_mean), 'method' = perm_method, 'data' = data_collect_normal10)
# Constructed a dataset with variables as (elevated mean - permuting method - p-value) combo

#----------Graphing----------------------------------
p_normal10 <- ggplot(data_normal10, aes(x = mean, y = data, color = method))
p_normal10 <- p_normal10 + scale_colour_manual(values = c("black", "#FF0000", "#990000", "#00DD00", "#009900"))
p_normal10 <- p_normal10 + geom_boxplot() + xlab(bquote(theta['\U2021'] * " (multiplication of " * theta['crit'] * ')')) + ylab("P-value") + ggtitle("Normal (m = 10, n = 15)")
p_normal10 <- p_normal10 + theme(legend.position="top") + geom_vline(xintercept = 4, linetype = 2, color = 'blue') + theme(legend.title=element_blank())

#-----------Normal entries (m = 30, n = 10)----------------------------
#-----------Data construction-------------------------
load('Normal30.Rdata')
data_mc_normal30 <- as.vector(t(MCNormal30))
data_bi_normal30 <- as.vector(t(biPermuNormal30))
data_uni_normal30 <- as.vector(t(uniPermuNormal30))
data_uni_rank30 <- as.vector(t(uniRanked30))
data_bi_rank30 <- as.vector(t(biRanked30))
data_collect_normal30 <- c(data_mc_normal30, data_bi_normal30, data_bi_rank30, data_uni_normal30, data_uni_rank30)
data_normal30 <- data.frame('mean' = as.factor(ele_mean), 'method' = perm_method, 'data' = data_collect_normal30)
# Constructed a dataset with variables as (elevated mean - permuting method - p-value) combo

#----------Graphing----------------------------------
p_normal30 <- ggplot(data_normal30, aes(x = mean, y = data, color = method ))
p_normal30 <- p_normal30 + scale_colour_manual(values = c("black", "#FF0000", "#990000", "#00DD00", "#009900"))
p_normal30 <- p_normal30 + geom_boxplot() + xlab(bquote(theta['\U2021'] * " (multiplication of " * theta['crit'] * ')')) + ylab("P-value") + ggtitle("Normal (m = 30, n = 10)")
p_normal30 <- p_normal30 + theme(legend.position="top") + geom_vline(xintercept = 4, linetype = 2, color = 'blue') + theme(legend.title=element_blank())



#-----------Poisson entries (m = 10, n = 15)----------------------------
#-----------Data construction-------------------------
load('Poi10.Rdata')
data_mc_poi10 <- as.vector(t(MCPoi10))
data_bi_poi10 <- as.vector(t(biPermuPoi10))
data_uni_poi10 <- as.vector(t(uniPermuPoi10))
data_uni_poi_rank10 <- as.vector(t(uniRankedPoi10))
data_bi_poi_rank10 <- as.vector(t(biRankedPoi10))
data_collect_poi10 <- c(data_mc_poi10, data_bi_poi10, data_bi_poi_rank10, data_uni_poi10, data_uni_poi_rank10)
ele_mean2 <- rep(rep(tep_mean, each = length(data_mc_poi10) / 8, ), 5)
perm_method_poi <- factor(rep(c('MC', 'Bidimensional', 'Bidimensional rank', 'Unidimensional', 'Unidimensional rank'), each = length(data_mc_poi10)), c('MC', 'Bidimensional', 'Bidimensional rank', 'Unidimensional', 'Unidimensional rank'))
data_poi10 <- data.frame('mean' = as.factor(ele_mean2), 'method' = perm_method_poi, 'data' = data_collect_poi10)

#----------Graphing----------------------------------
p_poi10 <- ggplot(data_poi10, aes(x = mean, y = data, color = method))
p_poi10 <- p_poi10 + scale_colour_manual(values = c("black", "#FF0000", "#990000", "#00DD00", "#009900"))
p_poi10 <- p_poi10 + geom_boxplot() + xlab(bquote(theta['\U2021'] * " (multiplication of " * theta['crit'] * ')')) + ylab("P-value") + ggtitle("Poisson (m = 10, n = 15)")
p_poi10 <- p_poi10 + theme(legend.position="top") + geom_vline(xintercept = 4, linetype = 2, color = 'blue') + theme(legend.title=element_blank())

#-----------Poisson entries (m = 30, n = 10)----------------------------
#-----------Data construction-------------------------
load('Poi30.Rdata')
data_mc_poi30 <- as.vector(t(MCPoi30))
data_bi_poi30 <- as.vector(t(biPermuPoi30))
data_uni_poi30 <- as.vector(t(uniPermuPoi30))
data_uni_poi_rank30 <- as.vector(t(uniRankedPoi30))
data_bi_poi_rank30 <- as.vector(t(biRankedPoi30))
data_collect_poi30 <- c(data_mc_poi30, data_bi_poi30, data_bi_poi_rank30, data_uni_poi30, data_uni_poi_rank30)
data_poi30 <- data.frame('mean' = as.factor(ele_mean2), 'method' = perm_method_poi, 'data' = data_collect_poi30)

#----------Graphing----------------------------------
p_poi30 <- ggplot(data_poi30, aes(x = mean, y = data, color = method))
p_poi30 <- p_poi30 + scale_colour_manual(values = c("black", "#FF0000", "#990000", "#00DD00", "#009900"))
p_poi30 <- p_poi30 + geom_boxplot() + xlab(bquote(theta['\U2021'] * " (multiplication of " * theta['crit'] * ')')) + ylab("P-value") + ggtitle("Poisson (m = 30, n = 10)")
p_poi30 <- p_poi30 + theme(legend.position="top") + geom_vline(xintercept = 4, linetype = 2, color = 'blue') + theme(legend.title=element_blank())


#-----------------Result----------------------
p_normal10 + theme(text = element_text(size = 25))
# Normal entries, m = 10, n = 15, comparison between 
# MC/Unidimensional/Bidimensional permutation methods
p_normal30 + theme(text = element_text(size = 25))
# Same as above, with m = 30, n = 10
p_poi10  + theme(text = element_text(size = 25))
# Poisson entries, m = 10, n = 15, 
#comparison between MC/Bidimensional/Unidimensional methods
p_poi30 + theme(text = element_text(size = 25))
# Same as above, with m = 30, n = 10