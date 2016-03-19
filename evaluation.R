#-----------Install Package ggplot2 to run the code----------
library('ggplot2')

#-----------Normal entries----------------------------
#-----------Data construction-------------------------
data_mc_normal <- as.vector(as.matrix(read.table('output_mc_normal.txt')))
data_bi_normal <- as.vector(as.matrix(read.table('output_permu_normal.txt')))
data_uni_normal <- as.vector(as.matrix(read.table('output_row_normal.txt')))
data_collect_normal <- c(data_mc_normal, data_uni_normal, data_bi_normal)
tep_mean <- (1:8) * 0.125 + 0.5
ele_mean <- rep(rep(tep_mean, each = length(data_mc_normal) / 8, ), 3)
perm_method <- factor(rep(c('MC', 'Unidimensional', 'Bidimensional'), each = length(data_mc_normal)), c('MC', 'Unidimensional', 'Bidimensional'))
data_normal <- data.frame('mean' = as.factor(ele_mean), 'method' = perm_method, 'data' = data_collect_normal)
# Constructed a dataset with variables as (elevated mean - permuting method - p-value) combo

#----------Graphing----------------------------------
p_normal <- ggplot(data_normal, aes(x = mean, y = data, color = method ))
p_normal <- p_normal + geom_boxplot() + xlab("Elevated Mean (*theta)") + ylab("P-value") + ggtitle("Normal")
p_normal <- p_normal + theme(legend.position="top") + scale_color_discrete("") + geom_vline(xintercept = 4, linetype = 2, color = 'purple')


#-----------Poisson entries----------------------------
#-----------Data construction-------------------------
data_mc_poi <- as.vector(as.matrix(read.table('output_poisson_mc.txt')))
data_bi_poi <- as.vector(as.matrix(read.table('output_poisson_permu.txt')))
data_collect_poi <- c(data_mc_poi, data_bi_poi)
ele_mean2 <- rep(rep(tep_mean, each = length(data_mc_poi) / 8, ), 2)
perm_method_poi <- factor(rep(c('MC', 'Bidimensional'), each = length(data_mc_poi)), c('MC', 'Bidimensional'))
data_poi <- data.frame('mean' = as.factor(ele_mean2), 'method' = perm_method_poi, 'data' = data_collect_poi)

#----------Graphing----------------------------------
p_poi <- ggplot(data_poi, aes(x = mean, y = data, color = method ))
p_poi <- p_poi + geom_boxplot() + xlab("Elevated Mean (*theta)") + ylab("P-value") + ggtitle("Poisson")
p_poi <- p_poi + theme(legend.position="top") + scale_color_discrete("") + geom_vline(xintercept = 4, linetype = 2, color = 'purple')


#-----------Rank methods (Unidimensional)---------------------
#-----------Data construction-------------------------
data_uni_perm <- as.vector(as.matrix(read.table('output_rank_unipermu.txt')))
data_uni_rank <- as.vector(as.matrix(read.table('output_rank_rowranktest.txt')))
data_collect_unirank <- c(data_uni_perm, data_uni_rank)
tep_mean_rank <- (1:8) * 0.125 + 0.75
ele_mean_rank <- rep(rep(tep_mean_rank, each = length(data_uni_perm) / 8, ), 2)
perm_method_unirank <- factor(rep(c('Original Data', 'Ranked Data'), each = length(data_uni_rank)), c('Original Data', 'Ranked Data'))
data_unirank <- data.frame('mean' = as.factor(ele_mean_rank), 'method' = perm_method_unirank, 'data' = data_collect_unirank)

#----------Graphing----------------------------------
p_unirank <- ggplot(data_unirank, aes(x = mean, y = data, color = method ))
p_unirank <- p_unirank + geom_boxplot() + xlab("Elevated Mean (*theta)") + ylab("P-value") + ggtitle("Unidimensional Rank")
p_unirank <- p_unirank + theme(legend.position="top") + scale_color_discrete("") + geom_vline(xintercept = 2, linetype = 2, color = 'purple')


#-----------Rank methods (Bidimensional)---------------------
#-----------Data construction-------------------------
data_bi_perm <- as.vector(as.matrix(read.table('output_rank_bipermu.txt')))
data_bi_rank <- as.vector(as.matrix(read.table('output_rank_ranktest.txt')))
data_collect_birank <- c(data_bi_perm, data_bi_rank)
perm_method_birank <- factor(rep(c('Original Data', 'Ranked Data'), each = length(data_bi_rank)), c('Original Data', 'Ranked Data'))
data_birank <- data.frame('mean' = as.factor(ele_mean_rank), 'method' = perm_method_birank, 'data' = data_collect_birank)

#----------Graphing----------------------------------
p_birank <- ggplot(data_birank, aes(x = mean, y = data, color = method ))
p_birank <- p_birank + geom_boxplot() + xlab("Elevated Mean (*theta)") + ylab("P-value") + ggtitle("Bidimensional Rank")
p_birank <- p_birank + theme(legend.position="top") + scale_color_discrete("") + geom_vline(xintercept = 2, linetype = 2, color = 'purple')


#-----------------Result----------------------
p_normal  # Normal entries, comparison between MC/Unidimensional/Bidimensional methods
p_poi  # Poisson entries, comparison between MC/Bidimensional methods
p_unirank  # Comparison between unidimensional permutation / ranked unidimensional methods
p_birank  # Comparison between bidimensional permutation / ranked bidimensional methods
