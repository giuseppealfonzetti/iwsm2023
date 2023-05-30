#' ---
#' title: "Stochastic graphical structure learning of NHANES data"
#' subtitle: "Code for IWSM2023"
#' author: "Giuseppe Alfonzetti"
#' format:
#'   html:
#'     embed-resources: true
#'     code-fold: true
#'     warning: false
#'     messages: false
#' cache: false
#' ---
#'
#' ## Download and rearrange the data
#'

library(nhanesA)
library(tidyverse)

#'
#' The `nhanesA` package provides access to the [National Health and Nutrition Examination Survey](https://www.cdc.gov/nchs/nhanes/index.htm) (NHANES) database. The data are organised in tables based on year and group of interest (demographics, dietary, examination, laboratory, questionnaire). They can be easily downloaded directly from R by specifying their coded names in the `nhanes()` function. We refer the reader to the [official package documentation](https://cran.r-project.org/web/packages/nhanesA/nhanesA.pdf) for further details.
#'
#' In order to retrieve the name of a particular group of data at a specific year, the function `nhanesTables()` is particularly convenient. For example, in the following we see `DEMO_J` is the name of the table collecting demographic data from the 2017-2018 survey.

nhanesTables('DEMO', 2018, details = T)$Data.File.Name

#'
#' Thus, we can download the table via

demo_j <- nhanes('DEMO_J')
as_tibble(demo_j)

#'
#' The table contains information over many variables. For further details about each of them, you can consult the official NHANES data documentation. The codebook for the `DEMO_j` table can be founded [here](https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/DEMO_J.htm).
#'
#' For this analysis we consider just a subset of all the available vaiables in order to exclude missing values
#'


# select
demo_j <- demo_j %>%
    select(SEQN, RIDAGEYR, DMDBORN4, DMDMARTL, DMDEDUC2,
           INDHHIN2,  DMDHHSIZ, DMDHREDZ)

# filtering
demo_j <- demo_j %>%
    filter(DMDBORN4 < 77, # drop refused, don't know and NA,
           DMDMARTL < 77, # "
           DMDEDUC2 < 7,  # "
           INDHHIN2 < 77, # "
           DMDHREDZ < 7   # "
           ) %>%
    mutate_at(vars(c(SEQN, DMDBORN4, DMDMARTL, DMDEDUC2, DMDHREDZ)), as.factor)

# recode factors
demo_j <- demo_j %>%
    mutate(
        # collapse marital status categories: 0 = Married or with partner, 1 = Widowed, divorced, separated, 2 = Never married
        DMDMARTL = fct_collapse(DMDMARTL, '0' = c("1","6"), '1' = c("2","3", "4"), '2' = c('5')),

        # collapse DMDEDUC2 on the same education categories of DMDHREDZ
        DMDEDUC2 = fct_collapse(DMDEDUC2, '0' = c("1","2"), '1' = c("3", "4"), '2' = c('5')),

        DMDBORN4 = recode_factor(DMDBORN4, '1' = '0', '2' = '1'),
        DMDHREDZ = recode_factor(DMDHREDZ, '1' = '0', '2' = '1', '3' = '2')
    ) %>% as_tibble()

demo_cookbook <- lapply(colnames(demo_j), function(x) nhanesCodebook('DEMO_J', x))

#'
#' We do the same for questionnairs about depression, smoking behaviours, housing, alcohol use. Likewise, we repeat the procedure to download examinations data for blood pressure and body indices. Variables descriptions can be founded [here](https://wwwn.cdc.gov/nchs/nhanes/continuousnhanes/default.aspx?BeginYear=2017).
#'

#### QUESTIONNAIRS ####
#nhanesTables('Q', 2018, details = T)

## Depression
#nhanesTableVars('Q', 'DPQ_J')
#nhanesCodebook('DPQ_J', 'DPQ010')
dpq_j <- as_tibble(nhanes('DPQ_J'))
dpq_j <- dpq_j %>%
    filter((if_all(starts_with("DP"), ~ . <7) ))%>% # drop refused, don't know and NA,
    mutate_all(as.factor)
#summary(dpq_j)
#lapply(colnames(dpq_j), function(x) nhanesCodebook('DPQ_J', x))

## Sleep Disorders
#nhanesTableVars('Q', 'SLQ_J')
#nhanesCodebook('SLQ_J', 'SLD012')
slq_j <- as_tibble(nhanes('SLQ_J'))
slq_j <- slq_j %>% select(-starts_with('SLQ3')) %>%
    filter(SLQ030 < 7, # drop refused, don't know and NA,
           SLQ040 < 7,
           SLQ050 < 7,
           SLQ120 < 7
           ) %>%
    mutate_at(vars(c(SEQN, SLQ050)), as.factor)
#lapply(colnames(slq_j), function(x) nhanesCodebook('SLQ_J', x))

# recode factors
slq_j <- slq_j %>%
    mutate(
        SLQ050 = recode_factor(SLQ050, '1' = '0', '2' = '1'),
    )
#summary(slq_j)

## Smoking
#nhanesTableVars('Q', 'SMQ_J')
#nhanesCodebook('SMQ_J', 'SMQ040')
#nhanesCodebook('SMQ_J', 'SMD650')
#nhanesCodebook('SMQ_J', 'SMD641')

smq_j <- as_tibble(nhanes('SMQ_J'))
smq_j <- smq_j %>% select(SEQN, SMQ020, SMQ040, SMD650, SMD641) %>%
    mutate(SEQN = as.factor(SEQN),
           SMQ040 = if_else(SMQ020==2, 3, SMQ040),
           SMD650 = if_else(SMQ040==3, 0, SMD650),
           SMD641 = if_else(SMQ040==3, 0, SMD641)
           ) %>%
    filter(SMQ020 < 7, SMD650 < 777, SMD641 < 77) %>%
    select(-SMQ020) %>%
    mutate_at(vars(c(SEQN, SMQ040)), as.factor)
#lapply(colnames(smq_j), function(x) nhanesCodebook('SMQ_J', x))

# recode factors
smq_j <- smq_j %>%
    mutate(
        SMQ040 = recode_factor(SMQ040, '1' = '0', '2' = '1', '3' = '2'),
    )
#summary(smq_j)

# Housing
#nhanesTableVars('Q', 'HOQ_J')
hoq_j <- as_tibble(nhanes('HOQ_J'))
hoq_j <- hoq_j %>%
    filter(HOD050 < 777, HOQ065 < 7) %>%
    mutate(SEQN=as.factor(SEQN),
           HOQ065 = HOQ065-1,
           HOQ065 = as.factor(HOQ065))

#lapply(colnames(hoq_j), function(x) nhanesCodebook('HOQ_J', x))


#summary(hoq_j)

## Alcohol
#nhanesTableVars('Q', 'ALQ_J')
#nhanesCodebook('ALQ_J', 'ALQ170')
alq_j <- as_tibble(nhanes('ALQ_J')) %>%
    mutate(
        ALQ130 = if_else(ALQ111==2,0,ALQ130),
        ALQ151 = if_else(ALQ111==2,0,ALQ151)

    ) %>%
    filter(ALQ130 < 777, ALQ151 < 7) %>%
    select(SEQN, ALQ130, ALQ151) %>%
    mutate_at(vars(c(SEQN, ALQ151)), as.factor)
#lapply(colnames(alq_j), function(x) nhanesCodebook('ALQ_J', x))

#summary(alq_j)

#### EXAMS ####
#nhanesTables('EXAM', 2018, details = T)

## Blood Pressure
#nhanesTableVars('EXAM', 'BPXO_J')
bpxo_j <- as_tibble(nhanes('BPXO_J')) %>%
    select(SEQN, BPXODI1, BPXOSY1, BPXOPLS1) %>%
    mutate_at(vars(SEQN), as.factor)
#lapply(colnames(bpxo_j), function(x) nhanesCodebook('BPXO_J', x))

#summary(bpxo_j)

## Body measures
#nhanesTableVars('EXAM', 'BMX_J')
bmx_j <- as_tibble(nhanes('BMX_J')) %>%
    select(SEQN, BMXWT, BMXHT, BMXBMI) %>%
    mutate_at(vars(SEQN), as.factor)
#lapply(colnames(bmx_j), function(x) nhanesCodebook('BMX_J', x))


#summary(bmx_j)

#'
#' We can now collect and join the variables in one single dataset. In order to allow for the estimation of of the graphical structure in the following section, we need to rearrange the variables such to have first the columns related to continuous data and then the categorical ones.
#'

#### Join datasets ####
survey_tib <- demo_j %>%
    left_join(dpq_j, by = 'SEQN') %>%
    left_join(slq_j, by = 'SEQN') %>%
    left_join(smq_j, by = 'SEQN') %>%
    left_join(hoq_j, by = 'SEQN') %>%
    left_join(alq_j, by = 'SEQN') %>%
    left_join(bpxo_j, by = 'SEQN') %>%
    left_join(bmx_j, by = 'SEQN') %>%
    drop_na() %>%
    select(where(is.numeric), where(is.factor),-SEQN)
#summary(survey_tib)

descr <- tibble(label = colnames(demo_j), area = 'demo') %>%
    bind_rows(tibble(label = colnames(dpq_j), area = 'depression')) %>%
    bind_rows(tibble(label = colnames(slq_j), area = 'sleep')) %>%
    bind_rows(tibble(label = colnames(smq_j), area = 'smoking')) %>%
    bind_rows(tibble(label = colnames(hoq_j), area = 'housing')) %>%
    bind_rows(tibble(label = colnames(alq_j), area = 'alcohol')) %>%
    bind_rows(tibble(label = colnames(bpxo_j), area = 'blood')) %>%
    bind_rows(tibble(label = colnames(bmx_j), area = 'body'))


# survey_tib %>%
#     ggplot(aes(x = RIDAGEYR))+
#     geom_histogram()
#
# str(survey_tib)
survey_nodes_type <- sort(map_dbl(survey_tib, ~if(is.numeric(.x)){1}else{length(levels(.x))}))

# length(levels(survey_tib[[30]]))

survey_tib <- survey_tib %>% select(names(survey_nodes_type))
save(survey_tib, file = 'survey_data.RData')

#'
#' ## Graphical structure estimation
#'
#' The estimation is performed via the custom package `mixedGraph` in development on [GitHub](https://github.com/giuseppealfonzetti/mixedGraph)

# Install via GitHub and load
devtools::install_github(
    "giuseppealfonzetti/mixedGraph@94bb96f",
    force = F
)
library(mixedGraph)

# For the sake of this analysis, we need to define some additional helper functions:
## R wrapper for the negative log-likelihood of the graph
graphR_obj <- function(THETA_, DATA_, NODES_TYPE_){
    graph <- graph_cl(
        DATA = as.matrix(DATA_),
        THETA = THETA_,
        NODES_TYPE = NODES_TYPE_,
        GRADFLAG = F,
        GRAD2FLAG = F
    )

    -graph$cl
}

## Estimate graph on a TRAIN_ dataset and evaluate the negative log-likelihood on a HOLDOUT_ partition. Function used to tune the regularisation parameter
learn_graph <- function(METHOD_, MAXITER_, NU_, TUNE_PAR_, STEPSIZE_ = 1, EACH_ = 10, TRAIN_, HOLDOUT_, NODES_TYPE_, THETA_INIT_, SEED_ = 123){
    set.seed(SEED_)
    est_mod <- mixedGraph(
        DATA = as.matrix(TRAIN_),
        SDS = fun_sd(TRAIN_, P = p, Q = q), #rep(1, p+q),#
        THETA = THETA_INIT_,
        NODES_TYPE = NODES_TYPE_,
        MAXITER = MAXITER_,
        STEPSIZE = STEPSIZE_,#step,
        SAMPLING_SCHEME = METHOD_,
        REG_PAR = TUNE_PAR_,#1*sqrt(log(p+q)/(nrow(survey_tib))),
        NU = NU_,
        TOL = 1e-10,
        TOL_MINCOUNT = 10,
        VERBOSEFLAG = F,
        EPS = .501,
        SEED = SEED_
    )

    out <- tibble(iter=0:(length(est_mod$path_theta)-1), theta = est_mod$path_theta) %>%
        filter(iter %in% seq(0,MAXITER_, EACH_)) %>%
        mutate(nll_hold = map_dbl(theta,
                                  ~graphR_obj(DATA = HOLDOUT_, THETA = .x, NODES_TYPE = NODES_TYPE_)/nrow(HOLDOUT_)
        ))
    return(out)
}

# Compute the norm of the edge of interest
check_edge_norm <- function(NODE_I, NODE_J, EDGEMAT, NODES_TYPE, CUMSUM_NODES_TYPE){
    out <- FALSE
    if(NODE_I < NODE_J){
        #cat("ERROR: interested cells must be in the lower triangular edge matrix!\n")
    }else{
        if(NODE_I <= p & NODE_J <= p){
            out <- norm(as.matrix(EDGEMAT[NODE_I, NODE_J]), type = 'F')
            #cat(paste0("beta_", NODE_I, NODE_J), "modified.\n")
        }else if(NODE_I > p & NODE_J <= p){
            cat_i <- NODES_TYPE[NODE_I]
            row_start <- CUMSUM_NODES_TYPE[NODE_I-1] + 1
            row_end <- CUMSUM_NODES_TYPE[NODE_I-1] + cat_i
            out <- norm(as.matrix(EDGEMAT[row_start:row_end, NODE_J]), type = 'F' )
            #cat(paste0("rho_", NODE_I, NODE_J), "modified.\n")


        }else if(NODE_I > p & NODE_J > p & NODE_I != NODE_J){
            cat_i <- NODES_TYPE[NODE_I]
            row_start <- CUMSUM_NODES_TYPE[NODE_I-1] + 1
            row_end <- CUMSUM_NODES_TYPE[NODE_I-1] + cat_i
            cat_j <- NODES_TYPE[NODE_J]
            col_start <- CUMSUM_NODES_TYPE[NODE_J-1] + 1
            col_end <- CUMSUM_NODES_TYPE[NODE_J-1] + cat_j
            out <- norm(as.matrix(EDGEMAT[row_start:row_end, col_start:col_end]), type = 'F')
            #cat(paste0("phi_", NODE_I, NODE_J), "modified.\n")
        }else if(NODE_I > p & NODE_J > p & NODE_I == NODE_J){
            cat_i <- NODES_TYPE[NODE_I]
            row_start <- CUMSUM_NODES_TYPE[NODE_I-1] + 1
            row_end <- CUMSUM_NODES_TYPE[NODE_I-1] + cat_i
            out <- norm(as.matrix(EDGEMAT[row_start:row_end, row_start:row_end]), type = 'F')
            #cat(paste0("phi_", NODE_I, NODE_J), "modified.\n")
        }
    }

    return(out)
}


#'
#' We need now to identify the dimensions of the problem and initialise the starting parameter value accordingly

cumsum_nodes_type <- cumsum(survey_nodes_type)
n_nodes <- length(survey_nodes_type)
p <- sum(survey_nodes_type==1)
q <- length(survey_nodes_type)-p
r <- sum(survey_nodes_type)
dim_edges_vec <- r^2-r*(r-1)/2 - sum(sapply(survey_nodes_type, function(x) if_else(x>1, x*(x-1)/2, 0)))

contCov_init <- matrix(0, p, p); diag(contCov_init) <- 1
edgeMat_init <- theta_to_edgesPar(c(rep(0,p), rep(0, dim_edges_vec)), survey_nodes_type, cumsum_nodes_type, p, r, n_nodes)
invContCov_init <- solve(contCov_init); invContCov_init[upper.tri(invContCov_init)] <- 0
edgeMat_init[1:p, 1:p] <- invContCov_init

# initial parameter vector
constrMat <- matrix(0, r, r)
for (node_j in 1:n_nodes) {
    for (node_i in node_j:n_nodes) {
        constrMat <- setUp_edge(node_i, node_j, VAL = 1, constrMat, NODES_TYPE = survey_nodes_type, CUMSUM_NODES_TYPE = cumsum_nodes_type)

    }
}
theta_init <- rep(0, p)
for (j in 1:r) {
    for (i in j:r) {
        if(constrMat[i,j] == 1){
            theta_init <- c(theta_init, edgeMat_init[i,j])
        }
    }
}
#as.double.factor <- function(x) {as.numeric(levels(x))[x]}


#' We have `r n_nodes` nodes, $p=$ `r p` out of which are continuous. The remaining $q=$ `r q` are categorical, accounting for a total of `r r-p` categories and a `r prod(survey_nodes_type[(p+1):n_nodes])` different observable patterns. The number of parameters to estimate is $d=$ `r length(theta_init)`.
#' Before proceeding, it is helpful to standardise the continuous variables in order to let them be on the same scale.

survey_data <- survey_tib %>%
    mutate_all(as.character) %>%
    mutate_all(as.numeric)
survey_data[,1:p] <- scale(survey_data[,1:p])

#'
#' We split the dataset in training and holdout partitions. In particular, we estimate the model on the training partition and tune the regularisation parameter by minimising the negative log-likelihood of the holdout one.

set.seed(123)
survey_data$id <- 1:nrow(survey_data)
train <- survey_data %>% sample_frac(0.80)
holdout <- anti_join(survey_data, train, by = 'id')
train <- train %>% select(-id); holdout <- holdout %>% select(-id)
lb <- 10^{-3}
ub <- 1
# log_tun_seq <- seq(log(lb), log(ub), length.out = 100)
# tun_par <- exp(log_tun_seq)
tun_par <- seq(lb, ub, length.out = 100)

#'
#' In particular we tune the regularisation parameter on a `r length(tun_par)`-points grid between `r min(tun_par)` and `r max(tun_par)`
#'

#### Holdout tuning ####
maxiter <- 10
library(furrr)
plan(multisession, workers = 4)
tuning_tib <- tibble(tune_par = tun_par) %>%
    mutate(
        path = future_map(tune_par,
                          ~learn_graph(METHOD_ = 1, MAXITER_ = maxiter, NU_ = .1*nrow(train), TUNE_PAR_ = .x, STEPSIZE_ = 1, TRAIN_ = train, HOLDOUT_ = holdout, NODES_TYPE_ = survey_nodes_type, THETA_INIT_ = theta_init, EACH = 1, SEED_ = 123),
                          .options = furrr_options(seed = T), .progress = T)
    )
selected <- tuning_tib %>% unnest(path) %>% filter(iter==maxiter) %>% arrange(nll_hold) %>% pluck('tune_par',1)

#'
#' and we choose the value minimising the negative log-likelihood of the holdout partition,  namely $\lambda=$ `r selected`.
#'

min_tib <- tuning_tib %>%
    unnest(path) %>%
    filter(iter>0) %>%
    mutate(data_passes = iter/maxiter) %>%
    select(tune_par, data_passes, nll_hold) %>%
    group_by(data_passes) %>%
    summarise(nll_hold = min(nll_hold)) %>%
    left_join(tuning_tib %>%
                  unnest(path) %>%
                  filter(iter>0) %>%
                  mutate(data_passes = iter/maxiter) %>%
                  select(tune_par, data_passes, nll_hold),
              by = c('data_passes', 'nll_hold'))
gg_hnll <- tuning_tib %>%
    unnest(path) %>%
    filter(iter>0) %>%
    mutate(data_passes = iter/maxiter) %>%
    ggplot(aes(x = (tune_par), y = (nll_hold), group = data_passes, color = as.factor(data_passes)))+
    geom_line(alpha = .8) +
    geom_point(data = min_tib, size = 3, alpha = .8, show.legend = F) +
    scale_color_viridis_d(option = 'D', direction = -1)+
    theme_minimal() +
    labs(x = 'Regularisation parameter', y = 'Holdout average negative log-likelihood', color = 'Data\npasses:') +
    theme(legend.position = 'right', panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          axis.line = element_line(color = '#888B8D'), text=element_text(size=16,  family="Helvetica", color = '#888B8D'),
          axis.text = element_text(size=12,  family="Helvetica", color = '#888B8D'),
          axis.ticks = element_line(linewidth = .5, color = '#888B8D') ) +
    scale_y_continuous(trans = 'log',  labels = scales::number_format(accuracy = 0.01), breaks = c(50, 100,  150, 200, 250, 300 ))

#gg_hnll
plotly::ggplotly(gg_hnll, dynamicTicks = T)
ggsave(plot = gg_hnll, filename = 'alfonzetti_tuning.pdf', height = 5, width = 10)

est_theta <- tuning_tib %>% #tuning_tib %>% #tuning_tib %>%
    unnest(path) %>%
    filter(iter==maxiter) %>%
    arrange(nll_hold) %>%
    #filter(tune_par==0.4) %>%
    pluck('theta',1)

#'

library(ggraph)
library(tidygraph)

# Get the edges matrix
emat <- theta_to_edgesPar(est_theta, survey_nodes_type, cumsum_nodes_type, p, r, n_nodes)
emat[upper.tri(emat)] <- NaN
diag(emat) <- NaN

# Complete dictionary variable-area of interest
dictionary <- tibble(id = 1:n_nodes, label = names(survey_nodes_type)) %>%
    left_join(descr, by = 'label')

# Dictionary node id - variable label
nodes_tib <- tibble(id = 1:n_nodes, label = names(survey_nodes_type))

# Rearrange edges from emat in a tibble, checking for presence/absence and assessing
# edge intensity via norm of the vector/matrix containing the parameters of interest
edges_tib <- expand_grid(
    from = 2:n_nodes,
    to = 1:(n_nodes-1)
    ) %>%
    filter(from > to) %>%
    mutate(
    presence = map2_dbl(from, to, ~check_edge(.x,.y,emat, survey_nodes_type, cumsum_nodes_type)),
    intensity = map2_dbl(from, to, ~check_edge_norm(.x,.y,emat, survey_nodes_type, cumsum_nodes_type)))
presence <- edges_tib %>% pluck('presence') %>% mean()

#'
#' The chosen estimate selects `r presence*100` $\%$ of the edges as active.
#'
#' After rearranging the graph components using the `tidygraph` package, we can visualise it with the `ggraph` package.

# Construct a table for the estimated graph via tbl_graph()
net.tidy <- tbl_graph(
    nodes = dictionary,
    edges = edges_tib%>% filter(presence!=0),
    directed = F )%>%
    mutate(centrality = centrality_degree(mode = 'in'))

library(jcolors)
gg <- net.tidy  %>%
    ggraph(layout = "stress") +
    geom_edge_link(aes(width=intensity), alpha = 0.2,     show.legend = FALSE) +
    geom_node_point(aes(col = area), alpha = .5, size = 2) +
    geom_node_point(aes(size = (centrality), col = area), alpha =.5,  show.legend = FALSE) +
    scale_edge_width(range = c(0.2, 2)) +
    geom_node_label(aes(label = label, col = area), repel = TRUE, alpha = .5, size = 5, label.size = .4, show.legend = F) +
    theme_graph(base_family = 'Helvetica', base_size = 20,   text_colour = "#888B8D",
                plot_margin = margin(1, 1, 1, 1)
                )+
    labs(col= ' ')+
    theme(legend.position = 'bottom')
gg <- gg +scale_color_jcolors(palette = 'pal7')
ggsave(plot = gg, filename = 'alfonzetti_network.pdf', height = 9, width = 10)
gg

#'
