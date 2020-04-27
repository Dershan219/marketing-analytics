library(dplyr)

retail <-
  read.csv("C:/Users/Andy/Desktop/DS/Learning Journal/OnlineRetail.csv")
retail$InvoiceDate <-
  as.Date(retail$InvoiceDate, tryFormat = "%d-%m-%Y")

# RFM---------------------------------------------------------------
retail.rfm <-
  retail %>% na.omit() %>% group_by(CustomerID) %>%
  summarise(
    recency = max(retail$InvoiceDate) - max(InvoiceDate),
    frequency = length(unique(InvoiceNo)),
    total_monetary = sum(Quantity * UnitPrice)
  ) %>%
  mutate(
    r_rank = rank(recency, ties.method = "min"),
    f_rank = rank(-frequency, ties.method = "min"),
    m_rank = rank(-total_monetary, ties.method = "min")
  )

# calculate RFM scores
scores <- function(x, rng) {
  if (x <= quantile(rng, .2))
    score = 5
  else if (x <= quantile(rng, .4))
    score = 4
  else if (x <= quantile(rng, .6))
    score = 3
  else if (x <= quantile(rng, .8))
    score = 2
  else
    score = 1
  return(score)
}
retail.rfm <- retail.rfm %>% mutate(
  r_score = lapply(r_rank, function(x)
    scores(x, r_rank)),
  f_score = lapply(f_rank, function(x)
    scores(x, f_rank)),
  m_score = lapply(m_rank, function(x)
    scores(x, m_rank))
) %>% sapply(., as.numeric) %>% as.data.frame()

# kmeans clustering
retail.kmeans <- kmeans(retail.rfm[, 8:10], 5)
retail.rfm$cluster <- retail.kmeans$cluster

# BGNBD-------------------------------------------------------------
library(BTYD)
library(BTYDplus)

retail.cbs <- retail[, c("CustomerID", "InvoiceDate")]
colnames(retail.cbs) <- c("cust", "date")
retail.bgnbd <- elog2cbs(retail.cbs)
params <- bgnbd.EstimateParameters(retail.bgnbd)
names(params) <- c('r', 'alpha', 's', 'beta')

# survival rate
palive <-
  bgnbd.PAlive(params,
               x = retail.bgnbd[, "x"],
               t.x = retail.bgnbd[, "t.x"],
               T.cal = retail.bgnbd[, "T.cal"])

# expected transactions
expected_trans <-
  bgnbd.ConditionalExpectedTransactions(
    params,
    T.star = 30,
    x = retail.bgnbd[, "x"],
    t.x = retail.bgnbd[, "t.x"],
    T.cal = retail.bgnbd[, "T.cal"]
  )
retail.bgnbd <- cbind(retail.bgnbd, palive, expected_trans)
