library("quanteda")
library("lda")
library("slam")

##Load UK texts/create corpus
doc_term_matrix <- dfm(review$comments, stem = FALSE, dictionary = mfdict)

head(doc_term_matrix)
colnames(doc_term_matrix)

doc_term_df = data.frame(doc_term_matrix)

##Convert to lda model 
doc_term_lda <- convert(doc_term_matrix, to = "lda")




UKcorp <- corpus(textfile(file="~Michael/DM6/*"))

##Create document feature matrix 
UKdfm2 <- dfm(UKcorp, ngrams =1, verbose = TRUE, toLower = TRUE,
              removeNumbers = TRUE, removePunct = TRUE, removeSeparators = TRUE,
              removeTwitter = FALSE, stem = TRUE, ignoredFeatures =     
                stopwords(kind="english"), keptFeatures = NULL, language = "english",     
              thesaurus = NULL, dictionary = NULL, valuetype = "fixed"))


##run model
UKmod2 <- lda.collapsed.gibbs.sampler(UKlda2$documents, K = 15, UKlda2$vocab,  
                                      num.iterations = 1500, alpha = .1,eta = .01, initial = NULL, burnin 
                                      = NULL, compute.log.likelihood = TRUE, trace = 0L, freeze.topics = FALSE)