###########################################################
#                 Naive Bayes Spam Predictor              #
#                 Taken from tutorial:                    #
#       https://rpubs.com/mzc/mlwr_nb_sms_spam            #
###########################################################


# Check for Packages that need to be installed & loaded
for (package in c("tm", "wordcloud", "e1071", "gmodels")) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

# Open the file, setting header and strings as factors to FALSE
sms = read.csv("SMSSpamCollection.csv", stringsAsFactors=FALSE, header = FALSE)

# Add in column names/headers
colnames(sms) <- c("type", "text")

# Examine the strucutre of the file
str(sms)

# 
round(prop.table(table(sms$type))*100, digits = 1)

sms$type = factor(sms$type)
str(sms)


sms_corpus = Corpus(VectorSource(sms$text))
print(sms_corpus)

inspect(sms_corpus[1:3])
corpus_clean = tm_map(sms_corpus, tolower)                    # convert to lower case
corpus_clean = tm_map(corpus_clean, removeNumbers)            # remove digits
corpus_clean = tm_map(corpus_clean, removeWords, stopwords()) # and but or you etc
corpus_clean = tm_map(corpus_clean, removePunctuation)        # you'll never guess!
corpus_clean = tm_map(corpus_clean, stripWhitespace)          # reduces w/s to 1
inspect(corpus_clean[1:3])

#corpus_clean = tm_map(corpus_clean, PlainTextDocument) # this is a tm API necessity
dtm = DocumentTermMatrix(corpus_clean)

# split the raw data:
sms.train = sms[1:4200, ] # about 75%
sms.test  = sms[4201:5574, ] # the rest

# then split the document-term matrix
dtm.train = dtm[1:4200, ]
dtm.test  = dtm[4201:5574, ]

# and finally the corpus
corpus.train = corpus_clean[1:4200]
corpus.test  = corpus_clean[4201:5574]

# let's just assert that our split is reasonable: raw data should have about 87% ham
# in both training and test sets:
round(prop.table(table(sms.train$type))*100)

round(prop.table(table(sms.test$type))*100)

#wordcloud(corpus.train,
#          min.freq=10,          # 10% of num docs in corpus is rough standard
#          random.order = FALSE) # biggest words are nearer the centre


spam = subset(sms.train, type == "spam")
ham  = subset(sms.train, type == "ham")

#wordcloud(spam$text,
#          max.words=100,     # look at the 40 most common words
#          scale=c(3, 0, 5)) # adjust max and min font sizes for words shown




freq_terms = findFreqTerms(dtm.train, 5)
reduced_dtm.train = DocumentTermMatrix(corpus.train, list(dictionary=freq_terms))
reduced_dtm.test =  DocumentTermMatrix(corpus.test, list(dictionary=freq_terms))
ncol(reduced_dtm.test)

convert_counts = function(x) {
  x = ifelse(x > 0, 1, 0)
  x = factor(x, levels = c(0, 1), labels=c("No", "Yes"))
  return (x)
}

# apply() allows us to work either with rows or columns of a matrix.
# MARGIN = 1 is for rows, and 2 for columns
reduced_dtm.train = apply(reduced_dtm.train, MARGIN=2, convert_counts)
reduced_dtm.test  = apply(reduced_dtm.test, MARGIN=2, convert_counts)

# have we reduced the number of features?
ncol(reduced_dtm.train)

# store our model in sms_classifier
sms_classifier = naiveBayes(reduced_dtm.train, sms.train$type)
sms_test.predicted = predict(sms_classifier,
                             reduced_dtm.test)

# once again we'll use CrossTable() from gmodels
#install.packages("gmodels")
CrossTable(sms_test.predicted,
           sms.test$type,
           prop.chisq = FALSE, # as before
           prop.t     = FALSE, # eliminate cell proprtions
           dnn        = c("predicted", "actual")) # relabels rows+cols


wordcloud(corpus.train,
          min.freq=10,          # 10% of num docs in corpus is rough standard
          random.order = FALSE) # biggest words are nearer the centre

