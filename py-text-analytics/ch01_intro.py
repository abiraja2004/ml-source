# http://stackoverflow.com/questions/42382662/using-nltk-corpora-with-aws-lambda-functions-in-python
# http://stackoverflow.com/questions/42394335/paths-in-aws-lambda-with-python-nltk
# serapis - https://github.com/wordnik/serapis/blob/master/lambda_handler.py
# Building a Serverless Customer Service Bot - https://github.com/awslabs/serverless-chatbots-workshop

# NLP keyword extraction tutorial with RAKE and Maui
# https://www.airpair.com/nlp/keyword-extraction-tutorial
# What are the best keyword extraction algorithms for natural language processing and how can they be implemented in Python?
# https://www.quora.com/What-are-the-best-keyword-extraction-algorithms-for-natural-language-processing-and-how-can-they-be-implemented-in-Python
# FUZZY STRING MATCHING IN PYTHON
# http://streamhacker.com/2011/10/31/fuzzy-string-matching-python/

#import nltk
#nltk.download('all')

import nltk

### Brown
from nltk.corpus import brown
print 'Total Categories:', len(brown.categories())
print brown.categories()

# tokenized sentenses
brown.sents(categories='mystery')

# POS tagged sentensences
brown.tagged_sents(categories='mystery')

# get sentences in natural Form
sentences = brown.sents(categories='mystery')
sentences = [' '.join(sentence_token) for sentence_token in sentences]

# get tagged words
tagged_words = brown.tagged_words(categories='mystery')
# get nouns from tagged words - NP, NN as well as NNS, NP$ ...
nouns = [(word, tag) for word, tag in tagged_words if any(noun_tag in tag for noun_tag in ['NP', 'NN'])]
print nouns[0:10]

# build frequency distribution for nouns
nouns_freq = nltk.FreqDist([word for word, tag in nouns])
# print top 10 occurring nouns
print nouns_freq.most_common(10)

### Reuters
from nltk.corpus import reuters

print 'Total Categories', len(reuters.categories())
print reuters.categories()

# get housing and income categories
sentenses = reuters.sents(categories=['housing', 'income'])
sentenses = [' '.join(sentense_token) for sentense_token in sentenses]
print sentenses[0:5]

# fileid based access
print reuters.fileids(categories=['housing', 'income'])
print reuters.sents(fileids=[u'test/16118', u'test/18534'])

### Wordnet
from nltk.corpus import wordnet as wn

word = 'hike' # taking hike as our word of interest
# get word synsets
# Undefined variables from imports: synsets <- Ctrl + 1
# http://stackoverflow.com/questions/2112715/how-do-i-fix-pydev-undefined-variable-from-import-errors
word_synsets = wn.synsets(word)  # @UndefinedVariable
print word_synsets

# get details for each synonym in synset
for synset in word_synsets:
    print 'Synset Name:', synset.name()
    print 'POS Tag: ', synset.pos()
    print 'Definition: ', synset.definition()
    print 'Examples: ', synset.examples()

    

