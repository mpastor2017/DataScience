import pandas as pd
from nltk.sentiment.vader import SentimentIntensityAnalyzer
from nltk.corpus import stopwords 
from nltk.stem.wordnet import WordNetLemmatizer
from langdetect import detect
import string
import re
## API Call for spanish sentiment
import indicoio
indicoio.config.api_key = '84d012ec3864706e819c1194737b5406'

# Reading in Text file
with open("./SocialTextFiles/PythonSentiment.txt") as f:
    content = f.readlines()
# you may also want to remove whitespace characters like `\n` at the end of each line
content = [x.strip().split(',') for x in content]

with open("./SocialTextFiles/FacebookStatus.txt") as g:
    content2 = g.readlines()
content2 = [x.strip().split(',') for x in content2]
'''
Function Call: Text Cleansing

Parameters
    doc: line of text to be cleansed
Returns: cleansed line of text
'''

def clean(doc):
	# For word cleaning
	stop = set(stopwords.words('english'))
	exclude = set(string.punctuation) 
	lemma = WordNetLemmatizer()
	stop_free = " ".join([i for i in doc.lower().split() if i not in stop])
	punc_free = ''.join(ch for ch in stop_free if ch not in exclude)
	normalized = " ".join(lemma.lemmatize(word) for word in punc_free.split())
	return normalized


## Exception Handling version
def clean_detect(x):
    try:
        return detect(x)
    except:
        return 'en'
		
'''
Function Call: Provides sentiment analysis using the VADER algorithm

http://comp.social.gatech.edu/papers/icwsm14.vader.hutto.pdf

Parameters
    csv: csv
    text_column: Column with text for sentiment analysis
Returns: Dataframe of CSV with added sentiment metrics from VADER
'''

def sentiment_analyzer(csv, text_column):
	df = pd.read_csv('./UnprocessedCSV/' + csv, encoding = 'latin-1')
    # Casting text column to string
	df[text_column] = df[text_column].map(str)
    # Removing URLs from text
	df[text_column] = df[text_column].apply(lambda x: re.sub(r"http\S+", "", x))
    
    # Initializing lists to append sentiment scores to
	compound = []
	sid = SentimentIntensityAnalyzer()
    
    ## detecting spanish and subsetting the
	df['language'] = df[text_column].apply(clean_detect)
	df2 = df[df['language'] == 'es']
	df = df[df['language'] != 'es']
    
	for comment in df[text_column]:
        ## Refer to clean function below
		comment = clean(comment)
		ss = sid.polarity_scores(comment)
		compound.append(ss['compound'])
        
	df[text_column + '_' + 'compound'] = compound
    
	if df2.empty:
		return df
	else:
		df2[text_column + '_' + 'compound'] = indicoio.sentiment(list(df2[text_column]), language = 'spanish')
        
        # Scaling to [-1,1]
		df2[text_column + '_' + 'compound'] = (2 * df2[text_column + '_' + 'compound']) - 1
        
		df = pd.concat([df,df2])

	return df

	
## Function to add a normalized version of the column specified
'''
Function Call: Provides a Z-score for a specified column
Parameters
    df: Pandas Dataframe
    column: Numeric column to be normalized
Returns: Same dataframe as before with an appaended normalized column
'''

def normalizer(df, column):
    df['Compound Norm'] = (df[column] - df[column].mean()) / df[column].std()
    return df

def main_func():
	fb = pd.DataFrame()
	twit = pd.DataFrame()
	for i in content:
		if i[2] == 'comment_message':
			fb_df = sentiment_analyzer(i[1],i[2])
			fb_df['candidate'] = i[0]
			fb_df = normalizer(fb_df, i[2] + '_compound')
			fb = pd.concat([fb,fb_df])
		else:
			twit_df = sentiment_analyzer(i[1],i[2])
			twit_df['candidate'] = i[0]
			twit_df = normalizer(twit_df, i[2] + '_compound')
			twit = pd.concat([twit,twit_df])
		
	## Concatenating Facebook Status Updates
	fb_status = pd.DataFrame()
	for i in content2:
		fb_stat = pd.read_csv(i[1])
		fb_stat['candidate'] = i[0]
		fb_status = pd.concat([fb_status,fb_stat])

	fb.to_csv("./ProcessedCSV/Facebook.csv", index = False)
	twit.to_csv("./ProcessedCSV/Twitter.csv", index = False)
	fb_status.to_csv("./ProcessedCSV/FB_Status.csv", index = False)
	
# Running Main function	
main_func()