import sys
import datetime
import csv
import time
## Python Files
import posts
import comments


try:
    from urllib.request import urlopen, Request
except ImportError:
    from urllib2 import urlopen, Request

# Reading in Text file
with open("./SocialTextFiles/PythonAuth.txt") as f:
    content = f.readlines()
# you may also want to remove whitespace characters like `\n` at the end of each line
content = [x.strip().split(':')[1] for x in content]

# Running Facebook scrape given facebook file id, app secret, and app id

app_id = content[0]
app_secret = content[1]
page_id = str(sys.argv[1])

access_token = app_id + "|" + app_secret

### Running other python script for scraping
if __name__ == "__main__":
	posts.scrapeFacebookPageFeedStatus(page_id, access_token)
	comments.scrapeFacebookPageFeedComments(page_id, access_token)

