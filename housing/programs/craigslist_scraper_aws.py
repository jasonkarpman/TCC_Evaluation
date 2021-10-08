__author__ = "Sam Lau, Luskin Center"
__date__ = "2019/07/16; Last Updated: 2021/01/04 by Jason Karpman"

from datetime import datetime as dt
from datetime import date, time, timedelta
from lxml import html
from sqlalchemy import create_engine
from random import randint
from time import sleep
from slack import WebClient
import watchtower
import sys
import logging
import urllib
import requests
import re
import pandas as pd
import boto3

'''
## Function definition ##
'''
def get_str(list):
	'''
	The xpath() function returns a list of items that may be empty. Most of the time,
	we want the first of any strings that match the xml query. This helper function
	returns that string, or null if the list is empty.
	'''
	if len(list) > 0:
		str_ascii = list[0].encode('ascii', 'ignore').decode('ascii')

		return str_ascii

	return ''

def get_int_prefix(str, label):
	'''
	Bedrooms and square footage have the format "xx 1br xx 450ft xx". This helper 
	function extracts relevant integers from strings of this format.
	'''
	for s in str.split(' '):
		if label in s:
			return s.strip(label)
			
	return ''

def parseListing(item):
	'''
	Note that xpath() returns a list with elements of varying types depending on the
	query results: xml objects, strings, etc.
	'''
	pid = repostid = dt = url = title = price = neighb = beds = sqft = ''

	# these items should always be present
	pid = get_str(item.xpath('@data-pid'))

	# Extract two lines of listing info
	line1 = item.xpath('div[@class="result-info"]')[0]
	
	dt = get_str(line1.xpath('time/@datetime'))
	url = get_str(line1.xpath('h3/a/@href'))

	# these items maybe not always be present
	repostid = get_str(item.xpath('@data-repost-of'))
	title = get_str(line1.xpath('h3/a/text()'))

	price = get_str(line1.xpath('span[@class="result-meta"]/span[@class="result-price"]/text()')).strip('$')
	neighb = get_str(line1.xpath('span[@class="result-meta"]/span[@class="result-hood"]/text()')).strip(' ()')
	bedsqft = get_str(line1.xpath('span[@class="result-meta"]/span[@class="housing"]/text()'))
	
	beds = get_int_prefix(bedsqft, "br")  # appears as "1br" to "8br" or missing
	sqft = get_int_prefix(bedsqft, "ft")  # appears as "000ft" or missing

	listing_data = {'pid': pid, 'repostid': repostid, 'dt': dt, 'url': url, 'title': title, 'price': price, 'neighb': neighb, 'beds': beds, 'sqft': sqft}

	return listing_data

def parsePostText(tree):
	'''
	Combine list of listing text and remove nextline tags
	'''
	body = ' '.join(tree.xpath('//*[@id="postingbody"]/text()'))

	if body != '':
		body1 = ''.join(body.splitlines()).strip()
		body2 = body1.encode('ascii', 'ignore').decode('ascii')
		bodytext = re.sub(' +', ' ', body2)

		return bodytext
	else:
		return ''

def scrapeListingPage(url):
	'''
	Grab listing page location and text data
	'''
	page = requests.get(url)
	if page.status_code != 200:
		page_error = page + ', ' + search_url
		print(page_error)
		logging.info(page_error)

	tree = html.fromstring(page.content)
	
	map = tree.xpath("//div[contains(@id, 'map')]")

	# Sometimes there's no location info, and no map on the page
	lat = lng = accuracy = address = posttext = ''
	
	if len(map) > 0:
		map = map[0]
		lat = get_str(map.xpath('@data-latitude'))
		lng = get_str(map.xpath('@data-longitude'))
		accuracy = get_str(map.xpath('@data-accuracy'))
		address = get_str(map.xpath("//div[contains(@class, 'mapaddress')]/text()"))

	posttext = parsePostText(tree)

	listingpage_data = {'lat': lat, 'lng': lng, 'accuracy': accuracy, 'address': address, 'posttext': posttext}
	
	return listingpage_data


## Download the data ##
#
# from Baird:
# note if sys.argv[1] doesn't work:
# import subprocess
# instance_id_output = subprocess.check_output(["ec2-metadata", "-i"])
# instance_id = instance_id_output[12:-1]
#

instance_id = sys.argv[1]
if instance_id == 'XXXX':
	region = 'Fresno'
	domains = ['https://fresno.craigslist.org/search/apa', 'https://fresno.craigslist.org/search/reb', 'https://fresno.craigslist.org/search/reo', 'https://fresno.craigslist.org/search/roo', 'https://fresno.craigslist.org/search/sub', 'https://fresno.craigslist.org/search/vac']

#elif instance_id == 'XXXX':
elif instance_id == 'XXXX':
	region = 'Watts1'
	domains = ['https://losangeles.craigslist.org/search/apa']

elif instance_id == 'XXXX':
	region = 'Watts2'
	domains = ['https://losangeles.craigslist.org/search/reb', 'https://losangeles.craigslist.org/search/reo', 'https://losangeles.craigslist.org/search/roo', 'https://losangeles.craigslist.org/search/sub', 'https://losangeles.craigslist.org/search/vac']

elif instance_id == 'XXXX':
	region = 'Ontario'
	domains = ['https://inlandempire.craigslist.org/search/apa', 'https://inlandempire.craigslist.org/search/reb', 'https://inlandempire.craigslist.org/search/reo', 'https://inlandempire.craigslist.org/search/roo', 'https://inlandempire.craigslist.org/search/sub', 'https://inlandempire.craigslist.org/search/vac']

#begin = dt.now() - timedelta(hours = 8) # convert AWS GMT to PST (-8 daylight savings, normally -7)
begin = dt.now() # convert AWS GMT to PST (-8 daylight savings, normally -7)
print(begin)
# get listings from last 24 hours
latest_ts = begin.replace(microsecond = 0, second = 0, minute = 0)
earliest_ts = latest_ts - timedelta(hours = 24)

time_stamp = begin.strftime('%Y%m%d-%H%M%S')
log_fname = '/home/ec2-user/housing/data/raw/craiglist_log_file_' + time_stamp + '.log'
logging.basicConfig(filename=log_fname, level=logging.INFO)

listings_dict = {}
for domain in domains:
	listings_all = []
	regionIsComplete = False
	search_url = domain
	logging.info('Beginning New Domain')
	print('Beginning New Domain')

	# grab listings for all pages at once (max: 3000 listings, 120 listings ea. pg.)
	pagesDone = False
	while not pagesDone:
		print(search_url)
		logging.info(search_url)	

		# get page request
		page = requests.get(search_url)
		if page.status_code != 200:
			page_error = page + ', ' + search_url
			print(page_error)
			logging.info(page_error)

		tree = html.fromstring(page.content)

		# Each listing on the search results page is labeled as <p class="row">
		listings = tree.xpath('//*[@id="sortable-results"]/ul/li')
		listings_all.extend(listings)

		# Go to next listing page
		next = tree.xpath('//a[@title="next page"]/@href')
		if next == ['', ''] or len(next) == 0:
			pagesDone = True
			logging.info('Reached Last Page')
			print('Reached Last Page')
		else:
			search_url = domain.split('/search')[0] + next[0]

		sleep(randint(1, 5))

	listings_dict.update({domain: listings_all})

	num_listings = 'Number of listings: %s' % len(listings_dict[domain])
	logging.info(num_listings)
	print(num_listings)

# get information for all listings
rows_list = []
for domain in listings_dict:
	domainIsComplete = False

	logging.info('Beginning New Domain')
	print('Beginning New Domain')
	logging.info(domain)
	print(domain)

	now = dt.now() - timedelta(hours = 7)
	now_timestamp = now.strftime('%Y-%m-%d %H:%M')
	logging.info(now_timestamp)
	print(now_timestamp)

	while not domainIsComplete:
		for item in listings_dict[domain]:
			try:
				row = parseListing(item)
				item_ts = dt.strptime(row['dt'], '%Y-%m-%d %H:%M')

				if (item_ts > latest_ts):
					# Skip this item but continue parsing search results
					if row['url'] != '':
						time_error = 'Over latest timestamp: %s' % row['url']
						logging.info(time_error)
						print(time_error)
					continue
				
				if (item_ts < earliest_ts):
					# Break out of loop and move on to the next region
					domainIsComplete = True
					logging.info('Reached Timestamp Cutoff')
					print('Reached Timestamp Cutoff')
					break

				# Parse listing page
				row_page = scrapeListingPage(row['url'])
				row.update(row_page)
				row.update({'domain': domain})

				logging.info(row)
				rows_list.append(row)

			except Exception as e:
				# Skip listing if there are problems parsing it
				logging.warning('%s: %s' % (type(e).__name__, e))

				if row['url'] != '':
					url_error = 'Error with listing: %s' % row['url']
					logging.info(url_error)
					print(url_error)
				continue

			sleep(randint(1, 10))

		domainIsComplete = True

# overall endtime
#end = dt.now() - timedelta(hours = 8) # convert AWS GMT to PST (for daylight savings)
end = dt.now() # convert AWS GMT to PST (for daylight savings)
print(end)
end_timestamp = end.strftime('%Y-%m-%d %H:%M')
begin_timestamp = begin.strftime('%Y-%m-%d %H:%M')

'''
## Upload the data ##
'''
craigslist_df = pd.DataFrame(rows_list)
craigslist_df['region'] = region
craigslist_df['begts'] = begin_timestamp
craigslist_df['endts'] = end_timestamp
logging.info(craigslist_df)
print(craigslist_df)

# try uploading the data to MySQL
try:
	engine = create_engine('XXXX', echo=True)
	craigslist_df.to_sql(name='craigslist_table', con=engine, if_exists='append', index=False, chunksize=500)

	logging.info('Domain listings written to MySQL')
	print('Domain listings written to MySQL')

except Exception as e:
	logging.warning('%s: %s' % (type(e).__name__, e))

# try saving the test data
try:
	file_name = "/home/ec2-user/housing/data/raw/craigslist_test_data.csv"
	craigslist_df.to_csv(file_name)

except Exception as e:
	logging.warning('%s: %s' % (type(e).__name__, e))

'''
## Update slack channel ##
'''
num_records = len(rows_list)
chat_text = "{} Program started at: {}, Program ended at: {}, # of Records: {}".format(region, begin_timestamp, end_timestamp, num_records)

sc = WebClient("XXXX")

response = sc.chat_postMessage(
	channel="craigslistscraper",
	text=chat_text
)
logging.info(response)
print(response)

'''
## Close the EC2 instance when finished ##
'''
ec2 = boto3.resource('ec2', aws_access_key_id='XXXX', aws_secret_access_key='XXXX', region_name='XXXX')
instance = ec2.Instance(instance_id)

txt = 'Closing Instance: {}'.format(instance_id)
logging.info(txt)
print(txt)

instance.stop()
