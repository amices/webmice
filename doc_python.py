


import requests
host = "http://localhost:8080"


import requests  # Import the requests library

# Query URL
url = ('http://ec.europa.eu/eurostat/wdds/rest/data/v2.1/json/en'
       '/nama_10_gdp?precision=1'
       '&unit=CLV05_MEUR'  # Unit: CLV (2005) Million EUR
       '&geo=NL&geo=DE'  # Country: Netherlands, Germany
       '&time=2010&time=2011&time=2012'  # Years: 2010, 2011, 2012
       '&na_item=B1GQ&na_item=D21'  # GDP (market prices) & taxes on products
       )
# Some api's will have nicer syntax like:
# `&time=2010..2012` or `&na_item=B1GQ,D21`
print(url)

response = requests.get(url)  # Make a GET request to the URL

# Print status code (and associated text)
print(f"Request returned {response.status_code} : '{response.reason}'")

# Print data returned (parsing as JSON)
payload = response.json()  # Parse `response.text` into JSON

import pprint
pp = pprint.PrettyPrinter(indent=1)
pp.pprint(payload)
# NOTE: Could use print(response.json()) but this wouldn't be formatted nicely
