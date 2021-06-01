from selenium import webdriver
from selenium.webdriver.support.ui import Select

import os
import time

driver = webdriver.Chrome(executable_path = "/Users/jim/Documents/drivers/chromedriver")
# driver.get('https://classic.sportsbookreview.com/betting-odds/nba-basketball/money-line/?date=20210201')
driver.get('https://il.sportsbook.fanduel.com/navigation/nba?tab=games')

headers = driver.execute_script("var req = new XMLHttpRequest();req.open('POST', document.location, false);req.send(null);return req.getAllResponseHeaders()")
headers = headers.splitlines()
print(headers)
