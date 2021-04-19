from selenium import webdriver
from selenium.webdriver.support.ui import Select

import os
import time

driver = webdriver.Chrome(executable_path = "/Users/jim/Documents/drivers/chromedriver")
# driver.get('https://classic.sportsbookreview.com/betting-odds/nba-basketball/money-line/?date=20210201')
driver.get('https://il.sportsbook.fanduel.com/sports/navigation/830.1/10107.3')
