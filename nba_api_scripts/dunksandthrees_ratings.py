from selenium.webdriver import Chrome, ChromeOptions
from bs4 import BeautifulSoup

base_path = "/Users/johnschroeder/Documents/gambling_stuff/data/01_raw/nba_team_ratings"

url = "https://dunksandthrees.com/"

chrome_options = ChromeOptions()
chrome_options.add_argument("--headless")

driver = Chrome(
    executable_path="/Users/johnschroeder/Documents/gambling_stuff/chromedriver",
    options=chrome_options,
)
