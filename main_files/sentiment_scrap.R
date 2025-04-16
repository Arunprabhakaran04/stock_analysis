library(reticulate)
 
virtualenv_create("r-reticulate")

use_virtualenv("r-reticulate", required = TRUE)

py_install(c("selenium", "beautifulsoup4", "pandas", "webdriver-manager"), pip = TRUE)

py_code <- "
from datetime import datetime
import time
import pandas as pd
from bs4 import BeautifulSoup
from selenium import webdriver
from selenium.webdriver.firefox.options import Options
from selenium.webdriver.firefox.service import Service
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from webdriver_manager.firefox import GeckoDriverManager

def convert_date(date_str):
    dt = datetime.strptime(date_str, '%b-%d-%y')
    return dt.strftime('%Y-%m-%d')

def setup_driver():
    options = Options()
    options.add_argument('--disable-gpu')
    options.add_argument('--window-size=1920,1080')

    service = Service(GeckoDriverManager().install())
    driver = webdriver.Firefox(service=service, options=options)
    return driver

def scrap_data(ticker):
    url = f'https://finviz.com/quote.ashx?t={ticker}'
    retry_count = 0
    max_retries = 3

    while retry_count < max_retries:
        driver = None
        try:
            driver = setup_driver()
            driver.set_page_load_timeout(20)
            driver.get(url)

            WebDriverWait(driver, 15).until(EC.presence_of_element_located((By.CLASS_NAME, 'body-table-news-wrapper')))
            soup = BeautifulSoup(driver.page_source, 'html.parser')
            news_table = soup.find('div', class_='body-table-news-wrapper')

            if not news_table:
                raise ValueError('News table not found.')

            parsed_data = []
            rows = news_table.findAll('tr')
            current_date = None

            for row in rows:
                a_tag = row.find('a', class_='tab-link-news')
                time_cell = row.td
                if a_tag and time_cell:
                    time_text = time_cell.text.strip()
                    if len(time_text.split()) > 1:
                        current_date = time_text.split()[0]
                    if current_date:
                        parsed_data.append([current_date, a_tag.text.strip()])

            df = pd.DataFrame(parsed_data, columns=['date', 'title'])
            df['date'] = df['date'].apply(convert_date)
            df['ticker'] = ticker

            return df

        except Exception as e:
            retry_count += 1
            time.sleep(2)
        finally:
            if driver:
                try:
                    driver.quit()
                except:
                    pass

    return pd.DataFrame(columns=['date', 'title', 'ticker'])

df = scrap_data('HDB')
"

py_file <- tempfile(fileext = ".py")
writeLines(py_code, py_file)
source_python(py_file)

print(df)
path <- "D:\\R\\stock_sentiment_analysis\\main_files\\HSB_sentiment.csv"
write.csv(df, file = path)