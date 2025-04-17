library(reticulate)

# Check which Python is being used
# py_config()
# 
# # Install Selenium in the same environment
# py_install("selenium", pip = TRUE)
# py_install("webdriver-manager", pip = TRUE)

py_code <- "
from selenium import webdriver
from selenium.webdriver.firefox.service import Service
from selenium.webdriver.common.by import By
from webdriver_manager.firefox import GeckoDriverManager

# Setup WebDriver
service = Service(GeckoDriverManager().install())
driver = webdriver.Firefox(service=service)

# Open Wikipedia page
driver.get('https://en.wikipedia.org/wiki/Web_scraping')

# Extract H2 headings
headings = [h2.text for h2 in driver.find_elements(By.TAG_NAME, 'h2')]

# Close driver
driver.quit()

# Store result in a variable accessible from R
result = headings
"

py_file <- tempfile(fileext = ".py")
writeLines(py_code, py_file)

# Run the Python script inside R
source_python(py_file)

# Retrieve output in R
print(result)
