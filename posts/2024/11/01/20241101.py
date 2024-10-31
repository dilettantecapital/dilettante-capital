import os
import time

import bs4
import selenium.webdriver
import webdriver_manager.firefox


# CONSTANTS


_PAUSE = 1 / 30
_BY_SELECTOR = selenium.webdriver.common.by.By.CSS_SELECTOR
_BY_XPATH = selenium.webdriver.common.by.By.XPATH


# FUNCTIONS


def cook(html):
    return bs4.BeautifulSoup(html, "lxml")


def driver_id(driver):
    return driver.find_element(_BY_XPATH, "html").id


def driver_read(driver, url):
    hold = driver_id(driver)
    driver.get(url)
    while hold == driver_id(driver):
        time.sleep(_PAUSE)
    return cook(driver.page_source)


# SCRIPT


if __name__ == "__main__":
    # setup
    os.environ["WDM_LOG_LEVEL"] = "0"
    
    # options
    opts = selenium.webdriver.firefox.options.Options()
    opts.add_argument("--headless")
    
    # webdriver
    gdm = webdriver_manager.firefox.GeckoDriverManager
    path = gdm(print_first_line=False).install()
    
    # initialization
    driver = selenium.webdriver.Firefox(executable_path=path, options=opts)

    # usage
    symbols = ["^SPX", "^IXIC", "^DJI", "^RUT", "000001.SS", "^HSI", "^N225",
               "^NSEI", "^GDAXI", "^FCHI", "^GSPTSE", "^AXJO", "^VIX", "^TNX",
               "CL=F", "GC=F", "DX-Y.NYB"]

    for sym in symbols:
        url = f"https://finance.yahoo.com/quote/{sym}/history/"
        fpath = f"data/html/{sym}.html"

        soup = driver_read(driver, url)
        with open(fpath, "w", encoding = "utf8") as fh:
            fh.write(str(soup))
