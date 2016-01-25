def start():
    import os
    import re
    import requests
    from bs4 import BeautifulSoup
    from selenium import webdriver
    import time
    import datetime
    
def read_page(driver, url):
    driver.get(url)
    text = driver.page_source
    return text
