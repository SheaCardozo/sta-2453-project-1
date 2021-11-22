from selenium.webdriver import Chrome
from selenium.webdriver.common.by import By
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.support.ui import WebDriverWait
from bs4 import BeautifulSoup

from time import sleep

PAGES = 50

with Chrome(executable_path=".\\driver\\chromedriver.exe") as driver:
    driver.implicitly_wait(10)
    totallist = []

    driver.get(f"https://www.realtor.ca/on/toronto/condos-for-sale")

    for iter in range(4):
        print(f"Iteration: {iter}")
        linklist = []
        input()

        for i in range(1, PAGES+1):

            soup=BeautifulSoup(driver.page_source, features="html.parser")
            sales = soup.find_all(attrs={'class':'blockLink listingDetailsLink'})

            links = [sale.attrs['href'] for sale in sales]

            linklist.extend(links)

            WebDriverWait(driver, 10).until(
                EC.presence_of_element_located((By.XPATH, "/html/body/form/div[5]/div[2]/span/div/div[4]/div[3]/span/span/div/a[3]"))).click()

            sleep(2)

        totallist.extend(list(set(linklist)))

totallist = list(set(totallist))
print(len(totallist))

with open(".\\data\\condolinks.txt", "w+") as f:
    for link in totallist:
        f.write("https://www.realtor.ca" + link + "\n")