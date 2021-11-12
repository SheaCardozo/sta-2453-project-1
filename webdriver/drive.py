from selenium.webdriver import Chrome
from selenium.webdriver.common.by import By
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.support.ui import WebDriverWait
from selenium.common.exceptions import StaleElementReferenceException
from time import sleep

PAGES = 5

with Chrome(executable_path=".\\webdriver\\driver\\chromedriver.exe") as driver:
    driver.implicitly_wait(10)
    aptlist = []

    for i in range(1, PAGES+1):
        driver.get(f"https://condos.ca/toronto/condos-for-sale?sublocality_id=14&page={i}")


        # Code to automatically close interactive map... don't think this is needed
        '''
        # Waits until condo map is loaded
        WebDriverWait(driver, 10).until(
            EC.presence_of_element_located((By.XPATH, "/html/body/div[1]/div/main/div/div/div[2]/div/div[2]/button")))

        #Closes map
        WebDriverWait(driver, 10).until(
            EC.presence_of_element_located((By.XPATH, "/html/body/div[1]/div/main/div/div/div[2]/div/span/button"))).click()
        '''

        attempts = 0
        while(attempts < 2):
            try:
                buff = [apt.get_attribute('href') for apt in \
                    driver.find_element(By.XPATH, "/html/body/div[1]/div/main/div/div/div[1]/div/div[3]/div[3]/div[3]/div/div[1]/div[1]/div/div[1]/div/div") \
                        .find_elements(By.TAG_NAME, "a")]
            except StaleElementReferenceException:
                pass
            attempts += 1

        aptlist.extend(buff)

    for apt in aptlist:
        print(apt)

    print(len(aptlist))