from selenium.webdriver import Chrome
from selenium.webdriver.common.by import By
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.support.ui import WebDriverWait
from time import sleep



with Chrome(executable_path=".\\webdriver\\driver\\chromedriver.exe") as driver:
    driver.implicitly_wait(10)
    driver.get("https://condos.ca/toronto/condos-for-sale?sublocality_id=14")

    # Waits until condo map is loaded
    WebDriverWait(driver, 10).until(
        EC.presence_of_element_located((By.XPATH, "/html/body/div[1]/div/main/div/div/div[2]/div/div[2]/button")))

    # Closes map
    WebDriverWait(driver, 10).until(
        EC.presence_of_element_located((By.XPATH, "/html/body/div[1]/div/main/div/div/div[2]/div/span/button"))).click()

    sleep(30)
