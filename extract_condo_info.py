from selenium.webdriver import Chrome
from bs4 import BeautifulSoup

from time import sleep

import csv
import json


with open(".\\data\\condolinks.txt", "r+") as f:
    links = [s.strip() for s in f.readlines()]


check = """<html style="height:100%"><head><meta name="ROBOTS" content="NOINDEX, NOFOLLOW">"""

key_lst = ['Address1', 'Address2', 'propertyID', 'listingID', 'propertyType', 'price', 'leasePrice', 'buildingType', 'neighbourhood', 'bathrooms', 'bedrooms', 'communityFeatures', 'nearbyAmenities', 'storeys', 'walkscore', 'transitscore', 'machineryIncluded', 'rentalEquipment', 'parkingType', 'majorBusinessType', 'minorBusinessType', 'totalUnits', 'landSize', 'zoningType', 'interiorFloorSpace', 'exteriorBuildingSize', 'franchise', 'buildingAmenities', 'buildingStyle', 'basementDevelopment', 'notificationUpdate', 'anchor', 'retailStorefront', 'clearCeilingHeight', 'hasphoto', 'photos', 'multimediaFeatures', 'listingVideoType', 'city', 'province', 'soldData', 'hasAlternateFeatureSheet', 'hasOpenHouse', 'hasVirtualOpenHouse', 'hasCMHCFTHBBanner', 'hasTDpreApprovalIcon', 'hasActiveVirtualOpenHouse', 'Property Type', 'Building Type', 'Community Name', 'Title', 'Annual Property Taxes', 'Parking Type', 'Time on REALTOR.ca', 'Above Grade', 'Total', 'Features', 'Building Amenities', 'Cooling', 'Heating Type', 'Exterior Finish', 'Pool Type', 'Community Features', 'Amenities Nearby', 'Maintenance Fees', 'Maintenance Management Company', 'Total Parking Spaces']
sales_info = []

with Chrome(executable_path=".\\driver\\chromedriver.exe") as driver:
    driver.implicitly_wait(10)
    for n, link in enumerate(links):

        if n % 100 == 0:
            print(n)

        driver.get(link)

        source = driver.page_source

        sleep(2)

        if source[:len(check)] == check:
            input()
            sleep(1)

        sale_soup = BeautifulSoup(driver.page_source, features="html.parser")

        sale_property = sale_soup.find(attrs={'id':'GTMDataLayerCode'}).text

        sale_property = sale_property[(sale_property.find("property: {") + 10):]
        sale_property = sale_property[:(sale_property.find('}')+1)]
        sale_property = sale_property.replace("'", "\"").replace(' ','').replace('\t','')

        tmp = "{\""

        for s in sale_property[2:]:
            if s == ":":
                tmp += "\""
            if s == "\n":
                tmp += "\""
            else:
                tmp += s

        sale_property = tmp[:-2]+"}"
        property_dict = json.loads(sale_property)

        # extract more info
        sale_address = sale_soup.find(attrs={'class':'listingTopDetailsLeft'}).h1.contents
        if sale_address[0] == 'Address not available':
            address_dict = {'Address1': 'N/A', 'Address2': 'N/A'}
        else:
            address_dict = {'Address1': sale_address[0], 'Address2': sale_address[2]}
        soup_building_info = sale_soup.find_all(attrs={'class':'propertyDetailsSectionContentSubCon'})
        building_info_dict = dict()
        for info in soup_building_info:
            building_info_dict[info.find(attrs={'class':'propertyDetailsSectionContentLabel'}).string] = info.find(attrs={'class':'propertyDetailsSectionContentValue'}).text    
        
        full_dict = address_dict.copy() 
        full_dict.update(property_dict)
        full_dict.update(building_info_dict)
        
        # add missing field with ''
        for key in key_lst:
            if key not in list(full_dict.keys()):
                full_dict[key] = ''
                
        reordered_dict = {k: full_dict[k] for k in key_lst}
        
        sales_info.append(reordered_dict)

with open('.\\data\\realtor.csv', 'w+', newline="") as csvfile:
    writer = csv.DictWriter(csvfile, fieldnames = key_lst)
    writer.writeheader()
    writer.writerows(sales_info)
