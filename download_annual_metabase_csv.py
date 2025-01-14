# %%
# # import requests
import sys
from selenium import webdriver
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.common.alert import Alert
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
import pandas as pd
import re  # Regular expressions!
import time
import os
import shutil
import glob
from datetime import datetime


# %%
def wait_for_download(directory, file_pattern, timeout=60, check_interval=5):
    """
    Wait for a file matching the file_pattern to appear in the directory and finish downloading.

    :param directory: Directory to watch
    :param file_pattern: File pattern to match
    :param timeout: Maximum time to wait (in seconds)
    :param check_interval: Time interval between checks (in seconds)
    :return: Path to the downloaded file if found and stable, None if timeout occurs
    """
    start_time = time.time()

    while time.time() - start_time < timeout:
        files = glob.glob(os.path.join(directory, file_pattern))
        if files:
            file_path = files[0]
            initial_size = -1
            while initial_size != os.path.getsize(file_path):
                initial_size = os.path.getsize(file_path)
                time.sleep(check_interval)
            return file_path
        time.sleep(check_interval)

    return None


# %%
## Read in options file that has log-in info for metabase
my_opts_filename = (
    # "C:/Users/CMADSEN/Downloads/LocalR/long_term_projects/ZQMussels/Options.csv"
    sys.argv[1]
)

print(my_opts_filename)

my_opts = pd.read_csv(my_opts_filename)

the_year = int(my_opts["year"].iloc[0])
# Create the WebDriver instance outside the loop
driver = webdriver.Chrome()

url = "https://metabase-7068ad-prod.apps.silver.devops.gov.bc.ca/question/366"

driver.get(url)

WebDriverWait(driver, 10).until(EC.presence_of_element_located((By.NAME, "username")))

username_slot = driver.find_element(By.NAME, "username")
password_slot = driver.find_element(By.NAME, "password")

username_slot.send_keys(my_opts["metabase_login"])
password_slot.send_keys(my_opts["metabase_password"])

# Find 'submit' button.
buttons = driver.find_elements(By.TAG_NAME, "button")
buttons[0].click()

# %%
# Navigate to collection of queries.
# collection_url = (
#    "https://metabase-7068ad-prod.apps.silver.devops.gov.bc.ca/question/366"
# )
#
# driver.get(collection_url)

# Find the download button.
WebDriverWait(driver, 20).until(
    EC.presence_of_element_located((By.CLASS_NAME, "Icon-download"))
)
download_button = driver.find_element(By.CLASS_NAME, "Icon-download")

download_button.click()

# %%
# New popup
WebDriverWait(driver, 10).until(
    EC.presence_of_element_located((By.CLASS_NAME, "PopoverContainer"))
)
new_popup = driver.find_element(By.CLASS_NAME, "PopoverContainer")

# Find the CSV download button (it's the first of these 'forms')
WebDriverWait(driver, 10).until(EC.presence_of_element_located((By.TAG_NAME, "form")))
csv_download_button = driver.find_element(By.TAG_NAME, "form")

csv_download_button.click()

# %%
# Wait for 3 minutes for download, just in case it takes that long.
time.sleep(180)
# We need to wait for this download to complete.

# %%

# Define the path to the Downloads folder
downloads_folder = os.path.expanduser("~/Downloads")

now = datetime.now()

# Extract the day of the month
current_day = str(now.day)
if len(current_day) == 1:
    current_day = "0" + current_day

# Extract the month as a number
current_month = str(now.month)
if len(current_month) == 1:
    current_month = "0" + current_month

# Extract year
current_year = str(now.year)
if len(current_year) == 1:
    current_year = "0" + current_year

# Define the pattern to find the file (modify according to your file's pattern)
file_pattern = f"query_result_{current_year}-{current_month}-{current_day}*csv"

# Wait until the download of this file is complete.

# %%

downloaded_file = wait_for_download(downloads_folder, file_pattern)

# %%

# Test that the download worked.
files = glob.glob(os.path.join(downloads_folder, file_pattern))

if files:
    downloaded_file = files[0]

    # Define the path to the network drive folder
    network_drive_folder = r"J:\2 SCIENCE - Invasives\SPECIES\Zebra_Quagga_Mussel\Operations\Watercraft Inspection Data\Raw inspection data for sharing (all years)\Clean files all years"  # Example path

    # Define the new file name
    new_file_name = f"metabase_{str(the_year)}.csv"
    new_file_path = os.path.join(downloads_folder, new_file_name)

    # Rename the file
    os.rename(downloaded_file, new_file_path)
    print(f"Renamed file to {new_file_path}")

    # LAN folder path plus file name
    lan_file_path = os.path.join(network_drive_folder, new_file_name)

    # Remove the old version of this file that's on the J: drive, if it exists.
    old_file_on_lan = glob.glob(lan_file_path)

    if old_file_on_lan != []:
        # Pull out the full path to the old metabase summary file.
        old_file_on_lan = old_file_on_lan[0]
        # Delete the old file currently in the LAN folder
        os.remove(old_file_on_lan)
        # Copy over the minty-fresh metabase summary file, from C:/.../Downloads to the LAN folder (J: for me)
        shutil.copyfile(new_file_path, old_file_on_lan)
        # Remove the metabase summary from the local Downloads folder
        os.remove(new_file_path)
else:
    shutil.copyfile(src=new_file_path, dst=lan_file_path)

print("Finished downloading metabase file from the website! Transferring to R now.")
driver.quit()

# %%
