# Bilateral Advanced Technology Products Trade
Pull and view year-on-year trends in U.S. bilateral trade in advanced technology products.

This program is designed to download and format international advanced technology trade data from the U.S. Census API. It allows users flexibility to specify the country and time period of interest. 

To access the U.S. Census API, first request an API key on their website:
http://api.census.gov/data/key_signup.html

Under "Paths and Parameters", specify the following parameters in your program: file_path, census_key, year_month, ctry_code, import_vars, and export_vars.  

The U.S. Census designates ten "high tech" fields whose trade is tracked in the Advanced Technology data: biotechnology, life science, opto-electronics, information and communications, electronics, flexible manufacturing, advanced materials, aerospace, weapons, and nuclear technology. Census provides product definitions for each field: https://www.census.gov/foreign-trade/reference/codes/atp/index.html

Note: Many fields are available for download. The program pulls "general value month" (monthly total value of general imports) and "general value year" (year-to-date total value of general imports) U.S. import fields and "all value month" (monthly total value of exports) and "all value year" (year-to-date total value of exports) U.S. export fields. Fields can be modified if desired.

For help or to learn more about international trade data available from the U.S. Census API, please see the Census guide:
https://www.census.gov/foreign-trade/reference/guides/Guide%20to%20International%20Trade%20Datasets.pdf

Import and export functions were created using a template provided by Yun Tai, CLIR Postdoctoral Fellow at the University of Virginia Library: https://data.library.virginia.edu/using-census-data-api-with-r/.  
