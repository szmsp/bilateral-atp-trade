# Bilateral Advanced Technology Products Trade
Pull and view year-on-year trends in U.S. bilateral trade in advanced technology products.

This program is designed as an example of how to download and format international advanced technology trade data from the U.S. Census API. It allows users flexibility to specify the country and time period of interest. 

The U.S. Census designates ten "high tech" fields whose trade is tracked in the Advanced Technology data: biotechnology, life science, opto-electronics, information and communications, electronics, flexible manufacturing, advanced materials, aerospace, weapons, and nuclear technology. Census provides product definitions for each field: https://www.census.gov/foreign-trade/reference/codes/atp/index.html

To access the U.S. Census API, first request an API key online:
http://api.census.gov/data/key_signup.html

Under "Paths and Parameters", specify the following parameters in your program: path, key, initial and final dates to pull, the country code of interest (ctry_code), and import and export variables (import_vars, export_vars).  

Note: Many fields are available for download. The program pulls "general value month" (monthly total value of general imports) and "general value year" (year-to-date total value of general imports) U.S. import fields and "all value month" (monthly total value of exports) and "all value year" (year-to-date total value of exports) U.S. export fields. Fields can be modified if desired.

For help or to learn more about international trade data available from the U.S. Census API, please see the Census guide:
https://www.census.gov/foreign-trade/reference/guides/Guide%20to%20International%20Trade%20Datasets.pdf

Import and export functions were created using a template provided by Yun Tai, CLIR Postdoctoral Fellow at the University of Virginia Library: https://data.library.virginia.edu/using-census-data-api-with-r/.  
