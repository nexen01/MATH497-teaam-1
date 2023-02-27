import numpy as np
import pandas as pd

cleanInitAddresses = pd.DataFrame(columns=["Address", "State", "City", "Zip"])
points = pd.DataFrame(columns=["Lat", "Long", "Coord"])

InitDraws = pd.read_csv('Lead Project\Initial.csv')
SeqDraws = pd.read_csv('Lead Project\Sequential.csv')


#SeqAddresses = SeqDraws["Address"]
cleanInitAddresses["Address"] = InitDraws["Address"].replace({'XX':'00'}, regex=True)
cleanInitAddresses["State"] = "IL"
cleanInitAddresses["City"] = "Chicago"
cleanInitAddresses["Zip"] = ""

#print(cleanInitAddresses.head())

#cleanInitAddresses.to_csv(r'C:\Users\Ibrahim\Documents\VSCode\Lead Project\FixedAddressData.csv', index=False, header=True)
points["Lat"] = 41.82609000019662 
points["Long"] = -87.63098199941716
points["Coord"] = points[["Lat", "Long"]]

print(points["Coord"].head())
#polygon.contains(points["Coord"])