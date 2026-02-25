import zipfile
import os
import xml.etree.ElementTree as ET
import pandas as pd
import geopandas as gpd

# Define the path to the KMZ file
kmz_file_path = (
    r"C:\github\doris\61863_lakach\data\61863feed\gis\POLIGONOS FISICOS_FCC_KMZ-KML.kmz"
)

# Define a temporary directory to extract the KMZ contents
temp_dir = os.path.join(os.path.dirname(kmz_file_path), "temp_kmz_extract")
os.makedirs(temp_dir, exist_ok=True)

# Extract the KMZ file
with zipfile.ZipFile(kmz_file_path, "r") as kmz:
    kmz.extractall(temp_dir)

# Locate the KML file within the extracted contents
kml_file_path = None
for root, dirs, files in os.walk(temp_dir):
    for file in files:
        if file.endswith(".kml"):
            kml_file_path = os.path.join(root, file)
            break
    if kml_file_path:
        break

if kml_file_path:
    # Parse the KML file
    tree = ET.parse(kml_file_path)
    root = tree.getroot()

    # Extract data from the KML file (for demonstration purposes)
    data = []
    for placemark in root.findall(".//{http://www.opengis.net/kml/2.2}Placemark"):
        name = placemark.find("{http://www.opengis.net/kml/2.2}name").text
        coordinates = placemark.find(
            ".//{http://www.opengis.net/kml/2.2}coordinates"
        ).text.strip()
        lon, lat, _ = map(float, coordinates.split(","))
        data.append({"Name": name, "Longitude": lon, "Latitude": lat})

    # Convert to a pandas DataFrame
    df = pd.DataFrame(data)
    print(df)

    # Convert to a GeoDataFrame
    gdf = gpd.GeoDataFrame(df, geometry=gpd.points_from_xy(df.Longitude, df.Latitude))
    print(gdf)
else:
    print("No KML file found in the KMZ archive.")

# Clean up the temporary directory
import shutil

shutil.rmtree(temp_dir)
