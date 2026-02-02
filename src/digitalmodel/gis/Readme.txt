https://support.esri.com/en/technical-article/000012334

need 2.7, 32 bit python


conda env create -f arcgis-2x.yml
conda env remove -n arcgis-2
https://support.anaconda.com/customer/en/portal/articles/2723599-conda-using-32-bit-and-64-bit-libraries-and-conda_force_32bit
set CONDA_FORCE_32BIT=1


https://gis.stackexchange.com/questions/119503/getting-arcpy-to-work-with-anaconda