import os
from pathlib import Path


def GetData_download_file_from_url(cfg):
    from common.data import GetData
    get_data = GetData()

    get_data.download_file_from_url(cfg)

# Bottom Hole Pressure;
cfg = {
    'url': 'https://www.data.bsee.gov/Well/Files/BHPSRawData.zip',
    'download_to': os.path.abspath(Path("./data_manager/data/bsee"))
}
GetData_download_file_from_url(cfg)

# BoreHole Data
cfg = {
    'url': 'https://www.data.bsee.gov/Well/Files/BoreholeRawData.zip',
    'download_to': os.path.abspath(Path("./data_manager/data/bsee"))
}
GetData_download_file_from_url(cfg)

# Decomm Cost Estimate
cfg = {
    'url': 'https://www.data.bsee.gov/Leasing/Files/DecomCostEstRawData.zip',
    'download_to': os.path.abspath(Path("./data_manager/data/bsee"))
}
GetData_download_file_from_url(cfg)

# API Data
cfg = {
    'url': 'https://www.data.bsee.gov/Well/Files/APIRawData.zip',
    'download_to': os.path.abspath(Path("./data_manager/data/bsee"))
}
GetData_download_file_from_url(cfg)

# Production Data
cfg = {
    'url': 'https://www.data.bsee.gov/Production/Files/ogora2020delimit.zip',
    'download_to': os.path.abspath(Path("./data_manager/data/bsee"))
}
GetData_download_file_from_url(cfg)
cfg = {
    'url': 'https://www.data.bsee.gov/Production/Files/ogoradelimit.zip',
    'download_to': os.path.abspath(Path("./data_manager/data/bsee"))
}
GetData_download_file_from_url(cfg)

# eWell APD
cfg = {
    'url': 'https://www.data.bsee.gov/Well/Files/eWellAPDRawData.zip',
    'download_to': os.path.abspath(Path("./data_manager/data/bsee"))
}
GetData_download_file_from_url(cfg)

# End of Operations (EOR)
cfg = {
    'url': 'https://www.data.bsee.gov/Well/Files/eWellEORRawData.zip',
    'download_to': os.path.abspath(Path("./data_manager/data/bsee"))
}
GetData_download_file_from_url(cfg)

# Well Activity Reports (WAR)
cfg = {
    'url': 'https://www.data.bsee.gov/Well/Files/eWellWARRawData.zip',
    'download_to': os.path.abspath(Path("./data_manager/data/bsee"))
}
GetData_download_file_from_url(cfg)

# Directional Survey
cfg = {
    'url': 'https://www.data.bsee.gov/Well/Files/dsptsdelimit.ZIP',
    'download_to': os.path.abspath(Path("./data_manager/data/bsee"))
}
GetData_download_file_from_url(cfg)
