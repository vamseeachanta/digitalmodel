if not defined DIGITALMODEL_PRIVATE_DATA (
    echo Set DIGITALMODEL_PRIVATE_DATA to the folder that holds the model files.
    exit /b 1
)
C:\Program Files\ANSYS Inc\v182\aqwa\bin\winx64\aqwa.exe /nowind %DIGITALMODEL_PRIVATE_DATA%\no_load_10moors
C:\Program Files\ANSYS Inc\v182\aqwa\bin\winx64\aqwa.exe /nowind %DIGITALMODEL_PRIVATE_DATA%\no_load_09moors
