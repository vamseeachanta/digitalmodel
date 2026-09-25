if not defined DIGITALMODEL_PRIVATE_DATA (
    echo Set DIGITALMODEL_PRIVATE_DATA to the folder that holds the model files.
    exit /b 1
)
"C:\Program Files\ANSYS Inc\v182\aqwa\bin\winx64\aqwa.exe" /nowind %DIGITALMODEL_PRIVATE_DATA%\F_FST1_L00_DAMPNO
"C:\Program Files\ANSYS Inc\v182\aqwa\bin\winx64\aqwa.exe" /nowind %DIGITALMODEL_PRIVATE_DATA%\F_FST1_L15_DAMPNO
"C:\Program Files\ANSYS Inc\v182\aqwa\bin\winx64\aqwa.exe" /nowind %DIGITALMODEL_PRIVATE_DATA%\F_FST1_L50_DAMPNO
"C:\Program Files\ANSYS Inc\v182\aqwa\bin\winx64\aqwa.exe" /nowind %DIGITALMODEL_PRIVATE_DATA%\F_FST1_L95_DAMPNO

"C:\Program Files\ANSYS Inc\v182\aqwa\bin\winx64\aqwa.exe" /nowind %DIGITALMODEL_PRIVATE_DATA%\F_FST2_L00_DAMPNO
"C:\Program Files\ANSYS Inc\v182\aqwa\bin\winx64\aqwa.exe" /nowind %DIGITALMODEL_PRIVATE_DATA%\F_FST2_L15_DAMPNO
"C:\Program Files\ANSYS Inc\v182\aqwa\bin\winx64\aqwa.exe" /nowind %DIGITALMODEL_PRIVATE_DATA%\F_FST2_L50_DAMPNO
"C:\Program Files\ANSYS Inc\v182\aqwa\bin\winx64\aqwa.exe" /nowind %DIGITALMODEL_PRIVATE_DATA%\F_FST2_L95_DAMPNO

"C:\Program Files\ANSYS Inc\v182\aqwa\bin\winx64\aqwa.exe" /nowind %DIGITALMODEL_PRIVATE_DATA%\F_125K_LNGC_L00_DAMPNO
"C:\Program Files\ANSYS Inc\v182\aqwa\bin\winx64\aqwa.exe" /nowind %DIGITALMODEL_PRIVATE_DATA%\F_125K_LNGC_L99_DAMPNO

"C:\Program Files\ANSYS Inc\v182\aqwa\bin\winx64\aqwa.exe" /nowind %DIGITALMODEL_PRIVATE_DATA%\F_180K_LNGC_L00_DAMPNO
"C:\Program Files\ANSYS Inc\v182\aqwa\bin\winx64\aqwa.exe" /nowind %DIGITALMODEL_PRIVATE_DATA%\F_180K_LNGC_L99_DAMPNO
