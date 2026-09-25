if not defined DIGITALMODEL_PRIVATE_DATA (
    echo Set DIGITALMODEL_PRIVATE_DATA to the folder that holds the model files.
    exit /b 1
)

cd C:\Program Files\ANSYS Inc\v222\aisol\bin\winx64\

CALL "..\..\workbench.bat -cmd" AqwaReader --Type "Graphical" --InFile "%DIGITALMODEL_PRIVATE_DATA%\Analysis.plt" --OutFile "%DIGITALMODEL_PRIVATE_DATA%\fst1_l015_ad000_rao_p_dx" --Format "csv" --PLT1 1 --PLT2 1 --PLT3 1 --PLT4 1
CALL "..\..\workbench.bat -cmd" AqwaReader --Type "Graphical" --InFile "%DIGITALMODEL_PRIVATE_DATA%\Analysis.plt" --OutFile "%DIGITALMODEL_PRIVATE_DATA%\fst1_l015_ad000_rao_p_dy" --Format "csv" --PLT1 1 --PLT2 1 --PLT3 1 --PLT4 2
CALL "..\..\workbench.bat -cmd" AqwaReader --Type "Graphical" --InFile "%DIGITALMODEL_PRIVATE_DATA%\Analysis.plt" --OutFile "%DIGITALMODEL_PRIVATE_DATA%\fst1_l015_ad000_rao_p_dz" --Format "csv" --PLT1 1 --PLT2 1 --PLT3 1 --PLT4 3

CALL "..\..\workbench.bat -cmd" AqwaReader --Type "Graphical" --InFile "%DIGITALMODEL_PRIVATE_DATA%\Analysis.plt" --OutFile "%DIGITALMODEL_PRIVATE_DATA%\fst1_l015_ad000_rao_p_rx" --Format "csv" --PLT1 1 --PLT2 1 --PLT3 1 --PLT4 4
CALL "..\..\workbench.bat -cmd" AqwaReader --Type "Graphical" --InFile "%DIGITALMODEL_PRIVATE_DATA%\Analysis.plt" --OutFile "%DIGITALMODEL_PRIVATE_DATA%\fst1_l015_ad000_rao_p_ry" --Format "csv" --PLT1 1 --PLT2 1 --PLT3 1 --PLT4 5
CALL "..\..\workbench.bat -cmd" AqwaReader --Type "Graphical" --InFile "%DIGITALMODEL_PRIVATE_DATA%\Analysis.plt" --OutFile "%DIGITALMODEL_PRIVATE_DATA%\fst1_l015_ad000_rao_p_rz" --Format "csv" --PLT1 1 --PLT2 1 --PLT3 1 --PLT4 6
