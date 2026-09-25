@echo off
if not defined DIGITALMODEL_PRIVATE_DATA (
    echo Set DIGITALMODEL_PRIVATE_DATA to the folder that holds the model files.
    exit /b 1
)

echo running workbench ...
"C:\Program Files\ANSYS Inc\v231\Framework\bin\Win64\RunWB2.exe" -R "%DIGITALMODEL_PRIVATE_DATA%\journal_oH1v2.wbjn" -I
echo model ran sucessfully!
