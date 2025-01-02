REM tempdele.bat 
REM Deletes ANSYS temporary or unecessary files from the current folder (directory)
REM and all subfolders.
REM By Bram Weisman April 26, 2003.  For ANSYS 7.0.
REM Use at your own risk.  I assume no liability for damages due to use or 
REM misuse of this batch file.
REM 
REM The swith /q enables the macro to proceed without user interation.  
REM There will be no confirm delete prompts.
REM Be sure you understand this macro before you execute it.



rem !
del *.bak* /q /s



rem !
del *.eqp* /q /s
rem !
del *.lis* /q /s
rem !
del *.mes* /q /s
rem !
del *.pac* /q /s
rem !
del *.pld* /q /s
rem !
del *.pls* /q /s
rem !
del *.plt* /q /s
rem !
del *.pos* /q /s
rem !
del *.pot* /q /s
rem !
del *.qtf* /q /s
rem !
del *.res* /q /s
rem !
del *.uss* /q /s
rem !
del *.vac* /q /s
rem !
del *.pag* /q /s