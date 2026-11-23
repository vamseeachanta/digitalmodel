
@echo ">>>>>>>>>>>>>>>>>>>>>>>>>>>>" & echo.    %time% & echo.">>>>>>>>>>>>>>>>>>>>>>>>>>>>"

del *.out

c:\wamitv6\wamit.exe

del *.p2f
del *.fpt
del *.hst
del *.pnl
del *.idf
del wamitlog.txt
del errorp.log
del errorf.log

@echo ">>>>>>>>>>>>>>>>>>>>>>>>>>>>" & echo.    %time% & echo.">>>>>>>>>>>>>>>>>>>>>>>>>>>>"

@pause