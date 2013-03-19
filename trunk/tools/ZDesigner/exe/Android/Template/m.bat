call "$antpath$\bin\ant" debug
IF %ERRORLEVEL% NEQ 0 GOTO error
goto end
:error
pause
:end
