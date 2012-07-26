call "%ANT_TOOL%" %ANT_PARAMS%
IF %ERRORLEVEL% NEQ 0 GOTO error
goto end
:error
pause
:end
