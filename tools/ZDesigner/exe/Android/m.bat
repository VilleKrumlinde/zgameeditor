@echo off
setlocal

rem dx.bat passes -Djava.ext.dirs, which Java 9 and later reject.
set "JAVA8_HOME="
if defined JAVA_HOME if exist "%JAVA_HOME%\bin\java.exe" (
  "%JAVA_HOME%\bin\java.exe" -version 2>&1 | find "1.8." >nul
  if not errorlevel 1 set "JAVA8_HOME=%JAVA_HOME%"
)
if not defined JAVA8_HOME (
  for /d %%D in ("%ProgramFiles%\Java\jdk1.8*" "%ProgramFiles(x86)%\Java\jdk1.8*" "%ProgramFiles%\Eclipse Adoptium\jdk-8*" "%ProgramFiles%\Java\jdk-8*") do (
    if exist "%%~fD\bin\java.exe" set "JAVA8_HOME=%%~fD"
  )
)
if not defined JAVA8_HOME (
  echo ERROR: JDK 8 not found. dx.bat cannot run on Java 9 or later.
  echo Install a JDK 8, or set JAVA_HOME to it.
  goto error
)

echo Using JDK 8: %JAVA8_HOME%
set "JAVA_HOME=%JAVA8_HOME%"
set "PATH=%JAVA_HOME%\bin;%PATH%"

call "%ANT_TOOL%" %ANT_PARAMS%
if errorlevel 1 goto error
goto end
:error
pause
:end
endlocal
