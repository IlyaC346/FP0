@echo off
rem Starts the disk analyzer. Builds disk-analyzer.jar first if it does not exist.
cd /d "%~dp0"
if not exist disk-analyzer.jar (
    echo Building disk-analyzer.jar ...
    call sbt assembly
)
java -jar disk-analyzer.jar %*
