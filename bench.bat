@echo off

stack build :main-bench

for /f "tokens=2 delims==" %%a in ('wmic OS Get localdatetime /value') do set "dt=%%a"
set "YY=%dt:~2,2%" & set "YYYY=%dt:~0,4%" & set "MM=%dt:~4,2%" & set "DD=%dt:~6,2%"
set "HH=%dt:~8,2%" & set "Min=%dt:~10,2%" & set "Sec=%dt:~12,2%"

set "fullstamp=%YYYY%.%MM%.%DD% %HH%.%Min%.%Sec%"

IF [%1] == [] GOTO :PRINT_ERROR
set out=%1
stack exec main-bench -- --output "out/benchmarks/%fullstamp% - %out%.html" --csv "out/benchmarks/%fullstamp% - %out%.csv"
GOTO :STOP
:PRINT_ERROR
stack exec main-bench -- --output "out/benchmarks/%fullstamp%.html" --csv "out/benchmarks/%fullstamp%.csv"
:STOP
