@echo off
stack build :main-bench
IF [%1] == [] GOTO :PRINT_ERROR
set out=%1
stack exec main-bench -- --output %out%
GOTO :STOP
:PRINT_ERROR
stack exec main-bench
:STOP
