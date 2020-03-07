@echo off
stack build :RayTracer-exe --fast --ghc-options=-O0
stack exec -- RayTracer-exe +RTS -N -RTS
