@echo off
set arg1=%1

tasm.exe mano
tlink.exe /t mano

tasm.exe %arg1%
tlink.exe %arg1%

tasm.exe /zi %arg1%
tlink.exe /v %arg1%







