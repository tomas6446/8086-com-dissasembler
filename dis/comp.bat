@echo off
set arg1=%1
cd /%arg1%

tasm.exe %arg1%
tlink.exe %arg1%

cd ..





