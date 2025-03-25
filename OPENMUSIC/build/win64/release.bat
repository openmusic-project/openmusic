@ECHO OFF

ECHO Compile, Pack OM? 

PAUSE

CHDIR ..\..\resources\lib\linux\

DEL libsdif.so

CHDIR ..\..\..\build

"C:\\Program Files\\LispWorks\\lispworks-8-0-0-x64-windows.exe" -init deliver.lisp

"C:\\Program Files\\LispWorks\\lispworks-8-0-0-x64-windows.exe" -init pack-om.lisp

cd win64

ECHO DONE! 