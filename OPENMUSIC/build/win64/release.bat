@ECHO OFF

ECHO Compile, Pack OM? 

PAUSE

CHDIR ..\..\resources\lib\linux\

DEL libsdif.so

CHDIR ..\..\..\build

DEL /S /Q sbcl

RMDIR /S /Q sbcl

DEL /S /Q ..\code\api\om-SBCL

RMDIR /S /Q ..\code\api\om-SBCL

"C:\\Program Files\\LispWorks\\lispworks-8-1-0-x64-windows.exe" -init deliver.lisp

"C:\\Program Files\\LispWorks\\lispworks-8-1-0-x64-windows.exe" -init pack-om.lisp

cd win64

ECHO DONE! 
