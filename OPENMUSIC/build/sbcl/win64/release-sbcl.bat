@ECHO OFF
REM OpenMusic on SBCL: Windows release (runtime with icon, deliver, pack)

SETLOCAL
CHDIR /D "%~dp0"

FOR %%R IN ("%~dp0..\..\..\..") DO SET "REPO_ROOT=%%~fR"
IF NOT EXIST "%REPO_ROOT%\OPENMUSIC\build\sbcl\win64\release-sbcl.bat" (
  ECHO This file must stay in OPENMUSIC\build\sbcl\win64\ of the repository.
  EXIT /B 1
)

where sbcl >NUL 2>NUL || (
  ECHO sbcl is not on the PATH.
  EXIT /B 1
)
SET SBCL_CORE=
FOR /F "delims=" %%C IN ('sbcl --noinform --no-userinit --non-interactive --eval "(princ (sb-ext:native-namestring sb-ext:*core-pathname*))"') DO SET "SBCL_CORE=%%C"
IF NOT DEFINED SBCL_CORE (
  ECHO SBCL did not report its core file.
  EXIT /B 1
)
IF NOT EXIST "%SBCL_CORE%" (
  ECHO SBCL's core file "%SBCL_CORE%" does not exist.
  EXIT /B 1
)
where objcopy.exe >NUL 2>NUL || (
  ECHO objcopy is not on the PATH.
  EXIT /B 1
)

ECHO === Runtime with the OM icon
sbcl --load make-om-runtime.lisp || GOTO :failed

FOR /D %%D IN ("%REPO_ROOT%\OM * SBCL") DO (
  ECHO === Removing the previous release "%%~fD"
  RMDIR /S /Q "%%~fD"
  IF EXIST "%%~fD" GOTO :failed
)

ECHO === GUI executable
om-runtime.exe --core "%SBCL_CORE%" --dynamic-space-size 8192 --load ..\deliver-sbcl.lisp || GOTO :failed

ECHO === Emacs-mode executable
om-runtime.exe --core "%SBCL_CORE%" --dynamic-space-size 8192 --eval "(defvar cl-user::*emacs-mode* t)" --load ..\deliver-sbcl.lisp || GOTO :failed

ECHO === Pack
sbcl --load ..\pack-om-sbcl.lisp || GOTO :failed

DEL om-runtime.exe
ECHO DONE!
EXIT /B 0

:failed
ECHO FAILED: see the output above.
IF EXIST om-runtime.exe DEL om-runtime.exe
EXIT /B 1
