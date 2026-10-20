# SBCL Deliver

Requires the SBCL environment of [BUILD.md](../../../BUILD.md) and the `deploy` library:

```lisp
(ql:quickload :deploy)
```

A release holds two executables, named after `*version-str*` of `build-om.lisp` (8.1 below):

- `OM <version>`, **GUI mode**: the OpenMusic application. Starting it opens the Workspaces dialog,
  then the OM windows; the Lisp listener is OM's own Listener window.
- `openmusic`, **Emacs mode**: a console image with swank loaded and no window of its own. Emacs
  starts it as the inferior Lisp of a SLIME connection; the SLIME REPL replaces the Listener (OM
  output goes to the buffer `*OM Listener*`) and the OM windows are opened from Emacs.
  `emacs-mode.el` is an example Emacs configuration.

With them: `code/` (no `.fasl`), `init/`, `resources/` (no `lib/`; the HTML reference regenerated in
`reference/`; only the platform's `fonts/<os>/`), and the C libraries: the bundled ones of
`OPENMUSIC/resources/lib/<os>/` plus `libglfw`, `libjpeg`, `libturbojpeg` and `libresvg`
(`-<os>-amd64`). Their locations follow.

## WINDOWS

Install GNU Binutils for Windows (`objcopy.exe` on PATH). From the repository root, in Command Prompt:

```bat
OPENMUSIC\build\sbcl\win64\release-sbcl.bat
```

```
<repo-root>/OM 8.1 SBCL/
    OM 8.1.exe
    openmusic.exe
    *.dll              all the libraries, plus libwinpthread-1.dll
    code/  init/  resources/   (resources/om.ico added)
```

Self-contained: copy the folder anywhere and run `OM 8.1.exe`.

## LINUX

From the repository root:

```bash
bash OPENMUSIC/build/sbcl/linux/release-sbcl.sh
```

```
OPENMUSIC/OM_8.1_SBCL
OPENMUSIC/openmusic
OPENMUSIC/resources/lib/linux/   all the libraries (libsdif.so -> libsdif-3.11.4.so)
```

The release folder and its tarball are made with `make` and `bzip2` (install them with the
distribution's package manager), from the repository root:

```bash
make -f OPENMUSIC/build/sbcl/linux/Makefile dist tar-dist
```

```
/tmp/OM/BUILD/OM_8.1_SBCL/            the two executables, code/, init/, resources/ (lib/linux/ only),
                                      omlogo.png and a Makefile
/tmp/OM/BUILD/OM_8.1_SBCL.tar.bz2     the release, compressed
```

`sudo make install` in the release folder (`PREFIX`, default `/usr`):

```
<PREFIX>/bin/OM_8.1_SBCL
<PREFIX>/bin/openmusic-sbcl                     the Emacs-mode executable
<PREFIX>/share/openmusic-sbcl/                  code/, init/, resources/
<PREFIX>/share/applications/OM-SBCL.desktop     menu entry of OM_8.1_SBCL
<PREFIX>/share/pixmaps/omlogo.png
```

`sudo make uninstall` removes them.

## MAC

Requires the Xcode command line tools (`codesign`, `hdiutil`). From the repository root:

```bash
bash OPENMUSIC/build/sbcl/mac/release-sbcl.sh
```

Options: `-d` deliver, `-p` pack, `-s` sign, `-dmg` disk image; no option runs all four. Signing
uses the identity in `OM_CODESIGN_IDENTITY` (ad-hoc `-` when unset).

```
<repo-root>/OM 8.1 SBCL/OM 8.1.app/Contents/
    MacOS/OM 8.1              SBCL runtime, shared by both modes
    MacOS/sbcl.core           GUI-mode image
    Resources/openmusic.core  Emacs-mode image
    Resources/openmusic       launcher: MacOS/OM 8.1 --core openmusic.core (the path for emacs-mode.el)
    Resources/                code/, the resources/ content (fonts/mac/* moved up to fonts/), document icons (.icns)
    Frameworks/               all the libraries (lib/mac/ on Intel, lib/m1/ on Apple Silicon)
    Init/                     init/
<repo-root>/OM-8.1-SBCL.dmg   HFS+ image of the release folder
```
