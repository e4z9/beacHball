## beacHball

Simple ball game written in Haskell, using Netwire and SDL2.

### Building

Building is most painless using [Stack](https://docs.haskellstack.org).
It requires SDL2 and SDL2_ttf.

#### (Ubuntu) Linux

Install the SDL2 dev packages via the package manager:

    sudo apt install libsdl2-dev libsdl2-ttf-dev

After that you can build with

    stack setup # needed if stack hasn't updated the resolver used by the project
    stack build

#### Windows

Install the SDL2 packages through the msys2 installation that comes with stack.
The following command lines assume that you are on 64 bit Windows:

    @rem Install ghc and msys2
    stack setup
    @rem Update pacman mirror list for msys, first by hand
    @rem It uses only sourceforge by default, and that is very flaky
    (echo. & echo Server = http://repo.msys2.org/msys/$arch) >> %LOCALAPPDATA%\Programs\stack\x86_64-windows\msys2-20150512\etc\pacman.d\mirrorlist.msys
    @rem Update package database, ignore errors for mingw
    stack exec pacman -- -Sy
    @rem Update mirror list, for real now
    stack exec pacman -- -S pacman-mirrors
    @rem Update package database, for real now
    stack exec pacman -- -Sy
    @rem Install SDL2, SDL2_ttf and pkg-config
    @rem SDL2 Haskell module does not support SDL2 2.0.6 on Windows yet, so get 2.0.5
    stack exec pacman -- -U http://repo.msys2.org/mingw/x86_64/mingw-w64-x86_64-SDL2-2.0.5-2-any.pkg.tar.xz
    stack exec pacman -- -S mingw-w64-x86_64-pkg-config mingw-w64-x86_64-SDL2_ttf

After that you can build with `stack build`.

If you get an error `The procedure entry point inflateValidate could not be
located in the dynamic link library zlib1.dll` when running, then you hit a
conflict between the `zlib` shipped with GHC and `zlib` from the MSYS
installation. Just copy the latter over the former and try again:

    copy /y C:\Users\berlin\AppData\Local\Programs\stack\x86_64-windows\msys2-20150512\mingw64\bin\zlib1.dll C:\Users\berlin\AppData\Local\Programs\stack\x86_64-windows\ghc-8.0.2\mingw\bin\

#### macOS

Install SDL2, SDL2_ttf and pkg-config for example from [Homebrew](https://brew.sh/index_de.html):

    brew install pkg-config sdl2 sdl2_ttf

After that you can build with

    stack setup # needed if stack hasn't updated the resolver used by the project
    stack build
