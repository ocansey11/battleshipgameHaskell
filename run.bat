@echo off
echo ================================
echo    Battleship Game - Haskell
echo ================================
echo.
echo Choose your battle mode:
echo 1. Command Line (Enhanced Visual)
echo 2. View Web Demo (Static HTML)
echo 3. Build only
echo.
set /p choice="Enter your choice (1-3): "

if "%choice%"=="1" goto cmd
if "%choice%"=="2" goto demo
if "%choice%"=="3" goto build
echo Invalid choice!
pause
exit /b 1

:build
echo Building Battleship...
cabal build battleship
if %errorlevel% neq 0 (
    echo Build failed!
    pause
    exit /b %errorlevel%
)
echo Build successful!
pause
exit /b 0

:cmd
echo Building command-line version...
cabal build battleship
if %errorlevel% neq 0 (
    echo Build failed!
    pause
    exit /b %errorlevel%
)
echo.
echo Starting Enhanced Command-Line Battleship...
echo (Much better than the original JavaScript version!)
echo.
cabal run battleship
pause
exit /b 0

:demo
echo Opening Web Demo...
echo This shows what the web version would look like!
echo (The Haskell web server needs additional setup on Windows)
start static\demo.html
pause
