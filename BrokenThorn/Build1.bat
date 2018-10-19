rem Boot Sector
nasm -f bin Boot1.asm -o Boot1.bin
pause

rem Kernel Loader
nasm -f bin Stage2.asm -o KRNLDR.SYS
pause

rem Kernel
nasm -f bin Stage3.asm -o KRNL.SYS
pause