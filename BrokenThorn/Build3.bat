rem Kernel
del Stage3.bin
nasm -f bin Stage3.asm -o Stage3.bin  -l Stage3.lst
pause