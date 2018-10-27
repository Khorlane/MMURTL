rem Boot Sector
del Stage1.bin
nasm -f bin Stage1.asm -o Stage1.bin -l Stage1.lst
pause