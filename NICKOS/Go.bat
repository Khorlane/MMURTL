del myosd.bin
del floppy.img
nasm myosd.asm -f bin -o myosd.bin
pause
dd bs=512 count=2880 if=/dev/zero of=floppy.img
dd if=myosd.bin of=floppy.img conv=notrunc
"C:\Program Files (x86)\Bochs-2.6.9\bochs.exe" -q -f "MyOs.bxrc"