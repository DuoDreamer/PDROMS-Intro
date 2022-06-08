del main.obj
del utils.obj
del vars.obj
rgbasm -omain.obj -zff main.asm
rgbasm -outils.obj -zff utils.asm
rgbasm -ovars.obj -zff vars.asm

xlink -mpdroms.map -npdroms.sym -zff linkfile.asm

rgbfix -v -o pdroms.gbc
rgbfix -p80 pdroms.gbc

