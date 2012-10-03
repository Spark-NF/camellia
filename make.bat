@echo off
echo Compilation en cours...
ocamlc  main.ml -I +graphics graphics.cma -o camelia.exe
echo Fini !