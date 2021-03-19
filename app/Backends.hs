module Backends where

gccCompileUnix outFolder = "bash -c 'cd " ++ outFolder ++ "/src; gcc *.c cache/*.c dyntypes/*.c functions/*.c primf/*.c tommath/*.c -o bin'"