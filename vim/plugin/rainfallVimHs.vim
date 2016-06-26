if exists("g:loaded_rainfallVimHs")
    finish
endif

let g:loaded_rainfallVimHs = 1

command! RainfallStart :call rainfallVimHs#start()
