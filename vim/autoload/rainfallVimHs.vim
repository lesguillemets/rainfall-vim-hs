function! rainfallVimHs#start()
    let l:port = get(g:, 'rainfallVimHs_port', 5678)
    let l:job = job_start(['rainfall-vim-hs-exe', '--port=' . l:port])
    " FIXME : how to wait
    call ch_read(l:job)
    let s:channel = ch_open('localhost:' . l:port)
    if (ch_status(s:channel) == "open")
        echom "Started. Powered by : Web Services by Yahoo! JAPAN http://developer.yahoo.co.jp/about"
    endif
endfunction

function! rainfallVimHs#handle(channel,msg)
    echom a:msg
endfunction

function! rainfallVimHs#send(loc)
    call ch_sendexpr(s:channel, a:loc, {'callback' : "rainfallVimHs#handle" })
endfunction

command! -nargs=1 Rainfall call rainfallVimHs#send(<q-args>)
