if !exists('g:ballish_command')
    let g:ballish_command = 'bl'
endif

if !exists('g:ballish_open_quickfix')
    let g:ballish_open_quickfix = 1
endif

function! BallishGrepInRepository(query)
    cexpr []
    let l:results = system(shellescape(g:ballish_command) . ' -g -r -q ' . shellescape(a:query))
    caddexpr l:results
    if g:ballish_open_quickfix
        copen
    endif
endfunction

command! -nargs=1 BallishGrepInRepository :call BallishGrepInRepository(<q-args>)
