if !exists('g:ballish_command')
    let g:ballish_command = 'bl'
endif

if !exists('g:ballish_open_quickfix')
    let g:ballish_open_quickfix = 1
endif

if !exists('g:ballish_max_grep_line_length')
    let g:ballish_max_grep_line_length = 500
endif

function! BallishGrepInRepository(query)
    cexpr []
    let l:bl_results = system(shellescape(g:ballish_command) . ' -g -r -q ' . shellescape(a:query))
    let l:results = []
    for l:line in split(l:bl_results, "\n")
        call add(l:results, l:line[:g:ballish_max_grep_line_length])
    endfor
    caddexpr l:results
    if g:ballish_open_quickfix
        copen
    endif
endfunction

command! -nargs=1 BallishGrepInRepository :call BallishGrepInRepository(<q-args>)
