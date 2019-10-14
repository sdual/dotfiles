set clipboard+=unnamedplus
set number
set title

syntax enable
colorscheme atom-dark
set termguicolors
set background=dark
set shiftwidth=4

imap <C-p> <Up>
imap <C-n> <Down>
imap <C-b> <Left>
imap <C-f> <Right>

autocmd BufWritePre * call s:remove_unnecessary_space()

function! s:remove_unnecessary_space()
    " delete last spaces
    %s/\s\+$//ge

    " delete last blank lines
    while getline('$') == ""
            $delete _
    endwhile
endfunction
