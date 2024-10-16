set nocp
syntax on
set title
filetype plugin indent on

function! IgnoreParenIndent()
    let indent = cindent(v:lnum)
    if indent > 4000
        if cindent(v:lnum - 1) > 4000
            return indent(v:lnum - 1)
        else
            return indent(v:lnum - 1) + 4
        endif
    else
        return (indent)
    endif
endfun


set hlsearch
set autoindent
set cindent
set cinoptions=(4200,u4200,+0.5s,*500,:0,t0,U4200
set indentexpr=IgnoreParenIndent()
set indentkeys=0{,0},0),:,0#,!^F,o,O,e
set noexpandtab
set shiftwidth=8
set tabstop=8
set textwidth=80
set bs=eol,start,indent
set history=256

" Highlight trailing spaces
" http://vim.wikia.com/wiki/Highlight_unwanted_spaces
highlight ExtraWhitespace ctermbg=red guibg=red
match ExtraWhitespace /\s\+$/
autocmd BufWinEnter * match ExtraWhitespace /\s\+$/
autocmd InsertEnter * match ExtraWhitespace /\s\+\%#\@<!$/
autocmd InsertLeave * match ExtraWhitespace /\s\+$/
autocmd BufWinLeave * call clearmatches()
