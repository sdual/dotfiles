# nvim
alias vim='nvim'

# web browser.
alias firefox='open -a Firefox'
alias chrome='open -a Google\ Chrome'

# MS Office.
alias mw='open -a Microsoft\ Word'
alias me='open -a Microsoft\ Excel'
alias mp='open -a Microsoft\ PowerPoint'

# other apps.
alias pv='open -a Preview'
alias finder='open -a finder'
alias dropbox='open -a Dropbox'
alias acroread='open -a Adobe\ Reader'
alias mi='open -a mi'
alias atom='open -a Atom'
alias jpy='jupyter qtconsole --matplotlib=inline --style=monokai'
alias julia='/Applications/Julia-1.0.app/Contents/Resources/julia/bin/julia'


plugins=(git ruby osx bundler brew rails emoji-clock theme)

ZSH_THEME="agnoster"

bindkey -e

autoload -U compinit; compinit

export PATH="$HOME/.pyenv/bin:$PATH"
if command -v pyenv 1>/dev/null 2>&1; then
  eval "$(pyenv init -)"
fi


alias ls='ls -G'
alias cp='cp -i'
alias rm='rm -vi'
alias ll='ls -l'

# use emacs daemon.
# alias emacs='emacsclient -nw -c -a ""'
# alias killemacs='emacsclient -e "(kill-emacs)"'
