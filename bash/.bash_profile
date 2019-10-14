USERNAME=$(whoami)

alias ls='ls -G'
alias cp='cp -i'
alias rm='rm -vi'
alias ll='ls -l'

# use emacs daemon.
alias emacs='emacsclient -nw -c -a ""'
alias killemacs='emacsclient -e "(kill-emacs)"'

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

source /usr/local/Cellar/git/2.16.3/share/zsh/site-functions/git-completion.bash
source /usr/local/Cellar/git/2.16.3/etc/bash_completion.d/git-prompt.sh
GIT_PS1_SHOWDIRTYSTATE=true
export PS1="\[\033[36m\]\u\[\033[m\]@\[\033[36m\]\h:\[\033[36m\]\W\[\033[m\]\$(__git_ps1)\[\033[00m\]\$ "
export CLICOLOR=1
export LSCOLORS=ExFxBxDxCxegedabagacad


if [ -f ~/.bashrc ]; then
   . ~/.bashrc
fi

# homebrew packege manager
export PATH=/usr/local/sbin:$PATH

# java home
export JAVA_HOME=/Library/Java/JavaVirtualMachines/adoptopenjdk-8.jdk/Contents/Home

# java class path
export CLASSPATH=/Library/Java/Extensions/

export M2_HOME=/usr/local/Cellar/maven/3.5.4/libexec/
export M2=$M2_HOME/bin

# scala home
export SCALA_HOME=/usr/local/opt/scala/libexec/

#rbenv
export PATH="$HOME/.rbenv/bin:$PATH"
if which rbenv > /dev/null; then eval "$(rbenv init -)"; fi

# 'go root' and 'go path'
export GOROOT=/usr/local/opt/go/libexec
export GOPATH=$HOME/go
export PATH=$PATH:$GOPATH/bin

# pyenv
export PATH="$HOME/.pyenv/bin:$PATH"
eval "$(pyenv init -)"


# require at importing pandas in ipython
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8

# python path
export PYTHONPATH="/usr/local/Cellar/pyqt5/5.7/lib/python3.5/site-packages"
export PYTHONPATH="/usr/local/Cellar/sip/4.18.1/lib/python3.5/site-packages:$PYTHONPATH"

# path for opencv3
export PATH="/usr/local/opt/opencv3/bin:$PATH"

# cargo path
export PAHT="$HOME/.cargo/bin:$PATH"

# home
export PATH="$HOME:$PATH"

# alias clang-omp='/usr/local/opt/llvm/bin/clang -fopenmp -L/usr/local/opt/llvm/lib -Wl,-rpath,/usr/local/opt/llvm/lib'
# alias clang-omp++='/usr/local/opt/llvm/bin/clang++ -fopenmp -L/usr/local/opt/llvm/lib -Wl,-rpath,/usr/local/opt/llvm/lib'

# The next line updates PATH for the Google Cloud SDK.
if [ -f "/Users/$HOME/google-cloud-sdk/path.bash.inc" ]; then source "/Users/$HOME/google-cloud-sdk/path.bash.inc"; fi

# The next line enables shell command completion for gcloud.
if [ -f "/Users/$HOME/google-cloud-sdk/completion.bash.inc" ]; then source "/Users/$HOME/google-cloud-sdk/completion.bash.inc"; fi

export PATH="$HOME/.cargo/bin:$PATH"

#export PATH="$HOME/.local/bin:$PATH"
export PATH="/usr/local/opt/curl/bin:$PATH"

# swift toolchain.
# export PATH="/Library/Developer/Toolchains/swift-latest/usr/bin:$PATH"
export PATH="/Library/Developer/Toolchains/swift-tensorflow-DEVELOPMENT-2019-08-28-a.xctoolchain/usr/bin:$PATH"

export SDKROOT=/Applications/Xcode-beta.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.15.sdk

# Nim path
export PATH="/Users/$HOME/.nimble/bin":$PATH
