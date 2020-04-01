# 'go root' and 'go path'
export GOROOT=/usr/local/opt/go/libexec
export GOPATH=$HOME/go
export PATH=$PATH:$GOPATH/bin

# pyenv
export PATH="$HOME/.pyenv/bin:$PATH"
eval "$(pyenv init -)"

# cargo path
export PAHT="$HOME/.cargo/bin:$PATH"

# home
export PATH="$HOME:$PATH"

export PATH="$HOME/.cargo/bin:$PATH"

#export PATH="$HOME/.local/bin:$PATH"
export PATH="/usr/local/opt/curl/bin:$PATH"

# swift toolchain.
# export PATH="/Library/Developer/Toolchains/swift-latest/usr/bin:$PATH"
export PATH="/Library/Developer/Toolchains/swift-tensorflow-DEVELOPMENT-2019-08-28-a.xctoolchain/usr/bin:$PATH"

# Nim path
export PATH="$HOME/.nimble/bin":$PATH



# # The next line updates PATH for the Google Cloud SDK.
# if [ -f "$HOME/google-cloud-sdk/path.bash.inc" ]; then source "$HOME/google-cloud-sdk/path.bash.inc"; fi

# # The next line enables shell command completion for gcloud.
# if [ -f "$HOME/google-cloud-sdk/completion.bash.inc" ]; then source "$HOME/google-cloud-sdk/completion.bash.inc"; fi

export SDKROOT=/Applications/Xcode-beta.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.15.sdk

# config path
export XDG_CONFIG_HOME="$HOME/.config"
