if [[ -d $HOME/Developer && -d $HOME/Developer/go ]]; then
    export GOPATH=$HOME/Developer/go
    export PATH=$GOPATH/bin:$PATH
fi

if [[ -d $HOME/Applications && -d $HOME/Applications/go ]]; then
    export PATH=$HOME/Applications/go/bin:$PATH
fi

if [[ -d /usr/local/go ]]; then
    export GOPATH=$HOME/Developer/go
    export PATH=/usr/local/go/bin:$(/usr/local/go/bin/go env GOPATH)/bin:$PATH
fi
