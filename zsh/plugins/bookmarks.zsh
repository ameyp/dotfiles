export MARKPATH=$HOME/.zsh-marks
if [[ ! -d $MARKPATH ]]; then mkdir -p $MARKPATH; fi

__ameyp_bookmark () {
    if (( ARGC != 2 )); then
        printf 'Usage: __bookmark link directory\n'
        return 1
    fi
    hash -d -- $1=$2
}

bookmark() {
    if (( $# == 0 )); then
        # When no arguments are provided, just display existing
        # bookmarks
        for link in $MARKPATH/*(N@); do
            local markname="$fg[green]${link:t}$reset_color"
            local markpath="$fg[blue]${link:A}$reset_color"
            printf "%-30s -> %s\n" $markname $markpath
        done
    else
        # Otherwise, we may want to add a bookmark or delete an
        # existing one.
        local -a delete
        zparseopts -D d=delete
        if (( $+delete[1] )); then
            # With `-d`, we delete an existing bookmark
            command rm $MARKPATH/$1
        else
            # Otherwise, add a bookmark to the current
            # directory. The first argument is the bookmark
            # name. `.` is special and means the bookmark should
            # be named after the current directory.
            local name=$1
            ln -s $PWD $MARKPATH/$name
            __ameyp_bookmark $name $PWD
        fi
    fi
}

for link ($MARKPATH/*(N@)) {
    __ameyp_bookmark -${link:t} ${link:A}
}
