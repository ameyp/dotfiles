if [[ -d $HOME/Applications/android-sdk ]]; then
    export ANDROID_HOME=$HOME/Applications/android-sdk
    export PATH=$ANDROID_HOME/platform-tools:$PATH
fi
