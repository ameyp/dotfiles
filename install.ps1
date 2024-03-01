# Create a symlink to the profile.
if (!(Test-Path -Path $PROFILE)) {
    New-Item -ItemType SymbolicLink -Path $PROFILE -Target $PSScriptRoot/PowerShell/Profile.ps1 -Force
}

# Create a symlink for emacs.d.
if (!(Test-Path -Path "$HOME/.emacs.d")) {
    New-Item -ItemType SymbolicLink -Path "$HOME/.emacs.d" -Target $PSScriptRoot/emacs.d -Force
}

# Set HOME as an environment variable because Emacs needs it to find .emacs.d
[System.Environment]::SetEnvironmentVariable("HOME", "$HOME", [System.EnvironmentVariableTarget]::User)
