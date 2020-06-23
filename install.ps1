# Create a symlink to the profile.
if (!(Test-Path -Path $PROFILE)) {
    New-Item -ItemType SymbolicLink -Path $PROFILE -Target $PSScriptRoot/PowerShell/Profile.ps1 -Force
}
