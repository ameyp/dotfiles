# Unbind Alt+num bindings
Remove-PSReadLineKeyHandler -Chord Alt+0
Remove-PSReadLineKeyHandler -Chord Alt+1
Remove-PSReadLineKeyHandler -Chord Alt+2
Remove-PSReadLineKeyHandler -Chord Alt+3
Remove-PSReadLineKeyHandler -Chord Alt+4
Remove-PSReadLineKeyHandler -Chord Alt+5
Remove-PSReadLineKeyHandler -Chord Alt+6
Remove-PSReadLineKeyHandler -Chord Alt+7
Remove-PSReadLineKeyHandler -Chord Alt+8
Remove-PSReadLineKeyHandler -Chord Alt+9

# Create emacs-like text and history navigation.
Set-PSReadLineKeyHandler -Chord Ctrl+p -Function PreviousHistory
Set-PSReadLineKeyHandler -Chord Ctrl+n -Function NextHistory
Set-PSReadLineKeyHandler -Chord Ctrl+b -Function BackwardChar
Set-PSReadLineKeyHandler -Chord Alt+b  -Function BackwardWord
Set-PSReadLineKeyHandler -Chord Ctrl+f -Function ForwardChar
Set-PSReadLineKeyHandler -Chord Alt+f  -Function ForwardWord
Set-PSReadLineKeyHandler -Chord Ctrl+a -Function BeginningOfLine
Set-PSReadLineKeyHandler -Chord Ctrl+e -Function EndOfLine

Set-PSReadLineKeyHandler -Chord Ctrl+h -Function BackwardDeleteChar
Set-PSReadLineKeyHandler -Chord Ctrl+w -Function BackwardDeleteWord
Set-PSReadLineKeyHandler -Chord Alt+d  -Function DeleteWord
Set-PSReadLineKeyHandler -Chord Ctrl+u -Function RevertLine
Set-PSReadLineKeyHandler -Chord Ctrl+k -ScriptBlock {
    [Microsoft.PowerShell.PSConsoleReadLine]::DeleteToEnd()
    [Microsoft.PowerShell.PSConsoleReadLine]::ForwardChar()
}

Set-PSReadLineKeyHandler -Chord Ctrl+d -Function DeleteCharOrExit

function refresh-path {
    $env:Path = [System.Environment]::GetEnvironmentVariable("Path","Machine") +
                ";" +
                [System.Environment]::GetEnvironmentVariable("Path","User")
}