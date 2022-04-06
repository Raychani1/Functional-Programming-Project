function runProject() {
    stack build
    Clear-Host
        
    $Query = $(Write-Host "FPP $([char]0x2C96) > " -ForegroundColor 'DarkMagenta' -NoNewLine; Read-Host)

    while ($Query -ne "exit()") {
        stack exec  -- Functional-Programming-Project-exe $Query 
        $Query = $(Write-Host "FPP $([char]0x2C96) > " -ForegroundColor 'DarkMagenta' -NoNewLine; Read-Host)
    }
    
    Clear-Host
}

runProject