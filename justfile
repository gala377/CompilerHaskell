set shell := ["powershell.exe", "-c"]

hello:
    Write-Host "Hello, world!"

# generate lexer
generate:
    alex "grammar\Lexer.x" -o "src\Syntax\Lexer.hs" --ghc

run: generate
    stack run

build: generate
    stack build

test: generate
    stack test