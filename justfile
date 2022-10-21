
set windows-shell := ["powershell.exe", "-c"]

# generate lexer

lexer_in := join("grammar", "Lexer.x")
lexer_out := join("src", "Syntax", "Lexer.hs")

generate:
    alex {{ lexer_in }} -o {{ lexer_out }} --ghc

run: generate
    stack run

build: generate
    stack build

test: generate
    stack test