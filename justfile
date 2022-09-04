
set windows-shell := ["powershell.exe", "-c"]

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

# special macos m1 and m2 command because macos 

mosgenerate:
    alex "grammar/Lexer.x" -o "src/Syntax/Lexer.hs" --ghc

mosrun *ARGS: mosgenerate
    PATH="$LLVM_12_BIN:$PATH" C_INCLUDE_PATH="$(xcrun --show-sdk-path)/usr/include/ffi" stack run {{ARGS}}

mosbuild *ARGS: mosgenerate
    PATH="$LLVM_12_BIN:$PATH" C_INCLUDE_PATH="$(xcrun --show-sdk-path)/usr/include/ffi" stack build {{ARGS}}

mostest *ARGS: mosgenerate
    PATH="$LLVM_12_BIN:$PATH" C_INCLUDE_PATH="$(xcrun --show-sdk-path)/usr/include/ffi" stack test {{ARGS}}