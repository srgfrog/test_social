# Assumes latest erlang installed 'apt-get install erlang' for Debian based
distributions
# and elm version 1.8 installed 'npm install elm@0.18.0' works
./rebar3 compile
(cd elm18; /home/stephen/node_modules/elm/binwrappers/elm-make src/social.elm --output index.html)
echo "Go to http://localhost:8080/" to test
