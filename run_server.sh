erl -boot start_sasl -config app -pa _build/default/deps/*/ebin -sname webserver -setcookie webserver -s server_app -detached
