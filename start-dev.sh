#!/bin/sh
exec erl -config monitor \
     -pa ebin edit deps/*/ebin \
     -boot start_sasl \
     -sname monitor_dev \
     -s monitor
