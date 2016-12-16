#!/bin/bash
curl -v --data-urlencode content@$2 http://crow.local:8080/update/$1
