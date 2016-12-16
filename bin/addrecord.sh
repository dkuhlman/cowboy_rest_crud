#!/bin/bash
curl -v --data-urlencode content@$1 http://crow.local:8080/create
