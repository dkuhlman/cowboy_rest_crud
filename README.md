# cowboy_rest_crud
A Cowboy REST application that exposes a CRUD API

## Introduction

This REST application is built on top of the Erlang Cowboy Web
server.  It exposes a CRUD HTTP API: create, read, update, and
delete.

Run this application by typing the following in the root directory:
```
$ make run
```

Then visit this address: `http://localhost:8080/help`.  Or, use `curl`:
```
$ curl http://localhost:8080/help
```

There is a blog post describing and explaining this example application
here: http://www.davekuhlman.org/cowboy-rest-add-get-update-list.html.
