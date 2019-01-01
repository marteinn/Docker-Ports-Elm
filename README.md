![Docker Port Registry](https://raw.githubusercontent.com/marteinn/Docker-Ports-Elm/develop/screenshot.png)


# Docker Ports Elm

A service for solving the issue with keeping track on docker ports for various projects and services. Interface is built in elm, backend is node.js with serverless and dynamodb.


## Getting started

### Frontend

#### Development
- `elm reactor`, then open `src/Reactor.elm`

#### Production
- `elm make src/Main.elm --output=elm.js` then open index.html

### Backend
- `cd backend`
- `docker-compose up -d`
- `serverless offline start --stage dev`
- `curl http://localhost:3000/create-local-db`


## TODO
- [x] Add new service
- [x] Edit existing service
- [x] Delete service
- [x] Reload services
- [x] Sorting
- [x] Fix bug with name beeing required
- [x] Css fix for add vs close
- [x] Css fix for update vs close
- [x] Css fix for add new service
- [ ] Add scrollto when editing
- [x] Add required fields on input
- [x] Refactor imports
- [x] Add a maybe monad to new service
- [x] Find a better way to include js
- [x] Find a proper way to include css


## License

Docker Ports Elm is released under the [MIT License](http://www.opensource.org/licenses/MIT).
