# Haskell pet
## Index

- [About](#about)
  - [Pre-Requisites](#pre-requisites)
  - [Build](#build)
  - [Usage](#usage)
  - [Run](#run)
  - [Commands](#commands)

## About
Simple pet storing user information (email).

### Pre-Requisites
- stack 2.7.5
- cabal 3.6.2.0
- ghc   9.0.2    base-4.15.1.0
### Build
```
$ stack build
```

### Usage
- postgres running on port 5432 (by default)

Docker example:
```
$ docker run --name postgres-pet -e POSTGRES_DB=pet -e POSTGRES_PASSWORD=postgres -d -p 5432:5432 postgres:13.3
```
Server starts on `localhost:8080` (by default)

Open `http://localhost:8080/swagger-ui` in your browser to see swagger-ui

### Run
```
$ stack exec pet-exe
```

### Commands
| Command | Description |
|----|------------|
| ```GET/users```  | receive all users |
| ```GET/users/:id``` |receive user by id |
| ```POST/users``` | create a user |
| ```DELETE/users/:id``` | delete a user |

```POST/users``` requires a json body 
```
{
  "email": "kek@lol",
  "name": "lol"
}
```

```DELETE/users/:id``` requires authentication (JWT).

It deletes a user only if the user's name equal to JWT's name in payload.

In order to create a JWT, type ```$name $email``` in the terminal where you run the application (see [Run](#run))