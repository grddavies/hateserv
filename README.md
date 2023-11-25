# Hateserv

Hateserv is an insult-serving API for when your rage blinds you to the plethora of insults at your disposal.

## Run

To build and run you'll need [Haskell](https://www.haskell.org/ghcup/) and [`cabal`](https://www.haskell.org/cabal/) on your system.

```
cabal run
```

### Test Endpoints

#### Get random insult

Get an insult using randomly selected adjective and noun. For inspiration.

```sh
curl http://localhost:8081/random
> "bumbling trumpet"
```

#### Add a new word

Add a new adjective (`/words/adjective`) or noun (`/words/noun`) to your arsenal.

```sh
curl -X POST -H "Content-Type: application/json" -d '{"word": "sausage"}' http://localhost:8081/words/noun
```

#### Get all words

See what you're working with

```sh
curl http://localhost:8081
> {"adjectives":["snivelling","bumbling"],"nouns":["servant","trumpet","sausage"]}
```

#### Delete a word

For anything that's beyond the pale, or getting old.

```sh
curl -X DELETE http://localhost:8081/words/noun/servant
```
