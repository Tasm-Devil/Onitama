# Onitama Multiplayer Game made with ELM and Haskell

## Makefile
There's a `Makefile` included with the following targets:

- `all`   -- Calls first `setup` and then `build`
- `setup` -- Set up everything: install ghc and dependencies. (Needs `stack`, `elm`
  and `elm-test`.)
- `build` -- Build the server and the client.
- `server-start` -- Calls `build` and then starts the server. Open <http://localhost:3000/> in your Browser. Requests sent to this server will trigger a recompilation (via make) of the client code (if its changed).

## JSON API
JSON Message from Server to Client after two game moves. Last move comes first, thats fp style (Head of the List).
```
{
  "cards": ["Eel", "Rabbit", "Tiger", "Rooster", "Horse"],
  "history": [
    { "color": "Black", "card": "Rooster", "from": [1, 4], "move": [-1, -1] },
    { "color": "White", "card": "Eel", "from": [2, 0], "move": [-1, 1] }
  ]
}
```

JSON Message from Client to server after third game move.
```
{"color":"White","card":"Rabbit","from":[3,0],"move":[1,1]}
```


## Client
The Game logic is implemented entirely on the client side only.
So there's no checking for cheating on the server side currently!
This is on purpose because I want to use it for demonstration.

Feel free to play the older [non-multiplayer version here ](https://tasm-devil.github.io/Onitama/) since it doesn't want you to setup a backend server.

### Thanks
- Inspiration came from [here](https://github.com/Lanny/Onitama)
- Http-Server HowTo from [here](https://elmprogramming.com/decoding-json-part-1.html)

## Server
The Server is my first haskell project. So don't expect very much. I still do not understand monads ;)

### Test with curl

You can easily test the API with some simple curl commands.

```
New Game:
curl -X POST http://localhost:3000/game -w "\n"

Get all Game Ids:
curl http://localhost:3000/game -w "\n"

Join Game
curl -X PUT http://localhost:3000/game/1?name=Wendy -w "\n"

Get Game with Id 1
curl http://localhost:3000/game/1 -w "\n"

Post new GameMove to Game 1
curl -X POST -d '{"color": "White","card": "Ox","from": [3,0],"move": [0,1]}' -H 'Content-Type: application/json' http://localhost:3000/game/1 -w "\n"
```

## ToDos
In the order in which I would like to tackle them.

- [ ] GetGame with UUID in the URL should work to
- [ ] The Server should check the player names on NewMove
- [ ] Authetification by player name using sessions
- [ ] Implement http polling temporarily.
- [ ] GetGames should also return all playernames with gameids.
- [ ] The common card should determine, which player starts the game.
- [ ] Check for checkmate!
- [ ] JSON for GameMove is to verbose. Simplyfy it to something like `{"move":"white:c1b2:elephant"}`
- [ ] Use WebSocket to get new moves without the need to do http polling.
- [ ] Implement Chat feature
- [ ] Add support for the Cards from the Senseis Path explansion.

### Expansion Cards

- Senseis Path ([image](https://www.gadgetsville.store/wp-content/uploads/2017/12/16096-c.jpg))
- Promo Cards ([link](https://www.arcanewonders.com/product/onitama-promo-cards/))

## Thanks
Thanks to <https://github.com/haskell-servant/example-servant-elm>
