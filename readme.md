# Pusherman

Send push notifications from a redis queue.

It's essentially a Drop-in replacement for Urban Airship push notification sending.

Setup
-

Compile with

    $ ghc -threaded Pusherman.hs

Then, create a file called `config.json` in the same directory as the `Pusherman` binary. `config.json` should look something like:

    {
      "certificate": "production-push.crt",
      "key": "production-push.key",
      "redisServer": "localhost",
      "redisQueue": "notifications",
      "notifLogFile": "push.log",
      "feedbackLogFile": "feedback.log"
    }

Pusherman reads payloads from Redis: they should look something like this:

    {
      "tokens": [ 
        "0805ab2e45ceddbbb5b83597a1f45fddd93708e1d107de34d5a3e71b6434232a"
      ],
      "badge": 0,
      "alert": "The message to send",
      "sound": "RecommendedOffer_1.aif",
      "data": {
        "custom": "variable"
      }
    }

there are a couple non-apns/custom fields in this JSON:

`tokens` is a list of APNS device tokens<br />
`data` is an object representing non-apns extra payload data.<br />

Running
-

It's as simple as running the executable, after all, this is Haskell and not someting like Ruby.

    $ ./Pusherman

