### RedProx.

About
-----------

redprox-core is a library to proxy queues/pubsub messages between redis servers on different networks.


Rant
-----------

I'm not sure if I like this code or not. It works well and does it's job, but it could be better. FWIW, it's far better than my pathetic blpop-proxy code.


TODO
------------

Things I want to add:
- Optional event system; for use with ekg etc.
- Persistence.
- Chaos function which goes in and randomly ruins Thread's within a session; to test persistence/recovery.
- Tests.


Introduction
------------

See examples/simple.hs
