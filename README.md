NETROBOT - Realtime
===================

Liberamente basato su P-ROBOTS [http://corewar.co.uk/probots/p-robo4.txt]

WEBSERVER
=========
Il webserver è scritto in Flask e sviluppato in TDD; le post sono codificate come normali POST, quindi ad esempio,
 `speed=100&degree=0` , ma le risposte sono in json.

Per le varie rotte consultare gli esempi o il file `test.py`

BOARD
=====
* L'arena è di 1000x1000 basata in 0,0 in basso a sinistra
* gli angoli si misurano in gradi

              135    90   45
                  \  |  /
                   \ | /
             180 --- x --- 0
                   / | \
                  /  |  \
              225   270   315

* il robot occupa le sue coordinate, con una raggio di 1 (usato per il calcolo delle collisioni)

ROBOT
======

~~I robot sono tutti uguali. Le costanti fisiche sono hardcodate per ora.~~

CANNONATE
=========

I proiettili si intendono in tiro balistico, quindi non vengono considerate le eventuali collisioni inaspettate con robot di passaggio,
 perché non vi sono, inquanto i colpi viaggiano più in alto.
I proiettili sparati fuori dall'arena esplodono fuori, non collidono coi bordi per lo stesso motivo di cui sopra.

NOTE TECNICHE
=============

La versione committata gira con un rapporto temporale di x2

GETTING STARTED
===============

For Ubuntu/Debian

  sudo aptitude install python-flask
  python run.py

Open the browser on

  http://localhost:8080/

Launch some demo robot

  cd example/python
  python rabbit.py
  python sniper.py

ROBOTS CODING INSTRUCTIONS
==========================

Programming Language
--------------------

Robots can be written using any programming language, because they communicate with the server, using http requests.

TODO
=====

* ~~I robot hanno tutti le stesse costanti di base, implementare un sistema a punti per personalizzarsi il robot.~~
* Raffinare la logica di sterzo sopra la velocità massima di sterzo (`_max_sterling_speed`), magari con aggiunta di danno autoinflitto
* L'urto con qualcosa infligge 2 punti di danno a prescindere dalla velocità al momento dell'impatto. Farlo dipendere dalla velocità?
* Muri; ora l'arena è vuota, si potrebbere prevedere muri casuali, ma questo imporrebbe modifiche anche allo scanner.
