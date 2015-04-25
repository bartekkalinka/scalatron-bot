# scalatron-bot
My bot for Scalatron meetup at Javeo on April 25, 2015

Started with reference bot that spawned copies of itself.  Then I took [marpiec's bot](https://github.com/marpiec/HackatonScalatronBot) (the bot that won in previous Scalatron meetup in Jan 2015) and tried to win against it.  In the meantime I adopted marpiec's bot key features such as:
- Minibots avoid each other to form a network.  To make it possible, I had to change reference bot's way of finding best direction.  The reference bot gives all directions a score, and then chooses a direction with best score.  To implement minibots taking care of several factors at once (i.e. avoiding "brothers" while searching for "food") I had to find final direction by adding vectors instead of choosing best score.
- Explosions - it turned out to be more effective than trying to bring the energy back to the master.  Actually, explosion over enemy is the fastest way of transfering energy to the master.

I tweaked some stuff in opposition to marpiec's bot ways:
- Spawning - my bot spawns as quickly as possible, no limit here.
- Exact condition for explosion in my bot is simpler - just Explode(3) when any enemy is 2 steps or closer.
