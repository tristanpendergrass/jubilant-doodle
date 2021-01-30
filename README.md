Demo at https://www.tristanpendergrass.com/elm-empty-project.

# Road map

## Milestone 1

First steps...

- [x] Fixed number of allies with health, attack, cooldown
- [x] Open the command panel with button or hotkey, do damage when in that context with button or hotkey
- [x] Enemy with health and defeat rewards (+attack or +health to team)
- [x] Cooldown on abilities

## Milestone 1.5 (Visual polish)

Visual polish

- [x] Better visuals for health bars on allies and enemy
- [x] Better visuals for ally cooldowns
- [x] Portraits for allies and enemy
- [ ] Sound effect

## Milestone 2

Now you're getting hit back.

- [ ] Enemies have a damage stat.
- [ ] Enemies do attacks on units that have to be dodged.
- [ ] Loss state, and a restart button
- [ ] Enemies can reward gold

## Milestone 2.5 (Visual polish)

- [ ] Consistent theme for showing keyboard shortcuts (black background, white text, gold border?)

## Milestone 3

More options.

- [ ] When enemy dies an option is given for a new enemy
- [ ] Each ally can have a different attack pattern to do damage.
- [ ] New reward: additional copies of units which make them evolve (or become 2\*?). Boosting stats/unlocking new attack command
- [ ] Sometimes get a shop where you can spend gold on units or stat upgrades

## Milestone 4

Allies are really feeling different now.

- [ ] New ally role: healer. Their abilities heal the team. 2*: Overheal. 3*: +100% effective for lowest percent health on team.
- [ ] New ally role: support. Their abilities give a temp buff to team, so strategically line up their availability with other abilities.
- [ ] No longer just a fixed number of allies: start with one, add additional as rewards.

# Development

```
$ npm install -g parcel-bundler
$ npm install
$ npm start
```

# Deployment

This command builds files in the /docs directory. If hosting on github pages, configure the repository to serve from there.

```
$ npm run build
```
