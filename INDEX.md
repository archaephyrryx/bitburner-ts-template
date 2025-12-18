# Script Overview

## Table Template

| script name | category    | kind | status | description |
|-------------|-------------|-----------------|------|-------------|
| [`aug.helper.ts`](src/aug.helper.ts) | Aug | daemon | ✅ | Daemon that keeps list of available augmentations up-to-date |
| [`aug.list-avail.ts`](src/aug.list-avail.ts) | Aug | aux | ✅ | Minified one-shot of `augs.ts` to allow listing augmentations without heavy RAM penalties |
| [`augs.ts`](src/augs.ts) | Aug | main (cli) | ✅ | All-in-one script for managing augmentations from the command-line |
| [`awdangit.ts`](src/awdangit.ts) | casino | 🚧 | 🚧 | WIP script for casino exploits |
| [`backdoor.ts`](src/backdoor.ts) | crack | main (task-loop) | ✅ | Long-running background process that roots every server in the network as soon as it is possible. |
| [`balance.ts`](src/balance.ts) | money | stat-dump | 🔧 | Summarizes every category of profit and expense as an overall balance |
| [`Batch/batch.ts`](src/Batch/batch.ts) | hacking | main | 🚧 | WIP HGW batcher |
| [`blade.skill.ts`](src/blade.skill.ts) |  bladeburner | subroutine | ✅ | Background loop for continually leveling bladeburner skills based on custom weight. |
| [`blade.snapshot.ts`](src/blade.snapshot.ts) | bladeburner | stat-dump | ✅ | Saves snapshots of bladeburner contract success/failure across augmentation installs and dumps the stats for the current install |
| [`blade.travel.ts`](src/blade.travel.ts) | bladeburner | lib | ✅ | Lib-helper for changing operating city for bladeburner purposes |
| [`blade.ts`](src/blade.ts) | bladeburner | main (hypervisor) | ✅  | Main entry-point for all bladeburner-related automation |
| [`bong.ts`](src/bong.ts) | util | daemon | 🔧 | Daemon that launches a batch of bespoke one-shot scripts once every minute (to avoid having to hog RAM by having such scripts remain running) |
| [`bootstrap.ts`](src/bootstrap.ts) | automation | main (hypervisor) | 🔧 | Start-up script to be run after every reset, which automates the vast majority of useful tasks and spawns multiple category-specific scripts |
| [`budget.ts`](src/budget.ts) | money | state | ✅ | Globally defines a budget-tracker to reserve money for specific purchases, and provides a method that excludes reserved money from the available pool. Can be run as a one-shot logger to show the current budget. |
| [`buy.ts`](src/buy.ts) | automation | main (cli) | ✅ | All-purpose CLI and API for executing purchases of TOR, stock access, and home-machine upgrades |
| [`census.ts`](src/census.ts) | util | lib+exec | ✅ | Library for exploring the reachable full-network-graph and surveying how much RAM each node possesses (as well as current used and free RAM) |
| [`custom-stats.ts`](src/custom-stats.ts) | qol | daemon | ✅ | Modifies the in-game overview window to include extra useful information |
| [`dispatch.ts`](src/dispatch.ts) | hacking | main (hypervisor) | ✅ | Automatically launches as many HGW threads as possible to target the best-candidate server for hacking; changes targets whenever a new candidate rises to the top, and launches more threads as RAM becomes available |
| [`exploiter.ts`](src/exploiter.ts) | | |  | |
| [`faction.ts`](src/faction.ts) | | |  | |
| [`factoid.ts`](src/factoid.ts) | | |  | |
| [`fcc.ts`](src/fcc.ts) | | |  | |
| [`find_server.ts`](src/find_server.ts) | | |  | |
| [`findpath.ts`](src/findpath.ts) | | |  | |
| [`global.ts`](src/global.ts) | | |  | |
| [`goto.ts`](src/goto.ts) | | |  | |
| [`graph.ts`](src/graph.ts) | | |  | |
| [`greensleeves.ts`](src/greensleeves.ts) | | |  | |
| [`grow.ts`](src/grow.ts) | | |  | |
| [`hack.ts`](src/hack.ts) | | |  | |
| [`hackman.ts`](src/hackman.ts) | | |  | |
| [`hackNet.ts`](src/hackNet.ts) | | |  | |
| [`hashnet.ts`](src/hashnet.ts) | | |  | |
| [`helper.ts`](src/helper.ts) | | |  | |
| [`jobber.ts`](src/jobber.ts) | | |  | |
| [`joblin.ts`](src/joblin.ts) | | |  | |
| [`kawaii.ts`](src/kawaii.ts) | | |  | |
| [`lgg.ts`](src/lgg.ts) | | |  | |
| [`lib`](src/lib) | | |  | |
| [`market.ts`](src/market.ts) | | |  | |
| [`maximize.ts`](src/maximize.ts) | | |  | |
| [`milestone.ts`](src/milestone.ts) | | |  | |
| [`money_helper.ts`](src/money_helper.ts) | | |  | |
| [`moneyCount.ts`](src/moneyCount.ts) | | |  | |
| [`monitor.ts`](src/monitor.ts) | | |  | |
| [`preblade.ts`](src/preblade.ts) | | |  | |
| [`rip.ts`](src/rip.ts) | | |  | |
| [`roulette.ts`](src/roulette.ts) | | |  | |
| [`rsvp.ts`](src/rsvp.ts) | | |  | |
| [`sandbox.ts`](src/sandbox.ts) | | |  | |
| [`scriptfiles.ts`](src/scriptfiles.ts) | | |  | |
| [`sentinel.ts`](src/sentinel.ts) | | |  | |
| [`server.ts`](src/server.ts) | | |  | |
| [`sleeve-man.blade.ts`](src/sleeve-man.blade.ts) | | |  | |
| [`sleeve-man.company.ts`](src/sleeve-man.company.ts) | | |  | |
| [`sleeve-man.consts.ts`](src/sleeve-man.consts.ts) | | |  | |
| [`sleeve-man.crime.ts`](src/sleeve-man.crime.ts) | | |  | |
| [`sleeve-man.faction.ts`](src/sleeve-man.faction.ts) | | |  | |
| [`stanek.ts`](src/stanek.ts) | | |  | |
| [`stock_helper.ts`](src/stock_helper.ts) | | |  | |
| [`sunset.ts`](src/sunset.ts) | | |  | |
| [`template.ts`](src/template.ts) | | |  | |
| [`threadpool.ts`](src/threadpool.ts) | | |  | |
| [`util`](src/util) | | |  | |
| [`watch.ts`](src/watch.ts) | | |  | |
| [`weaken.ts`](src/weaken.ts) | | |  | |
| [`worker.ts`](src/worker.ts) | | |  | |
| [`batch.ts`](src/Batch/batch.ts) | | |  | |
| [`arraytools.ts`](src/util/arraytools.ts) | | |  | |
| [`stringtools.ts`](src/util/stringtools.ts) | | |  | |
