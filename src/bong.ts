import { NS } from '@ns';
import { M } from './helper';
import { Cities } from './global';

export async function main(ns: NS) {
    await everyNSeconds(ns, [
        ["blade.skill.js", []],
        ["sleeve-man.faction.js", []],
        ["sleeve-man.company.js", []],
    ], M);
}

export async function everyNSeconds(ns: NS, commands: [string, string[]][], nSeconds = M) {
    for (; ;) {
        for (const [cmd, args] of commands) {
            const pid = ns.exec(cmd, "home", {}, ...args);
            while (ns.isRunning(pid)) {
                await ns.sleep(100);
            }
        }
        joinAllFactions(ns);
        await ns.sleep(nSeconds * 1000);
    }
}

export function joinAllFactions(ns: NS) {
    const outstanding = ns.singularity.checkFactionInvitations();
    for (const faction of outstanding) {
        if (!Cities.includes(faction)) {
            ns.singularity.joinFaction(faction);
        }
    }
}
