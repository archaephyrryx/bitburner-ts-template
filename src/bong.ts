import { NS } from '@ns';
import { M } from './helper';

enum CityName {
    Aevum = "Aevum",
    Chongqing = "Chongqing",
    Sector12 = "Sector-12",
    NewTokyo = "New Tokyo",
    Ishima = "Ishima",
    Volhaven = "Volhaven",
}

export async function main(ns: NS) {
    await everyNSeconds(ns, [
        ["greensleeves.js", ["faction"]],
        ["greensleeves.js", ["work"]],
    ], M);
}

export async function everyNSeconds(ns: NS, commands: [string, string[]][], nSeconds = M) {
    for (; ;) {
        for (const [cmd, args] of commands) {
            if (ns.exec(cmd, "home", {}, ...args) !== 0) {
                await ns.sleep(2000);
                continue;
            }
            ns.print(`WARNING: Unable to exec '${cmd} ${args.join(' ')}' automatically...`);
        }
        joinAllFactions(ns);
        await ns.sleep(nSeconds * 1000);
    }
}

export function joinAllFactions(ns: NS) {
    const outstanding = ns.singularity.checkFactionInvitations();
    for (const faction of outstanding) {
        if (!Object.values(CityName).includes(faction as CityName)) {
            ns.singularity.joinFaction(faction);
        }
    }
}
