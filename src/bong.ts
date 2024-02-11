import { NS } from '@ns';
import { M } from './helper';

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
                continue;
            }
            ns.tprint(`WARNING: Unable to exec '${cmd} ${args.join(' ')}' automatically...`);
        }
        joinAllFactions(ns);
        await ns.sleep(nSeconds * 1000);
    }
}

export function joinAllFactions(ns: NS) {
    const outstanding = ns.singularity.checkFactionInvitations();
    for (const faction of outstanding) {
        ns.singularity.joinFaction(faction);
    }
}
