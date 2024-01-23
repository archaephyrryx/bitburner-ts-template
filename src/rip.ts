import { NS } from "@ns";
import { getGraph } from "./census";
import { canHack } from "./helper";

export function listPossibleBackdoors(ns: NS): string[] {
    const ret = [];
    const nodes = getGraph(ns, true);
    for (const node of nodes) {
        const servName = node.name;
        if (!ns.serverExists(servName)) continue;
        if (ns.hasRootAccess(servName)) {
            const server = ns.getServer(servName);
            if (!(server.backdoorInstalled ?? false)) {
                ret.push(servName);
            }
        }
    }
    return ret;
}

export function rip(ns: NS): [string, number][] {
    const lucrative: [string, number][] = [];

    const nodes = getGraph(ns, true);
    for (const node of nodes) {
        const servName = node.name;
        if (!ns.serverExists(servName)) {
            continue;
        }
        if (!ns.hasRootAccess(servName)) {
            continue;
        }
        const maxMoney = ns.getServerMaxMoney(servName);
        if (maxMoney > 0) {
            lucrative.push([servName, maxMoney]);
        }
    }
    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    lucrative.sort(([_x, moneyA], [_y, moneyB]) => moneyB - moneyA);
    return lucrative;
}

export async function main(ns: NS): Promise<void> {
    listPossibleBackdoors(ns).forEach((serv) => {
        ns.tprint(`INFO: Backdoor possible on ${serv}`);
    })
    const bestServs = await rip(ns);
    if (bestServs.length === 0) {
        ns.tprint("ERROR: No servers to hack");
    } else {
        for (const serv of bestServs) {
            if (canHack(ns, serv[0])) {
                ns.tprint(`INFO: Can hack ${serv[0]} (max money $${ns.formatNumber(serv[1])})`);
            } else {
                ns.tprint(`WARN: Cannot hack ${serv[0]} (max money $${ns.formatNumber(serv[1])})`);
            }
        }
    }
}
