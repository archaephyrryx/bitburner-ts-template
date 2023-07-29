import { NS } from "@ns";
import { nodes } from "global";
import { autoHack as autoCrack, canHack as canCrack } from "./helper";

export async function hackOnce(ns: NS, server: string) {
    const moneyThresh = ns.getServerMaxMoney(server) * 0.75;
    const securityThresh = ns.getServerMinSecurityLevel(server) + 5;

    let hasHacked = false;
    while (!hasHacked) {
        if (ns.getServerSecurityLevel(server) > securityThresh) {
            await ns.weaken(server);
        } else if (ns.getServerMoneyAvailable(server) < moneyThresh) {
            await ns.grow(server);
        } else {
            // Otherwise, hack it
            await ns.hack(server);
            hasHacked = true;
        }
    }
}

export function listPossibleBackdoors(ns: NS): string[] {
    const ret = [];
    for (const node of nodes) {
        const servName = node.name;
        if (ns.hasRootAccess(servName)) {
            const server = ns.getServer(servName);
            if (!(server.backdoorInstalled ?? false)) {
                ret.push(servName);
            }
        }
    }
    return ret;
}

export async function rip(ns: NS): Promise<[string, number][]> {
    const lucrative: [string, number][] = [];

    for (const node of nodes) {
        const servName = node.name;
        if (!ns.hasRootAccess(servName)) {
            if (canCrack(ns, servName)) {
                if (!autoCrack(ns, servName)) {
                    ns.tprint(`ERROR: Failed to hack ${servName}`);
                    continue;
                }
            } else {
                continue;
            }
        }
        const moneyAvailable = ns.getServerMoneyAvailable(servName);
        if (moneyAvailable > 0) {
            lucrative.push([servName, moneyAvailable]);
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
    for (; ;) {
        const bestServs = await rip(ns);
        if (bestServs.length === 0) {
            ns.tprint("ERROR: No servers to hack");
        } else {
            const [bestServ,] = bestServs[0];
            await hackOnce(ns, bestServ);
        }
        await ns.sleep(1000);
    }
}