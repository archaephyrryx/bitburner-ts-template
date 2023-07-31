import { NS } from "@ns";
import { nodes } from 'global';
import { canHack } from 'helper';

const MoneyThresholdRatio = 0.75 as const;
const SecurityThresholdOffset = 5 as const;

export async function hackOnce(ns: NS, server: string) {
    const maxMoney = ns.getServerMaxMoney(server);
    if (maxMoney == 0) {
        return;
    }
    const minSecurity = ns.getServerMinSecurityLevel(server);

    const moneyThresh = maxMoney * MoneyThresholdRatio;
    const securityThresh = minSecurity + SecurityThresholdOffset;

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
        await ns.sleep(100);
    }
}

async function seasons(ns: NS): Promise<void> {
    async function season(server: string) {
        await hackOnce(ns, server);
        ns.print(`=== Finished season on ${server} ===`);
    }

    for (; ;) {
        for (const node of nodes) {
            if (canHack(ns, node.name)) {
                await season(node.name);
            } else {
                await ns.sleep(100);
            }
        }
    }
}

export async function reap(ns: NS): Promise<number> {
    let accum = 0;
    for (const node of nodes) {
        if (canHack(ns, node.name)) {
            if (ns.getServerMoneyAvailable(node.name) > 0) {
                accum += (await ns.hack(node.name));
            }
        }
    }
    return accum;
}

export async function replant(ns: NS): Promise<void> {
    for (const node of nodes) {
        if (canHack(ns, node.name)) {
            await ns.grow(node.name);
            await ns.weaken(node.name);
        }
    }
}

export async function main(ns: NS): Promise<void> {
    switch (ns.args[0] ?? "reap") {
        case "reap": {
            const totalHarvest = await reap(ns);
            ns.toast(`Harvested $${totalHarvest} in total`, "success", 5000);
            return;
        }
        case "replant": {
            for (; ;) {
                await replant(ns);
            }
            return;
        }
        case "seasons": {
            await seasons(ns);
            return;
        }
        default: {
            const totalHarvest = await reap(ns);
            ns.toast(`Harvested $${totalHarvest} in total`, "success", 5000);
            return;
        }
    }
}