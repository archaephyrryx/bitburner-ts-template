import { NS } from "@ns";
import { nodes } from 'global';
import { canHack } from 'helper';
import { hackOnce } from "./rip";

export async function seasons(ns: NS): Promise<void> {
    for (const node of nodes) {
        if (canHack(ns, node.name)) {
            await hackOnce(ns, node.name);
            ns.toast(`Finished season on ${node.name}`, "success", 5000);
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
        }
        case "seasons": {
            for (; ;) {
                await seasons(ns);
            }
        }
    }
}