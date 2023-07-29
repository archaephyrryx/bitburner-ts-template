import { NS } from "@ns";
import { nodes } from 'global';
import { canHack } from 'helper';

export async function skim(ns: NS): Promise<void> {
    for (const node of nodes) {
        if (canHack(ns, node.name)) {
            if (ns.getServerMoneyAvailable(node.name) > 0) {
                await ns.hack(node.name);
            }
        }
    }
}

export async function main(ns: NS): Promise<void> {
    for (; ;) {
        await skim(ns);
        await ns.sleep(60000);
    }
}