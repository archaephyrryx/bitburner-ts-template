import { NS, AutocompleteData } from '@ns';
import { selectTarget } from './monitor'
import { getRoute } from './findpath'

export async function main(ns: NS) {
    ns.tail();
    const tgt = ns.args[0] as string ?? await selectTarget(ns);
    const path = getRoute(ns, tgt);
    for (const step of path) {
        ns.singularity.connect(step);
    }
    for (; ;) {
        ns.tprint(`SUCCESS: obtained $${ns.formatNumber(await ns.singularity.manualHack())}`);
    }
}

export function autocomplete(data: AutocompleteData, args: string[]) {
    return data.servers;
}
