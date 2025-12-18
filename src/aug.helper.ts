import { NS } from '@ns';
import { M } from './helper';

export let GraftableAugs: { live: false } | { live: true, augs: string[] } = { live: false };

export async function main(ns: NS) {
    ns.ui.openTail();
    ns.disableLog('ALL');

    for (; ;) {
        GraftableAugs = { live: true, augs: ns.grafting.getGraftableAugmentations() };

        ns.clearLog();
        ns.print("WARN: This script is keeping 'GraftableAugs' up-to-date.");
        ns.print(`Free RAM: ${ns.formatRam(ns.getServerMaxRam('home') - ns.getServerUsedRam("home"))}`);
        ns.print(`RAM after this script is killed: ${ns.formatRam(ns.getServerMaxRam('home') - (ns.getServerUsedRam("home") - ns.getScriptRam(ns.getScriptName(), 'home')))}`);

        await ns.sleep(1000)
    }
}
