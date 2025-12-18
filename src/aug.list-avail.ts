import { NS } from "@ns";
import { canAfford } from "./money_helper";
import { uniqSort } from './util/arraytools';

const NeuroGov = "NeuroFlux Governor" as const;

type AugInfo = { augName: string, fromFaction: string, atPrice: number, prereqs: string[] };

export function listAvail(ns: NS, print = true, includeNeuro = false): AugInfo[] {
    const owned = ns.singularity.getOwnedAugmentations(true);

    const augSort = ((a: AugInfo, b: AugInfo) => b.atPrice - a.atPrice);
    const augEq = ((a: AugInfo, b: AugInfo) => a.augName === b.augName);

    const canGet: AugInfo[] = [];
    const canGetRaw: string[] = [];
    const delayed: AugInfo[] = [];
    const delayedRaw: string[] = [];

    const factions = ns.getPlayer().factions;

    for (const fromFaction of factions) {
        for (const augName of ns.singularity.getAugmentationsFromFaction(fromFaction)) {
            if (owned.includes(augName)) {
                if (augName.startsWith(NeuroGov)) {
                    if (!includeNeuro) continue;
                } else if (canGetRaw.includes(augName)) {
                    continue;
                } else {
                    continue;
                }
            }
            const prereqs = ns.singularity.getAugmentationPrereq(augName);
            const reqRep = ns.singularity.getAugmentationRepReq(augName);
            if (prereqs.length > 0) {
                if (ns.singularity.getFactionRep(fromFaction) >= reqRep) {
                    delayed.push({ augName, fromFaction, atPrice: ns.singularity.getAugmentationPrice(augName), prereqs });
                    if (!delayedRaw.includes(augName)) {
                        delayedRaw.push(augName);
                    }
                }
            } else {
                if (ns.singularity.getFactionRep(fromFaction) >= reqRep) {
                    canGet.push({ augName, fromFaction, atPrice: ns.singularity.getAugmentationPrice(augName), prereqs });
                    if (!canGetRaw.includes(augName)) {
                        canGetRaw.push(augName);
                    }
                }
            }
        }
    }

    const ret = uniqSort(canGet, augSort, augEq);

    if (print && ret.length > 0) {
        let shownHeader = false;
        const header = () => {
            if (!shownHeader) {
                ns.tprint(`=== Available Augmentations (Reputation Only) ===`);
                shownHeader = true;
            }
        };
        const notified: string[] = [];
        for (const augInfo of ret) {
            if (augInfo !== undefined && !notified.includes(augInfo.augName)) {
                const [now, afterSale] = canAfford(ns, augInfo.atPrice);
                if (now) {
                    header();
                    ns.tprint(`SUCCESS: Can purchase ${augInfo.augName} from ${augInfo.fromFaction} for $${ns.formatNumber(augInfo.atPrice)}!`);
                } else if (afterSale) {
                    header();
                    ns.tprint(`INFO: Could purchase ${augInfo.augName} from ${augInfo.fromFaction} after liquidating up to $${ns.formatNumber(augInfo.atPrice)}...`);
                } else {
                    header();
                    ns.tprint(`WARN: Additional funds required to purchase ${augInfo.augName} from ${augInfo.fromFaction} at base price of $${ns.formatNumber(augInfo.atPrice)}...`);
                }
                notified.push(augInfo.augName);
            }
        }
    }

    const temp: AugInfo[] = [];
    for (const aug of delayed) {
        if (aug !== undefined && aug.prereqs.every((req) => [...owned, ...canGet].includes(req))) {
            temp.push(aug);
        }
    }

    if (print && temp.length > 0) {
        let shownHeader = false;
        const header = () => {
            if (!shownHeader) {
                ns.tprint(`=== Available Augmentations (Prerequisites Installed) ===`);
                shownHeader = true;
            }
        };
        const notified: string[] = [];
        for (const augInfo of temp) {
            if (augInfo !== undefined && !notified.includes(augInfo.augName)) {
                const [now, afterSale] = canAfford(ns, augInfo.atPrice);
                if (now) {
                    header();
                    ns.tprint(`SUCCESS: Can purchase ${augInfo.augName} from ${augInfo.fromFaction} for $${ns.formatNumber(augInfo.atPrice)}!`);
                } else if (afterSale) {
                    header();
                    ns.tprint(`INFO: Could purchase ${augInfo.augName} from ${augInfo.fromFaction} after liquidating up to $${ns.formatNumber(augInfo.atPrice)}...`);
                } else {
                    header();
                    ns.tprint(`WARN: Additional funds required to purchase ${augInfo.augName} from ${augInfo.fromFaction} at base price of $${ns.formatNumber(augInfo.atPrice)}...`);
                }
                notified.push(augInfo.augName);
            }
        }
    }

    ret.push(...uniqSort(temp, augSort, augEq));
    // any further requirements cannot be met without forcing a sub-par ordering

    if (print && ret.filter((x) => x != undefined).length == 0) {
        ns.tprint(`WARN: No augmentations can be purchased at this time, try joining more factions or earning more reputation.`);
    }

    return ret.filter((x) => x !== undefined);
}


export async function main(ns: NS) {
    listAvail(ns, true);
}
