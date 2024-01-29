import { NS } from "@ns";
import { canAfford } from "./money_helper";

type AugInfo = { augName: string, fromFaction: string, atPrice: number, prereqs: string[] };

function listAvail(ns: NS): AugInfo[] {
    const alreadyHave = ns.singularity.getOwnedAugmentations();
    const canGet: AugInfo[] = [];
    const canGetRaw: string[] = [];
    const delayed: AugInfo[] = [];
    const delayedRaw: string[] = [];
    const factions = ns.getPlayer().factions;
    for (const fromFaction of factions) {
        for (const augName of ns.singularity.getAugmentationsFromFaction(fromFaction)) {
            if (alreadyHave.findIndex((name) => name == augName) != -1) {
                continue;
            } else if (canGetRaw.findIndex((name) => name == augName) != -1) {
                continue;
            }
            const prereqs = ns.singularity.getAugmentationPrereq(augName);
            const reqRep = ns.singularity.getAugmentationRepReq(augName);
            if (prereqs.length > 0) {
                if (ns.singularity.getFactionRep(fromFaction) >= reqRep) {
                    delayed.push({ augName, fromFaction, atPrice: ns.singularity.getAugmentationBasePrice(augName), prereqs });
                    if (delayedRaw.findIndex((name => name == augName)) == -1) {
                        delayedRaw.push(augName);
                    }
                }
            } else {
                if (ns.singularity.getFactionRep(fromFaction) >= reqRep) {
                    canGet.push({ augName, fromFaction, atPrice: ns.singularity.getAugmentationBasePrice(augName), prereqs });
                    if (canGetRaw.findIndex((name => name == augName)) == -1) {
                        canGetRaw.push(augName);
                    }
                }
            }
        }

    }
    // for now, just show directly purchaseable
    ns.tprint(`=== Available Augmentations (Reputation Only) ===`);
    const notified = [];
    canGet.sort((a, b) => b.atPrice - a.atPrice);
    for (const augInfo of canGet) {
        if (notified.findIndex((n) => n == augInfo.augName) == -1) {
            const [now, afterSale] = canAfford(ns, augInfo.atPrice);
            if (now) {
                ns.tprint(`SUCCESS: Can purchase ${augInfo.augName} from ${augInfo.fromFaction} for $${ns.formatNumber(augInfo.atPrice)}!`);
            } else if (afterSale) {
                ns.tprint(`INFO: Could purchase ${augInfo.augName} from ${augInfo.fromFaction} after liquidating up to $${ns.formatNumber(augInfo.atPrice)}...`);
            } else {
                ns.tprint(`WARN: Additional funds required to purchase ${augInfo.augName} from ${augInfo.fromFaction} at base price of $${ns.formatNumber(augInfo.atPrice)}...`);
            }
            notified.push(augInfo.augName);
        }
    }
    return canGet;
}

export async function main(ns: NS) {
    if (typeof ns.args[0] == "string") {
        const cmd = ns.args[0].replaceAll(" ", "-");
        switch (cmd) {
            case "list-avail":
                listAvail(ns);
                break;
            default:
                listAvail(ns);
        }
    }
}
