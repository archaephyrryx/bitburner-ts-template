import { NS } from "@ns";
import { affordableCopies, canAfford } from "./money_helper";
import { BUDGET, makePurchase } from "./budget";

type AugInfo = { augName: string, fromFaction: string, atPrice: number, prereqs: string[] };

function listAvail(ns: NS, print = true): AugInfo[] {
    const alreadyHave = ns.singularity.getOwnedAugmentations(true);
    const canGet: AugInfo[] = [];
    const canGetRaw: string[] = [];
    const delayed: AugInfo[] = [];
    const delayedRaw: string[] = [];
    const factions = ns.getPlayer().factions;
    for (const fromFaction of factions) {
        for (const augName of ns.singularity.getAugmentationsFromFaction(fromFaction)) {
            if (alreadyHave.includes(augName)) {
                continue;
            } else if (canGetRaw.includes(augName)) {
                continue;
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
                    canGet.push({ augName, fromFaction, atPrice: ns.singularity.getAugmentationBasePrice(augName), prereqs });
                    if (!canGetRaw.includes(augName)) {
                        canGetRaw.push(augName);
                    }
                }
            }
        }
    }
    canGet.sort((a, b) => b.atPrice - a.atPrice);
    if (print) {
        ns.tprint(`=== Available Augmentations (Reputation Only) ===`);
        const notified = [];
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
    }

    const temp1 = [];
    const rest1: AugInfo[] = [];
    for (const aug of delayed) {
        if (aug.prereqs.every((req) => alreadyHave.includes(req))) {
            temp1.push(aug);
        } else {
            rest1.push(aug);
        }
    }

    if (print) {
        ns.tprint(`=== Available Augmentations (Prerequisites Installed) ===`);
        const notified = [];
        for (const augInfo of temp1) {
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

    }

    canGet.push(...temp1);
    // any further requirements cannot be met without forcing a sub-par ordering
    return canGet;
}

async function buyAvail(ns: NS) {
    outer: for (; ;) {
        const remaining = listAvail(ns, false);
        if (remaining.length == 0) {
            break outer;
        }

        inner: for (const augInfo of remaining) {
            augInfo.atPrice = ns.singularity.getAugmentationPrice(augInfo.augName);
            const id = await BUDGET.request(ns, augInfo.atPrice);
            const success = await makePurchase(ns, id, (async (ns: NS) => ns.singularity.purchaseAugmentation(augInfo.fromFaction, augInfo.augName)));
            if (success) {
                ns.tprint(`SUCCESS: Bought ${augInfo.augName} from ${augInfo.fromFaction} for $${augInfo.atPrice}`);
                continue inner;
            } else {
                ns.tprint(`ERROR: Something went wrong in budget semaphore, or ${augInfo.augName} can no longer be purchased...`);
                break inner;
            }
        }
    }
    return;
}

export async function main(ns: NS) {
    if (typeof ns.args[0] == "string") {
        const cmd = ns.args.join("-");
        switch (cmd) {
            case "buy-avail":
                await buyAvail(ns);
                listAvail(ns);
                break;
            case "list-avail-sleeves":
                listAvailSleeves(ns);
                break;
            case "buy-avail-sleeves":
                await buyAvailSleeves(ns);
                break;
            case "list-avail":
            default:
                listAvail(ns);
                break;
        }
    }
}

function listAvailSleeves(ns: NS, print = true): { [k: string]: [number, number[]]; } {
    const nSleeves = ns.sleeve.getNumSleeves()
    const augIndex: { [k: string]: [number, number[]] } = {};
    for (let i = 0; i < nSleeves; i++) {
        const augs = ns.sleeve.getSleevePurchasableAugs(i);
        for (const aug of augs) {
            if (augIndex[aug.name] === undefined) {
                augIndex[aug.name] = [aug.cost, []];
            }
            augIndex[aug.name][1].push(i);
        }
    }
    if (print) {
        ns.tprint(`=== Available Sleeve Augmentations ===`);
        for (const key in augIndex) {
            const [cost, which] = augIndex[key];
            const [now, later] = affordableCopies(ns, Math.max(cost, 1));
            if (now >= which.length) {
                ns.tprint(`SUCCESS: Can afford to buy all remaining instances of ${key} for sleeves (total cost: $${ns.formatNumber(now * cost)})`)
            } else if (later >= which.length) {
                ns.tprint(`INFO: Must sell stocks to afford to buy all remaining instances of ${key} for sleeves (total cost: $${ns.formatNumber(later * cost)})`)
            } else if (now + later > 0) {
                ns.tprint(`WARNING: Can afford ${now} copies now, ${later} copies after liquidation, of ${key}: $${ns.formatNumber(now * cost)}, $${ns.formatNumber(later * cost)}`)
            } else {
                continue;
            }
        }
    }
    return augIndex;
}

async function buyAvailSleeves(ns: NS) {
    outer: for (; ;) {
        const remaining = listAvailSleeves(ns, false);
        if (Object.entries(remaining).length === 0) {
            break outer;
        }

        const order = Object.entries(remaining).toSorted(([_a, [a, __a]], [_b, [b, __b]]) => a - b);

        inner: for (const augInfo of order) {
            const [augName, [augPrice, whichSleeves]] = augInfo;
            for (let i = 0; i < whichSleeves.length; i++) {
                const id = await BUDGET.request(ns, augPrice);
                const success = await makePurchase(ns, id, (async (ns: NS) => ns.sleeve.purchaseSleeveAug(whichSleeves[i], augName)));
                if (success) {
                    ns.tprint(`SUCCESS: Bought ${augName} for sleeve ${whichSleeves[i]} for $${augPrice}`);
                    continue inner;
                } else {
                    ns.tprint(`ERROR: Something went wrong in budget semaphore, or ${augInfo} can no longer be purchased for sleeve ${whichSleeves[i]}...`);
                    break inner;
                }
            }
        }
    }
    return;
}
