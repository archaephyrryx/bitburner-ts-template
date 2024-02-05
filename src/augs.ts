import { AutocompleteData, NS } from "@ns";
import { affordableCopies, canAfford } from "./money_helper";
import { BUDGET, makePurchase } from "./budget";
import { liquidate } from "./market";
import { uniqSort } from './util/arraytools';
import { mimic } from "./util/stringtools";

const NeuroGov = "NeuroFlux Governor" as const;
const cmds = ["list-avail", "buy-avail", "list-avail-sleeves", "buy-avail-sleeves"];

enum CityName {
    Aevum = "Aevum",
    Chongqing = "Chongqing",
    Sector12 = "Sector-12",
    NewTokyo = "New Tokyo",
    Ishima = "Ishima",
    Volhaven = "Volhaven",
}

const Cities: string[] = [...Object.values(CityName)];

type AugInfo = { augName: string, fromFaction: string, atPrice: number, prereqs: string[] };

function listCities(ns: NS, print = true): AugInfo[] {
    const alreadyHave = ns.singularity.getOwnedAugmentations(true);
    const ret: AugInfo[] = [];

    for (const city of Cities) {
        const augs = ns.singularity.getAugmentationsFromFaction(city);
        const tmp: AugInfo[] = [];
        for (const augName of augs) {
            if (alreadyHave.includes(augName)) {
                continue;
            }
            const prereqs = ns.singularity.getAugmentationPrereq(augName);
            const atPrice = ns.singularity.getAugmentationPrice(augName);
            tmp.push({ atPrice, prereqs, fromFaction: city, augName })
        }
        if (print) {
            if (tmp.length > 0) {
                const header = `=== Augmentations from ${city} ===`;
                ns.tprint(header)
                for (const augInfo of tmp) {
                    const repReq = ns.singularity.getAugmentationRepReq(augInfo.augName);
                    ns.tprint(` * ${augInfo.augName} for $${ns.formatNumber(augInfo.atPrice)} with ${repReq}`);
                }
                ns.tprint(mimic(header));
                ns.tprint("");
            }
        }
        ret.push(...tmp);
    }

    return ret;
}

function listAvail(ns: NS, print = true, includeNeuro = true): AugInfo[] {
    const owned = ns.singularity.getOwnedAugmentations(true);
    let alreadyHave;
    if (includeNeuro) {
        alreadyHave = owned.filter((a) => !a.startsWith(NeuroGov));
    } else {
        alreadyHave = owned;
    }


    const augSort = ((a: AugInfo, b: AugInfo) => b.atPrice - a.atPrice);
    const augEq = ((a: AugInfo, b: AugInfo) => a.augName === b.augName);

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
                    canGet.push({ augName, fromFaction, atPrice: ns.singularity.getAugmentationPrice(augName), prereqs });
                    if (!canGetRaw.includes(augName)) {
                        canGetRaw.push(augName);
                    }
                }
            }
        }
    }

    const ret = uniqSort(canGet, augSort, augEq);

    if (print) {
        ns.tprint(`=== Available Augmentations (Reputation Only) ===`);
        const notified = [];
        for (const augInfo of ret) {
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

    const temp = [];
    for (const aug of delayed) {
        if (aug.prereqs.every((req) => [...alreadyHave, ...canGet].includes(req))) {
            temp.push(aug);
        }
    }

    if (print) {
        ns.tprint(`=== Available Augmentations (Prerequisites Installed) ===`);
        const notified = [];
        for (const augInfo of temp) {
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

    ret.push(...uniqSort(temp, augSort, augEq));
    // any further requirements cannot be met without forcing a sub-par ordering
    return ret;
}

async function buyFromSomeFaction(ns: NS, augName: string, repReq: number, price: number, fromFactions: string[]): Promise<string | undefined> {
    for (const faction of fromFactions) {
        const myRep = ns.singularity.getFactionRep(faction);
        if (myRep < repReq) continue;

        const id = await BUDGET.request(ns, price);
        const ability = canAfford(ns, await BUDGET.until(ns, id));
        if (ability[0] || (ability[1] && await liquidate(ns))) {
            const res = await makePurchase(ns, id, (async (ns: NS) => ns.singularity.purchaseAugmentation(faction, augName)));
            if (!res) {
                ns.tprint(`ERROR: unable to purchase ${augName} from ${faction} for $${ns.formatNumber(price)}...`);
                break;
            } else {
                return faction;
            }
        }
    }
    return undefined;
}

async function buyAvail(ns: NS, neuro: boolean) {
    outer: for (; ;) {
        const remaining = listAvail(ns, false, false);
        if (remaining.filter((x) => x ?? false).length == 0) {
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

    if (neuro) {
        const factions = ns.getPlayer().factions;

        let i = 0;
        for (; ; i++) {
            const nextNGPrice = ns.singularity.getAugmentationPrice(NeuroGov);
            const nextNGRepReq = ns.singularity.getAugmentationRepReq(NeuroGov);

            const faction = await buyFromSomeFaction(ns, NeuroGov, nextNGRepReq, nextNGPrice, factions);
            if (typeof faction === "string") {
                ns.tprint(`INFO: Bought ${NeuroGov} from ${faction} for ${nextNGPrice}`);
                continue;
            } else {
                if (i > 0) {
                    ns.tprint(`SUCCESS: Bought ${i} new levels of ${NeuroGov}`);
                } else {
                    ns.tprint(`INFO: No more levels of ${NeuroGov} were bought...`);
                }
                break;
            }
        }

    }
    return;
}

export async function main(ns: NS) {
    const flags = ns.flags([
        ["neuro", false],
        ["cities", false],
    ]);

    if (typeof ns.args[0] == "string") {
        const cmd = ns.args[0];
        switch (cmd) {
            case "buy-avail":
                await buyAvail(ns, flags.neuro as boolean);
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
                if (flags.cities as boolean) {
                    listCities(ns, true);
                } else {
                    listAvail(ns, true, true);
                }
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
                ns.tprint(`SUCCESS: Can afford to buy all remaining instances of ${key} for sleeves (total cost: $${ns.formatNumber(which.length * cost)})`)
            } else if (later >= which.length) {
                ns.tprint(`INFO: Must sell stocks to afford to buy all remaining instances of ${key} for sleeves (total cost: $${ns.formatNumber(which.length * cost)})`)
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

        const order = uniqSort(Object.entries(remaining), ([_a, [a, __a]], [_b, [b, __b]]) => a - b);

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

// eslint-disable-next-line @typescript-eslint/no-unused-vars
export function autocomplete(data: AutocompleteData, args: string[]) {
    data.flags([["neuro", false], ["cities", false]]);
    return cmds;
}
