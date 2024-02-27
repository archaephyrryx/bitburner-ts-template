import { AutocompleteData, NS } from "@ns";
import { affordableCopies, canAfford } from "./money_helper";
import { BUDGET, getAvailMoney, makePurchase } from "./budget";
import { liquidate } from "./market";
import { uniqSort } from './util/arraytools';
import { mimic } from "./util/stringtools";
import { GraftableAugs } from "./aug.helper";
import { M, formatTime } from "./helper";
import { travelTo } from "./faction";
import { getWork } from "./global";
import { Cities, CityName } from "./global";

const NeuroGov = "NeuroFlux Governor" as const;
const cmds = ["list-avail", "buy-avail", "list-avail-sleeves", "buy-avail-sleeves", "helper"];

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
                    ns.tprint(` * ${augInfo.augName} for $${ns.formatNumber(augInfo.atPrice)} with ${ns.formatNumber(repReq)}`);
                }
                ns.tprint(mimic(header));
                ns.tprint("");
            }
        }
        ret.push(...tmp);
    }

    return ret;
}

function listAvail(ns: NS, print = true, includeNeuro = false): AugInfo[] {
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

async function buyFromSomeFaction(ns: NS, augName: string, repReq: number, price: number, fromFactions: string[]): Promise<string | undefined> {
    for (const faction of fromFactions) {
        const myRep = ns.singularity.getFactionRep(faction);
        if (myRep < repReq) continue;
        if (ns.gang.inGang() && ns.gang.getGangInformation().faction == faction) continue;

        const id = await BUDGET.request(ns, price, "purchase next NeuroGov augmentation");
        const [before, exact] = await BUDGET.until(ns, id);
        const ability = canAfford(ns, before + exact);
        if (ability[0] || (ability[1] && await liquidate(ns)) || (ns.isRunning("sunset.js", "home") && canAfford(ns, exact)[0])) {
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

async function buyAvail(ns: NS, neuro: boolean, dryRun = false) {
    ns.disableLog("getServerMoneyAvailable");
    outer: for (; ;) {
        const remaining = listAvail(ns, false, false).filter((x) => (x ?? false) && canAfford(ns, x.atPrice * 2)[1]);
        if (remaining.length == 0) {
            break outer;
        }

        inner: for (const augInfo of remaining) {
            augInfo.atPrice = ns.singularity.getAugmentationPrice(augInfo.augName);
            if (dryRun) {
                ns.tprint(`INFO: Would buy ${augInfo.augName} from ${augInfo.fromFaction} at estimated (unadjusted) price of $${ns.formatNumber(augInfo.atPrice)}`)
                continue inner;
            } else {
                const id = await BUDGET.request(ns, augInfo.atPrice, `purchase ${augInfo.augName}`);
                const success = await makePurchase(ns, id, (async (ns: NS) => ns.singularity.purchaseAugmentation(augInfo.fromFaction, augInfo.augName)));
                if (success) {
                    ns.tprint(`SUCCESS: Bought ${augInfo.augName} from ${augInfo.fromFaction} for $${ns.formatNumber(augInfo.atPrice)}`);
                    continue inner;
                } else {
                    ns.tprint(`ERROR: Something went wrong in budget semaphore, or ${augInfo.augName} can no longer be purchased...`);
                    continue inner;
                }
            }
        }
        if (dryRun) break outer;
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
        ["graft", []],
        ["dryRun", false],
    ]);

    if ((flags.graft as string[]).length > 0) {
        for (const graftAug of flags.graft as string[]) {
            await graftAugment(ns, graftAug);
        }
        return;
    }

    if (typeof ns.args[0] == "string") {
        const cmd = ns.args[0];
        switch (cmd) {
            case "buy-avail":
                await buyAvail(ns, flags.neuro as boolean, flags.dryRun as boolean);
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
                    listAvail(ns, true, flags.neuro as boolean);
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
            const canInstall = which.filter((ix) => ns.sleeve.getSleeve(ix).shock === 0);
            const [now, later] = affordableCopies(ns, Math.max(cost, 1));
            if (now >= canInstall.length) {
                ns.tprint(`SUCCESS: Can afford to buy all remaining instances of ${key} for sleeves (total cost: $${ns.formatNumber(canInstall.length * cost)})`)
            } else if (later >= canInstall.length) {
                ns.tprint(`INFO: Must sell stocks to afford to buy all remaining instances of ${key} for sleeves (total cost: $${ns.formatNumber(canInstall.length * cost)})`)
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
    ns.disableLog('sleep');
    outer: for (; ;) {
        const remaining = listAvailSleeves(ns, false);
        if (Object.entries(remaining).filter(([_a, [price, which]]) => canAfford(ns, price)[1]).length === 0) {
            break outer;
        }

        const order = uniqSort(Object.entries(remaining), ([_a, [a, __a]], [_b, [b, __b]]) => a - b).filter(([_a, [price, which]]) => canAfford(ns, price * which.length)[1]);

        inner: for (const augInfo of order) {
            const [augName, [augPrice, whichSleeves]] = augInfo;
            for (let i = 0; i < whichSleeves.length; i++) {
                if (ns.sleeve.getSleeve(whichSleeves[i]).shock > 0) {
                    await ns.sleep(10);
                    continue;
                }
                const id = await BUDGET.request(ns, augPrice, `purchase '${augName}' for Sleeve #${whichSleeves[i]}`);
                const success = await makePurchase(ns, id, (async (ns: NS) => ns.sleeve.purchaseSleeveAug(whichSleeves[i], augName)), false);
                if (success) {
                    ns.tprint(`SUCCESS: Bought ${augName} for sleeve ${whichSleeves[i]} for $${augPrice}`);
                    await ns.sleep(100);
                    continue inner;
                } else {
                    ns.tprint(`ERROR: Something went wrong in budget semaphore, or ${augInfo} can no longer be purchased for sleeve ${whichSleeves[i]}...`);
                    break inner;
                }
            }
        }
        await ns.sleep(10);
    }
    return;
}

// eslint-disable-next-line @typescript-eslint/no-unused-vars
export function autocomplete(data: AutocompleteData, args: string[]) {
    const graftable = (GraftableAugs.live) ? [...GraftableAugs.augs] : [];
    data.flags([["neuro", false], ["cities", false], ["graft", []]]);
    if (args.length > 0 && args[0] === "--graft") {
        return graftable.map((s) => `"${s}"`);
    }
    return cmds;
}

async function graftAugment(ns: NS, aug: string) {
    ns.tail();
    ns.disableLog('ALL');

    try {
        const price = ns.grafting.getAugmentationGraftPrice(aug);
        const time = ns.grafting.getAugmentationGraftTime(aug);

        ns.print(`WARNING: Grafting ${aug} will take ${formatTime(time / 1000)} and cost $${ns.formatNumber(price)}`);
        const [liquid, unfrozen] = await getAvailMoney(ns);
        if (unfrozen < price) {
            if (liquid >= price) {
                ns.print(`WARNING: Can afford grafting price ${price}, but only by ignoring budget constraints.`);
                ns.print(`WARNING: Will proceed to graft anyway in 5 seconds...`);
                await ns.sleep(5000);
            } else {
                ns.print(`WARNING: not enough money to graft ${aug}, will skip for now...`);
                return;
            }
        }
        for (let i = 5; i > 0; i--) {
            ns.clearLog();
            ns.print(`WARNING: You have ${i} seconds remaining before grafting will begin automatically!`);
            await ns.sleep(1000);
        }
        if (ns.getPlayer().city !== CityName.NewTokyo) {
            await travelTo(ns, "New Tokyo");
        }
        while (!ns.grafting.graftAugmentation(aug)) {
            ns.clearLog();
            ns.print(`WARNING: Unable to start grafting ${aug}, something may have gone wrong.`);
            ns.print(`INFO: If you believe this is temporary, leave this window open.`);
            await ns.sleep(M * 1000);
        }
        ns.clearLog();
        const currentTime = Date.now();
        for (; ;) {
            const work = getWork(ns);
            if (work === null || work.type !== 'GRAFTING') {
                if (ns.singularity.getOwnedAugmentations().includes(aug)) {
                    ns.tprint(`SUCCESS: Grafted ${aug}`);
                    return;
                } else {
                    ns.print(`WARNING: Stopped working, but augmentation ${aug} was not grafted. If this was not intentional, some script is clobbering grafting.`);
                    return;
                }
            } else {
                const timeElapsed = Date.now() - currentTime;
                ns.clearLog();
                ns.print(`WARN: ${formatTime(timeElapsed / 1000)} elapsed of ${formatTime(time / 1000)} (${ns.formatPercent(timeElapsed / time, 2)})`);
                await ns.sleep(1000);
            }
        }
    } catch (err) {
        ns.print(`ERROR: ${aug} does not appear to be a graftable augmentation: ${err}`);
        return;
    }
}
