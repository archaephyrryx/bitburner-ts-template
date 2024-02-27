import { AutocompleteData, NS } from '@ns';
import { canAfford } from "./money_helper";
import { H, bold, reset } from './helper';

const MONEY_PER_HASH = 250_000;
const HASH_FOR_MONEY = 4;

const TIMEFRAME = 4 * H;

// type UpgradeKind = "level" | "ram" | "cores" | "cache";
// type Item = [Action, number, Yield];
// type Action = { kind: 'PURCHASE' } | { kind: 'UPGRADE', what: UpgradeKind, which: number } | { kind: 'EXCHANGE', nTimes: number };
// type Yield = { kind: 'production', amount: number } | { kind: 'capacity', amount: number }

export async function main(ns: NS) {
    const flags = ns.flags([["autoSpend", false], ["sellOnly", false], ["upgradeCache", false]]);
    ns.disableLog("getServerMoneyAvailable");
    ns.disableLog("sleep");
    ns.tail();


    await crawl(ns, flags.autoSpend as boolean, flags.sellOnly as boolean, flags.upgradeCache as boolean);
    // await optimize(ns);
    // await spendHashes(ns);
}

async function crawl(ns: NS, autoSpend = false, sellOnly = false, upgradeCache = false) {
    for (; ;) {
        const nServers = ns.hacknet.numNodes();
        const nHashes = ns.hacknet.numHashes();

        // eslint-disable-next-line @typescript-eslint/no-unused-vars
        const [capacity, prod] = hashCapacityProduction(ns, nServers);

        while (nHashes >= ((autoSpend || sellOnly) ? HASH_FOR_MONEY : capacity / 2) && ns.hacknet.spendHashes("Sell for Money")) {
            continue;
        }

        ns.clearLog();
        ns.print(`Hashes: ${ns.formatNumber(nHashes, 3)}/${ns.formatNumber(capacity)} (${capacity > 0 ? ns.formatPercent(nHashes / capacity) : "N/A"} | ${ns.formatNumber(prod)} h/s)`)


        if (!sellOnly) {
            let additionalPrice = ns.hacknet.getPurchaseNodeCost();
            while (canAfford(ns, additionalPrice)[0]) {
                if (ns.hacknet.purchaseNode() == -1) {
                    break;
                } else {
                    additionalPrice = ns.hacknet.getPurchaseNodeCost();
                }
            }
        }

        for (let i = 0; i < nServers; i++) {
            let atMax = false;
            if (!sellOnly) {
                const shouldLvl = shouldUpgradeLevel(ns, i);
                const shouldRam = shouldUpgradeRam(ns, i);
                const shouldCore = shouldUpgradeCores(ns, i);
                if (shouldLvl && ns.hacknet.upgradeLevel(i)) {
                    ns.toast(`Upgraded hacknet-server-${i} level.`, "success", 1000);
                } else if (shouldRam && ns.hacknet.upgradeRam(i)) {
                    ns.toast(`Upgraded hacknet-server-${i} RAM.`, "success", 1000);
                } else if (shouldCore && ns.hacknet.upgradeCore(i)) {
                    ns.toast(`Upgraded hacknet-server-${i} cores.`, "success", 1000);
                } else if (upgradeCache && ns.hacknet.upgradeCache(i)) {
                    ns.toast(`Upgraded hacknet-server-${i} cache.`, "success", 1000);
                } else if (!shouldLvl && !shouldRam && !shouldCore) {
                    atMax = true;
                }
            }
            const stats = ns.hacknet.getNodeStats(i);
            ns.print(`${atMax ? bold : ""}${stats.name}: ${ns.formatNumber(stats.production, 3)} h/s\t ${stats.level.toString().padStart(3, " ")} | ${((stats.ramUsed ?? 0) == 0) ? `${ns.formatRam(stats.ram)}` : `${ns.formatRam(stats.ram)} (used: ${ns.formatRam(stats.ramUsed ?? 0)})`} | ${stats.cores} ${stats.cache === undefined ? "" : `| ${stats.cache} (${ns.formatNumber(stats.hashCapacity ?? 0)})`}${atMax ? reset : ""}`);
        }
        await ns.sleep(200);
    }
}

// export async function optimize(ns: NS) {

//     for (; ;) {
//         const nHashes = ns.hacknet.numHashes();

//         const fillTime = timeUntilMax(prod, nHashes, capacity);

//         const items = getItems(ns, nServers);
//         await ns.sleep(200);
//     }
// }

export function hashCapacityProduction(ns: NS, numNodes: number): [number, number] {
    let totalCap = 0;
    let totalProd = 0;

    for (let i = 0; i < numNodes; i++) {
        const stats = ns.hacknet.getNodeStats(i);
        totalCap += stats.hashCapacity ?? 0;
        totalProd += stats.production;
    }

    return [totalCap, totalProd];
}

// async function spendHashes(ns: NS) {
//     ns.tprint(`WARN: No logic for spending hashes programmatically other than selling...`);
//     for (; ;) {
//         if (ns.hacknet.spendHashes("Sell for Money")) {
//             await ns.sleep(10);
//         } else {
//             ns.sleep(1000);
//         }
//     }
// }

// function timeUntilMax(rate: number, current: number, capacity: number): number {
//     if (current >= capacity) {
//         return 0;
//     }
//     if (rate <= 0) {
//         throw new Error("Rate of growth is non-positive");
//     }
//     return (capacity - current) / rate;
// }

// function getItems(ns: NS, nServers: number): Item[] {
//     const items: Item[] = [];

//     for (let i = 0; i < nServers; i++) {

//     }
// }

export function autocomplete(data: AutocompleteData, args: string[]) {
    data.flags([["autoSpend", false], ["sellOnly", false], ["upgradeCache", false]]);
    return [];
}

function shouldUpgradeLevel(ns: NS, nodeIx: number): boolean {
    const plyr = ns.getPlayer();

    const stats = ns.hacknet.getNodeStats(nodeIx);
    const currentLevel = stats.level;

    const costToUpgrade = ns.hacknet.getLevelUpgradeCost(nodeIx, 1);

    // FIXME - confirm whether default should be 0 or max
    const prodAfter = ns.formulas.hacknetServers.hashGainRate(currentLevel + 1, stats.ramUsed ?? 0, stats.ram, stats.cores, plyr.mults.hacknet_node_money);
    const deltaProd = prodAfter - stats.production;

    const payoffPerSecond = deltaProd * MONEY_PER_HASH;
    const secondsUntilProfit = costToUpgrade / payoffPerSecond;

    if (secondsUntilProfit <= TIMEFRAME) {
        return true;
    } else {
        return false;
    }
}


function shouldUpgradeRam(ns: NS, nodeIx: number): boolean {
    const plyr = ns.getPlayer();

    const stats = ns.hacknet.getNodeStats(nodeIx);
    const currentLevel = stats.level;

    const costToUpgrade = ns.hacknet.getRamUpgradeCost(nodeIx, 1);

    // FIXME - confirm whether default should be 0 or max
    const prodAfter = ns.formulas.hacknetServers.hashGainRate(currentLevel, stats.ramUsed ?? 0, stats.ram * 2, stats.cores, plyr.mults.hacknet_node_money);
    const deltaProd = prodAfter - stats.production;

    const payoffPerSecond = deltaProd * MONEY_PER_HASH;
    const secondsUntilProfit = costToUpgrade / payoffPerSecond;

    if (secondsUntilProfit <= TIMEFRAME) {
        return true;
    } else {
        return false;
    }
}

function shouldUpgradeCores(ns: NS, nodeIx: number): boolean {
    const plyr = ns.getPlayer();

    const stats = ns.hacknet.getNodeStats(nodeIx);
    const currentLevel = stats.level;

    const costToUpgrade = ns.hacknet.getCoreUpgradeCost(nodeIx, 1);

    // FIXME - confirm whether default should be 0 or max
    const prodAfter = ns.formulas.hacknetServers.hashGainRate(currentLevel, stats.ramUsed ?? 0, stats.ram, stats.cores + 1, plyr.mults.hacknet_node_money);
    const deltaProd = prodAfter - stats.production;

    const payoffPerSecond = deltaProd * MONEY_PER_HASH;
    const secondsUntilProfit = costToUpgrade / payoffPerSecond;

    if (secondsUntilProfit <= TIMEFRAME) {
        return true;
    } else {
        return false;
    }
}
