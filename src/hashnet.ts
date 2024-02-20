import { AutocompleteData, NS } from "@ns";
import { canAfford } from "./money_helper";

// const MONEY_PER_HASH = 250_000;
const HASH_FOR_MONEY = 4;

// type UpgradeKind = "level" | "ram" | "cores" | "cache";
// type Item = [Action, number, Yield];
// type Action = { kind: 'PURCHASE' } | { kind: 'UPGRADE', what: UpgradeKind, which: number } | { kind: 'EXCHANGE', nTimes: number };
// type Yield = { kind: 'production', amount: number } | { kind: 'capacity', amount: number }

export async function main(ns: NS) {
    const flags = ns.flags([["autoSpend", false], ["sellOnly", false]]);
    ns.disableLog("getServerMoneyAvailable");
    ns.disableLog("sleep");
    ns.tail();


    await crawl(ns, flags.autoSpend as boolean, flags.sellOnly as boolean);
    // await optimize(ns);
    // await spendHashes(ns);
}

async function crawl(ns: NS, autoSpend = false, sellOnly = false) {
    for (; ;) {
        const nServers = ns.hacknet.numNodes();
        const nHashes = ns.hacknet.numHashes();

        // eslint-disable-next-line @typescript-eslint/no-unused-vars
        const [capacity, _] = hashCapacityProduction(ns, nServers);

        while (nHashes >= ((autoSpend || sellOnly) ? HASH_FOR_MONEY : capacity / 2) && ns.hacknet.spendHashes("Sell for Money")) {
            continue;
        }

        ns.clearLog();
        ns.print(`Hashes: ${ns.formatNumber(nHashes, 3)}/${ns.formatNumber(capacity)} (${capacity > 0 ? ns.formatPercent(nHashes / capacity) : "N/A"})`)

        const additionalPrice = ns.hacknet.getPurchaseNodeCost();

        if (!sellOnly) {
            while (canAfford(ns, additionalPrice)[0]) {
                if (ns.hacknet.purchaseNode() == -1) {
                    break;
                }
            }
        }

        for (let i = 0; i < nServers; i++) {
            if (!sellOnly) {
                if (ns.hacknet.upgradeLevel(i)) {
                    ns.toast(`Upgraded hacknet-server-${i} level.`, "success", 1000);
                } else if (ns.hacknet.upgradeRam(i)) {
                    ns.toast(`Upgraded hacknet-server-${i} RAM.`, "success", 1000);
                } else if (ns.hacknet.upgradeCore(i)) {
                    ns.toast(`Upgraded hacknet-server-${i} cores.`, "success", 1000);
                } else if (ns.hacknet.upgradeCache(i)) {
                    ns.toast(`Upgraded hacknet-server-${i} cache.`, "success", 1000);
                }
            }
            const stats = ns.hacknet.getNodeStats(i);
            ns.print(`${stats.name}: ${ns.formatNumber(stats.production, 3)} h/s\t ${stats.level.toString().padStart(3, " ")} | ${((stats.ramUsed ?? 0) == 0) ? `${ns.formatRam(stats.ram)}` : `${ns.formatRam(stats.ram)} (used: ${ns.formatRam(stats.ramUsed ?? 0)})`} | ${stats.cores} ${stats.cache === undefined ? "" : `| ${stats.cache} (${ns.formatNumber(stats.hashCapacity ?? 0)})`}`);
        }
        await ns.sleep(100);
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
    data.flags([["sellOnly", false], ["autoSpend", false]]);
    return [];
}
